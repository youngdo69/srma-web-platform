web_schema_target_version <- function(settings) {
  files <- list.files(file.path(settings$engine_root, "web/db"), pattern = "^[0-9]{3}_.*[.]sql$")
  versions <- suppressWarnings(as.integer(sub("^([0-9]{3}).*$", "\\1", files)))
  max(versions[is.finite(versions)], na.rm = TRUE)
}
web_release_acceptance_status <- function(con, settings) {
  engine <- web_register_engine(con, settings)
  lock <- web_sha(file.path(settings$engine_root, "renv.lock"))
  rows <- web_query(con, "SELECT kind,evidence_json FROM release_acceptances WHERE engine_id=?e AND lock_sha256=?h", e = engine, h = lock)
  valid <- if (!nrow(rows)) character() else rows$kind[vapply(rows$evidence_json, function(x) {
    evidence <- tryCatch(web_parse(x), error = function(e) list())
    identical(evidence$web_sha256, web_c3_code_hash(settings))
  }, logical(1))]
  list(CANONICAL_RUNTIME_ACCEPTED = "CANONICAL_RUNTIME" %in% valid,
       POSTGRESQL_ACCEPTED = "POSTGRESQL" %in% valid,
       engine_id = engine, lock_sha256 = lock, web_sha256 = web_c3_code_hash(settings))
}
web_import_release_acceptance <- function(con, settings, path) {
  web_assert(file.exists(path), "Acceptance evidence file does not exist.", "SYSTEM_FAILED")
  envelope <- jsonlite::read_json(path, simplifyVector = FALSE)
  web_assert(envelope$kind %in% c("CANONICAL_RUNTIME", "POSTGRESQL") && is.list(envelope$evidence), "Acceptance evidence format is invalid.", "SYSTEM_FAILED")
  lock <- web_sha(file.path(settings$engine_root, "renv.lock")); code <- web_c3_code_hash(settings)
  web_assert(identical(envelope$lock_sha256, lock) && identical(envelope$evidence$lock_sha256, lock), "Acceptance evidence belongs to another dependency lock.", "SYSTEM_FAILED")
  web_assert(identical(envelope$evidence$web_sha256, code), "Acceptance evidence belongs to another Web code version.", "SYSTEM_FAILED")
  if (envelope$kind == "POSTGRESQL") web_assert(isTRUE(envelope$evidence$unprivileged) && identical(as.integer(envelope$evidence$exit_code), 0L), "PostgreSQL acceptance evidence did not pass.", "SYSTEM_FAILED")
  if (envelope$kind == "CANONICAL_RUNTIME") web_assert(identical(as.integer(envelope$evidence$exit_code), 0L), "Canonical runtime acceptance evidence did not pass.", "SYSTEM_FAILED")
  engine <- web_register_engine(con, settings)
  duplicate <- web_query(con, "SELECT id FROM release_acceptances WHERE engine_id=?e AND kind=?k AND lock_sha256=?h AND evidence_json=?j", e = engine, k = envelope$kind, h = lock, j = as.character(web_json(envelope$evidence)))
  if (!nrow(duplicate)) web_exec(con, "INSERT INTO release_acceptances VALUES(?i,?e,?k,?h,?x,?t)", i = web_id(), e = engine, k = envelope$kind, h = lock, x = as.character(web_json(envelope$evidence)), t = web_now())
  invisible(TRUE)
}
web_c6_status <- function(con, settings, actor, project_id) {
  web_authorize(con, actor, project_id)
  web_deployment_status(con, settings)
}
web_deployment_status <- function(con, settings) {
  mode <- settings$mode
  migration <- tryCatch(web_query(con, "SELECT COALESCE(MAX(version),0) AS version FROM schema_migrations")$version[1], error = function(e) 0L)
  target <- web_schema_target_version(settings)
  data_writable <- dir.exists(settings$data_root) && file.access(settings$data_root, 2L) == 0L
  acceptance <- tryCatch(web_release_acceptance_status(con, settings), error = function(e) list(CANONICAL_RUNTIME_ACCEPTED = FALSE, POSTGRESQL_ACCEPTED = FALSE))
  production_ready <- settings$backend == "postgres" && isTRUE(settings$trusted_auth_proxy) && nzchar(Sys.getenv("SRMA_WEB_DATA")) && data_writable && identical(as.integer(migration), as.integer(target)) && isTRUE(acceptance$CANONICAL_RUNTIME_ACCEPTED) && isTRUE(acceptance$POSTGRESQL_ACCEPTED) && grepl("^https://", settings$public_url)
  list(
    mode = mode,
    database = if (settings$backend == "postgres") "PostgreSQL" else "SQLite development database",
    database_ready = if (mode == "development") TRUE else settings$backend == "postgres",
    organization_sign_in = if (settings$trusted_auth_proxy) "Configured" else "Not configured",
    persistent_storage = if (nzchar(Sys.getenv("SRMA_WEB_DATA")) && data_writable) "Ready" else if (mode == "development" && data_writable) "Development storage" else "Not ready",
    schema = if (identical(as.integer(migration), as.integer(target))) "Current" else paste("Migration required", migration, "→", target),
    schema_ready = identical(as.integer(migration), as.integer(target)),
    test_identity_provider = identical(Sys.getenv("SRMA_WEB_TEST_IDP"), "1"),
    public_url = settings$public_url,
    canonical_runtime_acceptance = if (isTRUE(acceptance$CANONICAL_RUNTIME_ACCEPTED)) "Accepted" else "Pending",
    postgresql_acceptance = if (isTRUE(acceptance$POSTGRESQL_ACCEPTED)) "Accepted" else "Pending",
    production_enabled = production_ready
  )
}
web_deployment_preflight <- function(con, settings) {
  web_assert(settings$mode %in% c("development", "beta", "production"), "SRMA_WEB_MODE must be development, beta or production.", "SYSTEM_FAILED")
  status <- web_deployment_status(con, settings)
  if (settings$mode == "development") return(status)
  web_assert(settings$backend == "postgres", "Beta service requires PostgreSQL. Configure SRMA_WEB_DB=postgres.", "SYSTEM_FAILED")
  web_assert(!status$test_identity_provider, "The development test identity provider must be disabled for beta service.", "SYSTEM_FAILED")
  web_assert(isTRUE(settings$trusted_auth_proxy), "Configure the trusted organization authentication proxy before beta startup.", "SYSTEM_FAILED")
  web_assert(nzchar(Sys.getenv("SRMA_WEB_DATA")) && status$persistent_storage == "Ready", "Configure writable persistent SRMA_WEB_DATA before beta startup.", "SYSTEM_FAILED")
  web_assert(isTRUE(status$schema_ready), "Database migrations are incomplete. Run the operator preflight after applying migrations.", "SYSTEM_FAILED")
  if (settings$mode == "production") {
    web_assert(grepl("^https://", settings$public_url), "Production requires an HTTPS SRMA_PUBLIC_URL behind the trusted authentication proxy.", "SYSTEM_FAILED")
    web_assert(settings$host %in% c("127.0.0.1", "localhost", "::1"), "Production Shiny must bind to loopback behind the TLS/authentication proxy.", "SYSTEM_FAILED")
    web_assert(isTRUE(status$production_enabled), "Production acceptance is incomplete. Run the operator acceptance checks for this exact code/lock and import their evidence.", "SYSTEM_FAILED")
  }
  status
}
web_proxy_subject <- function(settings, request) {
  web_assert(isTRUE(settings$trusted_auth_proxy), "Organization sign-in is not configured.", "UNAUTHENTICATED")
  value <- request[[settings$auth_header]] %||% ""
  value <- trimws(as.character(value)[1])
  web_assert(nzchar(value) && nchar(value, type = "bytes") <= 256L && !grepl("[\\r\\n]", value), "Organization sign-in was not provided by the trusted proxy.", "UNAUTHENTICATED")
  value
}
web_proxy_sign_in <- function(con, settings, request, ttl = 3600) {
  subject <- web_proxy_subject(settings, request)
  row <- web_query(con, "SELECT user_id FROM external_identities WHERE provider='trusted-proxy' AND subject=?s", s = subject)
  web_assert(nrow(row) == 1L, "Ask your workspace administrator to grant access.", "UNAUTHENTICATED")
  token <- web_secret(); now <- as.numeric(Sys.time())
  web_exec(con, "INSERT INTO auth_sessions VALUES(?h,?u,'trusted-proxy',?n,?e,0)", h = web_token_hash(token), u = row$user_id, n = now, e = now + min(3600, max(1, ttl)))
  if (exists("web_auth_audit", mode = "function")) web_auth_audit(con, row$user_id, "trusted-proxy", "SIGN_IN", list(mode = settings$mode))
  token
}
web_provision_proxy_user <- function(con, subject, display_name, workspace_id, workspace_name = "Systematic Reviews") {
  web_assert(is.character(subject) && length(subject) == 1L && nzchar(trimws(subject)) && !grepl("[\\r\\n]", subject), "Provide a valid organization subject.")
  web_assert(is.character(display_name) && length(display_name) == 1L && nzchar(trimws(display_name)), "Provide a display name.")
  web_assert(is.character(workspace_id) && length(workspace_id) == 1L && nzchar(trimws(workspace_id)), "Provide a workspace identifier.")
  existing <- web_query(con, "SELECT user_id FROM external_identities WHERE provider='trusted-proxy' AND subject=?s", s = subject)
  user <- if (nrow(existing)) existing$user_id[1] else web_id()
  if (!nrow(existing)) {
    web_exec(con, "INSERT INTO app_users(id,display_name) VALUES(?u,?n)", u = user, n = display_name)
    web_exec(con, "INSERT INTO external_identities(provider,subject,user_id) VALUES('trusted-proxy',?s,?u)", s = subject, u = user)
  }
  if (!nrow(web_query(con, "SELECT id FROM workspaces WHERE id=?w", w = workspace_id))) web_exec(con, "INSERT INTO workspaces(id,name) VALUES(?w,?n)", w = workspace_id, n = workspace_name)
  if (!nrow(web_query(con, "SELECT user_id FROM workspace_members WHERE workspace_id=?w AND user_id=?u", w = workspace_id, u = user))) web_exec(con, "INSERT INTO workspace_members VALUES(?w,?u)", w = workspace_id, u = user)
  invisible(user)
}
