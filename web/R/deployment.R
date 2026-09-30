web_schema_target_version <- function(settings) {
  files <- list.files(file.path(settings$engine_root, "web/db"), pattern = "^[0-9]{3}_.*[.]sql$")
  versions <- suppressWarnings(as.integer(sub("^([0-9]{3}).*$", "\\1", files)))
  max(versions[is.finite(versions)], na.rm = TRUE)
}
web_deployment_status <- function(con, settings) {
  mode <- settings$mode
  migration <- tryCatch(web_query(con, "SELECT COALESCE(MAX(version),0) AS version FROM schema_migrations")$version[1], error = function(e) 0L)
  target <- web_schema_target_version(settings)
  data_writable <- dir.exists(settings$data_root) && file.access(settings$data_root, 2L) == 0L
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
    production_enabled = FALSE
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
  if (settings$mode == "production") web_error("SYSTEM_FAILED", "Production mode remains disabled until WEB-017 final integration.")
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
