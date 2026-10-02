# Trust boundary: the provider callback is configured by the server, never a request.
# Production adapters must verify issuer, signature/JWKS, audience, expiry, nonce,
# authorization-code + PKCE/state before returning an issuer-scoped subject.
web_identity_provider <- function(name, verify) {
  web_assert(is.character(name) && length(name) == 1L && nzchar(name) && is.function(verify))
  structure(list(name = name, verify = verify), class = "web_identity_provider")
}
web_secret <- function() {
  if (.Platform$OS.type == "windows") {
    powershell <- Sys.which("powershell.exe")
    web_assert(
      nzchar(powershell),
      "Secure session creation is unavailable.",
      "SYSTEM_FAILED"
    )

    script <- tempfile(fileext = ".ps1")
    on.exit(unlink(script), add = TRUE)

    writeLines(c(
      "$bytes = New-Object byte[] 32",
      "$rng = [System.Security.Cryptography.RandomNumberGenerator]::Create()",
      "try { $rng.GetBytes($bytes) } finally { $rng.Dispose() }",
      "($bytes | ForEach-Object { $_.ToString('x2') }) -join ''"
    ), script, useBytes = TRUE)

    value <- suppressWarnings(system2(
      powershell,
      c(
        "-NoLogo",
        "-NoProfile",
        "-NonInteractive",
        "-ExecutionPolicy", "Bypass",
        "-File", shQuote(script)
      ),
      stdout = TRUE,
      stderr = TRUE
    ))

    value <- tolower(trimws(paste(value, collapse = "")))
    web_assert(
      length(value) == 1L && grepl("^[0-9a-f]{64}$", value),
      "Secure session creation is unavailable.",
      "SYSTEM_FAILED"
    )
    return(value)
  }

  web_assert(
    file.exists("/dev/urandom"),
    "Secure session creation is unavailable.",
    "SYSTEM_FAILED"
  )
  con <- file("/dev/urandom", "rb", raw = TRUE)
  on.exit(close(con), add = TRUE)
  value <- readBin(con, "raw", n = 32L)
  web_assert(length(value) == 32L, "Secure session creation is unavailable.", "SYSTEM_FAILED")
  paste(format(value), collapse = "")
}
web_token_hash <- function(token) {
  web_assert(is.character(token) && length(token) == 1L && !is.na(token) && grepl("^[0-9a-f]{64}$", token),
    "Sign in with a valid session.", "UNAUTHENTICATED")
  digest::digest(token, algo = "sha256", serialize = FALSE)
}
web_sign_in <- function(con, provider, credential, ttl = 3600) {
  web_assert(inherits(provider, "web_identity_provider"), "Sign-in is unavailable.", "UNAUTHENTICATED")
  identity <- provider$verify(credential)
  web_assert(is.list(identity) && is.character(identity$subject) && length(identity$subject) == 1L && nzchar(identity$subject),
    "Sign-in could not be verified.", "UNAUTHENTICATED")
  # No automatic account linking by email and no automatic workspace enrollment.
  user <- web_query(con, "SELECT user_id FROM external_identities WHERE provider=?p AND subject=?s",
    p = provider$name, s = identity$subject)
  web_assert(nrow(user) == 1L, "Ask your workspace administrator to grant access.", "UNAUTHENTICATED")

  requested_ttl <- suppressWarnings(as.numeric(ttl)[1L])
  if (!is.finite(requested_ttl)) requested_ttl <- 3600
  # Production-capable providers remain capped at one hour. The local-test provider
  # may last up to 12 hours only when the explicit development test IDP is enabled.
  max_ttl <- if (identical(provider$name, "local-test") &&
                 identical(Sys.getenv("SRMA_WEB_TEST_IDP"), "1")) 43200 else 3600

  token <- web_secret(); now <- as.numeric(Sys.time())
  web_exec(con, "INSERT INTO auth_sessions VALUES(?h,?u,?p,?n,?e,0)",
    h = web_token_hash(token), u = user$user_id, p = provider$name, n = now,
    e = now + min(max_ttl, max(1, requested_ttl)))
  if (exists("web_auth_audit", mode = "function")) web_auth_audit(con, user$user_id, provider$name, "SIGN_IN")
  token
}
web_session <- function(con, token) {
  hash <- web_token_hash(token)
  value <- web_query(con, "SELECT user_id,provider,expires_at FROM auth_sessions WHERE token_hash=?h AND revoked=0 AND expires_at>?n",
    h = hash, n = as.numeric(Sys.time()))
  web_assert(nrow(value) == 1L, "Your session has ended. Sign in again.", "UNAUTHENTICATED")
  web_assert(value$provider != "local-test" || identical(Sys.getenv("SRMA_WEB_TEST_IDP"), "1"),
    "Test sign-in is disabled in this environment.", "UNAUTHENTICATED")
  structure(list(user_id = value$user_id, provider = value$provider), class = "web_session_identity")
}
web_logout <- function(con, token) {
  hash <- web_token_hash(token)
  current <- web_query(con, "SELECT user_id,provider FROM auth_sessions WHERE token_hash=?h", h = hash)
  web_exec(con, "UPDATE auth_sessions SET revoked=1 WHERE token_hash=?h", h = hash)
  if (nrow(current) == 1L && exists("web_auth_audit", mode = "function")) web_auth_audit(con, current$user_id, current$provider, "SIGN_OUT")
  invisible(TRUE)
}
# Internal operator/test provisioner. Never exposed to Shiny inputs.
web_test_provider <- function(subject) {
  web_assert(identical(Sys.getenv("SRMA_WEB_TEST_IDP"), "1"), "The test identity provider is disabled.")
  secret <- web_secret()
  provider <- web_identity_provider("local-test", function(credential) {
    web_assert(identical(credential, secret), "Sign-in could not be verified.", "UNAUTHENTICATED")
    list(subject = subject)
  })
  list(provider = provider, credential = secret)
}
web_request <- function(con, token, action, args = list(), csrf = NULL, expected_csrf = NULL, settings = NULL, upload_root = NULL) {
  identity <- web_session(con, token)
  # Only named public request fields may cross into service functions.
  # Upload size limits and other operator policy remain server-owned.
  fields <- list(
    qc_start=c("project_id","version_id"),qc_list=c("project_id"),qc_view=c("project_id","qc_id"),qc_accept=c("project_id","qc_id"),qc_resolve=c("project_id","qc_id","finding_id","decision","rationale"),qc_approve=c("project_id","qc_id","confirmed","rationale"),
    release_qc_list=c("project_id"),release_catalog=c("project_id","qc_id"),release_build=c("project_id","qc_id"),release_package_list=c("project_id"),release_download_item=c("project_id","qc_id","item_id","destination"),release_download_package=c("project_id","package_id","destination"),
    audit_list=c("project_id"),audit_export=c("project_id","destination"),security_summary=character(),logout_all=character(),
    service_status=c("project_id"),
    manuscript_catalog=c("project_id","version_id"),manuscript_view=c("project_id","version_id"),
    manuscript_plan=c("project_id","version_id","spec"),manuscript_begin=c("project_id","version_id"),
    manuscript_edit=c("project_id","version_id","section","text","decision","rationale"),
    manuscript_prepare_ai=c("project_id","version_id","section"),manuscript_accept=c("project_id","version_id","job_id"),
    manuscript_approve=c("project_id","version_id","confirmed","rationale"),
    analysis_catalog=c("project_id","version_id"),analysis_view=c("project_id","version_id"),
    analysis_plan=c("project_id","version_id","spec"),analysis_accept=c("project_id","version_id","job_id"),
    analysis_approve=c("project_id","version_id","confirmed","rationale"),
    evidence_review_proposal=c("project_id","version_id","table","row_id","decision","rationale"),
    evidence_add_row=c("project_id","version_id","row_id"),
    evidence_start=c("project_id","version_id"), evidence_view=c("project_id","version_id"),
    evidence_accept=c("project_id","version_id","job_id"),
    evidence_edit=c("project_id","version_id","table","row_id","values"),
    evidence_verify=c("project_id","version_id","table","row_id","approved","rationale"),
    evidence_upload=c("project_id","version_id","row_id","upload"),
    projects = character(), workspaces = character(), logout = character(),
    initialization_review=c("project_id","version_id","job_id"),
    manual_concepts=c("project_id","version_id","terms_yaml"),
    activate_draft=c("project_id","version_id","job_id","selections","rationale","human_approved"),
    decide_gate = c("project_id", "gate_id", "decision", "rationale"),
    view = "project_id", configuration = c("project_id", "version_id"),
    save_configuration = c("workspace_id", "values", "project_id", "version_id"),
    submit = c("project_id", "version_id", "operation", "idempotency_key", "payload"),
    download = c("project_id", "artifact_id", "destination"),
    download_strategy = c("project_id", "version_id", "database", "destination"),
    database_status = c("project_id", "version_id"),
    database_info = c("project_id", "version_id"),
    upload_database = c("project_id", "version_id", "database", "upload"))
  web_assert(is.character(action) && length(action) == 1L && !is.na(action) && action %in% names(fields),
    "This request is not available.")
  web_assert(is.list(args) && (length(args) == 0L ||
    (!is.null(names(args)) && all(!is.na(names(args)) & nzchar(names(args))) &&
     !anyDuplicated(names(args)) && all(names(args) %in% fields[[action]]))),
    "The request contains unsupported fields.")
  web_assert(is.list(args) && !any(c("actor", "user_id", "identity", "con", "settings", "upload_root", "private") %in% names(args)),
    "The request contains an unsupported identity field.")
  reads <- c("service_status","audit_list","audit_export","security_summary","release_qc_list","release_catalog","release_package_list","release_download_item","release_download_package","qc_list","qc_view","manuscript_catalog","manuscript_view","analysis_catalog","analysis_view","evidence_view", "initialization_review", "view", "projects", "workspaces", "configuration", "download", "database_info", "download_strategy", "database_status")
  if (!action %in% reads)
    web_assert(is.character(csrf) && length(csrf) == 1L && !is.null(expected_csrf) && identical(csrf, expected_csrf),
      "Refresh this page before trying again.")
  actor <- identity$user_id
  switch(action,
    service_status=do.call(web_c6_status,c(list(con=con,settings=settings,actor=actor),args)),
    audit_list=do.call(web_c5_audit_list,c(list(con=con,actor=actor),args)),
    audit_export=do.call(web_c5_audit_export,c(list(con=con,actor=actor),args)),
    security_summary=web_c5_auth_summary(con,actor),
    logout_all=web_c5_logout_all(con,actor),
    release_qc_list=do.call(web_c4_qc_list,c(list(con=con,actor=actor),args)),
    release_catalog=do.call(web_c4_catalog,c(list(con=con,settings=settings,actor=actor),args)),
    release_build=do.call(web_c4_build,c(list(con=con,settings=settings,actor=actor),args)),
    release_package_list=do.call(web_c4_package_list,c(list(con=con,settings=settings,actor=actor),args)),
    release_download_item=do.call(web_c4_download_item,c(list(con=con,settings=settings,actor=actor),args)),
    release_download_package=do.call(web_c4_download_package,c(list(con=con,settings=settings,actor=actor),args)),
    qc_start=do.call(web_c3_start,c(list(con=con,settings=settings,actor=actor),args)),
    qc_list=do.call(web_c3_list,c(list(con=con,actor=actor),args)),
    qc_view=do.call(web_c3_view,c(list(con=con,settings=settings,actor=actor),args)),
    qc_accept=do.call(web_c3_accept,c(list(con=con,settings=settings,actor=actor),args)),
    qc_resolve=do.call(web_c3_resolve,c(list(con=con,settings=settings,actor=actor),args)),
    qc_approve=do.call(web_c3_approve,c(list(con=con,settings=settings,actor=actor),args)),
    manuscript_catalog=do.call(web_c2_catalog,c(list(con=con,settings=settings,actor=actor),args)),
    manuscript_view=do.call(web_c2_view,c(list(con=con,settings=settings,actor=actor),args)),
    manuscript_plan=do.call(web_c2_plan,c(list(con=con,settings=settings,actor=actor),args)),
    manuscript_begin=do.call(web_c2_begin,c(list(con=con,settings=settings,actor=actor),args)),
    manuscript_edit=do.call(web_c2_edit,c(list(con=con,settings=settings,actor=actor),args)),
    manuscript_prepare_ai=do.call(web_c2_prepare_ai,c(list(con=con,settings=settings,actor=actor),args)),
    manuscript_accept=do.call(web_c2_accept,c(list(con=con,settings=settings,actor=actor),args)),
    manuscript_approve=do.call(web_c2_approve,c(list(con=con,settings=settings,actor=actor),args)),

    analysis_catalog=do.call(web_c1_catalog,c(list(con=con,settings=settings,actor=actor),args)),
    analysis_view=do.call(web_c1_view,c(list(con=con,settings=settings,actor=actor),args)),
    analysis_plan=do.call(web_c1_plan,c(list(con=con,settings=settings,actor=actor),args)),
    analysis_accept=do.call(web_c1_accept,c(list(con=con,settings=settings,actor=actor),args)),
    analysis_approve=do.call(web_c1_approve,c(list(con=con,settings=settings,actor=actor),args)),
    evidence_review_proposal=do.call(web_b2_review_proposal,c(list(con=con,settings=settings,actor=actor),args)),
    evidence_add_row=do.call(web_b2_add_row,c(list(con=con,settings=settings,actor=actor),args)),
    evidence_start=do.call(web_b2_start,c(list(con=con,settings=settings,actor=actor),args)),
    evidence_view=do.call(web_b2_view,c(list(con=con,settings=settings,actor=actor),args)),
    evidence_accept=do.call(web_b2_accept,c(list(con=con,settings=settings,actor=actor),args)),
    evidence_edit=do.call(web_b2_edit,c(list(con=con,settings=settings,actor=actor),args)),
    evidence_verify=do.call(web_b2_verify,c(list(con=con,settings=settings,actor=actor),args)),
    evidence_upload=do.call(web_b2_upload,c(list(con=con,settings=settings,actor=actor,upload_root=upload_root),args)),
    projects = web_query(con, "SELECT p.id,p.title,p.workspace_id FROM projects p JOIN workspace_members m ON p.workspace_id=m.workspace_id WHERE m.user_id=?u ORDER BY p.created_at DESC", u = actor),
    workspaces = web_query(con, "SELECT w.id,w.name FROM workspaces w JOIN workspace_members m ON w.id=m.workspace_id WHERE m.user_id=?u", u = actor),
    initialization_review=do.call(web_initialization_review,c(list(con=con,settings=settings,actor=actor),args)),
    manual_concepts=do.call(web_manual_concepts,c(list(con=con,settings=settings,actor=actor),args)),
    activate_draft=do.call(web_activate_draft,c(list(con=con,settings=settings,actor=actor),args)),
    view = do.call(web_view, c(list(con = con, actor = actor), args)),
    decide_gate = do.call(web_decide_gate, c(list(con = con, actor = actor), args)),
    configuration = do.call(web_edit_configuration, c(list(con = con, settings = settings, actor = actor), args)),
    save_configuration = do.call(web_save_configuration, c(list(con = con, settings = settings, actor = actor), args)),
    submit = do.call(web_submit, c(list(con = con, settings = settings, actor = actor), args)),
    download = do.call(web_download, c(list(con = con, settings = settings, actor = actor), args)),
    download_strategy = do.call(web_download_strategy, c(list(con = con, settings = settings, actor = actor), args)),
    database_status = do.call(web_database_status, c(list(con = con, settings = settings, actor = actor), args)),
    database_info = do.call(web_database_info, c(list(con = con, settings = settings, actor = actor), args)),
    upload_database = do.call(web_upload_database, c(list(con = con, settings = settings, actor = actor, upload_root = upload_root), args)),
    logout = web_logout(con, token),
    web_error("VALIDATION_FAILED", "This request is not available."))
}
