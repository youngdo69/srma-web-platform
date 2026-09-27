# Trust boundary: the provider callback is configured by the server, never a request.
# Production adapters must verify issuer, signature/JWKS, audience, expiry, nonce,
# authorization-code + PKCE/state before returning an issuer-scoped subject.
web_identity_provider <- function(name, verify) {
  web_assert(is.character(name) && length(name) == 1L && nzchar(name) && is.function(verify))
  structure(list(name = name, verify = verify), class = "web_identity_provider")
}
web_secret <- function() {
  web_assert(file.exists("/dev/urandom"), "Secure session creation is unavailable.", "SYSTEM_FAILED")
  con <- file("/dev/urandom", "rb", raw = TRUE); on.exit(close(con))
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
  token <- web_secret(); now <- as.numeric(Sys.time())
  web_exec(con, "INSERT INTO auth_sessions VALUES(?h,?u,?p,?n,?e,0)",
    h = web_token_hash(token), u = user$user_id, p = provider$name, n = now, e = now + min(3600, max(1, ttl)))
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
  web_exec(con, "UPDATE auth_sessions SET revoked=1 WHERE token_hash=?h", h = web_token_hash(token))
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
  web_assert(is.list(args) && !any(c("actor", "user_id", "identity", "con", "settings", "upload_root", "private") %in% names(args)),
    "The request contains an unsupported identity field.")
  reads <- c("view", "projects", "workspaces", "configuration", "download", "database_info", "download_strategy", "database_status")
  if (!action %in% reads)
    web_assert(is.character(csrf) && length(csrf) == 1L && !is.null(expected_csrf) && identical(csrf, expected_csrf),
      "Refresh this page before trying again.")
  actor <- identity$user_id
  switch(action,
    projects = web_query(con, "SELECT p.id,p.title,p.workspace_id FROM projects p JOIN workspace_members m ON p.workspace_id=m.workspace_id WHERE m.user_id=?u ORDER BY p.created_at DESC", u = actor),
    workspaces = web_query(con, "SELECT w.id,w.name FROM workspaces w JOIN workspace_members m ON w.id=m.workspace_id WHERE m.user_id=?u", u = actor),
    view = do.call(web_view, c(list(con = con, actor = actor), args)),
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
