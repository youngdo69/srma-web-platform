source("web/load.R")
s <- web_settings(data_root = tempfile("request-policy-"))
con <- web_db(s); web_migrate(con, s); web_local_identity(con)
Sys.setenv(SRMA_WEB_TEST_IDP = "1")
DBI::dbExecute(con, "INSERT INTO external_identities VALUES('local-test','policy-test','local-researcher')")
provider <- web_test_provider("policy-test")
token <- web_sign_in(con, provider$provider, provider$credential)
reject_fields <- function(action, args) {
  error <- tryCatch(web_request(con, token, action, args, "nonce", "nonce", s), error = identity)
  stopifnot(inherits(error, "srma_web_error"), error$code == "VALIDATION_FAILED",
    conditionMessage(error) == "The request contains unsupported fields.")
}
# Reject before touching upload paths or engine processes; these are service-only
# options which the old generic do.call boundary would otherwise accept.
for (limit in list(Inf, 1024^3, 0))
  reject_fields("upload_database", list(project_id = "unused", version_id = "unused",
    database = "embase", upload = NULL, max_bytes = limit))
reject_fields("database_info", list(project_id = "unused", version_id = "unused", private = TRUE))
reject_fields("projects", list(actor = "other-user"))
reject_fields("view", list("unused"))
reject_fields("view", structure(list("one", "two"), names = c("project_id", "project_id")))
reject_fields("logout", list(unexpected = TRUE))
stopifnot(nrow(web_request(con, token, "workspaces")) == 1L,
  nrow(web_request(con, token, "projects")) == 0L)
web_request(con, token, "logout", csrf = "nonce", expected_csrf = "nonce")
stopifnot(inherits(tryCatch(web_session(con, token), error = identity), "srma_web_error"))
DBI::dbDisconnect(con)
cat("PASS: upload policy override, private/identity fields, unnamed and duplicate fields rejected; valid reads/logout preserved\n")
