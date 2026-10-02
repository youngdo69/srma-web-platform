# Automated local development/testing only. Issues an opaque credential;
# never accepts a browser actor. The local-test session is capped at 12 hours.
source("web/load.R")
stopifnot(identical(Sys.getenv("SRMA_WEB_TEST_IDP"), "1"))
s <- web_settings(); con <- web_db(s); web_migrate(con, s); web_local_identity(con)
if (!nrow(web_query(con,"SELECT user_id FROM external_identities WHERE provider='local-test' AND subject='fixture'")))
  DBI::dbExecute(con,"INSERT INTO external_identities VALUES('local-test','fixture','local-researcher')")
p <- web_test_provider("fixture")
cat(web_sign_in(con, p$provider, p$credential, ttl = 43200), "\n")
DBI::dbDisconnect(con)
