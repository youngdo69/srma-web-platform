source("web/load.R"); source("web/tests/fixture.R"); source("web/ui/app.R")
s <- web_settings(data_root = tempfile("review-ui-")); con <- web_db(s)
web_migrate(con, s); web_local_identity(con)
Sys.setenv(SRMA_WEB_TEST_IDP="1")
DBI::dbExecute(con,"INSERT INTO external_identities VALUES('local-test','reviewer','local-researcher')")
p <- web_test_provider("reviewer"); login_token <- web_sign_in(con,p$provider,p$credential)
v <- web_import_project(con,s,"local-researcher","local-workspace",web_fixture(tempfile("review-fixture-")),draft_gates=TRUE)
gate <- web_view(con,"local-researcher",v$project_id)$gates$id[1]
reject <- function(expr) {e<-tryCatch({force(expr);NULL},error=identity);stopifnot(inherits(e,"srma_web_error"))}
reject(web_request(con,login_token,"decide_gate",list(project_id=v$project_id,gate_id=gate,decision="APPROVED",rationale="Synthetic review"),"wrong","expected",s))
shiny::testServer(web_app(s), {
 session$setInputs(credential=login_token,login=1);session$flushReact()
 session$setInputs(csrf=csrf,project=v$project_id,version=v$version_id,review_gate=gate,
   review_decision="APPROVED",review_rationale="",record_review=1);session$flushReact()
 stopifnot(web_query(con,"SELECT status FROM gates WHERE id=?g",g=gate)$status=="PENDING")
 session$setInputs(review_rationale="Explicit synthetic human review; no clinical approval",record_review=2);session$flushReact()
 stopifnot(grepl("Human decision recorded",output$notice),
   web_query(con,"SELECT status FROM gates WHERE id=?g",g=gate)$status=="APPROVED")
})
reject(web_request(con,login_token,"decide_gate",list(project_id=v$project_id,gate_id=gate,decision="REJECTED",rationale="Cannot overwrite"),"ok","ok",s))
stopifnot(web_query(con,"SELECT COUNT(*) AS n FROM audit_events WHERE event_type='GATE_DECIDED' AND object_id=?g",g=gate)$n==1L,
 nrow(DBI::dbGetQuery(con,"SELECT id FROM jobs"))==0L,
 !web_registry()$generate_search$enabled, !web_registry()$retrieve_pubmed$enabled)
v2 <- web_import_project(con,s,"local-researcher","local-workspace",web_fixture(tempfile("new-review-fixture-")),draft_gates=TRUE)
stopifnot(all(web_query(con,"SELECT status FROM gates WHERE version_id=?v",v=v2$version_id)$status=="PENDING"))
DBI::dbDisconnect(con)
cat("PASS: authenticated UI records explicit version review with rationale; CSRF, immutable decision and audit verified; new gates pending; no scientific job enabled\n")
