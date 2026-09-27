source("web/load.R");source("web/ui/app.R")
s<-web_settings(data_root=tempfile("b1-ui-"));con<-web_db(s);web_migrate(con,s);web_local_identity(con)
DBI::dbExecute(con,"INSERT INTO external_identities VALUES('local-test','wizard-test','local-researcher')")
Sys.setenv(SRMA_WEB_TEST_IDP="1");provider<-web_test_provider("wizard-test");login_token<-web_sign_in(con,provider$provider,provider$credential)
shiny::testServer(web_app(s), {
 session$setInputs(credential=login_token,login=1);session$flushReact()
 session$setInputs(csrf=csrf,workspace="local-workspace",title="Synthetic UI review",canonical_id="synthetic-ui",
  investigator="",framework="PICOS",question="Synthetic question",population="Synthetic population",intervention="Synthetic intervention",
  comparator="Synthetic comparator",primary="Synthetic primary outcome",secondary="",designs="Synthetic design",
  inclusion="Synthetic inclusion",exclusion="Synthetic exclusion",reason="Synthetic ineligibility",language="No restriction",
  databases=c("pubmed","embase"),inception=TRUE,start_date="",end_date="2026-09-27",save=1)
 session$flushReact()
 stopifnot(grepl("Validated draft saved",output$notice))
 project<-DBI::dbGetQuery(con,"SELECT id FROM projects")$id
 version<-DBI::dbGetQuery(con,"SELECT id FROM project_versions")$id
 session$setInputs(project=project,version=version);session$flushReact()
 stopifnot(grepl("Synthetic UI review",output$overview),grepl("Current Action",output$workspace_ui$html))
 session$setInputs(edit=1);session$flushReact()
 session$setInputs(title="Edited synthetic UI review",save=2);session$flushReact()
 stopifnot(DBI::dbGetQuery(con,"SELECT COUNT(*) AS n FROM project_versions")$n==2L,
  DBI::dbGetQuery(con,"SELECT COUNT(*) AS n FROM gates")$n==8L)
 session$setInputs(preflight=1);session$flushReact()
 stopifnot(grepl("scientific processing environment needs maintenance",output$notice),DBI::dbGetQuery(con,"SELECT COUNT(*) AS n FROM jobs")$n==0L)
 # Existing canonical scalar PICO and boolean database formats must resume too.
 source("web/tests/fixture.R")
 legacy<-web_import_project(con,s,"local-researcher","local-workspace",web_fixture(tempfile("legacy-")))
 session$setInputs(project=legacy$project_id,version=legacy$version_id);session$flushReact()
 session$setInputs(edit=2);session$flushReact()
 session$setInputs(canonical_id=web_query(con,"SELECT canonical_id FROM projects WHERE id=?id",id=legacy$project_id)$canonical_id,
   title="Edited legacy synthetic review",save=3);session$flushReact()
 stopifnot(grepl("Validated draft saved",output$notice),
   web_query(con,"SELECT COUNT(*) AS n FROM project_versions WHERE project_id=?id",id=legacy$project_id)$n==2L)
})
DBI::dbDisconnect(con)
cat("PASS: authenticated Shiny wizard saves, overview resumes, edit creates version, review gates persist, unvalidated preflight fails closed\n")
