source("web/load.R")
source("web/tests/fixture.R")
source("web/ui/app.R")
reject <- function(expr) {e <- tryCatch({force(expr); NULL}, error = identity); stopifnot(inherits(e, "error")); invisible(e)}
check <- function(name, expr) {force(expr); cat("PASS:", name, "\n")}
s <- web_settings(data_root = tempfile("b1-")); con <- web_db(s); web_migrate(con,s); web_migrate(con,s)
web_local_identity(con)
DBI::dbExecute(con,"INSERT INTO external_identities VALUES('local-test','alice','local-researcher')")
Sys.setenv(SRMA_WEB_TEST_IDP="1")
p <- web_test_provider("alice"); token <- web_sign_in(con,p$provider,p$credential)
check("identity: missing, invalid, actor tampering and CSRF rejected", {
 reject(web_request(con,NULL,"projects"));reject(web_request(con,"local-researcher","projects"))
 reject(web_request(con,paste(rep("a",64),collapse=""),"projects"))
 reject(web_request(con,token,"projects",list(actor="local-researcher")))
 reject(web_request(con,token,"logout",csrf="wrong",expected_csrf="right"))
 Sys.unsetenv("SRMA_WEB_TEST_IDP");reject(web_session(con,token));Sys.setenv(SRMA_WEB_TEST_IDP="1")
 stopifnot(web_session(con,token)$user_id == "local-researcher",nrow(web_request(con,token,"workspaces"))==1)
})
values <- list("project.title"="Synthetic B1 structural review", "project.project_id"="b1-fixture",
 "research_question.framework"="PICOS", "research_question.question"="Synthetic question",
 "research_question.population.description"="Synthetic population", "research_question.intervention.description"="Synthetic intervention",
 "research_question.comparator.description"="Synthetic comparator", "research_question.outcomes.primary"=list("Synthetic outcome"),
 "eligibility.inclusion"=list("Synthetic inclusion"),"eligibility.exclusion"=list("Synthetic exclusion"),
 "eligibility.study_designs.include"=list("Synthetic design"),"search.search_start_date"="","search.search_end_date"="2026-09-27",
 databases=list("pubmed","embase","central"),exclusion_reason="Synthetic ineligibility")
check("wizard: actual canonical configuration validation and immutable version", {
 v <- web_request(con,token,"save_configuration",list(workspace_id="local-workspace",values=values),"csrf","csrf",s)
 old <- web_version_root(con,s,"local-researcher",v$project_id,v$version_id)
 cfg <- yaml::read_yaml(file.path(old$root,"config/project_config.yml"))
 stopifnot(cfg$research_question$framework=="PICOS",cfg$project$title==values$project.title,nrow(web_view(con,"local-researcher",v$project_id)$gates)==4)
 values$project.title <- "Edited synthetic review"
 v2 <- web_save_configuration(con,s,"local-researcher","local-workspace",values,v$project_id,v$version_id)
 stopifnot(v2$project_id==v$project_id,v2$version_id!=v$version_id,web_sha(file.path(old$root,"config/project_config.yml"))==old$manifest[["config/project_config.yml"]])
})
check("database enforces immutable version and gate records", {
 reject(web_exec(con,"UPDATE project_versions SET config_hash='changed' WHERE id=?v",v=v$version_id))
 reject(web_exec(con,"DELETE FROM project_versions WHERE id=?v",v=v$version_id))
 reject(web_exec(con,"DELETE FROM gates WHERE project_id=?p",p=v$project_id))
})
check("wizard: duplicate ID, malformed config, scope and date rejected", {
 reject(web_save_configuration(con,s,"local-researcher","local-workspace",values))
 bad <- values;bad$project.project_id <- "../escape";reject(web_save_configuration(con,s,"local-researcher","local-workspace",bad))
 reject(web_save_configuration(con,s,"unknown","local-workspace",values))
 bad<-values;bad$search.search_start_date<-"2026-99-99";reject(web_save_configuration(con,s,"local-researcher","local-workspace",bad))
})
check("session logout and expiry", {
 web_logout(con,token);reject(web_session(con,token))
 expired <- web_sign_in(con,p$provider,p$credential)
 web_exec(con,"UPDATE auth_sessions SET expires_at=0 WHERE token_hash=?h",h=web_token_hash(expired));reject(web_session(con,expired))
})
check("new shell construction without fixed actor",stopifnot(inherits(web_app(s),"shiny.appobj")))
DBI::dbDisconnect(con)
cat("B1 tests complete\n")
