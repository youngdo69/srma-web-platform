# Focused recovery of a previously completed B2/C1 fixture, without rerunning statistics.
.libPaths(c(Sys.getenv("C1_TEST_LIBRARY"),file.path(dirname(Sys.getenv("C1_TEST_LIBRARY")),"library"),.libPaths()))
source("web/load.R");source("web/tests/fixture.R")
state<-readRDS(Sys.getenv("C2_FIXTURE_STATE"));s<-state$settings;s$engine_root<-getwd();s$sqlite_file<-Sys.getenv("C2_FIXTURE_DB");con<-web_db(s);web_migrate(con,s)
reject<-function(expr){e<-tryCatch({force(expr);NULL},error=identity);stopifnot(inherits(e,"error"));invisible(e)}
actor<-"local-researcher";ready<-state$ready;plan<-state$plan;res<-state$result;primary<-state$approved;approved<-primary;spec<-state$spec;j<-state$job
all<-web_query(con,"SELECT version_id FROM analysis_versions WHERE project_id=?p AND stage='APPROVED'",p=primary$project_id)$version_id
sens<-Filter(function(v)web_c1_context(con,s,actor,primary$project_id,v)$packet$plan$role=="SENSITIVITY",all);stopifnot(length(sens)==1);sens_approved<-list(project_id=primary$project_id,version_id=sens[[1]])
other<-list(project_id=web_query(con,"SELECT id FROM projects WHERE canonical_id='c1-other'")$id)
Sys.setenv(SRMA_WEB_TEST_IDP="1");idp<-web_test_provider(actor);token<-web_sign_in(con,idp$provider,idp$credential);csrf<-web_secret()
req<-function(action,args=list(),auth=token,csrf_value=csrf)web_request(con,auth,action,args,csrf_value,csrf,s)
original_science<-web_science_settings;web_science_settings<-function(settings,verify=TRUE)settings
tryCatch(source("web/tests/c2_reconciliation.R"),finally={web_science_settings<-original_science;DBI::dbGetQuery(con,"PRAGMA wal_checkpoint(TRUNCATE)");DBI::dbDisconnect(con)})
