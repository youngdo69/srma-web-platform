# Bounded REAL provider acceptance. Requires a prepared non-private review version.
# Rscript --vanilla web/tests/b2_operator_acceptance.R ACTOR PROJECT VERSION OPERATION
source("web/load.R")
a<-commandArgs(TRUE);stopifnot(length(a)==4L,a[4]%in%names(web_b2_ai_operations()))
s<-web_settings();s$timeout<-min(600L,max(30L,s$timeout));con<-web_db(s)
web_migrate(con,s)
stopifnot(nrow(web_query(con,"SELECT id FROM jobs WHERE state IN ('QUEUED','RUNNING')"))==0L)
before<-web_b2_context(con,s,a[1],a[2],a[3])$packet
job<-web_submit(con,s,a[1],a[2],a[3],a[4]);web_worker_once(con,s)
j<-web_query(con,"SELECT state,result_json FROM jobs WHERE id=?j",j=job)
if(j$state!="COMPLETE"){cat("NOT PASS: provider operation did not complete; inspect authorized operator diagnostics.\n");DBI::dbDisconnect(con);quit(status=1)}
v<-web_b2_accept(con,s,a[1],a[2],a[3],job);after<-web_b2_context(con,s,a[1],a[2],v$version_id)$packet
human<-names(before$tables)[!startsWith(names(before$tables),"AI_")]
stopifnot(identical(before$tables[human],after$tables[human]))
proposals<-unlist(after$tables[startsWith(names(after$tables),"AI_")],recursive=FALSE)
stopifnot(length(proposals)>0,all(vapply(proposals,function(r)identical(r$.verification,"UNVERIFIED"),logical(1))))
stopifnot(all(web_query(con,"SELECT status FROM gates WHERE version_id=?v",v=v$version_id)$status=="PENDING"))
DBI::dbDisconnect(con);cat("PASS: actual provider proposals, preserved Human decisions and pending gates. Review semantic accuracy separately.\n")
