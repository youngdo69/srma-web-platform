# Actual canonical preflight + real worker/storage on the development runtime.
# Exact locked scientific-runtime acceptance is a separate, unexecuted gate.
source("web/tests/b1_import.R")
con<-web_db(settings)
v3<-web_upload_database(con,settings,"local-researcher",v2$project_id,v2$version_id,"central",upload,upload_root)
# Test seam only: select this development R rather than asserting R 4.6.1 exists.
locked_runtime_check<-web_science_settings
web_science_settings<-function(settings,verify=TRUE)settings
job<-web_submit(con,settings,"local-researcher",v2$project_id,v2$version_id,"validate_database_inputs")
web_worker_once(con,settings)
j<-web_query(con,"SELECT * FROM jobs WHERE id=?j",j=job);stopifnot(j$state=="USER_ACTION_REQUIRED")
job2<-web_submit(con,settings,"local-researcher",v3$project_id,v3$version_id,"validate_database_inputs",idempotency_key="complete-test")
stopifnot(identical(job2,web_submit(con,settings,"local-researcher",v3$project_id,v3$version_id,"validate_database_inputs",idempotency_key="complete-test")))
web_worker_once(con,settings)
j2<-web_query(con,"SELECT * FROM jobs WHERE id=?j",j=job2)
if(j2$state!="COMPLETE")print(web_parse(j2$result_json))
stopifnot(j2$state=="COMPLETE",j2$attempts==1L)
art<-web_query(con,"SELECT * FROM artifacts WHERE job_id=?j",j=job2)
stopifnot(nrow(art)==2L,all(art$version_id==v3$version_id),all(art$approval_state=="UNREVIEWED"))
for(i in seq_len(nrow(art))){dest<-tempfile();web_download(con,settings,"local-researcher",v3$project_id,art$id[i],dest);stopifnot(web_sha(dest)==art$checksum[i])}
stopifnot(all(web_database_status(con,settings,"local-researcher",v3$project_id,v3$version_id)$Validation=="READY"),
 all(web_query(con,"SELECT status FROM gates WHERE version_id=?v",v=v3$version_id)$status=="PENDING"),
 all(web_query(con,"SELECT operation FROM jobs")$operation=="validate_database_inputs"))
web_science_settings<-locked_runtime_check
DBI::dbDisconnect(con)
cat("PASS: DEVELOPMENT R actual canonical preflight through worker, missing input action, complete inputs, artifact checksums, idempotence and pending gates; no scientific-runtime acceptance claimed\n")
