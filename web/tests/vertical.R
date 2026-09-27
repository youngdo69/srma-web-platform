source("web/load.R");source("web/tests/fixture.R")
s <- web_settings();c <- web_db(s);web_migrate(c,s)
action <- commandArgs(trailingOnly = TRUE)[[1L]]
id_file <- file.path(s$data_root,"vertical-ids.json")
if (action == "prepare") {
 web_local_identity(c)
 v <- web_import_project(c,s,"local-researcher","local-workspace",web_fixture(file.path(s$data_root,"input")))
 jsonlite::write_json(v,id_file,auto_unbox=TRUE)
 cat("Application persistence ready; project imported.\n")
} else if (action == "submit") {
 v <- jsonlite::read_json(id_file)
 j <- web_submit(c,s,"local-researcher",v$project_id,v$version_id,idempotency_key="vertical-slice")
 v$job_id <- j;jsonlite::write_json(v,id_file,auto_unbox=TRUE)
 stopifnot(web_view(c,"local-researcher",v$project_id)$jobs$state[1]=="QUEUED")
 cat("Job durably QUEUED; submission process now exits.\n")
} else if (action == "verify") {
 v <- jsonlite::read_json(id_file)
 j <- web_query(c,"SELECT * FROM jobs WHERE id=?id",id=v$job_id)
 stopifnot(j$state=="COMPLETE",j$attempts==1L)
 artifacts <- web_query(c,"SELECT * FROM artifacts WHERE job_id=?j ORDER BY logical_name",j=v$job_id)
 stopifnot(nrow(artifacts)==2L)
 dir.create(file.path(s$data_root,"downloads"),showWarnings=FALSE)
 for(i in seq_len(nrow(artifacts))) {
  dest<-file.path(s$data_root,"downloads",artifacts$logical_name[i])
  web_download(c,s,"local-researcher",v$project_id,artifacts$id[i],dest)
  stopifnot(web_sha(dest)==artifacts$checksum[i])
 }
 events <- web_query(c,"SELECT event_type,detail_json,created_at FROM audit_events WHERE object_id=?j ORDER BY created_at",j=v$job_id)
 stopifnot(all(c("JOB_QUEUED","JOB_RUNNING","JOB_COMPLETE") %in% events$event_type))
 result<-web_parse(j$result_json)
 evidence <- list(operation=j$operation,job_id=j$id,project_id=v$project_id,version_id=v$version_id,
   final_state=j$state,attempts=j$attempts,events=events,
   artifacts=artifacts[,c("id","logical_name","version","checksum","job_id","engine_id","approval_state")],
   download_paths=as.list(file.path("downloads",artifacts$logical_name)),provenance=result$provenance,
   engine_release=web_query(c,"SELECT id,source_commit,source_hash FROM engine_releases WHERE id=?id",id=j$engine_id))
 jsonlite::write_json(evidence,file.path(s$data_root,"vertical-evidence.json"),pretty=TRUE,auto_unbox=TRUE,dataframe="rows")
 cat("PASS: disconnected service; two concurrent workers; one attempt; real engine; two authorized checksum-verified downloads.\n")
} else stop("Unknown test action")
DBI::dbDisconnect(c)
