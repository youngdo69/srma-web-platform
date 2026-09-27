# Operator-only staged acceptance. Never invoked by the UI or ordinary tests.
# No credentials in arguments/files: inherit operator environment only.
source("web/load.R")
a<-commandArgs(trailingOnly=TRUE)
web_assert(length(a)==2L && a[1] %in% c("prepare","complete"),
 "Usage: Rscript --vanilla web/tests/b1_operator_acceptance.R prepare <non-sensitive-project-dir> OR complete <explicit-review.json>")
web_assert(nzchar(Sys.getenv("SRMA_WEB_DATA")),"Set SRMA_WEB_DATA to a dedicated acceptance directory.")
s<-web_settings();s$timeout<-180
web_pipeline_prerequisites(s,"initialize_project")
con<-web_db(s);web_migrate(con,s);web_local_identity(con)
state_file<-file.path(s$data_root,"b1_acceptance.json")
run<-function(version,operation,key) {
 id<-web_submit(con,s,"local-researcher",version$project_id,version$version_id,operation,idempotency_key=key)
 job<-web_query(con,"SELECT * FROM jobs WHERE id=?j",j=id)
 if(job$state=="QUEUED")web_worker_once(con,s)
 job<-web_query(con,"SELECT * FROM jobs WHERE id=?j",j=id)
 web_assert(job$state=="COMPLETE",paste("Acceptance stopped:",operation,job$state,job$message))
 web_assert(job$attempts==1L,"Acceptance needs exactly one engine attempt.")
 list(id=id,result=web_parse(job$result_json))
}
if(a[1]=="prepare") {
 web_assert(!file.exists(state_file)&&nrow(web_query(con,"SELECT id FROM projects"))==0L,
   "Preparation requires an empty dedicated acceptance database. Existing state is preserved.")
 v<-web_import_project(con,s,"local-researcher","local-workspace",normalizePath(a[2],mustWork=TRUE),configuration_only=TRUE,draft_gates=TRUE)
 # Save version before external work, so interruption cannot lose its identity.
 jsonlite::write_json(list(version=v),state_file,auto_unbox=TRUE,pretty=TRUE)
 job<-run(v,"initialize_project","b1-acceptance-initialize")
 review<-web_initialization_review(con,s,"local-researcher",v$project_id,v$version_id,job$id)
 jsonlite::write_json(list(version=v,initialization_job=job$id,review=review),state_file,auto_unbox=TRUE,pretty=TRUE)
 cat("Canonical draft registered. Inspect b1_acceptance.json and explicitly resolve every listed decision; no approval has been recorded.\n")
} else {
 state<-jsonlite::read_json(state_file,simplifyVector=FALSE)
 approval<-jsonlite::read_json(a[2],simplifyVector=FALSE)
 web_no_secrets(approval,s$engine_root)
 web_assert(identical(approval$project_id,state$version$project_id)&&identical(approval$draft_version_id,state$version$version_id)&&
   identical(approval$draft_job_id,state$initialization_job),"Review must identify this exact saved draft and initialization job.")
 web_assert(isTRUE(approval$human_approved)&&isTRUE(approval$protocol_approved)&&
   is.character(approval$protocol_reference)&&nzchar(trimws(approval$protocol_reference)),
   "A real explicit human decision and truthful protocol/test reference are required; do not fabricate registration.")
 v<-web_activate_draft(con,s,"local-researcher",state$version$project_id,state$version$version_id,
   approval$selections,approval$rationale,approval$human_approved,state$initialization_job)
 gates<-web_query(con,"SELECT id FROM gates WHERE project_id=?p AND version_id=?v AND gate_type='protocol_registration' AND status='PENDING'",p=v$project_id,v=v$version_id)
 for(g in gates$id)web_decide_gate(con,"local-researcher",v$project_id,g,"APPROVED",approval$protocol_reference)
 generated<-run(v,"generate_search","b1-acceptance-search")
 web_assert(identical(generated$result$provenance$search_quality,"PASS"),"Canonical quality did not PASS. No retrieval is permitted.")
 quality_artifact<-Filter(function(x)identical(x$logical_name,"search_quality_report.yml"),generated$result$artifacts)
 web_assert(length(quality_artifact)==1L,"Canonical quality evidence is missing.")
 row<-web_query(con,"SELECT * FROM artifacts WHERE id=?a AND project_id=?p AND version_id=?v",a=quality_artifact[[1]]$artifact_id,p=v$project_id,v=v$version_id)
 quality<-yaml::read_yaml(web_store(s)$get(row$storage_locator,row$checksum))$search_quality
 count<-quality$final_pubmed_count
 # Acceptance-test resource bound only; never edits canonical QC or retrieval logic.
 web_assert(is.numeric(count)&&length(count)==1L&&is.finite(count)&&count>0&&count<=100,
   "Acceptance uses a non-sensitive narrow review with 1–100 canonical PubMed matches. Choose a new test draft; do not alter a generated query.")
 retrieved<-run(v,"retrieve_pubmed","b1-acceptance-pubmed")
 web_assert(identical(generated$result$provenance$query_sha256,retrieved$result$provenance$query_sha256)&&
   identical(retrieved$result$provenance$project_version_id,v$version_id)&&
   generated$id %in% unlist(retrieved$result$provenance$source_job_ids),"Exact query/version provenance mismatch.")
 artifacts<-web_query(con,"SELECT * FROM artifacts WHERE job_id=?j AND project_id=?p AND version_id=?v",j=retrieved$id,p=v$project_id,v=v$version_id)
 web_assert(nrow(artifacts)>=3L&&all(artifacts$approval_state=="UNREVIEWED"),"Expected raw records and retrieval report were not registered.")
 for(i in seq_len(nrow(artifacts)))web_store(s)$get(artifacts$storage_locator[i],artifacts$checksum[i])
 jsonlite::write_json(list(verdict="CREDENTIALED_ACCEPTANCE_PASS",version=v,initialization_job=state$initialization_job,
   generated=generated,retrieved=retrieved,artifacts=artifacts[,c("logical_name","checksum","version_id")],
   bounded_matches=count,no_duplicate_execution=TRUE),file.path(s$data_root,"b1_credentialed_evidence.json"),auto_unbox=TRUE,pretty=TRUE,dataframe="rows")
 cat("PASS: actual canonical initialization, explicit human review, QC PASS, exact-query PubMed retrieval and checksummed raw artifacts. No merge or screening started.\n")
}
DBI::dbDisconnect(con)
