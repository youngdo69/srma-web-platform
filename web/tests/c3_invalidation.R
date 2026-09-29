# Called after new manuscript, analysis and evidence versions, respectively.
history<-web_c3_view(con,s,actor,qc$project_id,qc$qc_id)
stopifnot(!history$eligibility$CURRENT,!history$eligibility$RELEASE_ELIGIBLE,
 !history$eligibility$HUMAN_QC_APPROVED,history$effective_status=="INVALIDATED",
 web_c3_context(con,s,actor,qc$project_id,qc$qc_id)$row$packet_sha256==qc_history_hash)
reject(web_c3_start(con,s,actor,approved_ms$project_id,approved_ms$version_id))
reject(web_c3_approve(con,s,actor,qc$project_id,qc$qc_id,TRUE,"Cannot inherit historical approval"))
stopifnot(nrow(web_query(con,"SELECT id FROM gates WHERE version_id=?v AND gate_type='final_qc' AND status='APPROVED'",v=mp2$version_id))==0)
if(identical(Sys.getenv("C3_VALID_FIXTURE"),"1")) {
 stopifnot(history$status=="PASSED",nrow(web_query(con,"SELECT id FROM audit_events WHERE project_id=?p AND event_type='FINAL_QC_INVALIDATED'",p=qc$project_id))>0)
}
cat("PASS: upstream revision invalidates exact-version QC; approval not inherited and immutable history preserved\n")
