# Reuse canonical approval, submission and result registration at the exact
# version reached by a Human-verified correction.
web_qc_followup_job_key <- function(version_id,operation) {
  if(!operation %in% c("run_meta_analysis","generate_manuscript_docx"))return("")
  paste0("qc-followup-",web_c2_hash(list(version_id,operation)))
}

web_qc_followup_ancestor <- function(con,project_id,evidence_version) {
  web_query(con,paste0("WITH RECURSIVE lineage(version_id,parent_version_id,stage) AS (",
    "SELECT version_id,parent_version_id,stage FROM evidence_versions WHERE project_id=?p AND version_id=?v ",
    "UNION SELECT e.version_id,e.parent_version_id,e.stage FROM evidence_versions e ",
    "JOIN lineage l ON l.parent_version_id=e.version_id WHERE e.project_id=?p) ",
    "SELECT * FROM lineage"),p=project_id,v=evidence_version)
}

web_qc_followup_markers <- function(packet) {
  markers<-Filter(function(a)identical(a$event,"QC_CONFIRMATION_DRAFT"),packet$history)
  ids<-vapply(markers,function(a)web_b2_nr_one(a$row_id),character(1))
  markers[!nzchar(ids) | !duplicated(ids,fromLast=TRUE)]
}

web_qc_followup_record <- function(settings,project_id,marker) {
  q<-marker$confirmation
  scope<-web_b2_nr_one(q$scope)
  id<-web_b2_nr_one(q$event_id)
  if(!grepl("^[a-f0-9]{64}$",scope) || !grepl("^[a-f0-9]{32}$",id))return(NULL)
  path<-web_inside(file.path(settings$data_root,"qc-confirmation",scope,paste0(id,".json")),
    settings$data_root,exists=FALSE)
  if(!file.exists(path) || dir.exists(path) || file.info(path)$size>256L*1024L)return(NULL)
  envelope<-jsonlite::read_json(path,simplifyVector=FALSE)
  e<-envelope$event
  if(!identical(envelope$sha256,web_c2_hash(e)) || !identical(scope,web_c2_hash(e$binding)) ||
     !identical(e$evidence_sha256,web_c2_hash(e$evidence)) ||
     !identical(e$id,id) || !identical(e$decision,"MODIFY") ||
     !identical(e$binding$project_id,project_id) ||
     !identical(e$binding$evidence_version_id,marker$source_version) ||
     !identical(e$actor,marker$actor))return(NULL)
  if(nzchar(web_b2_nr_one(e$item_id))) {
    log<-web_qc_confirmation_log(settings,list(scope=scope,binding=e$binding))
    same<-Filter(function(a)identical(a$item_id,e$item_id),log$events)
    if(!length(same) || !identical(same[[length(same)]]$id,id))return(NULL)
  }
  e
}

web_qc_followup_marker_proof <- function(con,settings,project_id,versions,marker,rows,read_evidence) {
  rows<-Filter(function(r)identical(r$.row_id,marker$row_id) &&
    r$verification_status %in% c("Verified","Corrected") && nzchar(web_b2_nr_one(r$.verified_by)) &&
    !identical(r$.verified_by,r$.actor) && !identical(r$.verified_by,marker$actor),rows)
  if(length(rows)!=1L)return(NULL)
  e<-web_qc_followup_record(settings,project_id,marker)
  if(is.null(e))return(NULL)
  manuscript<-web_query(con,paste0("SELECT version_id FROM manuscript_versions ",
    "WHERE project_id=?p AND version_id=?m AND evidence_version_id=?e"),
    p=project_id,m=e$binding$manuscript_version_id,e=marker$source_version)
  draft<-versions$version_id[versions$parent_version_id==marker$source_version & versions$stage=="EXTRACTION"]
  if(nrow(manuscript)!=1L || length(draft)!=1L)return(NULL)
  d<-read_evidence(draft)
  if(!any(vapply(d$history,identical,logical(1),y=marker)))return(NULL)
  choice<-web_qc_bound_revision_choices(list(family=e$family,sources=e$evidence))[[marker$confirmation$choice_id]]
  row<-rows[[1L]]
  if(!is.list(choice) || !identical(choice$row_id,row$.row_id) ||
     !identical(choice$report,row$screening_id) || !identical(choice$sha256,row$.pdf_sha256))return(NULL)
  recorded<-Filter(function(x)identical(x$.row_id,choice$row_id) && identical(x$screening_id,choice$report) &&
    identical(x$.pdf_sha256,choice$sha256) && identical(x$source_location,paste("PDF page",choice$page)) &&
    identical(x$source_evidence,choice$excerpt),d$tables$Target_Outcome_Verification)
  if(length(recorded)!=1L)return(NULL)
  list(event=e,draft_version=draft,row=row,choice=choice)
}

web_qc_followup_affected <- function(con,settings,actor,project_id,ev,row_ids) {
  metadata<-web_qc_followup_markers(ev$packet)
  candidates<-Filter(function(x)web_b2_nr_one(x$outcome_row_id) %in% row_ids &&
    identical(x$human_include_in_planned_analysis,"Yes"),ev$packet$tables$Target_Outcome_Verification)
  if(!length(metadata) || !length(candidates))return(character())
  versions<-web_qc_followup_ancestor(con,project_id,ev$row$version_id)
  read_evidence<-function(draft)web_b2_context(con,settings,actor,project_id,draft)$packet
  matches<-lapply(tail(metadata,32L),function(marker)
    web_qc_followup_marker_proof(con,settings,project_id,versions,marker,candidates,read_evidence)$row$outcome_row_id)
  unique(as.character(unlist(matches,use.names=FALSE)))
}

web_qc_followup_eligible <- function(con,settings,actor,project_id,version_id,operation) {
  if(identical(operation,"run_meta_analysis")) {
    plan<-web_c1_context(con,settings,actor,project_id,version_id,TRUE)$packet
    if(!identical(plan$stage,"PLAN"))return(FALSE)
    evidence<-web_b2_context(con,settings,actor,project_id,plan$evidence_version)
    return(length(web_qc_followup_affected(con,settings,actor,project_id,evidence,unlist(plan$plan$row_ids)))>0L)
  }
  if(!identical(operation,"generate_manuscript_docx"))return(FALSE)
  m<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE)$packet
  if(!identical(m$stage,"HUMAN_EDITED") || identical(m$source$route,"NARRATIVE") ||
     !setequal(names(m$sections),web_c2_sections()) ||
     !all(vapply(m$sections,function(x)identical(x$status,"APPROVED"),logical(1))))return(FALSE)
  for(v in names(m$source$analyses)) {
    a<-web_c1_context(con,settings,actor,project_id,v,TRUE)$packet
    if(!identical(a$stage,"APPROVED") || !identical(a$evidence_version,m$evidence_version) ||
       !identical(web_c2_hash(a),web_c2_hash(m$source$analyses[[v]])))next
    evidence<-web_b2_context(con,settings,actor,project_id,m$evidence_version)
    if(length(web_qc_followup_affected(con,settings,actor,project_id,evidence,unlist(a$plan$row_ids))))return(TRUE)
  }
  FALSE
}

web_qc_followup_queue <- function(con,settings,actor,project_id,version_id,operation) {
  tryCatch({
    if(!web_qc_followup_eligible(con,settings,actor,project_id,version_id,operation))return(NULL)
    key<-web_qc_followup_job_key(version_id,operation)
    # Submission and worker both enforce the existing stage and approval guards.
    job<-web_submit(con,settings,actor,project_id,version_id,operation,idempotency_key=key)
    state<-web_scoped_job(con,actor,project_id,version_id,job,operation)$state
    if(identical(state,"COMPLETE"))web_qc_followup_register(con,settings,actor,project_id,version_id,job,operation)
    list(job_id=job,worker_state=state)
  },error=function(e)list(worker_state="FOLLOWUP_PENDING",reason=if(inherits(e,"srma_web_error"))e$code else "SYSTEM_FAILED"))
}

web_qc_followup_register <- function(con,settings,actor,project_id,version_id,job_id,operation) {
  if(identical(operation,"run_meta_analysis"))
    return(web_c1_accept(con,settings,actor,project_id,version_id,job_id))
  if(identical(operation,"generate_manuscript_docx")) {
    prior<-web_query(con,paste0("SELECT version_id FROM manuscript_versions WHERE project_id=?p ",
      "AND parent_version_id=?v AND source_job_id=?j AND stage='REVIEW_REQUIRED'"),p=project_id,v=version_id,j=job_id)
    if(nrow(prior)==1L)return(list(project_id=project_id,version_id=prior$version_id))
    return(web_c2_accept(con,settings,actor,project_id,version_id,job_id))
  }
  NULL
}

web_qc_followup_decide_gate <- function(con,settings,actor,project_id,gate_id,decision,rationale="") {
  if(identical(decision,"APPROVED")) {
    gate<-web_query(con,"SELECT gate_type FROM gates WHERE id=?g AND project_id=?p",g=gate_id,p=project_id)
    web_assert(!any(gate$gate_type=="manuscript_review"),
      "Use the manuscript author approval action for the frozen document.")
  }
  result<-web_decide_gate(con,actor,project_id,gate_id,decision,rationale)
  if(is.null(settings) || !identical(decision,"APPROVED"))return(result)
  gate<-web_query(con,paste0("SELECT version_id FROM gates WHERE project_id=?p AND id=?g ",
    "AND gate_type='analysis_plan' AND status='APPROVED' AND reviewer_id=?a"),p=project_id,g=gate_id,a=actor)
  if(nrow(gate)!=1L)return(result)
  list(gate=result,followup=web_qc_followup_queue(con,settings,actor,project_id,gate$version_id,"run_meta_analysis"))
}

web_qc_followup_manuscript_edit <- function(con,settings,actor,project_id,version_id,section,text,decision,rationale) {
  saved<-web_c2_edit(con,settings,actor,project_id,version_id,section,text,decision,rationale)
  if(identical(decision,"APPROVE"))
    saved$followup<-web_qc_followup_queue(con,settings,actor,project_id,saved$version_id,"generate_manuscript_docx")
  saved
}

web_qc_followup_worker_complete <- function(con,settings,actor,project,job) {
  operation<-job$operation
  key<-web_qc_followup_job_key(job$version_id,operation)
  outcome<-tryCatch({
    web_assert(identical(job$state,"COMPLETE") && nzchar(key) && identical(key,job$idempotency_key) &&
      web_qc_followup_eligible(con,settings,actor,job$project_id,job$version_id,operation),
      "This completed job no longer has a current confirmed source.","STALE_UPSTREAM_VERSION")
    web_qc_followup_register(con,settings,actor,job$project_id,job$version_id,job$id,operation)
  },error=function(e)list(error=if(inherits(e,"srma_web_error"))e$code else "SYSTEM_FAILED"))
  if(is.list(outcome))web_audit(con,project,
    if(is.null(outcome$error))"JOB_FOLLOWUP_RESULT_REGISTERED" else "JOB_FOLLOWUP_VALIDATION_PENDING",
    job$id,detail=list(operation=operation,version_id=job$version_id,result_version_id=outcome$version_id,
      error=outcome$error))
  invisible(outcome)
}

web_qc_followup_reconcile <- function(con,settings) {
  # Recover a completed automatic job even if the Worker previously stopped
  # after publishing it. Registration errors remain visible for manual review.
  j<-web_query(con,paste0("SELECT j.* FROM jobs j WHERE j.state='COMPLETE' ",
    "AND j.operation IN ('run_meta_analysis','generate_manuscript_docx') ",
    "AND j.idempotency_key LIKE 'qc-followup-%' AND NOT EXISTS (SELECT 1 FROM audit_events a ",
    "WHERE a.project_id=j.project_id AND a.object_id=j.id AND a.event_type IN ",
    "('JOB_FOLLOWUP_RESULT_REGISTERED','JOB_FOLLOWUP_VALIDATION_PENDING')) ORDER BY j.updated_at LIMIT 1"))
  if(nrow(j)!=1L)return(invisible(NULL))
  p<-web_query(con,"SELECT * FROM projects WHERE id=?p",p=j$project_id)
  if(nrow(p)!=1L)return(invisible(NULL))
  users<-web_query(con,"SELECT user_id FROM workspace_members WHERE workspace_id=?w ORDER BY user_id LIMIT 1",
    w=p$workspace_id)
  if(nrow(users)!=1L)return(invisible(NULL))
  web_qc_followup_worker_complete(con,settings,users$user_id,p,j)
}
