web_r40_operation <- function() "inspect_r40_deterministic"
web_r40_artifact_name <- function() "r40_deterministic.json"
web_r40_script_sha <- function(engine)
  web_sha(list.files(file.path(engine,"R"),pattern="^40_",full.names=TRUE))

web_r40_binding <- function(x,project_id,version_id) {
  analyses<-x$source$analyses
  ids<-sort(names(analyses))
  digests<-setNames(lapply(ids,function(v)web_c2_hash(analyses[[v]])),ids)
  list(project_id=project_id,manuscript_version_id=version_id,evidence_version_id=x$evidence_version,
    analysis_versions=as.list(ids),analysis_sha256=digests,source_sha256=x$source_sha256,
    reference_sha256=x$reference_sha256,docx_sha256=x$docx$docx_sha256)
}

web_r40_approved_binding <- function(z,x,project_id,version_id) {
  expected<-web_r40_binding(x,project_id,version_id)
  if(!identical(z$module,"R40") || !identical(z$project_id,project_id) ||
     !identical(z$version_id,version_id) || !identical(z$evidence_version_id,x$evidence_version) ||
     !identical(z$source_sha256,expected$source_sha256) || !identical(z$docx_sha256,expected$docx_sha256) ||
     !identical(web_c2_hash(x$source),expected$source_sha256) ||
     !grepl("^[a-f0-9]{64}$",expected$docx_sha256 %||b2% ""))return(FALSE)
  # The legacy source SHA commits to every analysis ID and full packet.
  # New receipts must additionally declare the complete, explicit scope.
  modern<-any(c("verification_scope","lineage_sha256","deterministic_state","semantic_state",
    "input_contract_sha256") %in% names(z))
  same_ids<-function(ids) {
    ids<-as.character(unlist(ids,use.names=FALSE))
    !anyDuplicated(ids) && identical(sort(ids),as.character(unlist(expected$analysis_versions)))
  }
  if("analysis_versions" %in% names(z) && !same_ids(z$analysis_versions))return(FALSE)
  if(!modern)return(TRUE)
  identical(z$verification_scope,"STANDALONE_R40") &&
    identical(z$lineage_sha256,web_c2_hash(expected)) &&
    "analysis_versions" %in% names(z) && same_ids(z$analysis_versions) &&
    identical(z$deterministic_state,"PASS") && identical(z$semantic_state,"PASS")
}

web_r40_load_runtime <- function(settings) {
  if(!exists("srma_qc_link_inspect_approved_json",mode="function"))
    source(file.path(settings$engine_root,"web/qc/runtime.R"),local=parent.frame(),encoding="UTF-8")
}

web_r40_approved <- function(con,settings,actor,project_id,version_id,x) {
  web_authorize(con,actor,project_id)
  web_r40_load_runtime(settings)
  rows<-web_query(con,paste0("SELECT * FROM artifacts WHERE project_id=?p AND version_id=?v ",
    "AND approval_state='APPROVED' AND status='AVAILABLE' ORDER BY id"),p=project_id,v=version_id)
  candidates<-Filter(function(a)srma_qc_link_artifact_family(a)=="r40",
    srma_qc_link_json_candidates(srma_qc_link_artifact_metadata(rows,project_id,version_id)))
  inspect<-function(a)web_c3_artifact(con,settings,actor,project_id,a$id,a$checksum,a$job)$path
  srma_qc_link_inspect_approved_json(inspect,project_id,version_id,x,candidates,character())
}

web_r40_proofs_pass <- function(proofs)
  length(proofs)>0L && all(vapply(proofs,function(a)identical(a$status,"PASS"),logical(1)))

web_r40_guard <- function(con,settings,actor,project_id,version_id) {
  x<-web_c3_source(con,settings,actor,project_id,version_id)$packet
  web_r40_load_runtime(settings)
  profiles<-setdiff(srma_qc_profiles(x),"CORE")
  narrative<-identical(profiles,"NARRATIVE_SWIM") && identical(x$source$route,"NARRATIVE") &&
    identical(x$source$narrative$finalization_scope,"NARRATIVE_ANALYSIS_LOCKED; NOT_CANONICAL_R26")
  pairwise<-identical(profiles,"PAIRWISE_META") && length(x$source$analyses)>0L &&
    all(vapply(x$source$analyses,function(a)identical(a$result$contract,"web-analysis-v1"),logical(1))) &&
    is.null(x$production) && setequal(names(x$source$analyses),srma_qc_adaptive_analysis_ids(con,project_id,x))
  web_assert(narrative || pairwise,"No deterministic R/40-compatible Web validator is available for this manuscript.",
    "USER_ACTION_REQUIRED")
  if(narrative) {
    e<-web_b2_context(con,settings,actor,project_id,x$evidence_version)$packet
    web_assert(identical(e$narrative_source_finalization$status,"FINALIZED") &&
      identical(e$narrative_source_finalization$source_digest,x$source$narrative$source_digest),
      "The manuscript has no matching finalized narrative evidence.","STALE_UPSTREAM_VERSION")
  }
  x
}

web_r40_job_key <- function(settings,binding) paste0("r40-det-",web_c2_hash(list(
  binding=binding,web_sha256=web_c3_code_hash(settings))))

web_r40_start <- function(con,settings,actor,project_id,version_id) {
  x<-web_c3_source(con,settings,actor,project_id,version_id)$packet
  prior<-web_r40_approved(con,settings,actor,project_id,version_id,x)
  if(web_r40_proofs_pass(prior))return(web_r40_status(con,settings,actor,project_id,version_id))
  job<-web_submit(con,settings,actor,project_id,version_id,web_r40_operation(),
    idempotency_key=web_r40_job_key(settings,web_r40_binding(x,project_id,version_id)))
  web_r40_status(con,settings,actor,project_id,version_id,job_id=job)
}

web_r40_status <- function(con,settings,actor,project_id,version_id,job_id=NULL) {
  x<-web_c2_context(con,settings,actor,project_id,version_id)$packet
  accepted<-web_r40_approved(con,settings,actor,project_id,version_id,x)
  publication<-tryCatch(web_c3_submission(con,settings,actor,project_id,version_id,x),error=function(e)NULL)
  status<-list(manuscript_stage=x$stage,registration="NOT_REGISTERED",execution="NOT_RUN",
    deterministic="NOT_ASSESSED",semantic="NOT_ASSESSED",approval="UNREVIEWED",
    standalone="NOT_ASSESSED",semantic_execution="BLOCKED",
    semantic_requirement="EXPLICIT_AUTHORIZATION_AND_BUDGET_REQUIRED")
  status$integration<-publication$r40_integration %||b2%
    list(IMPLEMENTATION_COMPLETE=FALSE,R40_VALIDATION_COMPLETE=FALSE)
  status$input_contract<-publication$r40_input_contract
  status$rule_matrix<-publication$r40_rule_matrix
  if(is.list(publication$r40_validation_plan))
    status$bound_worker<-web_r40_full_job(con,settings,actor,project_id,version_id,publication$r40_validation_plan)
  if(is.list(status$bound_worker$inspection))
    status$rule_matrix<-c(status$rule_matrix,web_r40_inspection_rules(status$bound_worker$inspection))
  status$bound_ready<-isTRUE(publication$r40_validation_plan$supported) &&
    is.list(publication$r40_validation_plan$execution_scope)
  if(is.list(status$bound_worker$inspection$rules) && !isTRUE(status$integration$R40_VALIDATION_COMPLETE))
    for(id in names(status$bound_worker$inspection$rules)) {
      row<-status$bound_worker$inspection$rules[[id]]
      if(identical(row$execution,"EXECUTED") && identical(row$applicability,"REQUIRED"))status$rule_matrix[[id]]<-row
    }
  status$integration$SUBMISSION_READY<-FALSE
  if(isTRUE(status$integration$R40_VALIDATION_COMPLETE) && !length(publication$blockers)) {
    last<-web_query(con,paste0("SELECT q.id FROM qc_versions q WHERE q.project_id=?p AND q.version_id=?v ",
      "AND q.status='PASSED' AND NOT EXISTS (SELECT 1 FROM qc_versions child WHERE child.parent_id=q.id) ",
      "ORDER BY q.created_at DESC LIMIT 1"),p=project_id,v=version_id)
    if(nrow(last)==1L)status$integration$SUBMISSION_READY<-isTRUE(tryCatch(
      web_c3_view(con,settings,actor,project_id,last$id)$eligibility$RELEASE_ELIGIBLE,error=function(e)FALSE))
  }
  if(isTRUE(status$integration$R40_VALIDATION_COMPLETE)) {
    worker<-isTRUE(publication$r40[[1L]]$source_ref %in% c("canonical_r40_worker","r40_manuscript_revision"))
    status$registration<-"REUSED"
    status$execution<-if(worker)"VERIFIED_WORKER_RESULT" else "PREVIOUS_APPROVED_RESULT"
    status$deterministic<-status$semantic<-status$standalone<-"PASS"
    status$approval<-if(worker)"UNREVIEWED" else "APPROVED"
    status$semantic_execution<-"REUSED"
    status$semantic_requirement<-NULL
    status$approved_proofs<-accepted
    return(status)
  }
  if(web_r40_proofs_pass(accepted)) {
    status$registration<-"REUSED"
    status$execution<-"PREVIOUS_APPROVED_RESULT"
    status$prior_standalone<-"PASS"
    status$approval<-"APPROVED"
    status$approved_proofs<-accepted
    return(status)
  }
  if(length(accepted)) {
    status$standalone<-"BLOCKED"
    status$approved_proofs<-accepted
  }
  binding<-web_r40_binding(x,project_id,version_id)
  key<-web_r40_job_key(settings,binding)
  if(is.null(job_id)) {
    j<-web_query(con,paste0("SELECT * FROM jobs WHERE project_id=?p AND version_id=?v ",
      "AND operation=?o AND idempotency_key=?k ORDER BY created_at DESC LIMIT 1"),
      p=project_id,v=version_id,o=web_r40_operation(),k=key)
    if(nrow(j)!=1L)return(status)
  } else j<-web_scoped_job(con,actor,project_id,version_id,job_id,web_r40_operation())
  status$job_id<-j$id
  status$registration<-"REGISTERED"
  status$execution<-j$state
  if(!identical(j$state,"COMPLETE"))return(status)
  result<-tryCatch({
    public<-web_parse(j$result_json)
    a<-Filter(function(z)identical(z$logical_name,web_r40_artifact_name()),public$artifacts)
    if(length(a)!=1L || !grepl("^[a-f0-9]{64}$",a[[1L]]$checksum %||b2% ""))return(status)
    a<-a[[1L]]
    ar<-web_c3_artifact(con,settings,actor,project_id,a$artifact_id,a$checksum,j$id)
    if(!identical(ar$row$version_id,version_id) || file.info(ar$path)$size>65536L)return(status)
    z<-jsonlite::read_json(ar$path,simplifyVector=FALSE)
    if(!identical(z$module,"R40_DETERMINISTIC") || !identical(z$job_id,j$id) ||
       !identical(z$lineage_sha256,web_c2_hash(z$binding)) ||
       !identical(z$lineage_sha256,web_c2_hash(binding)) ||
       !identical(z$web_sha256,web_c3_code_hash(settings)) ||
       !identical(z$canonical_r40_sha256,web_r40_script_sha(settings$engine_root)) ||
       !identical(z$semantic,"NOT_ASSESSED") || !identical(z$standalone,"NOT_ASSESSED") ||
       !identical(z$approval,"UNREVIEWED") ||
       !z$verification %in% c("PASS","BLOCKED","NOT_ASSESSED"))return(status)
    z
  },error=function(e)NULL)
  if(is.list(result) && identical(result$module,"R40_DETERMINISTIC")) {
    status$deterministic<-result$verification
    status$checks<-result$checks
  }
  status
}

web_r40_narrative_numerics <- function(x,proof,root,engine) {
  read_output<-function(name)web_inside(file.path(root,proof$outputs[[name]]$path),root)
  text<-paste(readLines(read_output("manuscript.md"),encoding="UTF-8",warn=FALSE),collapse="\n")
  mapping<-jsonlite::read_json(read_output("citation_map.json"),simplifyVector=TRUE)
  pr<-x$production$qc$publication_9cd
  expected<-web_c2_9cd_results(web_c2_9b_results(x),
    list(prisma_status=pr$prisma_status,studies_included=pr$included_studies))
  actual<-web_c3_r40(engine)$extract_section(text,"Results")
  web_assert(nzchar(actual) && identical(trimws(actual),
    trimws(web_c2_defs(engine,39)$replace_tokens_with_numbers(expected,mapping))),
    "The archived narrative Results do not match the approved study-level values.",
    "NUMERIC_CONSISTENCY_FAILED")
}

web_r40_inspect <- function(x,proof,root,engine) {
  narrative<-identical(x$source$route,"NARRATIVE")
  xml_ready<-narrative || requireNamespace("xml2",quietly=TRUE)
  inspected<-if(narrative)web_c3_narrative_checks(x,proof,root,engine,require_release=FALSE) else
    web_c3_checks(x,proof,root,engine,check_docx_xml=xml_ready)
  checks<-inspected$checks
  if(narrative && isTRUE(inspected$machine_pass)) {
    checks$numerics<-tryCatch({web_r40_narrative_numerics(x,proof,root,engine);"PASS"},
      error=function(e)if(inherits(e,"srma_web_error"))"FAIL" else "NOT_ASSESSED")
  }
  passed<-isTRUE(inspected$machine_pass) && identical(checks$numerics,"PASS") && xml_ready
  list(verification=if(passed)"PASS" else if(isTRUE(inspected$machine_pass) &&
    (!xml_ready || identical(checks$numerics,"NOT_ASSESSED")))"NOT_ASSESSED" else "BLOCKED",
    checks=checks)
}

web_r40_worker_scope <- function(settings,canonical_id,version_id,job_id,work,engine_id,operation) {
  con<-settings$r40_worker_connection
  web_assert(!is.null(con),"This inspection must run through the Web Worker.","SYSTEM_FAILED")
  j<-web_query(con,paste0("SELECT j.project_id,j.worker_token,j.idempotency_key,p.workspace_id FROM jobs j JOIN projects p ON p.id=j.project_id ",
    "WHERE j.id=?j AND j.version_id=?v AND j.engine_id=?e AND j.operation=?o AND j.state='RUNNING' ",
    "AND p.canonical_id=?p"),j=job_id,v=version_id,e=engine_id,o=operation,p=canonical_id)
  web_assert(nrow(j)==1L,"Inspection job does not match the exact running version.","STALE_UPSTREAM_VERSION")
  expected_work<-file.path(settings$data_root,"jobs",job_id,j$worker_token,"project")
  web_assert(identical(web_inside(work,settings$data_root),web_inside(expected_work,settings$data_root)),
    "Inspection staging belongs to a different worker attempt.","STALE_UPSTREAM_VERSION")
  actor<-web_query(con,"SELECT user_id FROM workspace_members WHERE workspace_id=?w ORDER BY user_id LIMIT 1",w=j$workspace_id)$user_id
  web_assert(length(actor)==1L,"Inspection workspace is unavailable.","SYSTEM_FAILED")
  list(row=j,actor=actor)
}

web_r40_worker_execute <- function(settings,canonical_id,version_id,job_id,work,engine_id) {
  attempt<-web_r40_worker_scope(settings,canonical_id,version_id,job_id,work,engine_id,web_r40_operation())
  con<-settings$r40_worker_connection
  j<-attempt$row
  actor<-attempt$actor
  x<-web_r40_guard(con,settings,actor,j$project_id,version_id)
  binding<-web_r40_binding(x,j$project_id,version_id)
  web_assert(identical(j$idempotency_key,web_r40_job_key(settings,binding)),
    "Inspection job was queued for different sources or Web code.","STALE_UPSTREAM_VERSION")
  proof<-web_c3_collect_inputs(con,settings,actor,j$project_id,version_id,x,work)
  outcome<-web_r40_inspect(x,proof,work,settings$engine_root)
  record<-c(list(module="R40_DETERMINISTIC",job_id=job_id,binding=binding,
    lineage_sha256=web_c2_hash(binding),web_sha256=web_c3_code_hash(settings),
    canonical_r40_sha256=web_r40_script_sha(settings$engine_root),
    semantic="NOT_ASSESSED",standalone="NOT_ASSESSED",approval="UNREVIEWED"),outcome)
  dest<-web_inside(file.path(work,web_r40_artifact_name()),work,exists=FALSE)
  jsonlite::write_json(record,dest,auto_unbox=TRUE,null="null")
  web_result(web_r40_operation(),canonical_id,job_id,"COMPLETE",
    "Deterministic inspection finished. See the recorded verification states.",engine_id,
    artifacts=list(list(path=dest,logical_name=web_r40_artifact_name(),type="r40_deterministic")),
    provenance=list(lineage_sha256=record$lineage_sha256,verification=outcome$verification,R=R.version.string))
}
