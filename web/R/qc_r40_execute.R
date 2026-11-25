web_r40_candidate <- function(con,settings,actor,project_id,version_id) {
  c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE)
  web_assert(c$packet$stage %in% c("REVIEW_REQUIRED","APPROVED") &&
    srma_qc_link_sha(c$packet$docx$docx_sha256),"R/40 requires a frozen manuscript export.","USER_ACTION_REQUIRED")
  c$packet
}

web_r40_approval_parent <- function(con,request,pid,vid,x) {
  review<-web_r40_manuscript_review(con,pid,vid,x)
  if(!is.list(review))return(NULL)
  parent<-request("manuscript_view",list(project_id=pid,version_id=review$version_id))
  frozen<-c("manuscript_id","evidence_version","source","sections","spec","docx","production","revision")
  if(identical(parent$stage,"REVIEW_REQUIRED") && identical(web_c2_hash(parent[frozen]),web_c2_hash(x[frozen])))
    list(version=review$version_id,packet=parent)
}

web_r40_semantic_rules <- function(rules) {
  ids<-c("STUDY_AND_PARTICIPANT_COUNTS","METHODS_PROTOCOL","STATISTICAL_METHODS_REPORTING","NARRATIVE_STUDY_RESULTS","NARRATIVE_INTERPRETATION",
    "PAIRWISE_STUDY_COUNTS","PAIRWISE_ESTIMATES_CI","PAIRWISE_ADDITIONAL_ANALYSES","PAIRWISE_ARTIFACT_NUMERICS",
    "PAIRWISE_DIRECTION","MIXED_CONSISTENCY")
  Filter(function(r)r$applicability=="REQUIRED",rules[intersect(ids,names(rules))])
}

web_r40_execution_scope <- function(plan,contract) {
  scope<-contract$execution$scope
  if(!is.list(scope))return(NULL)
  scope$inputs_sha256<-contract$execution$sha256
  scope$policy_sha256<-plan$binding$policy_sha256
  scope$engine_sha256<-plan$binding$canonical_sha256
  scope$web_sha256<-plan$binding$web_sha256
  scope$runtime_lock_sha256<-plan$binding$runtime_lock_sha256
  if(length(plan$binding$revision_sha256))scope$revision_sha256<-plan$binding$revision_sha256
  scope$rules_sha256<-web_c2_hash(list(ruleset=plan$binding$ruleset,profiles=plan$binding$profiles,
    semantic=lapply(web_r40_semantic_rules(plan$rules),function(r)r[c("id","basis")])))
  scope$fingerprint<-web_c2_hash(scope)
  scope
}

web_r40_bridge_run <- function(scientific,engine,root,scope,plan,settings,policy,check_lease,
                               transport=web_r40_ai_openai,mode="LIVE") {
  bridge<-tempfile("bridge-",tmpdir=root)
  web_assert(dir.create(bridge,mode="0700"),"Cannot isolate the R/40 invocation.","SYSTEM_FAILED")
  jsonlite::write_json(list(policy_sha256=policy$sha256,
    pubmed_path=web_r40_pubmed_path(),
    semantic_rules=lapply(web_r40_semantic_rules(plan$rules),`[[`,"basis"),
    statistical_records=lapply(plan$statistical_records,function(r)r[c("field","text")])),
    file.path(bridge,"rules.json"),auto_unbox=TRUE)
  prev<-Sys.getenv("R_LIBS",unset=NA_character_)
  on.exit(if(is.na(prev))Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS=prev),add=TRUE)
  Sys.setenv(R_LIBS=scientific$engine_library)
  launcher<-file.path(engine,"web/adapter/r40_bound_preflight.R")
  script<-list.files(file.path(engine,"R"),pattern="^40_",full.names=TRUE)
  response_file<-file.path(bridge,"native.json")
  request_file<-file.path(bridge,"request.json")
  calls<-list()
  done<-function(result)c(result[setdiff(names(result),"ai_calls")],list(mode=mode,calls=calls,
    dispatched=sum(vapply(calls,function(a)isTRUE(a$dispatched),logical(1))),
    accounted_cost_microusd=sum(vapply(calls,function(a)as.double(a$cost_microusd %||b2% 0),numeric(1)))))
  for(i in seq_len(length(web_r40_ai_steps())+1L)) {
    unlink(c(response_file,request_file))
    status<-suppressWarnings(system2(scientific$rscript,
      shQuote(c("--vanilla",launcher,script,root,response_file,scope$engine_sha256,bridge)),
      stdout=nullfile(),stderr=nullfile(),timeout=min(60,scientific$timeout)))
    if(!identical(as.integer(status),0L) || !isTRUE(file.info(response_file)$size<=65536L))
      return(done(list(state="CANONICAL_EXECUTION_INCOMPLETE")))
    result<-jsonlite::read_json(response_file,simplifyVector=FALSE)
    if(!identical(result$script_sha256,scope$engine_sha256))return(done(list(state="CANONICAL_EXECUTION_UNVERIFIED")))
    if(!identical(result$state,"SEMANTIC_REQUEST_PENDING"))return(done(result))
    failures<-c("numeric_claim_failures","citation_integrity_failures","artifact_coverage_failures","machine_identifier_leak_failures")
    if(any(vapply(result$canonical[failures],function(n)isTRUE(n>0L),logical(1)))) {
      result$state<-"DETERMINISTIC_FINDING_BLOCKS_AI"
      return(done(result))
    }
    if(!isTRUE(file.info(request_file)$size<=1e6))return(done(list(state="AI_INPUT_LIMIT_EXCEEDED")))
    envelop<-jsonlite::read_json(request_file,simplifyVector=FALSE)
    req<-envelop$request
    if(!identical(req$step,web_r40_ai_steps()[i]) || !identical(envelop$sha256,web_c2_hash(req)))
      return(done(list(state="SEMANTIC_REQUEST_UNVERIFIED")))
    record<-web_r40_ai_call(settings,scope,req,policy,transport,mode,check_lease)
    if(!is.null(record$id))calls[[req$step]]<-record[intersect(names(record),
      c("id","state","model","provider_id","usage","cost_microusd","reserve_microusd","reused","dispatched"))]
    if(!identical(record$state,"COMPLETE")) {
      result$state<-record$state
      return(done(result))
    }
    jsonlite::write_json(list(sha256=envelop$sha256,text=record$text),file.path(bridge,paste0(req$step,".json")),
      auto_unbox=TRUE)
  }
  done(list(state="CANONICAL_EXECUTION_INCOMPLETE"))
}

web_r40_execution_rules <- function(plan,native,manuscript_sha,canonical_id,source) {
  rules<-plan$rules
  z<-native$canonical
  if(!srma_qc_link_sha(manuscript_sha) || !identical(z$manuscript_sha256,manuscript_sha) ||
     !identical(z$project_id,canonical_id) || !isTRUE(z$final_evidence_ready))return(rules)
  fields<-c(NUMERIC_CLAIMS="numeric_claim_failures",CITATION_INTEGRITY="citation_integrity_failures",
    ARTIFACT_COVERAGE="artifact_coverage_failures",IDENTIFIER_INTEGRITY="machine_identifier_leak_failures")
  set_result<-function(id,result) {
    if(is.null(rules[[id]]) || rules[[id]]$applicability!="REQUIRED")return(NULL)
    rules[[id]]$result<<-result
    rules[[id]]$execution<<-"EXECUTED"
    rules[[id]]$source<<-source
  }
  for(id in names(fields)) {
    count<-z[[fields[[id]]]]
    if(web_r40_ai_uint(count))set_result(id,if(count==0L)"PASS" else "FAIL")
  }
  if(identical(native$state,"COMPLETE") && is.logical(z$semantic_pass) && length(z$semantic_pass)==1L && !is.na(z$semantic_pass))
    set_result("CROSS_SECTION_SEMANTICS",if(z$semantic_pass)"PASS" else "FAIL")
  for(id in names(web_r40_semantic_rules(rules))) {
    check<-native$semantic_checks[[id]]
    if(identical(id,"STATISTICAL_METHODS_REPORTING") && identical(check$result,"PASS") &&
       (!identical(check$records_sha256,web_stat_records_digest(plan$statistical_records)) ||
        !identical(unlist(check$record_ids,use.names=FALSE),names(plan$statistical_records))))next
    if(isTRUE(check$result %in% c("PASS","FAIL"))) {
      set_result(id,check$result)
      rules[[id]]$source<-c(source,list(semantic_check=check))
    }
  }
  rules
}

web_r40_execution_complete <- function(rules,native) {
  # Final author and reporting approvals are downstream release gates. The
  # content checks can finish while those authorities are still reviewing.
  release_only<-c("INPUT_HUMAN_CONFIRMATION","PRISMA_SELECTION","REFERENCE_APPROVAL","ROB_APPROVAL","GRADE_APPROVAL")
  required<-Filter(function(r)r$applicability=="REQUIRED",rules[setdiff(names(rules),release_only)])
  length(required)>0L && all(vapply(required,function(r)identical(r$result,"PASS"),logical(1))) &&
    identical(native$state,"COMPLETE") && identical(native$canonical$overall_status,"PASS_READY_FOR_SUBMISSION_FORMATTING") &&
    isTRUE(native$canonical$deterministic_pass) && isTRUE(native$canonical$semantic_pass)
}

web_r40_revision_complete <- function(rules,native) {
  release_only<-c("INPUT_HUMAN_CONFIRMATION","PRISMA_SELECTION","REFERENCE_APPROVAL","ROB_APPROVAL","GRADE_APPROVAL")
  rows<-Filter(function(r)identical(r$applicability,"REQUIRED"),rules[setdiff(names(rules),release_only)])
  length(rows)>0L && identical(native$state,"COMPLETE") &&
    all(vapply(rows,function(r)identical(r$result,"PASS"),logical(1)))
}

web_r40_execution_proof <- function(con,settings,actor,pid,plan,contract) {
  scope<-plan$execution_scope
  if(!is.list(scope) || !isTRUE(contract$execution$ready))return(NULL)
  version<-contract$binding$scope$manuscript_version_id
  runs<-lapply(unique(c(version,scope$manuscript_version_id)),function(v)
    web_r40_full_job(con,settings,actor,pid,v,plan))
  matches<-Filter(function(r)identical(r$execution,"COMPLETE") && identical(r$inspection$native$mode,"LIVE") &&
    identical(r$inspection$execution_inputs_sha256,scope$inputs_sha256) && identical(r$inspection$validation,"PASS"),runs)
  if(!length(matches))return(NULL)
  z<-matches[[1L]]$inspection
  roster<-plan$rules
  word<-z$rules$DOCUMENT_STRUCTURE
  if(!identical(word$result,"PASS") || !identical(word$execution,"EXECUTED") ||
     !identical(word$source$docx_sha256,scope$docx_sha256))return(NULL)
  roster$DOCUMENT_STRUCTURE<-word
  if(isTRUE(plan$revision$selective)) {
    part<-contract$binding$inputs$revision$binding
    if(!identical(z$native$revision_sha256,part$fingerprint) || !identical(z$native$section_sha256,part$section_sha256))return(NULL)
    for(id in c(plan$revision$pending,"IDENTIFIER_INTEGRITY")) {
      row<-z$rules[[id]]
      if(!identical(row$id,id) || !identical(row$result,"PASS") || !identical(row$execution,"EXECUTED") ||
         !identical(row$source$section_sha256,part$section_sha256))return(NULL)
      if(identical(id,"STATISTICAL_METHODS_REPORTING") &&
         !identical(row$source$check$records_sha256,web_stat_records_digest(plan$statistical_records)))return(NULL)
      roster[[id]]<-row
      roster[[id]]$source$receipt<-z$receipt
    }
    if(!web_r40_revision_complete(roster,z$native))return(NULL)
    return(list(rules=roster,receipt=list(family="r40",id=z$receipt$id,version=version,
      sha256=z$receipt$sha256,status="PASS",source_ref="r40_manuscript_revision",docx_sha256=scope$docx_sha256)))
  }
  if(is.list(plan$revision) && !identical(z$native$canonical$manuscript_sha256,
      contract$binding$inputs$revision$binding$revised_archive[["manuscript.md"]]$sha256))return(NULL)
  reusable<-names(web_r40_semantic_rules(roster))
  reusable<-c(reusable,"NUMERIC_CLAIMS","CITATION_INTEGRITY","ARTIFACT_COVERAGE","IDENTIFIER_INTEGRITY","CROSS_SECTION_SEMANTICS")
  for(id in reusable) {
    row<-z$rules[[id]]
    if(!identical(row$id,id) || !identical(row$result,"PASS") || !identical(row$execution,"EXECUTED"))return(NULL)
    roster[[id]]$result<-"PASS"
    roster[[id]]$execution<-"REUSED"
    roster[[id]]$source<-c(z$receipt,list(check=id))
  }
  if(!web_r40_execution_complete(roster,z$native))return(NULL)
  list(rules=roster,receipt=list(family="r40",id=z$receipt$id,version=version,
    sha256=z$receipt$sha256,status="PASS",source_ref="canonical_r40_worker",docx_sha256=scope$docx_sha256))
}
