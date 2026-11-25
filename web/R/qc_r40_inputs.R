# The full CLI's declared inputs are read from its pinned source. Only path
# assignments are evaluated in a base-only environment.
web_r40_cli_paths <- function(engine) {
  script<-list.files(file.path(engine,"R"),pattern="^40_",full.names=TRUE)
  web_assert(length(script)==1L,"The canonical R/40 entry point is unavailable.","SYSTEM_FAILED")
  manifest<-jsonlite::read_json(file.path(engine,"web/docs/engine-source-manifest.json"),simplifyVector=FALSE)
  web_assert(identical(web_sha(script),manifest[[paste0("R/",basename(script))]]),
    "The canonical R/40 entry point has changed.","SYSTEM_FAILED")
  keys<-c("production_root","draft_root","r38_state_file","r38_results_file","r38_registry_file",
    "r38_artifact_registry_file","r39_state_file","manuscript_file","token_manuscript_file",
    "citation_map_file","citation_audit_r39_file","selected_literature_file","required_inputs")
  paths<-new.env(parent=baseenv())
  paths$repo_root<-"."
  for(ex in parse(script,encoding="UTF-8"))
    if(is.call(ex) && length(ex)==3L && identical(ex[[1L]],as.name("<-")) &&
       is.symbol(ex[[2L]]) && as.character(ex[[2L]]) %in% keys)eval(ex,paths)
  required<-paths$required_inputs
  web_assert(is.character(required) && length(required)>0L && !is.null(names(required)) &&
    !anyDuplicated(required),"The canonical R/40 input declaration is invalid.","SYSTEM_FAILED")
  setNames(sub("^\\./","",chartr("\\","/",required)),names(required))
}

web_r40_item <- function(status,reason,binding=NULL)
  list(status=status,reason=reason,binding=binding)

web_r40_pubmed_path <- function() "results/manuscript/production/drafting/pubmed_reference_reverification.xml"

web_r40_evidence_approval <- function(packet,ancestry,field,event,evidence_version,packet_sha256,read_evidence) {
  record<-packet[[field]]
  if(!is.list(record) || !identical(record$status,"HUMAN_CONFIRMED") ||
     !nzchar(srma_qc_one(record$actor)) || !nzchar(srma_qc_one(record$at)))return(NULL)
  origin<-srma_qc_one(record$source_version)
  decisions<-Filter(function(h)identical(h$event,event) && identical(h$actor,record$actor) &&
    nzchar(srma_qc_one(h$at)) && identical(h$source_version,origin),srma_qc_link_list(packet$history))
  version<-ancestry$version_id[ancestry$parent_version_id %in% origin & ancestry$stage=="EXTRACTION"]
  if(!nzchar(origin) || length(version)!=1L || length(decisions)!=1L ||
     !srma_qc_link_sha(packet_sha256))return(NULL)
  if(field=="final_study_selection" && !identical(decisions[[1L]]$study_ids,record$study_ids))return(NULL)
  saved<-tryCatch(read_evidence(version),error=function(e)NULL)
  if(!identical(saved[[field]],record))return(NULL)
  list(version=version,source_version=origin,evidence_version=evidence_version,reviewer=record$actor,
    at=record$at,sha256=web_c2_hash(record),evidence_sha256=packet_sha256)
}

web_r40_dossier <- function(paths,artifacts,read_artifact,job_at,prior_inputs,project_key,state_key,validate_state) {
  keys<-names(paths)
  approved<-Filter(function(a)isTRUE(a$approved) && a$kind %in% basename(paths),artifacts)
  hits<-lapply(unique(vapply(approved,function(a)a$job,character(1))),function(job_id) {
    files<-Filter(function(a)identical(a$job,job_id),approved)
    kinds<-vapply(files,function(a)a$kind,character(1))
    versions<-unique(vapply(files,function(a)a$version,character(1)))
    if(length(versions)!=1L || !setequal(kinds,basename(paths)) || anyDuplicated(kinds))return(NULL)
    job<-job_at(job_id,versions)
    if(nrow(job)!=1L || !identical(job$state,"COMPLETE"))return(NULL)
    result<-web_parse(job$result_json)
    prov<-result$provenance
    if(!identical(result$project_id,project_key) || !identical(result$job_id,job_id) ||
       !identical(result$operation,job$operation) ||
       !identical(result$status,"COMPLETE") || !identical(result$engine_version,job$engine_id) ||
       !identical(prov$project_version_id,versions) ||
       !all(vapply(names(prior_inputs),function(p)identical(prov$input_hashes[[p]],prior_inputs[[p]]),logical(1))))
      return(NULL)
    by_path<-setNames(files[match(basename(paths),kinds)],keys)
    checked<-lapply(keys,function(k) {
      a<-by_path[[k]]
      entries<-Filter(function(o)identical(o$artifact_id,a$id) && identical(o$checksum,a$checksum) &&
        identical(chartr("\\","/",srma_qc_one(o$relative_path)),unname(paths[[k]])),result$artifacts)
      if(length(entries)!=1L || !srma_qc_link_sha(a$checksum))return(NULL)
      local_path<-read_artifact(a)
      if(length(local_path)!=1L || dir.exists(local_path) || !identical(web_sha(local_path),a$checksum))return(NULL)
      list(id=a$id,sha256=a$checksum,local_path=local_path)
    })
    names(checked)<-keys
    if(any(lengths(checked)==0L))return(NULL)
    state_path<-checked[[state_key]]$local_path
    if(!isTRUE(file.info(state_path)$size<=100000L))return(NULL)
    state<-yaml::read_yaml(state_path,eval.expr=FALSE)
    if(!isTRUE(validate_state(state)))return(NULL)
    list(version=versions,job=job_id,artifacts=setNames(lapply(checked,function(a)a[c("id","sha256")]),unname(paths)))
  })
  hits<-Filter(Negate(is.null),hits)
  if(length(hits)==1L)hits[[1L]] else NULL
}

web_r40_manuscript_review <- function(con,pid,vid,x) {
  gate<-x$review$gate
  if(!identical(x$stage,"APPROVED") || !nzchar(srma_qc_one(gate)))return(NULL)
  review<-web_query(con,paste0("SELECT g.version_id,g.reviewer_id,g.decided_at FROM manuscript_versions m ",
    "JOIN gates g ON g.project_id=m.project_id AND g.version_id=m.parent_version_id ",
    "WHERE m.project_id=?p AND m.version_id=?v AND m.stage='APPROVED' ",
    "AND g.id=?g AND g.gate_type='manuscript_review' AND g.status='APPROVED' AND g.decision='APPROVED'"),
    p=pid,v=vid,g=gate)
  if(nrow(review)!=1L || !identical(review$reviewer_id,x$review$actor) ||
     !nzchar(srma_qc_one(review$decided_at)))return(NULL)
  c(list(gate_id=gate),as.list(review[1L,]))
}

web_r40_confirmation_trace <- function(settings,pid,vid,x,policy,qc,snapshot) {
  binding<-list(project_id=as.character(pid),manuscript_version_id=as.character(vid),
    evidence_version_id=srma_qc_one(x$evidence_version),source_sha256=srma_qc_one(x$source_sha256),
    docx_sha256=srma_qc_one(x$docx$docx_sha256),policy_sha256=policy$sha256,
    qc_finding_sha256=srma_qc_recon_finding_digest(qc),canonical_snapshot_sha256=snapshot$snapshot_sha256)
  tryCatch({
    log<-web_qc_confirmation_log(settings,list(binding=binding,scope=web_c2_hash(binding)))
    lapply(log$events,function(e)c(e[c("id","actor","at","decision")],list(sha256=web_c2_hash(e))))
  },error=function(e)NULL)
}

web_r40_input_contract <- function(con,settings,pid,vid,x,payload,snapshot,policy,qc,actor=NULL) {
  declared<-web_r40_cli_paths(settings$engine_root)
  r38_paths<-declared[startsWith(names(declared),"r38_")]
  r39_paths<-declared[!startsWith(names(declared),"r38_")]
  e<-payload$evidence$packet
  ev<-x$evidence_version
  manifest<-payload$evidence$version$manifest
  packet_sha<-manifest[["config/generated/web_evidence.json"]]
  lineage<-if(is.list(e))web_qc_followup_ancestor(con,pid,ev) else data.frame()
  full_protocol<-payload$protocol
  protocol_ok<-is.list(full_protocol) && identical(full_protocol$version,x$source$project_version) &&
    nzchar(srma_qc_one(full_protocol$approval_version)) && length(full_protocol$approval_ids)>0L &&
    srma_qc_link_sha(full_protocol$sha256)
  approved_selection<-if(isTRUE(snapshot$meta$selection_final))web_r40_evidence_approval(e,lineage,
    "final_study_selection","FINAL_STUDY_SELECTION",ev,packet_sha,payload$read_evidence)
  harmonized<-web_r40_evidence_approval(e,lineage,"human_outcome_harmonization",
    "HUMAN_OUTCOME_HARMONIZATION",ev,packet_sha,payload$read_evidence)
  methods<-unlist(snapshot$adaptive$profiles,use.names=FALSE)
  needed_harmonization<-"NARRATIVE_SWIM" %in% methods || is.list(e$human_outcome_harmonization) ||
    any(vapply(srma_qc_link_list(e$tables$Target_Outcome_Verification),function(r)isTRUE(r$.web_harmonized_result),logical(1)))
  frozen<-isTRUE(snapshot$meta$selection_final) && srma_qc_link_sha(packet_sha)
  ids<-sort(names(x$source$analyses))
  analysis_ok<-length(ids)>0L && frozen && setequal(ids,payload$verified_analysis_ids) &&
    identical(snapshot$links$analysis$approval,"APPROVED")
  no_pool<-frozen && isTRUE(snapshot$meta$narrative_final) && identical(methods,c("CORE","NARRATIVE_SWIM")) &&
    length(e$synthesis_plan$decisions)>0L && all(vapply(e$synthesis_plan$decisions,
      function(d)identical(d$mode,"NARRATIVE"),logical(1)))
  scope<-web_r40_binding(x,pid,vid)
  actor_review<-web_r40_manuscript_review(con,pid,vid,x)
  review_version<-actor_review$version_id %||b2% vid
  revision<-if(is.list(x$revision) && !is.null(actor))tryCatch(
    web_revision_binding(con,settings,actor,pid,x),error=function(e)NULL)
  dossier_docx<-if(is.list(revision))revision$origin_docx else x$docx
  dossier_version<-if(is.list(revision))revision$origin_version else review_version
  job_at<-function(id,version)web_query(con,paste0("SELECT engine_id,operation,state,result_json FROM jobs ",
    "WHERE id=?j AND project_id=?p AND version_id=?v"),j=id,p=pid,v=version)
  # Dedicated producer versions can be outside the usual manuscript registry
  # lookup. Verify their explicit source version IDs as well as the input hashes.
  dossier_job<-function(stage,r38=NULL)function(id,version) {
    job<-job_at(id,version)
    if(nrow(job)!=1L)return(job)
    if(version %in% c(vid,review_version,dossier_version,ev,x$source$project_version,dossier_docx$version_id))return(job)
    expected<-list(evidence_version_id=ev,protocol_version_id=full_protocol$version)
    if(stage=="r39")expected<-c(expected,list(r38_version_id=r38$version,manuscript_version_id=dossier_version))
    scope<-web_parse(job$result_json)$provenance
    if(all(vapply(names(expected),function(k)identical(scope[[k]],expected[[k]]),logical(1))))job else job[FALSE,,drop=FALSE]
  }
  names_at<-unique(c(basename(declared),basename(web_r40_pubmed_path())))
  slots<-paste0("?n",seq_along(names_at))
  rows<-do.call(web_query,c(list(con,paste0("SELECT * FROM artifacts WHERE project_id=?p AND status='AVAILABLE' ",
    "AND approval_state='APPROVED' AND logical_name IN (",paste(slots,collapse=","),") LIMIT 1001"),p=pid),
    setNames(as.list(names_at),substring(slots,2L))))
  dossiers<-if(nrow(rows)<1001L)srma_qc_link_artifact_metadata(rows,pid,unique(rows$version_id)) else list()
  known<-vapply(payload$artifacts,function(a)a$id,character(1))
  artifacts<-c(payload$artifacts,Filter(function(a)!a$id %in% known,dossiers))
  read_dossier<-function(a) {
    if(a$id %in% known)return(payload$read_artifact(a))
    row<-rows[rows$id==a$id & rows$version_id==a$version & rows$checksum==a$checksum,,drop=FALSE]
    if(nrow(row)==1L)web_store(settings)$get(row$storage_locator,a$checksum)
  }
  p<-web_query(con,"SELECT canonical_id FROM projects WHERE id=?p",p=pid)
  canonical<-if(nrow(p)==1L)p$canonical_id else ""
  analysis_inputs<-unlist(lapply(unname(x$source$analyses),function(a)a$result$output_hashes),recursive=FALSE)
  # Colliding producer paths must be disambiguated by the dossier producer,
  # not merged or overwritten in the Worker workspace.
  if(anyDuplicated(names(analysis_inputs)))analysis_inputs<-unlist(lapply(ids,function(id) {
    a<-x$source$analyses[[id]]$result$output_hashes
    setNames(a,paste0("analyses/",id,"/",names(a)))
  }),recursive=FALSE)
  inputs<-c(list("config/generated/web_evidence.json"=packet_sha,"config/project_config.yml"=full_protocol$sha256),analysis_inputs)
  r38<-if(frozen && protocol_ok && nzchar(canonical) && (analysis_ok || no_pool) && !anyDuplicated(names(inputs)))
    tryCatch(web_r40_dossier(r38_paths,artifacts,read_dossier,dossier_job("r38"),inputs,canonical,"r38_state",function(state) {
      s<-state$manuscript_production
      identical(s$project_id,canonical) && identical(s$status,"FINAL_EVIDENCE_READY")
    }),error=function(e)NULL)
  r39<-if(is.list(r38))tryCatch({
    docx_job<-job_at(dossier_docx$job_id,dossier_docx$version_id)
    result<-if(nrow(docx_job)==1L && identical(docx_job$state,"COMPLETE"))web_parse(docx_job$result_json)
    markdown<-if(identical(result$job_id,dossier_docx$job_id) &&
      identical(result$project_id,canonical) && identical(result$status,"COMPLETE") &&
      identical(result$provenance$project_version_id,dossier_docx$version_id))
      Filter(function(a)identical(a$logical_name,"manuscript.md"),result$artifacts)
    if(length(markdown)==1L) {
      md<-markdown[[1L]]
      archived<-Filter(function(a)identical(a$id,md$artifact_id) && identical(a$checksum,md$checksum) &&
        identical(a$job,dossier_docx$job_id) && identical(a$version,dossier_docx$version_id),payload$artifacts)
      word<-Filter(function(a)identical(a$id,dossier_docx$artifact_id) && identical(a$checksum,dossier_docx$docx_sha256) &&
        identical(a$job,dossier_docx$job_id) && identical(a$version,dossier_docx$version_id),payload$artifacts)
      expected<-vapply(r38$artifacts,function(a)a$sha256,character(1))
      matching<-if(length(archived)==1L && length(word)==1L &&
        identical(web_sha(payload$read_artifact(archived[[1L]])),md$checksum) &&
        identical(web_sha(payload$read_artifact(word[[1L]])),dossier_docx$docx_sha256))
        web_r40_dossier(r39_paths,artifacts,read_dossier,dossier_job("r39",r38),
        as.list(expected),canonical,"r39_state",function(state) {
          s<-state$manuscript_drafting
          identical(s$project_id,canonical) && identical(s$drafting_status,"DRAFT_COMPLETE_READY_FOR_R40_VALIDATION")
        })
      if(identical(matching$artifacts[[r39_paths[["manuscript"]]]]$sha256,md$checksum))matching
    }
  },error=function(e)NULL)
  if(is.list(r39)) {
    recorded<-Filter(function(a)a$kind==basename(web_r40_pubmed_path()) && a$job==r39$job && a$version==r39$version &&
      isTRUE(a$approved),artifacts)
    if(length(recorded)==1L)tryCatch({
      a<-recorded[[1L]]
      producer<-web_parse(job_at(r39$job,r39$version)$result_json)
      entries<-Filter(function(z)identical(z$artifact_id,a$id) && identical(z$checksum,a$checksum) &&
        identical(chartr("\\","/",srma_qc_one(z$relative_path)),web_r40_pubmed_path()),producer$artifacts)
      if(length(entries)==1L && identical(web_sha(read_dossier(a)),a$checksum))
        r39$artifacts[[web_r40_pubmed_path()]]<-list(id=a$id,sha256=a$checksum)
    },error=function(e)NULL)
  }
  conf<-web_r40_confirmation_trace(settings,pid,vid,x,policy,qc,snapshot)
  markers<-web_qc_followup_markers(e)
  applied<-lapply(markers,function(marker)tryCatch({
    proof<-web_qc_followup_marker_proof(con,settings,pid,lineage,marker,
      e$tables$Target_Outcome_Verification,payload$read_evidence)
    if(is.list(proof) && nzchar(srma_qc_one(proof$row$verified_at)) &&
       identical(proof$row$source_location,paste("PDF page",proof$choice$page)) &&
       identical(proof$row$source_evidence,proof$choice$excerpt)) {
      z<-proof$event
      list(id=z$id,actor=z$actor,decision=z$decision,sha256=web_c2_hash(z),
        draft_version=proof$draft_version,row_id=proof$row$.row_id,
        reviewer=proof$row$.verified_by,reviewed_at=proof$row$verified_at,
        row_sha256=web_c2_hash(proof$row))
    }
  },error=function(e)NULL))
  # Current review-log scopes can change when a receipt is registered. Bind the
  # author gate and corrections incorporated in Evidence, and report the visible
  # log separately so an R/40 receipt never hashes its own registry entry.
  approval_trace<-list(author_review=actor_review,applied=Filter(Negate(is.null),applied))
  items<-list(
    protocol=if(protocol_ok)web_r40_item("PASS","Approved protocol configuration is bound.",full_protocol) else
      web_r40_item("NOT_ASSESSED","The selected version has no bound approval of the full protocol.",payload$upstream$definition),
    study_selection=if(is.list(approved_selection))web_r40_item("PASS","Finalized study selection is bound.",approved_selection) else
      web_r40_item("NOT_ASSESSED","The exact Human Finalized Study Selection approval is unavailable."),
    outcome_harmonization=if(is.list(harmonized) && frozen)web_r40_item("PASS","Human outcome harmonization is bound.",harmonized) else
      if(!needed_harmonization && analysis_ok)web_r40_item("NOT_APPLICABLE","The approved quantitative input contains no harmonized outcomes.") else
      web_r40_item("NOT_ASSESSED","Applicability or approval of outcome harmonization is pending."),
    analyses=if(analysis_ok)web_r40_item("PASS","Executed analyses match approved versions.",scope$analysis_sha256) else
      if(no_pool && !length(ids))web_r40_item("NOT_APPLICABLE","The verified finalized synthesis plan specifies no pooling.",
        list(evidence_version=ev,sha256=web_c2_hash(e$synthesis_plan),evidence_sha256=packet_sha)) else
      web_r40_item("NOT_ASSESSED","The exact approved executions or an approved no-pooling plan are missing."),
    human_confirmation=if(is.list(actor_review) && is.list(conf) && length(markers)==length(approval_trace$applied))
      web_r40_item("PASS","Author review and available decision history are bound.",approval_trace) else
      web_r40_item("NOT_ASSESSED","Exact author approval or an applicable decision record is pending.",approval_trace),
    r38=if(is.list(r38))web_r40_item("PASS","Approved R/38 dossier and input hashes verified.",r38) else
      web_r40_item("NOT_ASSESSED","A complete approved R/38 dossier is missing or unverified.",as.list(unname(r38_paths))),
    r39=if(is.list(r39))web_r40_item("PASS","Approved R/39 dossier matches R/38 and the registered manuscript.",r39) else
      web_r40_item("NOT_ASSESSED","A complete approved R/39 dossier is missing or unverified.",as.list(unname(r39_paths))),
    qc_policy=web_r40_item("PASS","The pinned scientific QC policy is verified.",
      c(policy[c("id","version","sha256")],list(web_ruleset=web_c3_ruleset(),
        r40_script_sha256=web_r40_script_sha(settings$engine_root)))))
  statistical<-if(protocol_ok && frozen && !is.null(actor))tryCatch({
    web_stat_registry_report(con,settings,actor,pid,x,e,methods,payload$configuration$protocol,
      payload$verified_analysis_ids %||b2% character())
  },error=function(e)list(status="NOT_ASSESSED",gaps=list("source_verification"))) else
    list(status="NOT_ASSESSED",gaps=list("approved_protocol_or_frozen_evidence"))
  items$statistical_methods<-web_r40_item(statistical$status,
    if(identical(statistical$status,"PASS"))"Statistical methods are bound to approved protocol and verified execution records." else
      "Required statistical methods evidence is unavailable, incomplete, or inconsistent.",statistical)
  if(is.list(x$revision))items$revision<-if(is.list(revision))
    web_r40_item("PASS","The revised archive derives from the unchanged registered R/39 output.",revision) else
    web_r40_item("NOT_ASSESSED","The original R/39 archive or revised manuscript could not be bound.")
  ready<-all(vapply(items,function(a)a$status %in% c("PASS","NOT_APPLICABLE"),logical(1)))
  binding<-list(scope=scope,inputs=items)
  execution_scope<-scope
  execution_scope$manuscript_version_id<-review_version
  execution_scope$content_sha256<-web_c2_hash(x$sections)
  execution_scope$spec_sha256<-web_c2_hash(x$spec)
  execution_scope$archive<-x$docx[c("job_id","version_id","artifact_id","docx_sha256")]
  execution_inputs<-items[setdiff(names(items),"human_confirmation")]
  execution_inputs$applied_decisions<-approval_trace$applied
  list(binding=binding,sha256=web_c2_hash(binding),ready=ready,decision_history=conf,
    execution=list(scope=execution_scope,sha256=web_c2_hash(list(scope=execution_scope,inputs=execution_inputs)),
      ready=length(markers)==length(approval_trace$applied) && all(vapply(items[setdiff(names(items),"human_confirmation")],
        function(a)a$status %in% c("PASS","NOT_APPLICABLE"),logical(1)))))
}

web_r40_apply_input_contract <- function(publication,contract,plan,reused=NULL) {
  required<-!is.null(contract$binding$inputs$statistical_methods) || isTRUE(publication$adaptive$r40_required) ||
    "R40_REQUIRED_RESULT_PENDING" %in% unlist(publication$blockers,use.names=FALSE)
  complete<-is.list(reused)
  blockers<-unlist(publication$blockers,use.names=FALSE)
  if(required && !complete)blockers<-c(blockers,"R40_REQUIRED_RESULT_PENDING",
    if(isTRUE(contract$ready))"R40_RECEIPT_BINDING_PENDING" else "R40_SOURCE_BINDING_PENDING")
  if(required && !identical(contract$binding$inputs$human_confirmation$status,"PASS"))
    blockers<-c(blockers,"R40_AUTHOR_APPROVAL_PENDING")
  rows<-if(complete)reused$rules else plan$rules
  if(!is.null(contract$binding$inputs$statistical_methods) &&
     (!identical(rows$INPUT_STATISTICAL_METHODS$result,"PASS") ||
      !identical(rows$STATISTICAL_METHODS_REPORTING$result,"PASS")))
    blockers<-c(blockers,"R40_STATISTICAL_METHODS_PENDING")
  for(id in c("ROB_APPROVAL","GRADE_APPROVAL"))
    if(identical(rows[[id]]$applicability,"REQUIRED") && !identical(rows[[id]]$result,"PASS"))
      blockers<-c(blockers,paste0("R40_",id,"_PENDING"))
  publication$blockers<-as.list(unique(blockers))
  publication$r40<-if(complete)list(reused$receipt[c("id","version","sha256","source_ref","status","docx_sha256")]) else list()
  publication$r40_input_contract<-contract
  publication$r40_validation_plan<-plan
  publication$r40_rule_matrix<-rows
  publication$r40_integration<-list(IMPLEMENTATION_COMPLETE=TRUE,
    R40_VALIDATION_COMPLETE=complete, standalone_required=required)
  publication
}
