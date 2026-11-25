# Policy applicability describes available validators separately from approvals.
srma_qc_adaptive_method_catalog <- function() list(
  NARRATIVE_SWIM=list(aliases=c("narrative","narrative synthesis","swim","synthesis without meta-analysis"),
    reporting="SWiM",validator="BOUND_NARRATIVE_SOURCES"),
  PAIRWISE_META=list(aliases=c("quantitative","meta-analysis","pairwise","pairwise meta-analysis",
    "quantitative candidate"),validator="CANONICAL_PAIRWISE_CONSISTENCY"),
  IPD_SURVIVAL=list(aliases=c("ipd","ipd survival","reconstructed ipd","reconstructed km","original ipd"),
    reporting="PRISMA-IPD"),
  NMA=list(aliases=c("nma","network meta","network meta-analysis"),reporting="PRISMA-NMA"),
  DTA=list(aliases=c("dta","diagnostic","diagnostic test accuracy"),reporting="PRISMA-DTA"),
  ADVANCED=list(aliases=c("advanced","bayesian","multilevel","dose response","meta regression","prognostic",
    "prognosis","prediction model","qualitative","qualitative synthesis","scoping","scoping review")))

srma_qc_adaptive_key <- function(x) tolower(trimws(gsub("[^[:alnum:]]+"," ",as.character(x))))
srma_qc_adaptive_field <- function(x,key) if(is.list(x))x[[key]] else NULL
srma_qc_adaptive_method <- function(raw) {
  key<-srma_qc_adaptive_key(srma_qc_one(raw))
  if(!nzchar(key))return("")
  catalog<-srma_qc_adaptive_method_catalog()
  hit<-names(catalog)[vapply(names(catalog),function(m)
    key %in% srma_qc_adaptive_key(c(m,catalog[[m]]$aliases)),logical(1))]
  if(length(hit)==1L)hit else "OTHER_METHOD"
}
srma_qc_adaptive_proof <- function(status,reason,ref="",approval="UNCONFIRMED") {
  list(status=status,reason=reason,source_ref=ref,approval=approval)
}

srma_qc_adaptive_analysis_ids <- function(con,project_id,x) {
  analyses<-x$source$analyses
  ids<-names(analyses)
  if(!length(ids) || length(ids)>100L)return(character())
  slots<-paste0("?v",seq_along(ids))
  bound<-do.call(web_query,c(list(con,paste0(
    "SELECT a.version_id,a.plan_sha256,j.id AS job_id,j.version_id AS plan_version,g.id AS gate_id ",
    "FROM analysis_versions a JOIN analysis_versions r ON r.version_id=a.parent_version_id ",
    "AND r.project_id=a.project_id AND r.evidence_version_id=a.evidence_version_id AND r.stage='RESULT' ",
    "JOIN jobs j ON j.id=r.source_job_id AND j.project_id=r.project_id AND j.version_id=r.parent_version_id ",
    "JOIN gates g ON g.project_id=r.project_id AND g.version_id=r.version_id AND g.gate_type='analysis_results' ",
    "WHERE a.project_id=?p AND a.evidence_version_id=?e AND a.stage='APPROVED' AND a.source_job_id=j.id ",
    "AND j.state='COMPLETE' AND g.status='APPROVED' AND g.decision='APPROVED' AND g.reviewer_id IS NOT NULL ",
    "AND g.decided_at IS NOT NULL AND a.version_id IN (",paste(slots,collapse=","),")"),p=project_id,e=x$evidence_version),
    setNames(as.list(ids),substring(slots,2L))))
  Filter(function(id) {
    a<-analyses[[id]]
    row<-bound[bound$version_id==id,,drop=FALSE]
    nrow(row)==1L && identical(row$plan_sha256,web_c1_hash(a$plan)) &&
      identical(row$gate_id,a$result_review$gate_id) &&
      identical(row$job_id,a$result$job_id) && identical(row$plan_version,a$result$project_version) &&
      identical(a$result$evidence_version,x$evidence_version)
  },ids)
}

srma_qc_adaptive_methods <- function(x,e=NULL,manifest_bound=FALSE,verified_analysis_ids=character()) {
  source<-x$source
  entries<-list()
  add<-function(method,outcome="",timepoint="",ref="",execution="UNVERIFIED",origin="",analysis_id="") {
    if(nzchar(method))entries[[length(entries)+1L]]<<-list(method=method,
      outcome=srma_qc_one(outcome),timepoint=srma_qc_one(timepoint),source_ref=ref,
      execution=execution,ipd_origin=origin,analysis_id=analysis_id)
  }
  for(id in names(source$analyses)) {
    a<-source$analyses[[id]]
    result<-a$result
    native<-identical(result$contract,"web-analysis-v1")
    execution<-if(id %in% verified_analysis_ids && identical(result$status,"PASS") &&
      identical(a$stage,"APPROVED") && identical(a$evidence_version,x$evidence_version))"VERIFIED_EXECUTION" else "UNVERIFIED"
    method<-srma_qc_adaptive_method(result$analysis_profile)
    if(!nzchar(method))method<-srma_qc_adaptive_method(a$plan$synthesis_method)
    if(!nzchar(method))method<-if(native)"PAIRWISE_META" else "OTHER_METHOD"
    origin<-srma_qc_adaptive_key(srma_qc_one(result$ipd_origin))
    add(method,a$plan$outcome,a$plan$timepoint,"source.analyses",execution,origin,id)
  }
  if(manifest_bound && identical(e$synthesis_plan$status,"HUMAN_CONFIRMED") &&
     nzchar(srma_qc_one(e$synthesis_plan$actor)) && nzchar(srma_qc_one(e$synthesis_plan$at))) {
    for(d in srma_qc_link_list(e$synthesis_plan$decisions)) {
      group<-srma_qc_one(d$analysis_group)
      sets<-Filter(function(a)nzchar(group) && identical(srma_qc_one(a$analysis_group),group) &&
        identical(a$route,d$mode),srma_qc_link_list(e$synthesis_execution$sets))
      a<-if(length(sets)==1L)sets[[1L]] else NULL
      planned<-srma_qc_adaptive_method(d$mode)
      matched<-any(vapply(entries,function(z)identical(z$method,planned) &&
        nzchar(z$outcome) && identical(z$outcome,a$common_variable) &&
        identical(z$timepoint,a$timepoint),logical(1)))
      if(!matched)add(planned,a$common_variable,a$timepoint,"evidence.synthesis_plan",
        if(identical(planned,"NARRATIVE_SWIM") && length(sets)==1L &&
          isTRUE(e$.synthesis_execution_fresh))"BOUND_SYNTHESIS" else "APPROVED_PLAN")
    }
  }
  route<-srma_qc_adaptive_method(source$route)
  named<-function(m)any(vapply(entries,function(z)identical(z$method,m),logical(1)))
  for(flag in srma_qc_link_ids(source$analysis_profiles)) {
    method<-srma_qc_adaptive_method(flag)
    if(!named(method))add(method,ref="manuscript.source.analysis_profiles")
  }
  generic<-srma_qc_adaptive_key(srma_qc_one(source$route)) %in% c("quantitative","meta analysis")
  explicit<-length(source$analyses)>0L || any(vapply(entries,function(z)
    !z$method %in% c("NARRATIVE_SWIM","PAIRWISE_META"),logical(1)))
  if(nzchar(route) && !named(route) && !(generic && explicit))add(route,ref="manuscript.source.route")
  entries
}

srma_qc_adaptive_reporting <- function(entries,links,intervention_review=TRUE) {
  catalog<-srma_qc_adaptive_method_catalog()
  active<-unique(vapply(entries,function(a)a$method,character(1)))
  reporting<-list(PRISMA_2020=srma_qc_adaptive_proof("NOT_ASSESSED",
    if(identical(links$prisma$stage,"VERIFIED_SOURCE"))
      "The flow evidence is verified; full checklist and abstract coverage are separate."
    else "The review requires PRISMA 2020; an exact-version flow/checklist audit is pending.",
    "canonical_selection_and_reporting"))
  if(!is.null(links$prisma))reporting$PRISMA_2020$flow<-links$prisma[c("stage","approval","source")]
  for(m in names(catalog)) {
    extension<-catalog[[m]]$reporting
    if(is.null(extension))next
    relevant<-Filter(function(r)identical(r$method,m),entries)
    confirmed<-Filter(function(r)r$execution %in% c("VERIFIED_EXECUTION","BOUND_SYNTHESIS"),relevant)
    if(!length(relevant)) {
      known<-length(active)>0L && all(active %in% names(catalog))
      reporting[[extension]]<-srma_qc_adaptive_proof(if(known)"NOT_APPLICABLE" else "NOT_ASSESSED",
        if(known)"This synthesis method was not declared or executed for an outcome."
        else "Synthesis method is not established.", "synthesis_route")
    } else if(m=="NARRATIVE_SWIM" && !intervention_review) {
      reporting[[extension]]<-srma_qc_adaptive_proof("NOT_APPLICABLE",
        "This approved review domain is outside quantitative intervention-effect synthesis.","protocol.domain")
    } else if(m=="IPD_SURVIVAL" && length(confirmed) && all(vapply(confirmed,function(r)
      r$ipd_origin %in% c("reconstructed ipd","reconstructed km","reconstructed"),logical(1)))) {
      reporting[[extension]]<-srma_qc_adaptive_proof("NOT_ASSESSED",
        "Review applicable IPD items and disclose reconstruction. Original-data access items do not apply.",
        "source.analyses.result.ipd_origin")
      reporting[["PRISMA-IPD original data access"]]<-srma_qc_adaptive_proof("NOT_APPLICABLE",
        "Verified analysis used reconstructed curves.","source.analyses.result.ipd_origin")
    } else {
      reporting[[extension]]<-srma_qc_adaptive_proof("NOT_ASSESSED",
        if(length(confirmed))"The execution selects this extension; checklist compliance remains unverified."
        else "The method is declared; the qualifying execution and checklist are unverified.","synthesis_route")
    }
  }
  reporting
}

srma_qc_adaptive_design_catalog <- function() list(
  RCT=list(aliases=c("rct","randomized controlled trial","randomised controlled trial",
    "individually randomized parallel group trial"),tool="ROB2",validator="CANONICAL_R36_R37"),
  NRSI=list(aliases=c("nrsi","nonrandomized study of interventions","non randomized study of interventions",
    "nonrandomized comparative cohort study","nonrandomized comparative study","nonrandomized comparative cohort",
    "comparative cohort","comparative cohort study","comparative observational study","comparative registry"),
    tool="ROBINS_I",validator="CANONICAL_R36_R37"),
  CLUSTER_RCT=list(aliases=c("cluster rct","cluster randomized trial","cluster randomized controlled trial",
    "cluster randomised trial","cluster randomised controlled trial"),tool="ROB2_CLUSTER"),
  CROSSOVER_RCT=list(aliases=c("crossover rct","randomized crossover trial","randomised crossover trial",
    "cross over rct","randomized cross over trial"),tool="ROB2_CROSSOVER"),
  DTA=list(aliases=c("diagnostic test accuracy","diagnostic accuracy study","dta"),tool="QUADAS"),
  EXPOSURE=list(aliases=c("exposure study","nonrandomized study of exposures"),tool="ROBINS_E"),
  PREDICTION=list(aliases=c("prediction model","prognostic prediction model"),tool="PROBAST"),
  SINGLE_ARM=list(aliases=c("single arm","single arm observational cohort","case series","single arm cohort")))

srma_qc_adaptive_design <- function(value) {
  key<-srma_qc_adaptive_key(srma_qc_one(value))
  hit<-names(Filter(function(z)key %in% z$aliases,srma_qc_adaptive_design_catalog()))
  if(length(hit)==1L)hit else if(nzchar(key))"OTHER_DESIGN" else ""
}
srma_qc_adaptive_tool <- function(value) {
  key<-srma_qc_adaptive_key(srma_qc_one(value))
  switch(key,"rob 2"="ROB2","cochrane rob 2"="ROB2","rob 2 2019"="ROB2","rob2"="ROB2","robins i"="ROBINS_I",
    "robins i 2016"="ROBINS_I","robins i v2"="ROBINS_I_V2","rob 2 cluster"="ROB2_CLUSTER",
    "rob 2 crossover"="ROB2_CROSSOVER","quadas 2"="QUADAS","quadas c"="QUADAS","robins e"="ROBINS_E",
    "probast"="PROBAST",if(nzchar(key))key else "")
}

srma_qc_adaptive_tools <- function(config,design) {
  if(!is.list(config))return(srma_qc_link_ids(config))
  specific<-switch(design,RCT=c("randomized_trials","RCT"),NRSI=c("nonrandomized_studies","NRSI"),
    CLUSTER_RCT="cluster_trials",CROSSOVER_RCT="crossover_trials",character())
  chosen<-srma_qc_link_ids(config[intersect(specific,names(config))])
  if(length(chosen))chosen else srma_qc_link_ids(config[intersect(c("tool","tools","instrument"),names(config))])
}

srma_qc_adaptive_study_rob <- function(e,ids,config,protocol,links,intervention_review,manifest) {
  plan<-protocol$risk_of_bias
  if(is.list(plan) && identical(plan$applicable,FALSE) && nzchar(srma_qc_one(plan$rationale)))
    return(list(overall=srma_qc_adaptive_proof("NOT_APPLICABLE",plan$rationale,
      "approved_protocol.risk_of_bias","APPROVED"),studies=list()))
  if(!length(ids))return(list(overall=srma_qc_adaptive_proof("NOT_ASSESSED",
    "No bound Human Finalized Study Selection is available.","evidence.final_study_selection"),studies=list()))
  catalog<-srma_qc_adaptive_design_catalog()
  assess<-srma_qc_link_list(e$tables$Overall_Review)
  studies<-srma_qc_link_list(e$tables$Target_Study_Verification)
  selected_reports<-srma_qc_link_list(e$tables$Author_Final_Review)
  verified<-function(r)srma_qc_one(r$verification_status) %in% c("Verified","Corrected") &&
    nzchar(srma_qc_one(r$.verified_by)) && nzchar(srma_qc_one(r$source_location))
  planned_designs<-unique(vapply(srma_qc_link_ids(config$eligibility$study_designs$include),
    srma_qc_adaptive_design,character(1)))
  approved<-identical(links$rob$approval,"APPROVED") && identical(links$rob$stage,"VERIFIED_SOURCE")
  reviews<-lapply(ids,function(id) {
    records<-Filter(function(r)identical(srma_qc_one(r$study_id),id),selected_reports)
    reports<-srma_qc_link_ids(lapply(records,function(r)r$screening_id))
    src<-Filter(function(r)verified(r) && srma_qc_link_sha(r$.pdf_sha256) &&
      any(vapply(records,function(p)identical(r$screening_id,p$screening_id) &&
        identical(r$.pdf_sha256,p$.pdf_sha256) && nzchar(srma_qc_one(p$.pdf_relative)) &&
        identical(manifest[[p$.pdf_relative]],r$.pdf_sha256),logical(1))),studies)
    actual_designs<-unique(vapply(src,function(r)srma_qc_adaptive_design(
      srma_qc_one(r$human_verified_study_design,srma_qc_one(r$study_design))),character(1)))
    actual_designs<-actual_designs[nzchar(actual_designs)]
    design<-if(length(actual_designs)==1L)actual_designs[[1L]] else ""
    expected<-if(nzchar(design))catalog[[design]]$tool else NULL
    choices<-lapply(list(config$risk_of_bias,plan),function(z)
      vapply(srma_qc_adaptive_tools(z,design),srma_qc_adaptive_tool,character(1)))
    tools<-unique(unlist(choices,use.names=FALSE))
    if(!intervention_review && design %in% c("NRSI","RCT"))expected<-NULL
    assessed<-Filter(function(r)nzchar(srma_qc_one(r$.actor)) &&
      nzchar(srma_qc_one(r$author_final_overall_judgement)) &&
      nzchar(srma_qc_one(r$author_final_overall_rationale)) &&
      (identical(srma_qc_one(r$study_id),id) || srma_qc_one(r$screening_id) %in% reports),assess)
    used<-unique(vapply(assessed,function(r)srma_qc_adaptive_tool(r$rob_tool),character(1)))
    used<-used[nzchar(used)]
    decision<-if(!nzchar(design))srma_qc_adaptive_proof("NOT_ASSESSED",
      "Study design needs an unambiguous Human-verified source.","evidence.study_verification")
    else if(length(planned_designs) && !"OTHER_DESIGN" %in% planned_designs &&
            design!="OTHER_DESIGN" && !design %in% planned_designs)
      srma_qc_adaptive_proof("BLOCKED","Verified design conflicts with approved eligibility.","approved_protocol.study_designs")
    else if(!length(expected) || !length(catalog[[design]]$validator))
      srma_qc_adaptive_proof("NOT_ASSESSED","Study-specific RoB validator is unavailable.","evidence.study_verification")
    else if(any(vapply(choices,function(t)length(t)>0L && !expected %in% t,logical(1))) ||
            (length(used) && any(used!=expected)))
      srma_qc_adaptive_proof("BLOCKED","Configured or recorded RoB tool conflicts with the verified design.",
        "approved_protocol.risk_of_bias / evidence.Overall_Review")
    else if(approved && length(used))
      srma_qc_adaptive_proof("PASS","The approved assessment tool matches the verified design.",
        "evidence.Overall_Review","APPROVED")
    else srma_qc_adaptive_proof("NOT_ASSESSED","A source-bound, approved study/result assessment is pending.",
      "evidence.Overall_Review")
    c(list(study_id=id,design=design,tool=expected,configured_tools=as.list(tools),actual_tools=as.list(used)),decision)
  })
  list(studies=reviews)
}

srma_qc_adaptive_outcome_catalog <- function() list(
  DICHOTOMOUS=list(aliases=c("dichotomous","binary","raw dichotomous"),measures=c("RR","OR","RD")),
  CONTINUOUS=list(aliases=c("continuous","raw continuous"),measures=c("MD","SMD")),
  TIME_TO_EVENT=list(aliases=c("time to event","survival"),measures="HR"),
  REPORTED_EFFECT=list(aliases=c("reported effect"),measures=c("RR","OR","RD","HR","MD","SMD")))

srma_qc_adaptive_pairwise <- function(entry,x,e) {
  if(!nzchar(entry$analysis_id) || !identical(entry$execution,"VERIFIED_EXECUTION"))
    return(srma_qc_adaptive_proof("NOT_ASSESSED","Approved execution for this known outcome/timepoint is pending.",entry$source_ref))
  a<-x$source$analyses[[entry$analysis_id]]
  p<-a$plan
  result<-a$result
  valid_ids<-srma_qc_link_ids(p$row_ids)
  rows<-Filter(function(r)srma_qc_one(r$outcome_row_id) %in% valid_ids,
    srma_qc_link_list(e$tables$Target_Outcome_Verification))
  if(!identical(result$contract,"web-analysis-v1"))return(srma_qc_adaptive_proof(
    "NOT_ASSESSED","The approval has no supported pairwise execution contract.",entry$source_ref))
  if(!length(valid_ids) || !setequal(valid_ids,srma_qc_link_ids(result$included_rows)) ||
     !identical(srma_qc_one(p$effect_measure),srma_qc_one(result$summary$effect_measure)))
    return(srma_qc_adaptive_proof("BLOCKED","The execution differs from the approved participant/measure plan.",entry$source_ref))
  unique_rows<-length(rows)==length(valid_ids) && setequal(valid_ids,srma_qc_link_ids(lapply(rows,function(r)r$outcome_row_id)))
  verified<-unique_rows && all(vapply(rows,function(r)nzchar(srma_qc_one(r$.verified_by)) &&
    srma_qc_one(r$verification_status) %in% c("Verified","Corrected"),logical(1)))
  labels<-if(verified)unique(vapply(rows,function(r)srma_qc_adaptive_key(srma_qc_one(r$human_verified_data_type,
    srma_qc_one(r$data_type))),character(1))) else character()
  types<-srma_qc_adaptive_outcome_catalog()
  known<-vapply(labels,function(label)any(vapply(types,function(t)label %in% t$aliases,logical(1))),logical(1))
  compatible<-vapply(labels[known],function(label)any(vapply(types,function(t)
    label %in% t$aliases && srma_qc_one(p$effect_measure) %in% t$measures,logical(1))),logical(1))
  if(any(!compatible))return(srma_qc_adaptive_proof("BLOCKED",
    "The approved effect measure conflicts with the verified outcome data type.",entry$source_ref))
  required<-c(p$definition,p$timepoint,p$population)
  documented<-length(required)==3L && all(nzchar(as.character(unlist(required))))
  timing<-if(verified)vapply(rows,function(r)srma_qc_adaptive_key(srma_qc_one(r$human_verified_timepoint,
    srma_qc_one(r$timepoint))),character(1)) else character()
  definitions<-if(verified)vapply(rows,function(r)srma_qc_adaptive_key(srma_qc_one(r$human_verified_outcome_definition,
    srma_qc_one(r$outcome_definition))),character(1)) else character()
  if(length(timing) && (any(nzchar(timing) & timing!=srma_qc_adaptive_key(srma_qc_one(p$timepoint))) ||
     any(nzchar(definitions) & definitions!=srma_qc_adaptive_key(srma_qc_one(p$definition)))))
    return(srma_qc_adaptive_proof("NOT_ASSESSED",
      "The reported and planned outcome definitions or timepoints require approved harmonization.",entry$source_ref))
  if(!verified || !documented || !length(labels) || !all(known) || !identical(result$consistency$status,"PASS"))
    return(srma_qc_adaptive_proof("NOT_ASSESSED",
      "The outcome definition, time, population, type or machine table/figure comparison is incomplete.",entry$source_ref))
  srma_qc_adaptive_proof("PASS","Approved plan and result match on outcome type, effect measure and contributing rows.",
    entry$source_ref,"APPROVED")
}

srma_qc_adaptive_swim <- function(rows,entries,final) {
  applicable<-Filter(function(a)identical(a$method,"NARRATIVE_SWIM"),entries)
  grouped<-length(applicable)>0L && all(vapply(applicable,function(a)
    identical(a$execution,"BOUND_SYNTHESIS") && nzchar(a$outcome) && nzchar(a$timepoint),logical(1)))
  applicable_rows<-Filter(function(r)srma_qc_one(r$decision) %in% c("CONFIRMED","CORRECTED"),rows)
  linked<-final && length(applicable_rows)>0L && all(vapply(applicable_rows,function(r)
    nzchar(srma_qc_one(r$study_id)) && nzchar(srma_qc_one(r$source_location)) &&
      nzchar(srma_qc_one(r$numeric_summary)),logical(1)))
  list(grouping=srma_qc_adaptive_proof(if(grouped)"PASS" else "NOT_ASSESSED",
    "Checks for saved grouping by outcome and timepoint; the rationale still needs reporting review.","evidence.synthesis_plan"),
    methods=srma_qc_adaptive_proof(if(grouped)"PASS" else "NOT_ASSESSED",
      "Checks the recorded decision to use synthesis without pooling.","evidence.synthesis_plan"),
    contributor_lineage=srma_qc_adaptive_proof(if(linked)"PASS" else "NOT_ASSESSED",
      "Checks links between synthesized results and their finalized study sources.","manuscript.source.narrative"),
    remaining_items=srma_qc_adaptive_proof("NOT_ASSESSED",
      "Metric transformations, prioritization, heterogeneity, study presentation, certainty, and limitations require a separate checklist audit.",
      "SWiM"))
}

srma_qc_adaptive_context <- function(x,e,configuration,manifest,manifest_bound,protocol_bound,
                                    definition_bound,links,selection_final,narrative_final,verified_analysis_ids,
                                    approved_artifacts,primary) {
  source<-x$source
  protocol<-if(protocol_bound && is.list(source$protocol))source$protocol else list()
  config<-if(manifest_bound && is.list(configuration) &&
    identical(source$project_version,x$evidence_version))configuration else list()
  approved_config<-if(protocol_bound)config else list()
  domain<-srma_qc_adaptive_key(srma_qc_one(protocol$review_type,srma_qc_one(approved_config$review_type)))
  non_intervention<-c("diagnostic","diagnostic test accuracy","prognostic","prognosis",
    "prediction model","qualitative","qualitative synthesis","scoping","scoping review","exposure")
  intervention<-!domain %in% non_intervention
  entries<-srma_qc_adaptive_methods(x,e,manifest_bound,verified_analysis_ids)
  planned_setting<-srma_qc_one(protocol$synthesis_method,
    srma_qc_one(srma_qc_adaptive_field(approved_config$analysis,"synthesis_method")))
  planned<-srma_qc_adaptive_method(planned_setting)
  if(srma_qc_adaptive_key(planned_setting) %in% c("quantitative","meta analysis") &&
     any(vapply(entries,function(a)!a$method %in% c("NARRATIVE_SWIM","PAIRWISE_META"),logical(1))))
    planned<-""
  if(!intervention && !nzchar(planned))planned<-srma_qc_adaptive_method(domain)
  # A recorded plan establishes a requirement; it cannot assert an execution.
  if(nzchar(planned) && !any(vapply(entries,function(a)identical(a$method,planned),logical(1))))
    entries<-c(entries,list(list(method=planned,outcome="",timepoint="",source_ref="protocol.synthesis_method",
      execution="APPROVED_PLAN",ipd_origin="",analysis_id="")))
  catalog<-srma_qc_adaptive_method_catalog()
  profiles<-unique(c("CORE",vapply(entries,function(r)r$method,character(1))))
  reporting<-srma_qc_adaptive_reporting(entries,links,intervention)
  if("NARRATIVE_SWIM" %in% profiles && intervention)
    reporting$SWiM$coverage<-srma_qc_adaptive_swim(srma_qc_link_list(source$narrative$rows),entries,narrative_final)
  ids<-if(selection_final)srma_qc_link_ids(e$final_study_selection$study_ids) else character()
  # The screening definition approves its eligibility fields, not RoB outcomes.
  rob_config<-approved_config
  if(definition_bound)rob_config$eligibility<-config$eligibility
  rob<-srma_qc_adaptive_study_rob(e,ids,rob_config,protocol,links,intervention,manifest)
  methods<-lapply(entries,function(a) {
    validator<-catalog[[a$method]]$validator
    check<-if(identical(a$method,"PAIRWISE_META") && intervention)srma_qc_adaptive_pairwise(a,x,e)
    else if(identical(a$method,"NARRATIVE_SWIM") && narrative_final)
      srma_qc_adaptive_proof("PASS","The narrative sources are bound to finalized study results.",
        a$source_ref,"APPROVED")
    else srma_qc_adaptive_proof("NOT_ASSESSED",if(length(validator))
      "The approved synthesis sources are pending." else "No approved validator is installed for this method.",a$source_ref)
    c(a,list(validator=validator,validation=check))
  })
  blockers<-character()
  # Older records may omit the route. New or declared unsupported methods
  # cannot become release-ready through the generic pairwise checks.
  for(a in methods) {
    legacy_unset<-identical(a$source_ref,"manuscript.source.route") &&
      identical(srma_qc_adaptive_key(srma_qc_one(source$route)),"other")
    if((!length(a$validator) && !legacy_unset) ||
       (!intervention && identical(a$method,"PAIRWISE_META")))
      blockers<-c(blockers,"ADAPTIVE_METHOD_VALIDATOR_UNAVAILABLE")
    if(identical(a$validation$status,"BLOCKED"))
      blockers<-c(blockers,"ADAPTIVE_OUTCOME_CONFLICT")
    if(identical(a$method,"PAIRWISE_META") && a$execution %in% c("VERIFIED_EXECUTION","APPROVED_PLAN") &&
       identical(a$validation$status,"NOT_ASSESSED"))blockers<-c(blockers,"ADAPTIVE_PAIRWISE_VALIDATION_PENDING")
  }
  rob_states<-vapply(rob$studies,function(r)r$status,character(1))
  if(any(rob_states=="BLOCKED"))blockers<-c(blockers,"ADAPTIVE_STUDY_DESIGN_OR_ROB_CONFLICT")
  designs<-srma_qc_adaptive_design_catalog()
  has_validator<-vapply(rob$studies,function(r)nzchar(r$design) &&
    length(designs[[r$design]]$validator)>0L,logical(1))
  known_design<-vapply(rob$studies,function(r)nzchar(r$design),logical(1))
  if(any(known_design & !has_validator))
    blockers<-c(blockers,"ADAPTIVE_ROB_VALIDATOR_UNAVAILABLE")
  requires_design<-definition_bound && length(srma_qc_link_ids(config$eligibility$study_designs$include))>0L
  if(any(rob_states=="NOT_ASSESSED" & (has_validator | (requires_design & !known_design))))
    blockers<-c(blockers,"ADAPTIVE_ROB_ASSESSMENT_PENDING")
  if(narrative_final && any(vapply(primary,function(r)r$state %in% c("REVIEW_PENDING","NO_MATCH_IN_BOUND_INVENTORY"),logical(1))))
    blockers<-c(blockers,"ADAPTIVE_PRIMARY_OUTCOME_PENDING")
  if("PAIRWISE_META" %in% profiles) {
    labels<-srma_qc_link_ids(source$research_question$outcomes[c("primary","critical")])
    if(is.null(primary) && length(labels))primary<-lapply(labels,function(label) {
      matching<-Filter(function(z)identical(srma_qc_adaptive_key(z$outcome),srma_qc_adaptive_key(label)),methods)
      list(label=label,state=if(any(vapply(matching,function(z)
        identical(z$validation$status,"PASS"),logical(1))))"VERIFIED_ROWS" else "REVIEW_PENDING")
    })
    if(length(labels) && any(vapply(primary,function(r)identical(r$state,"REVIEW_PENDING"),logical(1))))
      blockers<-c(blockers,"ADAPTIVE_PRIMARY_OUTCOME_PENDING")
  }
  pending_r40_requirement<-!protocol_bound && isTRUE(srma_qc_adaptive_field(source$protocol,"r40_required"))
  if(pending_r40_requirement)blockers<-c(blockers,"R40_PROTOCOL_APPROVAL_PENDING")
  required_r40<-length(x$production$qc$publication_9cd)>0L || isTRUE(protocol$r40_required) ||
    any(profiles %in% c("PAIRWISE_META","IPD_SURVIVAL","NMA","DTA","ADVANCED"))
  results<-Filter(function(a)identical(a$family,"r40"),approved_artifacts)
  r40_pass<-length(results)>0L && all(vapply(results,function(a)identical(a$status,"PASS"),logical(1)))
  r40<-if(r40_pass)srma_qc_adaptive_proof("PASS","Exact-version, approved R/40 evidence is registered.",
    "canonical_registry.r40","APPROVED")
  else if(required_r40)srma_qc_adaptive_proof("NOT_ASSESSED",
    "Exact-version R/40 proof is required. The Web Worker has no standalone R/40 action.",
    "canonical_registry.r40")
  else if(pending_r40_requirement)srma_qc_adaptive_proof("NOT_ASSESSED",
    "The draft protocol declares an R/40 requirement that needs protocol approval.","protocol.r40_required")
  else if(identical(profiles,c("CORE","NARRATIVE_SWIM")) && narrative_final)
    srma_qc_adaptive_proof("NOT_APPLICABLE","This finalized non-production route has no quantitative or explicit R/40 requirement.",
      "evidence.synthesis_plan")
  else srma_qc_adaptive_proof("NOT_ASSESSED","The applicable R/40 route is not established.","synthesis_route")
  if(any(profiles %in% c("IPD_SURVIVAL","NMA","DTA","ADVANCED"))) {
    r40<-srma_qc_adaptive_proof("NOT_ASSESSED","Specialized method validation is unavailable for the selected route.",
      "synthesis_route")
    r40$registered_generic_proof<-r40_pass
    r40$execution_path<-"NO_APPROVED_SPECIALIZED_WORKER_ROUTE"
  } else r40$execution_path<-if(required_r40)"EXACT_VERSION_APPROVED_IMPORT" else "NO_AUTOMATIC_EXECUTION"
  route_profiles<-setdiff(profiles,"CORE")
  if((identical(route_profiles,"NARRATIVE_SWIM") && identical(source$route,"NARRATIVE")) ||
     (identical(route_profiles,"PAIRWISE_META") && is.null(x$production)))
    r40$deterministic_execution_path<-"WEB_WORKER_SUBSET_FOR_CURRENT_APPROVED_MANUSCRIPT"
  pico<-if((definition_bound || protocol_bound) && is.list(config$research_question) &&
    identical(web_c2_hash(config$research_question),web_c2_hash(source$research_question)))
    srma_qc_adaptive_proof("PASS","The frozen question matches the approved screening definition; clinical eligibility remains separate.",
      "approved_configuration.research_question","APPROVED")
  else srma_qc_adaptive_proof("NOT_ASSESSED","No matching approved question and eligibility definition was established.",
    "configuration.research_question")
  certainty<-config$certainty_of_evidence
  certainty_required<-isTRUE(certainty) || (is.list(certainty) &&
    (isTRUE(certainty$required) || nzchar(srma_qc_one(certainty$framework))))
  list(source_sha256=x$source_sha256,evidence_version=x$evidence_version,
    profiles=as.list(profiles),methods=methods,reporting=reporting,
    unclassified_synthesis=if(!length(entries))srma_qc_adaptive_proof(
      "NOT_ASSESSED","There is no bound synthesis plan or executed method.","synthesis_route"),
    pico=pico,rob=rob,primary_outcomes=primary,r40=r40,r40_required=required_r40,
    protocol_requirements_approved=protocol_bound,
    certainty_required=certainty_required,
    blockers=as.list(unique(blockers)))
}
