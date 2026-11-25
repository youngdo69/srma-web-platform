# Statistical reporting consumes registered executions. It never fits a model.
web_stat_text <- function(x) {
  if(length(x)!=1L || !is.atomic(x) || is.na(x))return("")
  trimws(as.character(x))
}

web_stat_hash <- function(x)grepl("^[0-9a-f]{64}$",web_stat_text(x))

web_stat_registry_report <- function(con,settings,actor,project_id,x,evidence,profiles,protocol,ids) {
  script<-"27_run_universal_meta_analysis.R"
  manifest<-if(length(ids))jsonlite::read_json(file.path(settings$engine_root,"web/docs/engine-source-manifest.json"))
  expected_script<-manifest[[paste0("R/",script)]]
  verified<-setNames(lapply(ids,function(id)tryCatch({
    a<-web_c1_context(con,settings,actor,project_id,id)
    if(web_stat_hash(expected_script) && identical(a$packet$result$canonical_scripts[[script]],expected_script) &&
       identical(web_c2_hash(a$packet),web_c2_hash(x$source$analyses[[id]])))a
  },error=function(e)NULL)),ids)
  read_output<-function(v,path) {
    a<-verified[[v]]
    expected<-a$packet$result$output_hashes[[path]]
    if(!web_stat_hash(expected) || !identical(a$version$manifest[[path]],expected))return(NULL)
    file<-web_inside(file.path(a$version$root,path),a$version$root)
    if(identical(web_sha(file),expected))web_c1_csv(file)
  }
  web_stat_report(x,evidence,profiles,protocol,names(Filter(Negate(is.null),verified)),read_output)
}

web_stat_draft_source <- function(con,settings,actor,project_id,x,evidence,version) {
  if(!exists("srma_qc_link_configuration_gate",mode="function"))
    source(file.path(settings$engine_root,"web/qc/runtime.R"),local=environment(web_stat_draft_source),encoding="UTF-8")
  id<-x$source$project_version
  config<-web_edit_configuration(con,settings,actor,project_id,id)
  row<-web_query(con,"SELECT version,config_hash FROM project_versions WHERE project_id=?p AND id=?v",p=project_id,v=id)
  sha<-version$manifest[["config/project_config.yml"]]
  gate<-if(nrow(row)==1L && web_stat_hash(sha) && identical(row$config_hash,sha))
    srma_qc_link_configuration_gate(con,project_id,id,row$version,sha,"configuration_human_review")
  if(is.null(gate) || !is.list(config$protocol) || !length(config$protocol) ||
     !identical(config,web_edit_configuration(con,settings,actor,project_id,gate$approval_version)))
    return(list(status="NOT_ASSESSED",gaps=list("approved_protocol"),records=list()))
  ids<-srma_qc_adaptive_analysis_ids(con,project_id,x)
  methods<-srma_qc_adaptive_methods(x,evidence,TRUE,ids)
  profiles<-unique(vapply(methods,`[[`,character(1),"method"))
  report<-web_stat_registry_report(con,settings,actor,project_id,x,evidence,profiles,config$protocol,ids)
  report$protocol<-gate[c("version","approval_version","sha256","approval_ids")]
  report
}

web_stat_report <- function(x,evidence,profiles,protocol,verified_analyses=character(),read_output=NULL) {
  s<-x$source
  profiles<-sort(setdiff(unlist(profiles,use.names=FALSE),"CORE"))
  planned<-if(is.list(protocol$statistical_methods))protocol$statistical_methods else list()
  narrative_plan<-if(is.list(planned$narrative))planned$narrative else list()
  analysis_plans<-if(is.list(planned$analyses))planned$analyses else list()
  records<-list()
  gaps<-character()
  conflicts<-character()
  add<-function(id,field,text,proof) {
    records[[paste(id,field,sep="/")]]<<-list(field=field,text=text,proof=proof)
  }
  gap<-function(id,field)gaps<<-c(gaps,paste(id,field,sep="/"))
  required<-function(values,id) {
    for(k in names(values))if(!nzchar(web_stat_text(values[[k]])))gap(id,k)
  }
  narrative<-"NARRATIVE_SWIM" %in% profiles
  pairwise<-"PAIRWISE_META" %in% profiles
  if(!length(profiles) || !all(profiles %in% c("NARRATIVE_SWIM","PAIRWISE_META")))gap("synthesis","approved_supported_route")
  if(narrative) {
    plan<-evidence$synthesis_plan
    sets<-evidence$synthesis_execution$sets
    witness<-list(evidence_version=x$evidence_version,plan_sha256=web_c2_hash(plan),
      synthesis_sha256=evidence$synthesis_execution$source_digest)
    if(!identical(plan$status,"HUMAN_CONFIRMED") || !isTRUE(evidence$.synthesis_execution_fresh) ||
       !web_stat_hash(witness$synthesis_sha256))gap("narrative","finalized_synthesis") else {
      selected<-Filter(function(a)identical(a$mode,"NARRATIVE"),plan$decisions)
      if(!length(selected))gap("narrative","outcome_scope")
      for(d in selected) {
        id<-d$analysis_group
        match<-Filter(function(a)identical(a$analysis_group,id) && identical(a$route,"NARRATIVE"),sets)
        if(length(match)!=1L) {gap(id,"narrative_execution");next}
        a<-match[[1L]]
        group<-if(is.list(narrative_plan$groups))narrative_plan$groups[[id]]
        fields<-list(outcome=a$common_variable,timepoint=a$timepoint,
          no_pooling_reason=(if(is.list(group))group$rationale) %||b2% narrative_plan$rationale,
          comparison_method=narrative_plan$comparison_method)
        required(fields,id)
        if(all(vapply(fields,function(v)nzchar(web_stat_text(v)),logical(1))))
          add(id,"narrative",paste0(a$common_variable," at ",a$timepoint," was synthesized without pooling. ",
            fields$no_pooling_reason," Study-level results were grouped by the approved outcome and time point. ",
            "The protocol specified the following comparison approach. ",fields$comparison_method),c(witness,list(group=id)))
        if(!identical(narrative_plan$missing_data,"PRESERVE_UNREPORTED"))gap(id,"missing_data") else
          add(id,"missing_data","Missing or unreported results were left unquantified in the study-level synthesis.",witness)
      }
    }
  }
  ids<-names(s$analyses)
  if(pairwise && (!length(ids) || !setequal(ids,verified_analyses)))gap("pairwise","approved_executions")
  if(!pairwise && length(ids))gap("synthesis","unexpected_quantitative_execution")
  for(id in intersect(ids,verified_analyses)) {
    a<-s$analyses[[id]]
    p<-a$plan
    r<-a$result
    z<-r$summary
    config<-analysis_plans[[p$group]]
    if(!is.list(config))config<-list()
    proof<-list(analysis_version=id,plan_sha256=r$plan_sha256,job_id=r$job_id,
      engine_script_sha256=r$canonical_scripts[["27_run_universal_meta_analysis.R"]],
      execution_sha256=web_c2_hash(r),approval=a$result_review$gate_id)
    if(!identical(a$stage,"APPROVED") || !identical(r$status,"PASS") ||
       !identical(a$evidence_version,x$evidence_version) || !identical(r$evidence_version,x$evidence_version) ||
       !identical(r$plan_sha256,web_c1_hash(p)) || !nzchar(web_stat_text(proof$approval)) ||
       !web_stat_hash(proof$engine_script_sha256)) {
      gap(id,"result_binding");next
    }
    fields<-list(outcome=p$outcome,timepoint=p$timepoint,design=p$design,definition=p$definition,
      population=p$population,estimate_basis=p$estimate_basis,effect=z$effect_measure,
      model=z$model_name,tau=z$tau2_method,ci_method=z$inference_test,ci_level=p$methods$confidence,
      effect_direction=config$effect_direction,reference_group=config$reference_group,
      software=r$R,metafor=r$packages$metafor)
    required(fields,id)
    study<-r$study_effects
    types<-unique(vapply(study,function(row)web_stat_text(row$meta_input_type),character(1)))
    direction_ok<-length(types)>0L && all(types %in% c("raw_dichotomous","raw_continuous")) &&
      identical(config$effect_direction,"INTERVENTION_VS_COMPARATOR")
    if(identical(types,"reported_effect"))direction_ok<-length(study)>0L && all(vapply(study,function(row)
      nzchar(web_stat_text(row$effect_direction)) && identical(row$effect_direction,config$effect_direction) &&
      nzchar(web_stat_text(row$reference_group)) && identical(row$reference_group,config$reference_group),logical(1)))
    if(!direction_ok)gap(id,"effect_direction_execution")
    if(!identical(z$effect_measure,p$effect_measure) || !identical(z$model_name,p$methods$model) ||
       !identical(z$tau2_method,p$methods$estimator) || !identical(z$inference_test,p$methods$inference))
      conflicts<-c(conflicts,paste0(id,"/settings_execution_conflict"))
    number<-function(v)is.numeric(v) && length(v)==1L && is.finite(v)
    if(!all(vapply(list(z$tau2,z$i2,z$q,z$q_df,z$q_p_value),number,logical(1))))gap(id,"heterogeneity")
    if(direction_ok && all(vapply(fields,function(v)nzchar(web_stat_text(v)),logical(1)))) {
      direction<-if(identical(config$effect_direction,"INTERVENTION_VS_COMPARATOR"))"intervention versus comparator" else
        config$effect_direction
      add(id,"design_effect",paste0("For ",p$outcome," (",p$definition,") at ",p$timepoint,
        ", studies of ",p$design," in ",p$population," contributed ",tolower(p$estimate_basis),
        " estimates as ",z$effect_measure,". The effect direction was ",direction,
        " with ",config$reference_group," as the reference group."),proof)
      add(id,"model_ci",paste0(z$model_name," models estimated between-study variance using ",z$tau2_method,
        ". Pooled ",p$methods$confidence," confidence intervals used ",z$inference_test," inference."),proof)
      add(id,"software",paste0("The models were fitted in ",r$R," using metafor version ",r$packages$metafor,"."),proof)
    }
    missing<-config$missing_data %||b2% planned$missing_data
    if(!identical(missing,"AVAILABLE_VERIFIED_INPUTS"))gap(id,"missing_data") else
      add(id,"missing_data","Fitted analyses used the verified, estimable inputs in each approved analysis set. Missing inputs outside that set were not included in the fitted model.",proof)
    significance<-config$significance %||b2% planned$significance
    alpha<-if(is.list(significance))significance$alpha
    if(!is.numeric(alpha) || length(alpha)!=1L || !is.finite(alpha) || alpha<=0 || alpha>=1 ||
       !identical(significance$sidedness,"TWO_SIDED"))gap(id,"significance") else
      add(id,"significance",paste0("The protocol prespecified a two-sided significance level of ",alpha,"."),proof)
    if(all(vapply(list(z$tau2,z$i2,z$q,z$q_df,z$q_p_value),number,logical(1))))
      add(id,"heterogeneity","Heterogeneity was assessed using Cochran's Q, I², and τ².",proof)
    read_field<-function(k)vapply(study,function(row)if(number(row[[k]]))as.numeric(row[[k]]) else NA_real_,numeric(1))
    vi<-read_field("vi")
    weights<-read_field("random_effect_weight_percent")
    if(length(vi) && number(z$tau2) && z$tau2>=0 && all(is.finite(vi) & vi>0)) {
      expected<-1/(vi+z$tau2)
      if(isTRUE(all.equal(weights,100*expected/sum(expected),tolerance=1e-8)))
        add(id,"weighting","Studies were weighted by the inverse of their sampling variance plus the estimated between-study variance.",proof)
      else gap(id,"weights")
    } else gap(id,"weights")
    if(!length(types) || any(!nzchar(types)))gap(id,"effect_input_type")
    if("raw_dichotomous" %in% types) {
      if(!identical(p$methods$binary_correction,web_c1_methods()$binary_correction))gap(id,"sparse_data_policy") else
        add(id,"sparse_data","For arm-level binary data, 0.5 was added to the cells of tables containing a zero cell. Double-zero studies were retained at the effect-size preparation step.",proof)
    }
    if(p$role %in% c("SUBGROUP","SENSITIVITY","EXPLORATORY")) {
      parent<-s$analyses[[web_stat_text(p$parent_version)]]
      if(is.null(parent) || !p$parent_version %in% verified_analyses || !nzchar(web_stat_text(p$rationale)))gap(id,"linked_analysis") else
        add(id,"linked_analysis",paste0(tools::toTitleCase(tolower(p$role))," analysis of ",parent$plan$outcome,
          " at ",parent$plan$timepoint,". ",p$rationale),c(proof,list(parent=p$parent_version)))
    }
    if(isTRUE(p$diagnostics) || isTRUE(p$exploratory)) {
      manifests<-c(if(isTRUE(p$diagnostics))"meta_analysis_diagnostics_summary.csv",
        if(isTRUE(p$exploratory))"exploratory_diagnostics_summary.csv")
      for(name in manifests) {
        path<-names(r$output_hashes)[basename(names(r$output_hashes))==name]
        csv<-if(length(path)==1L && is.function(read_output))tryCatch(read_output(id,path),error=function(e)NULL)
        if(!is.data.frame(csv) || nrow(csv)!=1L) {gap(id,name);next}
        if(identical(name,"meta_analysis_diagnostics_summary.csv")) {
          if(!identical(csv$diagnostic_status,"PASS"))gap(id,"diagnostic_execution")
          if(identical(csv$leave_one_out_performed,"Yes"))
            add(id,"influence","Influence was examined by refitting the meta-analysis with each study omitted in turn.",
              c(proof,list(output_sha256=r$output_hashes[[path]])))
        } else {
          if(!identical(csv$effect_contract_status,"PASS"))gap(id,"exploratory_execution")
          if(isTRUE(csv$egger_status %in% c("STANDARD_ELIGIBLE","EXPLORATORY_UNDERPOWERED"))) {
            threshold<-web_stat_text(csv$publication_bias_standard_min_k)
            if(!grepl("^[0-9]+$",threshold) ||
               !identical(threshold,web_stat_text(p$exploratory_policy$publication_bias)))gap(id,"small_study_threshold") else
            add(id,"small_study_effects",paste0("Small-study effects were explored with Egger's regression using the effect-estimate standard error as the predictor.",
              " The prespecified minimum for standard interpretation was ",threshold," studies.",
              if(identical(csv$egger_status,"EXPLORATORY_UNDERPOWERED"))" The test was below this threshold and was interpreted as exploratory."),
              c(proof,list(output_sha256=r$output_hashes[[path]])))
          }
          if(isTRUE(csv$funnel_status %in% c("STANDARD_ELIGIBLE","DESCRIPTIVE_ONLY")))
            add(id,"funnel_plot","Funnel plots displayed study-level estimates on the analysis scale against their standard errors.",
              c(proof,list(output_sha256=r$output_hashes[[path]])))
          if(!isTRUE(csv$meta_regression_status %in% c("NOT_ESTIMABLE","NOT_APPLICABLE","NOT_REQUESTED")))
            gap(id,"meta_regression_reporting")
        }
      }
    }
    hashes<-unlist(r$output_hashes,use.names=FALSE)
    if(!identical(r$consistency$status,"PASS") || !web_stat_hash(r$consistency$figure_sha256) ||
       !web_stat_hash(r$consistency$figure_annotation_source_sha256) ||
       !all(c(r$consistency$figure_sha256,r$consistency$figure_annotation_source_sha256) %in% hashes))
      gap(id,"result_table_figure_consistency")
  }
  if(narrative && pairwise) {
    boundary<-web_stat_text(planned$mixed_synthesis_relationship)
    if(!nzchar(boundary))gap("mixed","allocation_criteria") else
      add("mixed","allocation_criteria",boundary,list(source="approved_protocol"))
  }
  if(!length(records))gap("synthesis","reportable_evidence")
  list(status=if(length(conflicts))"FAIL" else if(length(gaps))"NOT_ASSESSED" else "PASS",profiles=as.list(profiles),
    gaps=as.list(unique(c(conflicts,gaps))),records=records)
}

web_stat_methods_prose <- function(report) {
  prose<-paste(unique(vapply(report$records,`[[`,character(1),"text")),collapse="\n\n")
  if(!identical(report$status,"PASS"))paste(prose,
    "[AUTHOR REQUIRED: complete the statistical methods evidence for the selected analyses.]",sep="\n\n") else prose
}

web_stat_methods_facts <- function(report,version) {
  records<-report$records
  texts<-vapply(records,`[[`,character(1),"text")
  unique_text<-unique(texts)
  setNames(lapply(unique_text,function(text) {
    matching<-records[texts==text]
    list(value=text,rendered=text,field=matching[[1L]]$field,version=version,provenance=lapply(matching,`[[`,"proof"))
  }),vapply(unique_text,function(text)paste0("M",web_c2_hash(text)),character(1)))
}

web_stat_methods_editor_text <- function(x) {
  if(!is.null(x$sections$Methods$text))return(x$sections$Methods$text)
  if(!identical(x$source$statistical_methods$status,"PASS"))return("")
  ids<-names(web_stat_methods_facts(x$source$statistical_methods,x$evidence_version))
  paste(paste0("{{FACT:",ids,"}}"),collapse="\n\n")
}

web_stat_records_digest <- function(records) digest::digest(as.character(jsonlite::toJSON(
  lapply(records,function(r)r[c("field","text")]),auto_unbox=TRUE,null="null",digits=NA)),algo="sha256",serialize=FALSE)

web_stat_methods_witnesses <- function(rows,records,methods) {
  if(!is.list(rows) || !is.list(records) || !length(records))return(NULL)
  ids<-vapply(rows,function(r)web_stat_text(r$id),character(1))
  if(anyDuplicated(ids) || !setequal(ids,names(records)))return(NULL)
  confirm<-function(id) {
    row<-rows[[match(id,ids)]]
    reference<-web_stat_text(row$evidence_quote)
    quote<-web_stat_text(row$manuscript_quote)
    identical(row$result,"PASS") && nzchar(reference) && nchar(quote)>=12L &&
      identical(reference,trimws(records[[id]]$text)) && grepl(quote,methods,fixed=TRUE)
  }
  if(!all(vapply(names(records),confirm,logical(1))))return(NULL)
  list(manuscript_sha256=digest::digest(methods,algo="sha256",serialize=FALSE),
    records_sha256=web_stat_records_digest(records),
    record_ids=as.list(names(records)))
}
