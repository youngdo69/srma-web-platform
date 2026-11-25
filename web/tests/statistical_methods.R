source("web/load.R",encoding="UTF-8")
source("web/qc/runtime.R",encoding="UTF-8")

sha<-strrep("a",64L)
figure<-strrep("b",64L)
protocol<-list(statistical_methods=list(
  missing_data="AVAILABLE_VERIFIED_INPUTS",
  significance=list(alpha=.05,sidedness="TWO_SIDED"),
  narrative=list(rationale="Definitions differed across the prespecified strata.",
    comparison_method="We compared the direction and precision of study-specific estimates within each stratum.",
    missing_data="PRESERVE_UNREPORTED"),
  analyses=list(g=list(effect_direction="INTERVENTION_VS_COMPARATOR",reference_group="usual care")),
  mixed_synthesis_relationship="We pooled eligible controlled estimates and reported the remaining outcome strata separately."))
ev<-list(.synthesis_execution_fresh=TRUE,
  synthesis_plan=list(status="HUMAN_CONFIRMED",decisions=list(list(analysis_group="n",mode="NARRATIVE"))),
  synthesis_execution=list(source_digest=sha,sets=list(list(analysis_group="n",route="NARRATIVE",
    common_variable="Recovery",timepoint="discharge"))))
p<-list(group="g",outcome="Complications",definition="Prespecified serious event",timepoint="30 days",
  design="Randomized trials",population="eligible adults",estimate_basis="RAW",effect_measure="RR",
  methods=web_c1_methods(),role="PRIMARY",diagnostics=FALSE,exploratory=FALSE)
variances<-c(.1,.2)
weights<-100/(variances+.03)/sum(1/(variances+.03))
a<-list(stage="APPROVED",evidence_version="e",plan=p,result_review=list(gate_id="approval"),
  result=list(status="PASS",evidence_version="e",plan_sha256=web_c1_hash(p),job_id="job",
    summary=list(effect_measure="RR",model_name=p$methods$model,tau2_method=p$methods$estimator,
      inference_test=p$methods$inference,tau2=.03,i2=22,q=1.32,q_df=1,q_p_value=.25),
    R="R version 4.6.1",packages=list(metafor="4.8-0"),
    canonical_scripts=list("27_run_universal_meta_analysis.R"=sha),
    study_effects=lapply(seq_along(variances),function(i)list(meta_input_type="raw_dichotomous",
      vi=variances[[i]],random_effect_weight_percent=weights[[i]])),
    consistency=list(status="PASS",figure_sha256=figure,figure_annotation_source_sha256=sha),
    output_hashes=list("summary.csv"=sha,"forest.pdf"=figure)))
x<-list(evidence_version="e",source=list(analyses=list(a=a)))
pairwise<-function(x=x0,protocol=protocol0,ids="a",read_output=NULL)
  web_stat_report(x,ev,"PAIRWISE_META",protocol,ids,read_output)
x0<-x;protocol0<-protocol
pair<-pairwise()
text<-web_stat_methods_prose(pair)
stopifnot(identical(pair$status,"PASS"),grepl("REML",text,fixed=TRUE),grepl("Knapp-Hartung",text,fixed=TRUE),
  grepl("usual care",text,fixed=TRUE),grepl("inverse",text,fixed=TRUE),grepl("0.5",text,fixed=TRUE),
  !grepl("Egger|Subgroup|Sensitivity",text),identical(pairwise(ids=character())$status,"NOT_ASSESSED"))
bad<-x;bad$source$analyses$a$result$summary$model_name<-"Fixed effects"
stopifnot(identical(pairwise(bad)$status,"FAIL"))
bad<-x;bad$source$analyses$a$result$study_effects[[1L]]$random_effect_weight_percent<-10
stopifnot(identical(pairwise(bad)$status,"NOT_ASSESSED"))
bad<-x;bad$source$analyses$a$result$consistency$figure_sha256<-strrep("c",64L)
stopifnot(identical(pairwise(bad)$status,"NOT_ASSESSED"))
bad<-x
bad$source$analyses$a$result$study_effects<-lapply(a$result$study_effects,function(r) {
  r$meta_input_type<-"reported_effect";r
})
stopifnot("a/effect_direction_execution" %in% unlist(pairwise(bad)$gaps))

extra<-x
extra$source$analyses$a$plan$exploratory<-TRUE
extra$source$analyses$a$plan$exploratory_policy<-list(publication_bias=10L)
extra$source$analyses$a$result$plan_sha256<-web_c1_hash(extra$source$analyses$a$plan)
extra$source$analyses$a$result$output_hashes[["exploratory_diagnostics_summary.csv"]]<-sha
read_diagnostics<-function(id,path)data.frame(effect_contract_status="PASS",egger_status="EXPLORATORY_UNDERPOWERED",
  publication_bias_standard_min_k="10",meta_regression_status="NOT_ESTIMABLE")
stopifnot(identical(pairwise(extra)$status,"NOT_ASSESSED"),
  grepl("Egger",web_stat_methods_prose(pairwise(extra,read_output=read_diagnostics)),fixed=TRUE),
  !grepl("Egger",web_stat_methods_prose(pair),fixed=TRUE))

narrative<-web_stat_report(list(evidence_version="e",source=list()),ev,"NARRATIVE_SWIM",protocol)
mixed<-web_stat_report(x,ev,c("PAIRWISE_META","NARRATIVE_SWIM"),protocol,"a")
stopifnot(identical(narrative$status,"PASS"),identical(mixed$status,"PASS"),
  grepl("without pooling",web_stat_methods_prose(narrative),fixed=TRUE),
  !grepl("REML|Egger|tau|I²",web_stat_methods_prose(narrative)),
  all(c("n/narrative","a/model_ci","mixed/allocation_criteria") %in% names(mixed$records)),
  identical(web_stat_report(x,ev,"NMA",protocol)$status,"NOT_ASSESSED"))

records<-pair$records
verified_text<-web_stat_methods_prose(pair)
witness<-lapply(names(records),function(id)list(id=id,result="PASS",evidence_quote=records[[id]]$text,
  manuscript_quote=records[[id]]$text))
stopifnot(is.list(web_stat_methods_witnesses(witness,records,verified_text)),
  is.null(web_stat_methods_witnesses(witness[-1L],records,verified_text)),
  is.null(web_stat_methods_witnesses(witness,records,sub("REML","ML",verified_text,fixed=TRUE))))

x$source$statistical_methods<-pair
x$source$facts<-web_stat_methods_facts(pair,x$evidence_version)
x$source_sha256<-web_c2_hash(x$source)
x$stage<-"PLAN";x$review_mode<-"FINAL_MANUSCRIPT_ONLY"
scope<-new.env(parent=globalenv())
scope$web_c2_context<-function(...)list(packet=x)
scope$web_c2_mutate<-function(con,settings,actor,project_id,version_id,c,x)x
edit<-web_c2_edit;environment(edit)<-scope
draft<-paste("Wording revision",web_stat_methods_editor_text(x),sep="\n\n")
edited<-edit(NULL,list(engine_root=getwd()),"actor","p","v","Methods",draft,"SAVE","Prose refinement")
stopifnot(identical(edited$source,x$source),identical(edited$stage,"PLAN"),
  identical(edited$sections$Methods$status,"REVIEW_REQUIRED"),
  grepl("Wording revision",web_c2_9b_methods(edited,narrative=FALSE),fixed=TRUE))

settings<-list(engine_root=normalizePath(getwd(),winslash="/"))
inputs<-list(protocol=web_r40_item("PASS","Approved fixture",list(sha256=sha)),
  analyses=web_r40_item("PASS","Executed fixture"),qc_policy=web_r40_item("PASS","Policy",
    list(sha256=sha,r40_script_sha256=sha)),statistical_methods=web_r40_item("PASS","Bound methods",pair))
contract<-list(ready=TRUE,sha256=sha,binding=list(inputs=inputs),execution=list(ready=TRUE,scope=list(),sha256=sha))
adaptive<-list(profiles=list("CORE","PAIRWISE_META"))
plan<-web_r40_validation_plan(settings,contract,adaptive,list(),list(),protocol,x$source$analyses)
publication<-list(blockers=list(),adaptive=list(r40_required=FALSE),finding_sha256=sha)
blocked<-web_r40_apply_input_contract(publication,contract,plan)
stopifnot(identical(plan$rules$INPUT_STATISTICAL_METHODS$result,"PASS"),
  "STATISTICAL_METHODS_REPORTING" %in% names(web_r40_semantic_rules(plan$rules)),
  identical(blocked$r40_rule_matrix$STATISTICAL_METHODS_REPORTING$result,"NOT_ASSESSED"),
  "R40_STATISTICAL_METHODS_PENDING" %in% unlist(blocked$blockers),
  inherits(tryCatch(web_c3_require_submission(blocked),error=identity),"srma_web_error"))

# Exercise the real Worker bridge's semantic response parser with a local
# journal response. No provider client, live Worker, or canonical engine runs.
bridge<-tempfile("statistical-methods-bridge-");dir.create(bridge)
bridge_env<-new.env(parent=globalenv())
bridge_env$bridge<-bridge
bridge_env$cfg<-list(policy_sha256=sha,semantic_rules=list(STATISTICAL_METHODS_REPORTING="Check methods"),
  statistical_records=lapply(records,function(r)r[c("field","text")]))
bridge_env$semantic_checks<-list()
bridge_env$context<-list(read_text_file=function(...)"Approved protocol",extract_section=function(...)verified_text,
  strip_markdown_fence=identity)
bridge_env$offline<-function(state)stop(state)
exprs<-parse("web/adapter/r40_bound_preflight.R",encoding="UTF-8")
for(expr in exprs)if(is.call(expr) && length(expr)==3L && identical(expr[[2L]],as.name("semantic_request")))eval(expr,bridge_env)
execute<-function()bridge_env$semantic_request("system","user","02_cross_section_consistency")
stopifnot(identical(conditionMessage(tryCatch(execute(),error=identity)),"SEMANTIC_REQUEST_PENDING"))
req<-jsonlite::read_json(file.path(bridge,"request.json"))
record<-function(witnesses)jsonlite::write_json(list(sha256=req$sha256,text=as.character(jsonlite::toJSON(list(
  checks=list(list(id="STATISTICAL_METHODS_REPORTING",result="PASS",witnesses=witnesses,unsupported_claims=list()))),
  auto_unbox=TRUE))),file.path(bridge,"02_cross_section_consistency.json"),auto_unbox=TRUE)
record(witness[-1L]);invisible(execute())
stopifnot(is.null(bridge_env$semantic_checks$STATISTICAL_METHODS_REPORTING))
record(witness);invisible(execute())
native<-list(state="COMPLETE",canonical=list(manuscript_sha256=sha,project_id="p",final_evidence_ready=TRUE),
  semantic_checks=bridge_env$semantic_checks)
stopifnot(identical(web_r40_execution_rules(plan,native,sha,"p",list())$STATISTICAL_METHODS_REPORTING$result,"PASS"))
native$semantic_checks$STATISTICAL_METHODS_REPORTING$records_sha256<-figure
stopifnot(identical(web_r40_execution_rules(plan,native,sha,"p",list())$STATISTICAL_METHODS_REPORTING$result,"NOT_ASSESSED"))
cat("Statistical methods narrative, pairwise, mixed, exact witnesses, editing and fail-closed R/40 PASS\n")
