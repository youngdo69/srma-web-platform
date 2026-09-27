# Private adapter: canonical scripts are the sole numerical/figure authority.
web_c1_numeric_contract <- function(root,summary,studies) {
 tables<-file.path(root,"results/meta_analysis/tables")
 tab<-readr::read_csv(file.path(tables,"meta_analysis_summary.csv"),show_col_types=FALSE)
 wb<-openxlsx::read.xlsx(file.path(tables,"meta_analysis_outputs.xlsx"),sheet="Meta_Analysis_Summary",check.names=FALSE)
 map<-c(pooled_effect="pooled_effect",lower_ci="pooled_lower_ci",upper_ci="pooled_upper_ci",i2="heterogeneity_i2",tau2="heterogeneity_tau2",p_value="p_value",q="heterogeneity_q",q_p_value="heterogeneity_p",studies="n_studies")
 web_assert(nrow(tab)==1&&nrow(wb)==1,"The publication table contains unexpected analyses.","ARTIFACT_GENERATION_FAILED")
 for(k in names(map))for(t in list(tab,wb))web_assert(isTRUE(all.equal(as.numeric(summary[[k]]),as.numeric(t[[map[[k]]]]),tolerance=1e-12)),"Canonical result and publication table disagree.","ARTIFACT_GENERATION_FAILED")
 study_table<-readr::read_csv(file.path(tables,"study_level_effects.csv"),show_col_types=FALSE)
 study_book<-openxlsx::read.xlsx(file.path(tables,"meta_analysis_outputs.xlsx"),sheet="Study_Level_Effects",check.names=FALSE)
 for(t in list(study_table,study_book)) {
  web_assert(identical(as.character(studies$outcome_row_id),as.character(t$outcome_row_id)),"Study membership differs across outputs.","ARTIFACT_GENERATION_FAILED")
  for(k in c("study_effect","study_lower_ci","study_upper_ci","random_effect_weight_percent"))web_assert(isTRUE(all.equal(as.numeric(studies[[k]]),as.numeric(t[[k]]),tolerance=1e-12)),"Study-level values differ across outputs.","ARTIFACT_GENERATION_FAILED")
 }
 fig<-web_c1_csv(file.path(tables,"figure_manifest.csv"))
 web_assert(nrow(fig)==1&&fig$figure_source=="R/27_CANONICAL_FOREST","Unexpected figure annotation source.","ARTIFACT_GENERATION_FAILED")
 path<-function(p)web_inside(if(startsWith(p,root))p else file.path(root,p),root)
 web_assert(identical(web_sha(path(fig$source_r27_forest_pdf)),web_sha(path(fig$pdf_file))),"Forest figure differs from the canonical model output.","ARTIFACT_GENERATION_FAILED")
 web_assert(identical(web_sha(path(fig$source_r27_summary)),web_sha(path(summary$summary_path))),"Figure references a different result summary.","ARTIFACT_GENERATION_FAILED")
 list(status="PASS",tolerance=1e-12,registry_table_fields=as.list(names(map)),study_fields=as.list(c("study_effect","study_lower_ci","study_upper_ci","random_effect_weight_percent")),figure_sha256=web_sha(path(fig$pdf_file)),figure_annotation_source_sha256=web_sha(path(fig$source_r27_summary)),figure_path=substring(path(fig$pdf_file),nchar(root)+2L))
}
web_c1_canonical <- function(request,config,paths,root,input_manifest) {
 x<-web_c1_read(root);plan<-x$plan;engine<-request$engine_root
 web_assert(x$stage=="PLAN"&&identical(plan$methods,web_c1_methods()),"Invalid immutable analysis plan.")
 needed<-c("metafor","openxlsx","readr","dplyr","tibble","stringr","purrr","tidyr")
 web_assert(all(vapply(needed,requireNamespace,logical(1),quietly=TRUE)),"Canonical analysis dependencies are unavailable.","DEPENDENCY_FAILED")
 source<-web_inside(file.path(root,plan$dataset_path),root)
 web_assert(web_sha(source)==plan$dataset_sha256,"Analysis input checksum mismatch.")
 d<-web_c1_csv(source);ids<-unlist(plan$row_ids);web_assert(all(ids %in% d$outcome_row_id)&&!anyDuplicated(ids),"Analysis row binding failed.")
 d<-d[match(ids,d$outcome_row_id),,drop=FALSE]
 web_assert(identical(as.character(d$screening_id),unlist(plan$report_ids,use.names=FALSE)),"Analysis report binding failed.")
 # Each attempt owns its outputs. Never discover another version's models/figures.
 for(p in c("results/meta_analysis","results/meta_analysis_diagnostics","results/meta_analysis_exploratory_diagnostics","results/additional_analysis","results/web_analysis"))unlink(file.path(root,p),recursive=TRUE)
 dir.create("R/utils",recursive=TRUE,showWarnings=FALSE)
 for(p in list.files(file.path(engine,"R/utils"),full.names=TRUE))file.copy(p,file.path("R/utils",basename(p)),overwrite=TRUE)
 input_path<-"data/processed/meta_analysis_ready/web_selected_dataset.csv"
 readr::write_csv(d,input_path,na="")
 mf<-"data/processed/meta_analysis_ready/meta_analysis_manifest.csv";m<-web_c1_csv(mf);m<-m[m$analysis_group==plan$group,,drop=FALSE];web_assert(nrow(m)==1,"Unknown canonical group.")
 m$dataset_file<-normalizePath(input_path);m$study_count<-nrow(d);m$outcome_row_count<-nrow(d);m$meta_analysis_role<-tolower(plan$role);m$analysis_group<-paste0(tolower(plan$role),"__",plan$group)
 readr::write_csv(m,mf,na="")
 # Only explicit, reviewed exploratory policy enters the attempt-local config.
 if(isTRUE(plan$exploratory)) {
  config$analysis$exploratory_diagnostics<-list(allow_partial_effects=FALSE,publication_bias=list(standard_min_k=plan$exploratory_policy$publication_bias),meta_regression=list(standard_min_k=plan$exploratory_policy$meta_regression,allow_auto_moderators=FALSE,candidate_moderators=plan$moderators))
  config$analysis$statistical_significance$confidence_interval<-.95
  config$analysis$heterogeneity<-list(tau_squared_estimator="REML",hartung_knapp_adjustment=TRUE)
  config$analysis$zero_event_handling$continuity_correction<-.5
  yaml::write_yaml(config,"config/project_config.yml")
 }
 warning_text<-character();scripts<-character()
 run<-function(name){scripts<<-c(scripts,name);env<-new.env(parent=globalenv());withCallingHandlers(sys.source(file.path(engine,"R",name),env),warning=function(w){warning_text<<-c(warning_text,conditionMessage(w));invokeRestart("muffleWarning")});env}
 run("27_run_universal_meta_analysis.R")
 execution_path<-"results/meta_analysis/meta_analysis_execution_summary.csv"
 execution<-readr::read_csv(execution_path,show_col_types=FALSE)
 web_assert(nrow(execution)==1,"Canonical execution returned unexpected groups.","ENGINE_FAILED")
 dir.create("results/web_analysis",recursive=TRUE,showWarnings=FALSE)
 file.copy(input_path,"results/web_analysis/selected_input.csv",overwrite=TRUE)
 registry<-list(contract="web-analysis-v1",analysis_id=x$analysis_id,project_version=request$version_id,evidence_version=x$evidence_version,evidence_manifest_sha256=x$evidence_manifest_sha256,job_id=request$job_id,engine=request$engine_version,plan_sha256=web_c1_hash(plan),source_extraction_sha256=plan$dataset_sha256,selected_dataset_sha256=web_sha(input_path),included_rows=plan$row_ids,included_studies=plan$study_ids,included_reports=plan$report_ids,role=plan$role,parent_version=plan$parent_version,exclusions=plan$exclusions,R=R.version.string,packages=setNames(lapply(needed,function(p)as.character(utils::packageVersion(p))),needed),lock_sha256=web_sha(file.path(engine,"renv.lock")),adapter_sha256=web_sha(file.path(engine,"web/adapter/analysis.R")),created_at=web_now(),status=execution$status[[1]],summary=as.list(execution[1,,drop=FALSE]))
 if(execution$status[[1]]=="PASS") {
  summary_paths<-list.files("results/meta_analysis",pattern="^meta_analysis_summary[.]csv$",recursive=TRUE,full.names=TRUE)
  web_assert(length(summary_paths)==1,"Ambiguous canonical result.")
  summary<-readr::read_csv(summary_paths,show_col_types=FALSE);summary$summary_path<-summary_paths
  study_path<-file.path(dirname(summary_paths),"study_effects_used.csv");studies<-readr::read_csv(study_path,show_col_types=FALSE)
  web_assert(identical(as.character(studies$outcome_row_id),ids),"Canonical execution changed study membership.")
  if(isTRUE(plan$diagnostics))run("28_run_meta_analysis_diagnostics.R")
  if(isTRUE(plan$exploratory))run("28B_generate_universal_exploratory_diagnostics.R")
  run("33_generate_forest_plots_and_editable_tables.R")
  registry$consistency<-web_c1_numeric_contract(root,summary,studies)
  registry$summary<-as.list(summary[1,,drop=FALSE]);registry$study_effects<-lapply(seq_len(nrow(studies)),function(i)as.list(studies[i,,drop=FALSE]))
  registry$forest_path<-registry$consistency$figure_path
 }
 registry$warnings<-as.list(unique(c(warning_text,if(nrow(d)<3)"Fewer than three studies: interpret estimates and heterogeneity cautiously.",if(isTRUE(plan$exploratory))"Exploratory diagnostics were explicitly requested; underpowered tests do not establish publication bias or causal moderator effects.")))
 registry$canonical_scripts<-setNames(lapply(scripts,function(p)web_sha(file.path(engine,"R",p))),scripts)
 outputs<-list.files("results",recursive=TRUE,full.names=TRUE);outputs<-outputs[grepl("^results/(meta_analysis/|meta_analysis_diagnostics/|meta_analysis_exploratory_diagnostics/|exploratory|web_analysis/)",outputs)&!dir.exists(outputs)]
 registry$output_hashes<-setNames(lapply(outputs,web_sha),outputs)
 jsonlite::write_json(registry,"results/web_analysis/result.json",auto_unbox=TRUE,null="null",pretty=TRUE,digits=NA)
 outputs<-c(outputs,"results/web_analysis/result.json")
 ok<-identical(registry$status,"PASS")
 web_result(request$action,request$project_id,request$job_id,if(ok)"COMPLETE" else "USER_ACTION_REQUIRED",if(ok)"Analysis and outputs generated. Review warnings and save the result for explicit Human approval." else "The canonical analysis was not estimable or its inputs need review. Inspect the execution report and revise the evidence/plan.",request$engine_version,code=if(ok)NULL else if(registry$status=="SKIP")"ANALYSIS_NOT_ESTIMABLE" else "INPUT_VALIDATION_FAILED",artifacts=lapply(outputs,function(p)list(path=web_inside(file.path(root,p),root),logical_name=basename(p),type="analysis")),provenance=list(analysis_id=x$analysis_id,evidence_version=x$evidence_version,plan_sha256=registry$plan_sha256,R=R.version.string,input_hashes=input_manifest,result_sha256=web_sha("results/web_analysis/result.json")))
}
