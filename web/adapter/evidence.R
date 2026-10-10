# Canonical file-contract adapter. Never computes screening, dedup, effects or RoB scores.
web_b2_exports <- function(path,env,constants=character()) {
 for(expr in parse(path))if(is.call(expr)&&identical(expr[[1]],as.name("<-"))&&is.symbol(expr[[2]])) {
  key<-as.character(expr[[2]]);rhs<-expr[[3]]
  if(key %in% constants||(is.call(rhs)&&identical(rhs[[1]],as.name("function"))))eval(expr,env)
 }
 invisible(env)
}
web_b2_rows <- function(df) {
 if(!nrow(df))return(list())
 df[]<-lapply(df,function(v){v<-as.character(v);v[is.na(v)]<-"";v})
 lapply(seq_len(nrow(df)),function(i)c(as.list(df[i,,drop=FALSE]),list(.row_id=web_id(),.source="SYSTEM OUTPUT")))
}
web_b2_df <- function(rows) {
 if(!length(rows))return(data.frame())
 keys<-unique(unlist(lapply(rows,names)));keys<-keys[!startsWith(keys,".")]
 as.data.frame(setNames(lapply(keys,function(k)vapply(rows,function(x)as.character(x[[k]] %||b2% ""),character(1))),keys),stringsAsFactors=FALSE)
}
web_b2_book <- function(path,tables,start=3L) {
 dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE);wb<-openxlsx::createWorkbook()
 for(n in names(tables)){openxlsx::addWorksheet(wb,n);openxlsx::writeData(wb,n,web_b2_df(tables[[n]]),startRow=if(n %in% c("Reviewer_1","Reviewer_2"))4L else start)}
 openxlsx::saveWorkbook(wb,path,overwrite=TRUE)
}
web_b2_canonical <- function(request,config,paths,root,input_manifest) {
 source(file.path(request$engine_root,"web/R/evidence.R"))
 engine<-request$engine_root;x<-web_b2_packet(root);op<-request$action
 web_assert(op %in% names(web_b2_operations())&&x$stage==unname(web_b2_operations()[op]),"Evidence stage does not match operation.")
 needed<-c("openxlsx","readr","dplyr","tibble","stringr","tidyr","purrr")
 web_assert(all(vapply(needed,requireNamespace,logical(1),quietly=TRUE)),"Canonical workflow packages are unavailable.","DEPENDENCY_FAILED")
 # Copy exact canonical dependencies into this private attempt only; no .env.
 for(p in c(list.files(file.path(engine,"R/utils"),full.names=FALSE),character())){
  dir.create("R/utils",recursive=TRUE,showWarnings=FALSE);file.copy(file.path(engine,"R/utils",p),file.path("R/utils",p),overwrite=TRUE)
 }
 for(p in c("agent_screening.yml","data_extraction_schema.yml","initialization_agent.yml"))file.copy(file.path(engine,"config",p),file.path("config",p),overwrite=TRUE)
 file.copy(file.path(engine,"R/TASK018_database_input_gate_hardening.R"),"R/TASK018_database_input_gate_hardening.R",overwrite=TRUE)
 run<-function(name){env<-new.env(parent=globalenv());sys.source(file.path(engine,"R",name),env);env}
 strip<-function(p)sub(paste0(root,"/"),"",p,fixed=TRUE)
 csv<-function(p)readr::read_csv(p,col_types=readr::cols(.default=readr::col_character()),show_col_types=FALSE)
 if(op %in% names(web_b2_ai_operations())) {
  # Canonical AI scripts call load_project_env(".env"). In Web execution the
  # operator credential is already inherited through the process environment;
  # secrets must never be copied into an immutable snapshot or job workspace.
  # Create an empty private compatibility file so the canonical loader keeps
  # using the inherited environment variable without persisting the secret.
  env_bridge<-file.path(root,".env")
  if(!file.exists(env_bridge)){
   web_assert(file.create(env_bridge),"Could not prepare the private provider environment bridge.","SYSTEM_FAILED")
   try(Sys.chmod(env_bridge,mode="0600"),silent=TRUE)
  }
  on.exit(unlink(env_bridge),add=TRUE)
  source(file.path(engine,"web/adapter/evidence_ai.R"))
  x<-web_b2_ai(op,engine,root,paths,x,run)
 } else if(op=="prepare_screening") {
  run("17_import_merge_deduplicate.R");env<-run("18_prepare_screening_workbook.R")
  x$tables<-list(Reviewer_1=web_b2_rows(env$reviewer_1_data),Reviewer_2=web_b2_rows(env$reviewer_2_data),Conflict_Resolution=web_b2_rows(env$conflict_data),Source_Summary=web_b2_rows(env$source_summary))
  x$choices<-list(decisions=as.list(env$screening_decisions %||b2% config$screening$decisions),reasons=as.list(env$exclusion_reason_table$label %||b2% vapply(config$screening$exclusion_reasons,`[[`,character(1),"label")))
  x$stage<-"SCREENING"
 } else if(op=="finalize_screening") {
  a<-x$tables$Reviewer_1;b<-x$tables$Reviewer_2
  web_assert(length(a)>0&&length(a)==length(b)&&all(vapply(seq_along(a),function(i)!is.null(a[[i]]$.actor)&&!is.null(b[[i]]$.actor)&&a[[i]]$.actor!=b[[i]]$.actor,logical(1))),"Two independent human reviews must be recorded before finalization.")
  ac<-yaml::read_yaml("config/agent_screening.yml")$agent_screening
  book<-file.path(paths$screening_results_directory,paste0(ac$output$workbook_basename,".xlsx"))
  tables<-x$tables[c("Reviewer_1","Reviewer_2","Conflict_Resolution")]
  tables$Source_Summary<-x$tables$Source_Summary
  wb<-openxlsx::loadWorkbook(paths$screening_workbook)
  for(t in names(tables))openxlsx::writeData(wb,t,web_b2_df(tables[[t]]),startRow=if(t %in% c("Reviewer_1","Reviewer_2"))4L else 3L)
  openxlsx::saveWorkbook(wb,book,overwrite=TRUE)
  screened<-run("21_finalize_title_abstract_screening.R")
  web_assert(all(screened$final_screening$final_decision %in% c("Include","Exclude")),"Resolve uncertain title/abstract decisions before finalizing the screening set.","USER_ACTION_REQUIRED")
  env<-run("22_prepare_full_text_registry.R")
  handoff<-csv(file.path(paths$processed_screening_directory,"included_for_full_text_review.csv"))
  defs<-web_b2_exports(file.path(engine,"R/24_finalize_included_studies.R"),new.env(parent=globalenv()),"required_author_columns")
  author<-as.data.frame(setNames(lapply(defs$required_author_columns,function(n)if(n %in% names(handoff))handoff[[n]] else rep("",nrow(handoff))),defs$required_author_columns))
  x$tables$Author_Final_Review<-web_b2_rows(author)
  for(i in seq_along(x$tables$Author_Final_Review)) {
   row<-x$tables$Author_Final_Review[[i]];row$study_id<-"";row$retrieval_status<-"Missing";row$pdf_status<-"Missing"
   row$.pdf_relative<-file.path("data/raw/full_text/pdfs",paste0(env$sanitize_screening_id_for_filename(row$screening_id),".pdf"))
   x$tables$Author_Final_Review[[i]]<-row
  }
  x$stage<-"FULLTEXT"
 } else if(op=="register_fulltext") {
  run("22_prepare_full_text_registry.R")
 } else if(op=="finalize_included_studies") {
  rows<-x$tables$Author_Final_Review
  web_assert(length(rows)>0&&all(vapply(rows,function(a)!is.null(a$.actor)&&nzchar(a$author_final_decision)&&nzchar(a$study_id),logical(1))),"Record explicit eligibility decisions and report-to-study links for every report.")
  for(i in seq_along(rows))if(nzchar(rows[[i]]$pdf_path))rows[[i]]$pdf_path<-web_inside(file.path(root,rows[[i]]$.pdf_relative),root)
  book<-"results/full_text/full_text_eligibility_agent.xlsx"
  web_b2_book(book,list(Instructions=list(list(note="Human-entered review; blank agent fields are not AI output")),Agent_Review=list(list(note="No AI recommendation requested")),Author_Final_Review=rows,Validation_Lists=list(list(note="Canonical configuration defines exclusion reasons"))))
  run("24_finalize_included_studies.R")
  mapping<-web_b2_df(rows)[,c("screening_id","study_id"),drop=FALSE];readr::write_csv(mapping,"data/processed/full_text/report_study_crosswalk.csv")
  x$included_reports<-as.list(csv("data/processed/full_text/included_studies.csv")$screening_id)
  x$stage<-"INCLUDED"
 } else if(op=="prepare_extraction") {
  schema<-yaml::read_yaml("config/data_extraction_schema.yml");inc<-csv("data/processed/full_text/included_studies.csv")
  fields<-unlist(schema$study_characteristics$fields);outfields<-unlist(schema$outcome_data$fields)
  study<-lapply(seq_len(nrow(inc)),function(i)c(list(screening_id=inc$screening_id[i]),setNames(as.list(rep("",length(fields))),fields),list(source_location="",source_evidence="",verification_status="Needs clarification",verifier="",verified_at="",.row_id=web_id(),.source="HUMAN ENTRY REQUIRED")))
  names_outcomes<-unique(as.character(unlist(config$research_question$outcomes,use.names=FALSE)));web_assert(length(names_outcomes)>0,"Define project outcomes first.")
  outcomes<-list()
  for(id in inc$screening_id)for(out in names_outcomes){row<-setNames(as.list(rep("",length(outfields))),outfields);row$outcome_name<-out
   row<-c(list(screening_id=id,outcome_row_id=paste0(id,"_",length(outcomes)+1L)),row,list(analysis_group="",meta_analysis_role="",parent_analysis_group="",human_include_in_planned_analysis="No",verification_status="Needs clarification",verifier="",verified_at="",value_status="MISSING",transformation_rule="",raw_value="",.row_id=web_id(),.source="HUMAN ENTRY REQUIRED"));outcomes[[length(outcomes)+1L]]<-row}
  x$tables$Target_Study_Verification<-study;x$tables$Target_Outcome_Verification<-outcomes
  x$editable<-list(Target_Study_Verification=as.list(c(fields,"source_location","source_evidence")),Target_Outcome_Verification=as.list(c(outfields,"analysis_group","meta_analysis_role","parent_analysis_group","human_include_in_planned_analysis","value_status","transformation_rule","raw_value")))
  for(t in c("Target_Study_Verification","Target_Outcome_Verification"))for(i in seq_along(x$tables[[t]])) {
   r<-x$tables[[t]][[i]];src<-Filter(function(a)identical(a$screening_id,r$screening_id),x$tables$Author_Final_Review)[[1]]
   r$.pdf_sha256<-src$.pdf_sha256;r$study_id<-src$study_id;x$tables[[t]][[i]]<-r
  }
  x$stage<-"EXTRACTION"
 } else if(op=="finalize_extraction") {
  studies<-x$tables$Target_Study_Verification;outcomes<-x$tables$Target_Outcome_Verification
  ids<-unlist(x$included_reports)
  web_assert(all(vapply(c(studies,outcomes),function(a)a$screening_id %in% ids,logical(1))),"Extraction includes an unapproved report.")
  for(row in c(studies,outcomes)) {
   needed<-!is.null(row$study_design)||identical(row$human_include_in_planned_analysis,"Yes")
   if(needed)web_assert(!is.null(row$.verified_by)&&row$verification_status %in% c("Verified","Corrected"),"Independent Human verification is incomplete.")
   if(!is.null(row$value_status)){
    web_assert(row$value_status %in% c("REPORTED","DERIVED","CALCULATED","IMPUTED","MISSING","NOT APPLICABLE"),"Select an explicit extracted-value status.")
    if(row$value_status %in% c("DERIVED","CALCULATED","IMPUTED"))web_assert(nzchar(row$raw_value)&&nzchar(row$transformation_rule),"Preserve the raw value and transformation rule.")
   }
  }
  # Human-selected analysis roles feed the unchanged canonical validator. No model is fitted.
  book<-"results/extraction/targeted_human_verification/targeted_human_verification.xlsx"
  web_b2_book(book,x$tables[c("Target_Study_Verification","Target_Outcome_Verification")]);run("26_finalize_human_verified_extraction.R")
  x$stage<-"VERIFIED"
 } else if(op=="prepare_risk_of_bias") {
  Sys.setenv(SRMA_ROB_AGENT_RUN="false")
  run("36_generate_risk_of_bias.R")
  book<-"results/risk_of_bias/risk_of_bias_author_editable.xlsx"
  for(t in c("Study_Result_Manifest","Signalling_Questions","Domain_Review","Overall_Review"))x$tables[[t]]<-web_b2_rows(openxlsx::read.xlsx(book,sheet=t,check.names=FALSE))
  for(i in seq_along(x$tables$Signalling_Questions))for(n in c("human_response","human_evidence_location","human_rationale"))x$tables$Signalling_Questions[[i]][[n]]<-""
  x$stage<-"ROB"
 } else if(op=="finalize_evidence") {
  for(t in c("Domain_Review","Overall_Review"))web_assert(all(vapply(x$tables[[t]],function(a)!is.null(a$.actor),logical(1))),"Every RoB judgment requires explicit Human review.")
  for(row in x$tables$Domain_Review)web_assert(nzchar(row$author_domain_rationale),"Record a rationale for every RoB domain.")
  for(row in x$tables$Overall_Review)web_assert(nzchar(row$author_final_overall_rationale),"Record the overall RoB rationale.")
  book<-"results/risk_of_bias/risk_of_bias_author_editable.xlsx";web_b2_book(book,x$tables[c("Study_Result_Manifest","Signalling_Questions","Domain_Review","Overall_Review")],1L)
  e<-web_b2_exports(file.path(engine,"R/37_finalize_risk_of_bias_and_generate_figures.R"),new.env(parent=globalenv()),c("rob2_domains","robins_domains"))
  final<-e$import_and_finalize_author_workbook(book);e$validate_tool_domains(final,"RoB 2",e$rob2_domains);e$validate_tool_domains(final,"ROBINS-I",e$robins_domains)
  readr::write_csv(final,"results/risk_of_bias/rob_final_combined.csv")
  x$stage<-"READY"
  jsonlite::write_json(list(status="human_verified_dataset",project_version=request$version_id,project=request$project_id,input_sha256=input_manifest,analysis_ready_manifest="data/processed/meta_analysis_ready/meta_analysis_manifest.csv",included="data/processed/full_text/included_studies.csv",crosswalk="data/processed/full_text/report_study_crosswalk.csv",rob="results/risk_of_bias/rob_final_combined.csv",engine=request$engine_version,source_job=request$job_id,unresolved_blocking=list(),not_selected=Filter(function(a)!identical(a$human_include_in_planned_analysis,"Yes"),x$tables$Target_Outcome_Verification)),"results/analysis_ready_evidence.json",auto_unbox=TRUE,pretty=TRUE)
 }
 # Absolute canonical PDF locations are ephemeral; retain canonical relative identity in the packet.
 for(t in names(x$tables))for(i in seq_along(x$tables[[t]]))for(n in names(x$tables[[t]][[i]]))if(is.character(x$tables[[t]][[i]][[n]]))x$tables[[t]][[i]][[n]]<-strip(x$tables[[t]][[i]][[n]])
 web_b2_write(root,x)
 outputs<-list.files(root,recursive=TRUE,full.names=TRUE)
 outputs<-outputs[!dir.exists(outputs)];rel<-vapply(outputs,strip,character(1))
 # Canonical scripts copied into jobs and provider diagnostic/prompt material are never published.
 keep<-grepl("^(data/raw/full_text/pdfs/|data/processed/|data/screening/|results/|config/generated/web_evidence[.]json$)",rel)&!grepl("(prompt|jsonl|raw_response|checkpoint)",rel,ignore.case=TRUE)
 outputs<-outputs[keep]
 web_result(op,request$project_id,request$job_id,"COMPLETE","Canonical processing finished. Save its results as a new evidence version, then review the next Human decision.",request$engine_version,
  artifacts=lapply(outputs,function(p)list(path=web_inside(p,root),logical_name=basename(p),type="evidence")),provenance=list(stage=x$stage,R=R.version.string,source_kind="SYSTEM OUTPUT",input_sha256=input_manifest,project_version=request$version_id))
}
