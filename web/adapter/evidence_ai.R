# Real provider boundary; all returned content remains an AI RECOMMENDATION.
# Called only in a worker with operator credential and validated scientific runtime.
web_b2_ai <- function(op,engine,root,paths,x,run) {
 web_assert(nzchar(Sys.getenv("OPENAI_API_KEY","")),"AI assistance needs an operator credential. Continue with manual Human review.","USER_ACTION_REQUIRED")
 for(p in list.files(file.path(engine,"prompts"),full.names=TRUE)){dir.create("prompts",showWarnings=FALSE);file.copy(p,file.path("prompts",basename(p)),overwrite=TRUE)}
 before_tables<-x$tables
 if(op=="recommend_extraction") {
  p<-"data/processed/full_text/included_studies.csv";d<-readr::read_csv(p,show_col_types=FALSE,col_types=readr::cols(.default=readr::col_character()))
  for(i in seq_len(nrow(d))) {
   r<-Filter(function(a)identical(a$screening_id,d$screening_id[i]),x$tables$Author_Final_Review)[[1]]
   if(!is.null(r$.pdf_sha256)){f<-web_inside(file.path(root,r$.pdf_relative),root);web_assert(identical(web_sha(f),r$.pdf_sha256),"Source PDF checksum failed.");d$pdf_path[i]<-f}
  };readr::write_csv(d,p)
 }
 if(op=="recommend_screening") {
  run("19_run_agent_title_abstract_screening.R")
  cfg<-yaml::read_yaml("config/agent_screening.yml")$agent_screening
  file<-file.path(paths$screening_results_directory,paste0(cfg$output$workbook_basename,".xlsx"))
  x$tables$AI_Screening<-web_b2_rows(openxlsx::read.xlsx(file,sheet="Reviewer_1",startRow=4))
 } else if(op=="recommend_eligibility") {
  run("22_prepare_full_text_registry.R");run("23_run_multimodal_full_text_eligibility_2stage.R")
  x$tables$AI_Eligibility<-web_b2_rows(openxlsx::read.xlsx("results/full_text/full_text_eligibility_agent.xlsx",sheet="Agent_Review",startRow=3))
 } else if(op=="recommend_extraction") {
  run("25_run_multimodal_data_extraction.R")
  for(n in c("study_characteristics","group_characteristics","outcome_data"))x$tables[[paste0("AI_",n)]]<-web_b2_rows(readr::read_csv(file.path("data/processed/extraction",paste0(n,"_agent.csv")),show_col_types=FALSE,col_types=readr::cols(.default=readr::col_character())))
 } else if(op=="recommend_risk_of_bias") {
  Sys.setenv(SRMA_ROB_AGENT_RUN="true");run("36_generate_risk_of_bias.R")
  x$tables$AI_Risk_of_Bias<-web_b2_rows(readr::read_csv("results/risk_of_bias/rob_agent_assessments.csv",show_col_types=FALSE,col_types=readr::cols(.default=readr::col_character())))
 }
 cfg<-yaml::read_yaml("config/agent_screening.yml")$agent_screening$provider
 for(t in names(x$tables)[startsWith(names(x$tables),"AI_") & !vapply(names(x$tables),function(n)identical(x$tables[[n]],before_tables[[n]]),logical(1))])for(i in seq_along(x$tables[[t]])) {
  x$tables[[t]][[i]]$.source<-"AI RECOMMENDATION";x$tables[[t]][[i]]$.verification<-"UNVERIFIED"
  x$tables[[t]][[i]]$.model<-if(op=="recommend_risk_of_bias")Sys.getenv("SRMA_ROB_AGENT_MODEL","gpt-5.6") else cfg$model
  x$tables[[t]][[i]]$.provider<-cfg$name;x$tables[[t]][[i]]$.timestamp<-web_now()
 }
 # Human tables are intentionally retained, including prior override rationale.
 x
}
