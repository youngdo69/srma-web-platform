# Explicit synthetic content; real canonical transforms, worker, storage and Human actions.
.libPaths(c(Sys.getenv("C1_TEST_LIBRARY"),file.path(dirname(Sys.getenv("C1_TEST_LIBRARY")),"library"),.libPaths()));source("web/load.R");source("web/tests/fixture.R")
reject<-function(expr){e<-tryCatch({force(expr);NULL},error=identity);stopifnot(inherits(e,"error"));invisible(e)}
s<-web_settings(data_root=Sys.getenv("C1_TEST_ROOT",tempfile("c1-e2e-")));s$timeout<-120L;con<-web_db(s);web_migrate(con,s);web_local_identity(con)
for(u in c("second","adjudicator")){web_exec(con,"INSERT INTO app_users VALUES(?u,?u)",u=u);web_exec(con,"INSERT INTO workspace_members VALUES('local-workspace',?u)",u=u)}
actor<-"local-researcher";f<-web_fixture(tempfile("b2-source-"),"b2-synthetic")
cfg<-yaml::read_yaml(file.path(f,"config/project_config.yml"));cfg$search$databases<-list(central=TRUE);cfg$research_question$outcomes<-list(primary=list("Synthetic endpoint"),secondary=list());yaml::write_yaml(cfg,file.path(f,"config/project_config.yml"))
file.copy(file.path(f,"search_strategies/pubmed_search.txt"),file.path(f,"search_strategies/central_search.txt"))
v<-web_import_project(con,s,actor,"local-workspace",f)
upload_root<-tempfile();dir.create(upload_root);ris<-file.path(upload_root,"export.ris")
writeLines(unlist(lapply(c("Alpha","Alpha","Beta","Gamma","Delta"),function(t)c("TY  - JOUR",paste0("TI  - Synthetic ",t),"AU  - Example, A","PY  - 2024","AB  - Explicitly synthetic eligibility fixture","ER  - ",""))),ris)
v<-web_upload_database(con,s,actor,v$project_id,v$version_id,"central",data.frame(name="export.ris",datapath=ris,size=file.info(ris)$size),upload_root)
# Existing labeled B1 test seam only: development R/SQLite preflight, no canonical-runtime PASS.
original_science<-web_science_settings
canonical_acceptance<-identical(Sys.getenv("C1_CANONICAL_ACCEPTANCE"),"1")
if(!canonical_acceptance)web_science_settings<-function(settings,verify=TRUE)settings
j<-web_submit(con,s,actor,v$project_id,v$version_id,"validate_database_inputs");web_worker_once(con,s)
stopifnot(web_query(con,"SELECT state FROM jobs WHERE id=?j",j=j)$state=="COMPLETE")
v<-web_b2_start(con,s,actor,v$project_id,v$version_id)
checkpoints<-list(INPUT=v)
context<-function()web_b2_context(con,s,actor,v$project_id,v$version_id)$packet
step<-function(op){
 g<-web_query(con,"SELECT id FROM gates WHERE project_id=?p AND version_id=?v AND required_operation=?o AND status='PENDING'",p=v$project_id,v=v$version_id,o=op)
 if(nrow(g))web_decide_gate(con,actor,v$project_id,g$id,"APPROVED","Explicit synthetic human test confirmation")
 j<-web_submit(con,s,actor,v$project_id,v$version_id,op);web_worker_once(con,s)
 x<-web_query(con,"SELECT * FROM jobs WHERE id=?j",j=j)
 if(x$state!="COMPLETE"){print(web_parse(x$result_json));stop(paste("STEP FAILED",op))}
 v<<-web_b2_accept(con,s,actor,v$project_id,v$version_id,j);checkpoints[[context()$stage]]<<-v;cat("PASS:",op,context()$stage,"\n")
}
edit<-function(table,i,values,who=actor){x<-context();v<<-web_b2_edit(con,s,who,v$project_id,v$version_id,table,x$tables[[table]][[i]]$.row_id,values)}
step("prepare_screening");x<-context();stopifnot(length(x$tables$Reviewer_1)==4L)
for(i in 1:4){edit("Reviewer_1",i,list(decision="Include",notes="Human screening",exclusion_reason=""));reject(edit("Reviewer_2",i,list(decision="Include")));edit("Reviewer_2",i,list(decision="Include",notes="Independent human screening",exclusion_reason=""),"second")}
step("finalize_screening")
# Real small PDF signature fixture; no real paper or provider-generated content.
pdf<-file.path(upload_root,"synthetic.pdf");writeLines(c("%PDF-1.4","1 0 obj << /Type /Catalog >> endobj","trailer << /Root 1 0 R >>","%%EOF"),pdf)
for(i in 1:4){x<-context();v<-web_b2_upload(con,s,actor,v$project_id,v$version_id,x$tables$Author_Final_Review[[i]]$.row_id,data.frame(name="synthetic.pdf",datapath=pdf,size=file.info(pdf)$size),upload_root)
 edit("Author_Final_Review",i,list(author_final_decision=if(i==4)"Exclude" else "Include",author_final_exclusion_reason=if(i==4)"Ineligible" else "",author_notes="Explicit synthetic full-text review",study_id=paste0("STUDY_",i)))}
step("register_fulltext");step("finalize_included_studies");stopifnot(length(context()$included_reports)==3)
step("prepare_extraction")
for(i in 1:3){
 edit("Target_Study_Verification",i,list(study_design="randomized trial",source_location="Page 1",source_evidence="Synthetic fixture, not clinical evidence"))
 x<-context();v<-web_b2_verify(con,s,"second",v$project_id,v$version_id,"Target_Study_Verification",x$tables$Target_Study_Verification[[i]]$.row_id,TRUE,"Independent synthetic verification")
 edit("Target_Outcome_Verification",i,list(data_type="dichotomous",timepoint="30 days",effect_measure="RR",intervention_events=as.character(i+1),intervention_total="10",comparator_events="4",comparator_total="10",analysis_group="primary_rct",meta_analysis_role="primary",human_include_in_planned_analysis="Yes",value_status="REPORTED",source_location="Page 1 table 1",source_evidence="Synthetic counts, no clinical observations"))
 x<-context();v<-web_b2_verify(con,s,"second",v$project_id,v$version_id,"Target_Outcome_Verification",x$tables$Target_Outcome_Verification[[i]]$.row_id,TRUE,"Independent synthetic verification")
}
step("finalize_extraction");step("prepare_risk_of_bias")
x<-context();stopifnot(any(grepl("RoB 2",vapply(x$tables$Domain_Review,`[[`,character(1),"rob_tool"))))
for(i in seq_along(x$tables$Domain_Review))edit("Domain_Review",i,list(author_domain_judgement="Low",author_domain_rationale="Explicit synthetic human judgment"))
for(i in seq_along(x$tables$Overall_Review))edit("Overall_Review",i,list(author_final_overall_judgement="Low",author_final_overall_rationale="Synthetic human assessment"))
step("finalize_evidence");stopifnot(context()$stage=="READY")
cat("PASS: C1 synthetic fixture traversed real B2 Human-approved READY evidence\n")
saveRDS(list(settings=s,version=v),file.path(s$data_root,"c1-fixture.rds"))
tryCatch(source("web/tests/c1_reconciliation.R"),finally={web_science_settings<-original_science;if(inherits(con,"SQLiteConnection"))DBI::dbGetQuery(con,"PRAGMA wal_checkpoint(TRUNCATE)");DBI::dbDisconnect(con)})
