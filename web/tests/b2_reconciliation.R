# Runs after the real deterministic E2E; content is synthetic, state/auth/gates are real.
ready<-v;ready_root<-web_b2_context(con,s,actor,v$project_id,v$version_id)$version$root
ready_hash<-web_sha(file.path(ready_root,"config/generated/web_evidence.json"))
Sys.setenv(SRMA_WEB_TEST_IDP="1")
for(u in c(actor,"second"))web_exec(con,"INSERT INTO external_identities VALUES('local-test',?u,?u)",u=u)
login<-function(u){p<-web_test_provider(u);web_sign_in(con,p$provider,p$credential)}
token<-login(actor);csrf<-web_secret()
req<-function(action,args=list(),auth=token,csrf_value=csrf)web_request(con,auth,action,args,csrf_value,csrf,s,upload_root)
baseargs<-list(project_id=ready$project_id,version_id=ready$version_id)
reject(req("evidence_view",baseargs,auth="invalid"))
web_exec(con,"INSERT INTO app_users VALUES('outsider','Outsider')")
reject(web_b2_view(con,s,"outsider",ready$project_id,ready$version_id))
other<-web_import_project(con,s,actor,"local-workspace",web_fixture(tempfile(),"b2-other"))
reject(web_b2_view(con,s,actor,other$project_id,ready$version_id))
reject(req("evidence_view",c(baseargs,list(actor="second"))))
reject(req("evidence_add_row",c(baseargs,list(row_id="injected")),csrf_value="stale"))
expired<-login(actor);web_exec(con,"UPDATE auth_sessions SET expires_at=0 WHERE token_hash=?h",h=web_token_hash(expired));reject(req("evidence_view",baseargs,auth=expired))
cat("PASS: unauthenticated/workspace/project/version/actor/session/CSRF isolation\n")
full<-checkpoints$FULLTEXT;fx<-web_b2_context(con,s,actor,full$project_id,full$version_id)$packet
rid<-fx$tables$Author_Final_Review[[1]]$.row_id
upload<-data.frame(name="fixture.pdf",datapath=pdf,size=file.info(pdf)$size)
pdf_reject<-function(u=upload,who=actor,p=full$project_id,id=rid)reject(web_b2_upload(con,s,who,p,full$version_id,id,u,upload_root))
pdf_reject(who="outsider");pdf_reject(p=other$project_id);pdf_reject(id="other-report")
for(n in c("../x.pdf","/x.pdf","x.exe","folder/x.pdf")){u<-upload;u$name<-n;pdf_reject(u)}
u<-upload;u$size<-21*1024^2;pdf_reject(u)
wrong<-file.path(upload_root,"wrong.pdf");writeLines("not a PDF",wrong);u<-upload;u$datapath<-wrong;u$size<-file.info(wrong)$size;pdf_reject(u)
link<-file.path(upload_root,"link.pdf");stopifnot(file.symlink(pdf,link));u<-upload;u$datapath<-link;pdf_reject(u)
outside<-tempfile(fileext=".pdf");file.copy(pdf,outside);u<-upload;u$datapath<-outside;pdf_reject(u)
big<-file.path(upload_root,"large.pdf");fout<-file(big,"wb");writeBin(charToRaw("%PDF-"),fout);seek(fout,21*1024^2,"start");writeBin(as.raw(0),fout);close(fout);u<-upload;u$datapath<-big;u$size<-file.info(big)$size;pdf_reject(u);unlink(big)
arts<-web_query(con,"SELECT * FROM artifacts WHERE project_id=?p AND logical_name LIKE '%.pdf'",p=ready$project_id);stopifnot(nrow(arts)>0)
reject(web_download(con,s,"outsider",ready$project_id,arts$id[1],tempfile()))
reject(web_download(con,s,actor,other$project_id,arts$id[1],tempfile()))
download<-tempfile();web_download(con,s,actor,ready$project_id,arts$id[1],download);stopifnot(web_sha(download)==arts$checksum[1])
cat("PASS: PDF registration/download/checksum; traversal, symlink, signature, size, unsupported type and report substitution rejected\n")
x<-context();row<-x$tables$Target_Outcome_Verification[[1]]
reject(web_b2_edit(con,s,actor,other$project_id,ready$version_id,"Target_Outcome_Verification",row$.row_id,list(intervention_events="3")))
reject(web_b2_edit(con,s,actor,ready$project_id,checkpoints$SCREENING$version_id,"Target_Outcome_Verification",row$.row_id,list(intervention_events="3")))
for(k in c("screening_id","study_id",".actor",".verified_by","verification_status"))reject(web_b2_edit(con,s,actor,ready$project_id,ready$version_id,"Target_Outcome_Verification",row$.row_id,setNames(list("forged"),k)))
rob<-x$tables$Domain_Review[[1]]
for(k in c("screening_id","rob_tool","domain_id"))reject(web_b2_edit(con,s,actor,ready$project_id,ready$version_id,"Domain_Review",rob$.row_id,setNames(list("other-study"),k)))
revision<-web_b2_edit(con,s,actor,ready$project_id,ready$version_id,"Target_Outcome_Verification",row$.row_id,list(intervention_events="3"))
y<-web_b2_context(con,s,actor,revision$project_id,revision$version_id)
stopifnot(y$packet$stage=="EXTRACTION",is.null(y$packet$tables$Domain_Review),is.null(y$packet$tables$Study_Result_Manifest),!file.exists(file.path(y$version$root,"results/analysis_ready_evidence.json")),is.null(y$packet$tables$Target_Outcome_Verification[[1]]$.verified_by))
stopifnot(y$packet$tables$Target_Outcome_Verification[[1]]$.values$intervention_events$reported_value=="2",y$packet$tables$Target_Outcome_Verification[[1]]$.values$intervention_events$human_value=="3")
reject(web_b2_verify(con,s,actor,revision$project_id,revision$version_id,"Target_Outcome_Verification",row$.row_id,TRUE,"self"))
reject(web_b2_verify(con,s,"second",revision$project_id,revision$version_id,"Target_Outcome_Verification",row$.row_id,FALSE,"no approval"))
g<-web_query(con,"SELECT * FROM gates WHERE version_id=?v",v=revision$version_id);stopifnot(all(g$status=="PENDING"))
reject(web_decide_gate(con,actor,revision$project_id,g$id[g$gate_type=="b2_extraction"],"APPROVED",""))
reject(web_submit(con,s,actor,revision$project_id,revision$version_id,"finalize_extraction"))
reject(web_submit(con,s,actor,revision$project_id,revision$version_id,"finalize_evidence"))
reject(web_b2_edit(con,s,actor,revision$project_id,revision$version_id,"Domain_Review",rob$.row_id,list(author_domain_judgement="Low")))
job<-web_query(con,"SELECT id FROM jobs WHERE project_id=?p AND operation='finalize_evidence' AND state='COMPLETE'",p=ready$project_id)$id
reject(web_b2_accept(con,s,actor,revision$project_id,revision$version_id,job))
inclusion_revision<-web_b2_edit(con,s,actor,ready$project_id,ready$version_id,"Author_Final_Review",rid,list(author_notes="Reconsider eligibility"))
y<-web_b2_context(con,s,actor,inclusion_revision$project_id,inclusion_revision$version_id)
stopifnot(y$packet$stage=="FULLTEXT",is.null(y$packet$included_reports),is.null(y$packet$tables$Target_Outcome_Verification))
reject(web_submit(con,s,actor,inclusion_revision$project_id,inclusion_revision$version_id,"prepare_extraction"))
reject(web_b2_edit(con,s,actor,inclusion_revision$project_id,inclusion_revision$version_id,"Target_Outcome_Verification",row$.row_id,list(intervention_events="4")))
stopifnot(web_sha(file.path(ready_root,"config/generated/web_evidence.json"))==ready_hash)
extra<-web_b2_add_row(con,s,actor,revision$project_id,revision$version_id,row$.row_id)
y<-web_b2_context(con,s,actor,extra$project_id,extra$version_id)$packet;newrow<-tail(y$tables$Target_Outcome_Verification,1)[[1]]
stopifnot(is.null(newrow$.verified_by),newrow$verification_status=="Needs clarification",newrow$human_include_in_planned_analysis=="No",newrow$.row_id!=row$.row_id)
reject(web_exec(con,"UPDATE evidence_versions SET stage='READY' WHERE version_id=?v",v=revision$version_id))
reject(web_exec(con,"DELETE FROM audit_events WHERE project_id=?p",p=ready$project_id))
cat("PASS: value provenance, independent verification, immutable versions/audit, no inherited gates, stale inclusion/extraction/result and RoB substitutions rejected\n")
# No credential or production runtime bypass: tests use the explicit dev seam only for deterministic execution.
key<-Sys.getenv("OPENAI_API_KEY",unset=NA_character_);Sys.unsetenv("OPENAI_API_KEY")
for(op in names(web_b2_ai_operations())) {
 stage<-unname(web_b2_ai_operations()[op]);vv<-checkpoints[[stage]]
 err<-reject(web_submit(con,s,actor,vv$project_id,vv$version_id,op));stopifnot(err$code=="USER_ACTION_REQUIRED")
}
if(!is.na(key))Sys.setenv(OPENAI_API_KEY=key)
probe<-s;web_science_settings<-original_science
reject(web_b2_guard(con,probe,actor,ready$project_id,revision$version_id,"finalize_extraction"))
web_science_settings<-function(settings,verify=TRUE)settings
stopifnot(!isTRUE(web_registry()$run_meta_analysis$enabled))
cat("PASS: all four AI operations fail closed without credentials; canonical runtime gate retained; WEB-011 disabled\n")
# Real authenticated UI boundary and saved immutable decision. No UI/storage mocks.
source("web/ui/evidence.R")
ui_version<-checkpoints$SCREENING
shiny::testServer(function(input,output,session){
 notice<-shiny::reactiveVal("");refresh<-shiny::reactiveVal(0L)
 safe<-function(expr)tryCatch(expr,error=function(e){notice(conditionMessage(e));NULL})
 request<-function(action,args=list())req(action,args)
 web_b2_server(input,output,session,request,safe,notice,refresh)
 output$notice<-shiny::renderText(notice())
},{
 session$setInputs(project=ui_version$project_id,version=ui_version$version_id);session$flushReact()
 session$setInputs(evidence_refresh=1);session$flushReact()
 z<-web_b2_context(con,s,actor,ui_version$project_id,ui_version$version_id)$packet
 session$setInputs(evidence_table="Reviewer_1",evidence_row=z$tables$Reviewer_1[[1]]$.row_id);session$flushReact()
 stopifnot(grepl("SCREENING",output$evidence_stage),grepl("Synthetic",output$evidence_context$html))
 session$setInputs(evf_1="Include",evf_2="",evf_3="",evf_4="Human UI decision",evidence_save=1);session$flushReact()
 stopifnot(grepl("Saved a new evidence version",output$notice))
 latest<-web_query(con,"SELECT id FROM project_versions WHERE project_id=?p ORDER BY version DESC LIMIT 1",p=ui_version$project_id)$id
 z<-web_b2_context(con,s,actor,ui_version$project_id,latest)$packet
 stopifnot(z$tables$Reviewer_1[[1]]$notes=="Human UI decision",z$tables$Reviewer_1[[1]]$.actor==actor)
})
cat("PASS: Shiny screening display and authenticated Human decision/new version\n")
