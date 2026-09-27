# Uses primary state created by c1_reconciliation; scientific values remain canonical.
primary<-approved;primary_packet<-web_c1_view(con,s,actor,primary$project_id,primary$version_id)
primary_root<-web_c1_context(con,s,actor,primary$project_id,primary$version_id)$version$root
primary_hash<-web_sha(file.path(primary_root,"config/generated/web_analysis.json"))
parent_options<-web_c1_catalog(con,s,actor,ready$project_id,ready$version_id)$parents;stopifnot(primary$version_id %in% parent_options$version_id)
Sys.setenv(SRMA_WEB_TEST_IDP="1")
web_exec(con,"INSERT INTO external_identities VALUES('local-test',?u,?u)",u=actor)
idp<-web_test_provider(actor);token<-web_sign_in(con,idp$provider,idp$credential);csrf<-web_secret()
req<-function(action,args=list(),auth=token,csrf_value=csrf)web_request(con,auth,action,args,csrf_value,csrf,s)
baseargs<-list(project_id=primary$project_id,version_id=primary$version_id)
reject(req("analysis_view",baseargs,auth="invalid"))
web_exec(con,"INSERT INTO app_users VALUES('c1-outsider','Outsider')")
reject(web_c1_view(con,s,"c1-outsider",primary$project_id,primary$version_id))
other<-web_import_project(con,s,actor,"local-workspace",web_fixture(tempfile(),"c1-other"))
reject(web_c1_view(con,s,actor,other$project_id,primary$version_id))
reject(req("analysis_view",c(baseargs,list(actor="second"))))
reject(req("analysis_view",c(baseargs,list(analysis_id="forged"))))
for(k in c("rscript","script","settings","engine_root","reviewer","user_id"))reject(req("analysis_plan",c(list(project_id=ready$project_id,version_id=ready$version_id,spec=spec),setNames(list("injected"),k))))
reject(req("analysis_plan",list(project_id=ready$project_id,version_id=ready$version_id,spec=spec),csrf_value="bad"))
bad<-spec;bad$row_ids<-list("other-version-row");reject(web_c1_plan(con,s,actor,ready$project_id,ready$version_id,bad))
bad<-spec;bad$methods$estimator<-"DL";reject(web_c1_plan(con,s,actor,ready$project_id,ready$version_id,bad))
bad<-spec;bad$outcome<-"Different outcome";reject(web_c1_plan(con,s,actor,ready$project_id,ready$version_id,bad))
bad<-spec;bad$row_ids<-rep(spec$row_ids[1],2);reject(web_c1_plan(con,s,actor,ready$project_id,ready$version_id,bad))
bad<-spec;bad$pooled_effect<-1;reject(web_c1_plan(con,s,actor,ready$project_id,ready$version_id,bad))
reject(web_c1_plan(con,s,actor,other$project_id,ready$version_id,spec))
reject(web_c1_context(con,s,actor,primary$project_id,"forged"))
reject(web_exec(con,"UPDATE analysis_versions SET stage='APPROVED' WHERE version_id=?v",v=plan$version_id))
reject(web_exec(con,"DELETE FROM analysis_versions WHERE version_id=?v",v=plan$version_id))
cat("PASS: workspace/project/version/row, actor/reviewer/script/runtime injection, CSRF, immutable AnalysisVersion\n")
# A revised plan has no inherited approval.
revision_plan<-web_c1_plan(con,s,actor,ready$project_id,ready$version_id,spec)
reject(web_submit(con,s,actor,revision_plan$project_id,revision_plan$version_id,"run_meta_analysis"))
reject(web_c1_accept(con,s,actor,revision_plan$project_id,revision_plan$version_id,j))
reject(web_c1_approve(con,s,actor,revision_plan$project_id,revision_plan$version_id,TRUE,"not a result"))
reject(web_c1_approve(con,s,actor,res$project_id,res$version_id,FALSE,"not confirmed"))
arts<-web_query(con,"SELECT * FROM artifacts WHERE job_id=?j",j=j);stopifnot(any(grepl("[.]xlsx$",arts$logical_name)),any(grepl("[.]pdf$",arts$logical_name)))
for(i in seq_len(nrow(arts))) {
 f<-tempfile();web_download(con,s,actor,primary$project_id,arts$id[i],f);stopifnot(web_sha(f)==arts$checksum[i]);unlink(f)
}
reject(web_download(con,s,"c1-outsider",primary$project_id,arts$id[1],tempfile()))
reject(web_download(con,s,actor,other$project_id,arts$id[1],tempfile()))
# Test-only corruption of synthetic stored bytes; restore exact bytes immediately.
p<-web_store(s)$get(arts$storage_locator[1],arts$checksum[1]);orig<-readBin(p,"raw",file.info(p)$size);writeBin(charToRaw("substituted"),p)
reject(web_download(con,s,actor,primary$project_id,arts$id[1],tempfile()));writeBin(orig,p)
cat("PASS: approval non-inheritance, result substitution, authorized CSV/XLSX/PDF downloads and checksum rejection\n")
run_plan<-function(sp) {
 pv<-web_c1_plan(con,s,actor,ready$project_id,ready$version_id,sp)
 gid<-web_query(con,"SELECT id FROM gates WHERE version_id=?v AND gate_type='analysis_plan'",v=pv$version_id)$id
 web_decide_gate(con,actor,pv$project_id,gid,"APPROVED","Explicit synthetic analysis policy and subset review")
 id<-web_submit(con,s,actor,pv$project_id,pv$version_id,"run_meta_analysis");web_worker_once(con,s)
 job<-web_query(con,"SELECT * FROM jobs WHERE id=?j",j=id)
 if(job$state!="COMPLETE")print(web_parse(job$result_json))
 list(plan=pv,job=id,state=job$state)
}
ss<-spec;ss$role<-"SENSITIVITY";ss$parent_version<-primary$version_id;ss$row_ids<-spec$row_ids[1:2];ss$exclusions<-setNames(list("Prespecified synthetic exclusion to exercise linked sensitivity"),unlist(spec$row_ids[3]));ss$diagnostics<-FALSE;ss$rationale<-"Explicit leave-one-study subset, primary remains unchanged"
sens<-run_plan(ss);stopifnot(sens$state=="COMPLETE")
sens_result<-web_c1_accept(con,s,actor,sens$plan$project_id,sens$plan$version_id,sens$job)
sens_approved<-web_c1_approve(con,s,actor,sens_result$project_id,sens_result$version_id,TRUE,"Reviewed linked sensitivity, exclusion and small study-count warning")
sx<-web_c1_view(con,s,actor,sens_approved$project_id,sens_approved$version_id)
stopifnot(sx$plan$parent_version==primary$version_id,sx$plan$role=="SENSITIVITY",sx$result$summary$studies==2,sx$result$consistency$status=="PASS",web_sha(file.path(primary_root,"config/generated/web_analysis.json"))==primary_hash)
cat("PASS: linked sensitivity with independent plan/result gates; primary immutable\n")
# One-study canonical SKIP is actionable, never a successful pool.
one<-ss;one$row_ids<-spec$row_ids[1];one$exclusions<-setNames(as.list(rep("One-study estimability contract fixture",2)),unlist(spec$row_ids[2:3]));small<-run_plan(one)
stopifnot(small$state=="USER_ACTION_REQUIRED");jr<-web_parse(web_query(con,"SELECT result_json FROM jobs WHERE id=?j",j=small$job)$result_json);stopifnot(jr$error_code=="ANALYSIS_NOT_ESTIMABLE")
reject(web_c1_accept(con,s,actor,small$plan$project_id,small$plan$version_id,small$job))
cat("PASS: canonical one-study non-estimable status; approval blocked\n")
# Optional exploratory bundle uses explicit, not automatic, moderator selection.
ex<-spec;ex$role<-"EXPLORATORY";ex$parent_version<-primary$version_id;ex$diagnostics<-FALSE;ex$exploratory<-TRUE;ex$moderators<-list("final_intervention_events");ex$rationale<-"Synthetic explicit exploratory/underpowered test request, not clinical evidence"
expl<-run_plan(ex);stopifnot(expl$state=="COMPLETE")
explres<-web_c1_accept(con,s,actor,expl$plan$project_id,expl$plan$version_id,expl$job)
exx<-web_c1_view(con,s,actor,explres$project_id,explres$version_id);stopifnot(any(grepl("exploratory",exx$artifacts$logical_name)),length(exx$result$warnings)>0)
cat("PASS: canonical exploratory funnel/Egger/configured moderator outputs; underpowered warnings retained\n")
# Actual Shiny handler through authenticated request, with immutable save.
source("web/ui/analysis.R")
shiny::testServer(function(input,output,session){
 notice<-shiny::reactiveVal("");refresh<-shiny::reactiveVal(0L)
 safe<-function(expr)tryCatch(expr,error=function(e){notice(conditionMessage(e));NULL})
 request<-function(action,args=list())req(action,args)
 web_c1_server(input,output,session,request,safe,notice,refresh)
 output$notice<-shiny::renderText(notice())
},{
 session$setInputs(project=primary$project_id,version=primary$version_id);session$flushReact();session$setInputs(analysis_load=1);session$flushReact()
 stopifnot(grepl("APPROVED",output$analysis_status),grepl("pooled effect",output$analysis_results),grepl("Canonical forest",output$analysis_forest$html))
 session$setInputs(version=ready$version_id);session$flushReact();session$setInputs(analysis_catalog=1);session$flushReact();session$setInputs(analysis_group=spec$group);session$flushReact()
 session$setInputs(analysis_rows=unlist(spec$row_ids),analysis_role="PRIMARY",analysis_parent="",analysis_outcome=spec$outcome,analysis_definition=spec$definition,analysis_timepoint=spec$timepoint,analysis_design=spec$design,analysis_population=spec$population,analysis_basis="RAW",analysis_methods_confirm=FALSE,analysis_exclusion_reason="",analysis_rationale=spec$rationale,analysis_diagnostics="no",analysis_exploratory="no",analysis_moderators="",analysis_save=1);session$flushReact()
 stopifnot(grepl("Confirm canonical methods",output$notice))
 before_count<-nrow(web_query(con,"SELECT version_id FROM analysis_versions WHERE project_id=?p",p=primary$project_id))
 session$setInputs(analysis_methods_confirm=TRUE,analysis_save=2);session$flushReact();stopifnot(grepl("New immutable analysis",output$notice))
 stopifnot(nrow(web_query(con,"SELECT version_id FROM analysis_versions WHERE project_id=?p",p=primary$project_id))==before_count+1L)
})
cat("PASS: Shiny canonical result/forest display and explicit-method plan save through authenticated requests\n")
# Production guard remains hard; only this test process has a local seam.
web_science_settings<-original_science
if(!isTRUE(canonical_acceptance)){reject(web_science_settings(s));web_science_settings<-function(settings,verify=TRUE)settings}else web_science_settings(s)
if(identical(Sys.getenv("C2_TEST"),"1"))source("web/tests/c2_reconciliation.R")
# Upstream revision invalidates current use of every old analysis; historical approvals unchanged.
up<-web_b2_context(con,s,actor,ready$project_id,ready$version_id)$packet
revised<-web_b2_edit(con,s,actor,ready$project_id,ready$version_id,"Target_Outcome_Verification",up$tables$Target_Outcome_Verification[[1]]$.row_id,list(intervention_events="1"))
stopifnot(!isTRUE(web_c1_view(con,s,actor,primary$project_id,primary$version_id)$current))
reject(web_c1_plan(con,s,actor,ready$project_id,ready$version_id,spec));reject(web_c1_guard(con,s,actor,plan$project_id,plan$version_id,"run_meta_analysis"));reject(web_c1_accept(con,s,actor,plan$project_id,plan$version_id,j))
reject(web_c1_approve(con,s,actor,res$project_id,res$version_id,TRUE,"stale"))
stopifnot(web_sha(file.path(primary_root,"config/generated/web_analysis.json"))==primary_hash,web_c1_view(con,s,actor,primary$project_id,primary$version_id)$stage=="APPROVED",web_b2_context(con,s,actor,revised$project_id,revised$version_id)$packet$stage=="EXTRACTION")
stopifnot(nrow(web_query(con,"SELECT id FROM audit_events WHERE event_type='DOWNSTREAM_ANALYSIS_INVALIDATED' AND project_id=?p",p=primary$project_id))>0)
cat("PASS: stale evidence and downstream reuse blocked; historical approved result unchanged\n")
