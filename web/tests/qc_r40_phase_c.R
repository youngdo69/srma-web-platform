source("web/load.R")
source("web/qc/runtime.R",encoding="UTF-8")

local({
  root<-tempfile("r40-phase-c-")
  dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  settings<-list(data_root=root,engine_root=web_root)
  digest<-web_c2_hash(list("fixture"))
  policy<-srma_qc_policy_load(web_root)
  scope<-list(fingerprint=digest,project_id="synthetic",manuscript_version_id="review",
    engine_sha256=web_r40_script_sha(web_root),inputs_sha256=digest,policy_sha256=policy$sha256)
  grant<-list(operator="test-operator",model="mock-2026-01-01",expires_at=ceiling(as.numeric(Sys.time())+3600),
    budget_microusd=1000000L,max_input_tokens=500000L,max_output_tokens=12000L,
    input_microusd_per_million=100000L,cached_input_microusd_per_million=50000L,output_microusd_per_million=200000L)
  req<-list(step=web_r40_ai_steps()[1L],system="Check frozen facts",user="Synthetic input",policy_sha256=policy$sha256)
  calls<-0L
  response<-function(text="{\"test\":true}")list(text=text,model=grant$model,provider_id="mock-response",
    usage=list(input_tokens=100L,cached_input_tokens=20L,output_tokens=10L))
  mock<-function(body) {stopifnot(startsWith(body$system,policy$text),body$model==grant$model);calls<<-calls+1L;response()}
  lease<-function()TRUE
  call<-function(s=scope,r=req,tr=mock,l=lease,mode="MOCK")web_r40_ai_call(settings,s,r,policy,tr,mode,l)
  invalid<-function(expr)inherits(tryCatch(force(expr),error=identity),"srma_web_error")
  stopifnot(call()$state=="AI_APPROVAL_REQUIRED",calls==0L)
  too_low<-grant;too_low$budget_microusd<-1L
  stopifnot(invalid(web_r40_ai_grant(settings,scope,too_low,"MOCK")))
  web_r40_ai_grant(settings,scope,grant,"MOCK")
  stopifnot(call(mode="LIVE")$state=="AI_APPROVAL_MODE_MISMATCH",call(l=function()FALSE)$state=="WORKER_LEASE_LOST",calls==0L)
  first<-call()
  stopifnot(first$state=="COMPLETE",first$cost_microusd==11L,first$usage$input_tokens==100L,
    call()$reused,calls==1L)
  changed<-req;changed$user<-"Changed manuscript prompt"
  stopifnot(invalid(call(r=changed)),calls==1L)
  failed<-scope;failed$fingerprint<-web_c2_hash(list("interrupted"))
  web_r40_ai_grant(settings,failed,grant,"MOCK")
  interrupt<-function(body){calls<<-calls+1L;stop("Connection lost after dispatch")}
  stopifnot(call(s=failed,tr=interrupt)$state=="AI_REQUEST_OUTCOME_UNKNOWN",
    call(s=failed)$state=="AI_REQUEST_OUTCOME_UNKNOWN",calls==2L)
  web_r40_ai_revoke(settings,scope,grant$operator)
  second<-req;second$step<-web_r40_ai_steps()[2L]
  stopifnot(call(r=second)$state=="AI_APPROVAL_REVOKED",call()$reused,calls==2L)
  live<-scope;live$fingerprint<-web_c2_hash(list("no live enable"))
  live_grant<-c(grant,list(rate_source="synthetic fixture",model_context_tokens=grant$max_input_tokens))
  web_r40_ai_grant(settings,live,live_grant,"LIVE")
  stopifnot(call(s=live,mode="LIVE")$state=="AI_LIVE_DISABLED",calls==2L)
  cat("Phase C journal PASS: no authorization/no spend, capped reservation, scope identity, usage, retry and uncertain dispatch\n")

  input<-function(status="PASS",basis="Verified fixture",binding=NULL)web_r40_item(status,basis,binding)
  approved<-list(stage="VERIFIED_SOURCE",approval="APPROVED")
  links<-list(prisma=approved,references=approved,rob=approved,certainty=approved)
  binding<-list(protocol=input(binding=list(version="protocol")),r38=input(),r39=input(),study_selection=input(),
    outcome_harmonization=input(),analyses=input("NOT_APPLICABLE","Finalized narrative plan does not pool outcomes"),
    qc_policy=input(binding=list(sha256=policy$sha256,r40_script_sha256=scope$engine_sha256)),
    human_confirmation=input("NOT_ASSESSED","Author approval pending"))
  contract<-list(binding=list(scope=list(manuscript_version_id="review"),inputs=binding),sha256=digest,ready=FALSE,
    execution=list(scope=scope,sha256=digest,ready=TRUE))
  make_plan<-function(route,inputs=contract,protocol=list(),analyses=list())web_r40_validation_plan(settings,inputs,
    list(profiles=as.list(c("CORE",route)),r40_required=TRUE),links,list(),protocol,analyses)
  narrative<-make_plan("NARRATIVE_SWIM")
  pairwise_input<-contract;pairwise_input$binding$inputs$analyses<-input()
  exemption<-list(applicable=FALSE,rationale="Approved protocol did not specify this assessment.")
  pairwise<-make_plan("PAIRWISE_META",pairwise_input,list(risk_of_bias=exemption,certainty=exemption,additional_analyses=exemption))
  mixed<-make_plan(c("NARRATIVE_SWIM","PAIRWISE_META"),pairwise_input,list(certainty=list(required=TRUE)),
    list(subgroup=list(plan=list(role="SUBGROUP"))))
  stopifnot(narrative$native_ready,!narrative$ready,
    narrative$rules$PAIRWISE_ESTIMATES_CI$applicability=="NOT_APPLICABLE",
    pairwise$rules$ROB_APPROVAL$applicability=="NOT_APPLICABLE",pairwise$rules$GRADE_APPROVAL$applicability=="NOT_APPLICABLE",
    pairwise$rules$PAIRWISE_ADDITIONAL_ANALYSES$applicability=="NOT_APPLICABLE",
    mixed$rules$PAIRWISE_ADDITIONAL_ANALYSES$applicability=="REQUIRED",mixed$rules$MIXED_CONSISTENCY$applicability=="REQUIRED",
    mixed$rules$GRADE_APPROVAL$applicability=="REQUIRED")
  approved_contract<-contract
  approved_contract$binding$inputs$human_confirmation<-input()
  approved_contract$binding$scope$manuscript_version_id<-"approved"
  approved_contract$ready<-TRUE
  stopifnot(identical(make_plan("NARRATIVE_SWIM",approved_contract)$execution_scope$fingerprint,narrative$execution_scope$fingerprint))

  scientific<-list(rscript=Sys.getenv("SRMA_R40_TEST_RSCRIPT",""),engine_library=Sys.getenv("SRMA_R40_TEST_LIBRARY",""),timeout=90)
  stopifnot(nzchar(scientific$rscript),nzchar(scientific$engine_library))
  paths<-web_r40_cli_paths(web_root)
  manuscript_quote<-"A total of 27 participants contributed data."
  pooled_quote<-"The pooled effect was lower in the intervention group."
  method_quote<-"We used the planned synthesis method."
  protocol_quote<-"synthesis_procedure: We used the planned synthesis method."
  make_fixture<-function(where,mode) {
    dir.create(where)
    write_source<-function(path,lines) {
      path<-file.path(where,path)
      dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
      writeLines(lines,path,useBytes=TRUE)
    }
    write_source("config/project_config.yml",c("project:","  id: synthetic","analysis:","  manuscript:",
      paste0("    reverify_pubmed_references: ",if(mode=="pairwise")"true" else "false"),"protocol:",paste0("  ",protocol_quote)))
    write_source(paths[["r38_state"]],c("manuscript_production:","  status: FINAL_EVIDENCE_READY"))
    write_source(paths[["r38_results"]],c("## Results",paste("-",manuscript_quote),if(mode!="narrative")pooled_quote))
    write_source(paths[["r38_registry"]],c("metric,status","fixture,PASS"))
    write_source(paths[["r38_artifact_registry"]],paste(c("artifact_id","artifact_type","title","manuscript_role",
      "manuscript_priority","coverage_required","coverage_token","source_relative_path"),collapse=","))
    write_source(paths[["r39_state"]],c("manuscript_drafting:","  drafting_status: DRAFT_COMPLETE_READY_FOR_R40_VALIDATION"))
    manuscript<-c("## Abstract","The review summarized results.","## Methods",method_quote,"## Results",
      manuscript_quote,if(mode!="narrative")pooled_quote,"## Discussion","The results are uncertain [1].",
      "## References","1. Synthetic report. PMID:12345678.")
    write_source(paths[["manuscript"]],manuscript)
    write_source(paths[["token_manuscript"]],manuscript)
    write_source(paths[["citation_map"]],c("pmid,citation_number","12345678,1"))
    write_source(paths[["citation_audit"]],c("citation_number,pmid,status","1,12345678,PASS"))
    write_source(paths[["literature_selected"]],c("pmid,title,doi","12345678,Synthetic report,10.1234/synthetic-fixture"))
    if(mode=="pairwise")write_source(web_r40_pubmed_path(),paste0("<PubmedArticleSet><PubmedArticle>",
      "<MedlineCitation><PMID>12345678</PMID><Article><ArticleTitle>Synthetic report</ArticleTitle></Article></MedlineCitation>",
      "<PubmedData><ArticleIdList><ArticleId IdType='doi'>10.1234/synthetic-fixture</ArticleId></ArticleIdList></PubmedData>",
      "</PubmedArticle></PubmedArticleSet>"))
    if(mode!="narrative") {
      group<-"results/meta_analysis/group/"
      write_source(paste0(group,"meta_analysis_summary.csv"),c("analysis_group,n_studies,pooled_effect,pooled_lower_ci,pooled_upper_ci,heterogeneity_i2",
        "Group,2,0.8,0.5,1.2,15"))
      write_source(paste0(group,"study_effects_used.csv"),c("study_id,intervention_total,comparator_total","study-a,6,7","study-b,7,7"))
    }
    web_sha(file.path(where,paths[["manuscript"]]))
  }
  transport_for<-function(plan,where,bad=FALSE)function(body) {
    calls<<-calls+1L
    stopifnot(startsWith(body$system,policy$text))
    csv<-read.csv(file.path(where,"results/manuscript/production/qc/canonical_numeric_fact_registry.csv"))
    if(grepl("MANUSCRIPT SENTENCES WITH NUMBERS:",body$user,fixed=TRUE)) {
      id<-csv$fact_id[which(csv$canonical_value==27)[1L]]
      return(response(as.character(web_json(list(claims=list(list(sentence_id="RES_001",classification="CURRENT_REVIEW",
        fact_ids=list(id),reason="Matches frozen participant count")))))))
    }
    checks<-lapply(names(web_r40_semantic_rules(plan$rules)),function(id)list(id=id,result="PASS",
      manuscript_quote=if(id=="METHODS_PROTOCOL")method_quote else manuscript_quote,
      evidence_quote=if(id=="METHODS_PROTOCOL")protocol_quote else if(bad)"Fabricated quote outside bound evidence" else manuscript_quote,
      aux_manuscript_quote=pooled_quote,aux_evidence_quote=pooled_quote,
      fact_ids=as.list(csv$fact_id[csv$metric %in% c("n_studies","pooled_effect","pooled_lower_ci","pooled_upper_ci")])))
    response(as.character(web_json(list(issues=list(),overall="PASS",checks=checks))))
  }
  run_native<-function(plan,label,approved=TRUE,attempt="first",bad=FALSE,lease_check=lease,damage=NULL) {
    scoped<-plan$execution_scope
    if(approved && !dir.exists(file.path(web_r40_ai_dir(settings,scoped$fingerprint),"approval")))
      web_r40_ai_grant(settings,scoped,grant,"MOCK")
    work<-file.path(root,paste(label,attempt,sep="-"))
    sha<-make_fixture(work,label)
    if(is.function(damage)) {
      damage(work)
      sha<-web_sha(file.path(work,paths[["manuscript"]]))
    }
    native<-web_r40_bridge_run(scientific,web_root,work,scoped,plan,settings,policy,lease_check,
      transport=transport_for(plan,work,bad),mode="MOCK")
    rules<-web_r40_execution_rules(plan,native,sha,"synthetic",list(source="fixture"))
    list(native=native,rules=rules,complete=web_r40_execution_complete(rules,native),work=work)
  }
  blocked<-run_native(narrative,"narrative",FALSE,"without-approval")
  stopifnot(blocked$native$state=="AI_APPROVAL_REQUIRED",blocked$rules$IDENTIFIER_INTEGRITY$result=="PASS",
    blocked$rules$NUMERIC_CLAIMS$result=="NOT_ASSESSED",!blocked$complete,calls==2L)
  citation_error<-function(work) {
    p<-file.path(work,paths[["manuscript"]]);writeLines(sub("PMID:12345678","PMID:12345679",readLines(p),fixed=TRUE),p)
  }
  blocked<-run_native(narrative,"narrative",FALSE,"bad-citation",damage=citation_error)
  stopifnot(blocked$native$state=="DETERMINISTIC_FINDING_BLOCKS_AI",blocked$rules$CITATION_INTEGRITY$result=="FAIL",calls==2L)
  for(type in c("narrative","pairwise","mixed")) {
    plan<-get(type)
    out<-run_native(plan,type)
    if(!identical(out$native$state,"COMPLETE"))print(out$native)
    stopifnot(out$native$state=="COMPLETE",length(out$native$calls)==2L,
      out$rules$METHODS_PROTOCOL$result=="PASS",out$rules$IDENTIFIER_INTEGRITY$result=="PASS")
    if(!out$complete) {
      print(list(canonical=out$native$canonical,rules=lapply(out$rules,`[[`,"result")))
      print(read.csv(file.path(out$work,"results/manuscript/production/qc/citation_integrity_audit.csv")))
    }
    stopifnot(out$complete)
    if(type=="mixed") {
      stopifnot(out$rules$MIXED_CONSISTENCY$result=="PASS")
      bad<-plan;bad$execution_scope$fingerprint<-web_c2_hash(list("different mixed evidence"))
      rejected<-run_native(bad,type,attempt="bad-quotes",bad=TRUE)
      stopifnot(!rejected$complete,rejected$rules$MIXED_CONSISTENCY$result=="NOT_ASSESSED")
    }
    cat("Phase C pinned engine /",type,"PASS\n")
  }
  replay<-run_native(narrative,"narrative",attempt="fresh-worker")
  stopifnot(length(replay$native$calls)==2L,all(vapply(replay$native$calls,function(r)isTRUE(r$reused),logical(1))),calls==10L)
  resumed<-narrative;resumed$execution_scope$fingerprint<-web_c2_hash(list("lease expired between model steps"))
  checks<-0L
  missed<-run_native(resumed,"narrative",attempt="lost-lease",lease_check=function(){checks<<-checks+1L;checks<=2L})
  stopifnot(missed$native$state=="WORKER_LEASE_LOST",length(missed$native$calls)==1L,calls==11L)
  completed<-run_native(resumed,"narrative",attempt="resumed")
  stopifnot(completed$complete,isTRUE(completed$native$calls[[1L]]$reused),
    !isTRUE(completed$native$calls[[2L]]$reused),completed$native$dispatched==1L,calls==12L)
  cat("Phase C canonical replay PASS: both durable model responses reused in a fresh Worker workspace\n")

  con<-DBI::dbConnect(RSQLite::SQLite(),":memory:")
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  for(sql in c("CREATE TABLE jobs(id,project_id,version_id,engine_id,operation,state,worker_token,idempotency_key,result_json)",
    "CREATE TABLE projects(id,workspace_id,canonical_id)","CREATE TABLE workspace_members(workspace_id,user_id)",
    "INSERT INTO projects VALUES('synthetic','ws','synthetic')",
    "INSERT INTO workspace_members VALUES('ws','owner')"))DBI::dbExecute(con,sql)
  job<-"job-fixture";version<-"review"
  work<-file.path(root,"jobs",job,"lease","project")
  dir.create(work,recursive=TRUE)
  settings$r40_worker_connection<-con
  web_exec(con,"INSERT INTO jobs VALUES(?j,'synthetic','review','engine',?o,'RUNNING','lease',?k,NULL)",
    j=job,o=web_r40_full_operation(),k=web_r40_full_key(narrative))
  mock_scope<-new.env(parent=environment(web_r40_full_worker_execute))
  for(name in c("srma_run","web_r40_full_worker_execute","web_r40_full_job","web_r40_execution_proof")) {
    f<-get(name,mode="function");environment(f)<-mock_scope;assign(name,f,mock_scope)
  }
  bound<-contract
  bound$binding$inputs$r39$binding<-list(artifacts=setNames(list(list(sha256=completed$native$canonical$manuscript_sha256)),paths[["manuscript"]]))
  mock_scope$web_r40_full_preflight<-function(...)list(packet=list(stage="REVIEW_REQUIRED"),
    publication=list(r40_validation_plan=narrative,r40_input_contract=bound))
  mock_scope$web_c3_collect_inputs<-function(...)list()
  mock_scope$web_r40_inspect<-function(...)list(verification="PASS",checks=list(numerics="PASS"))
  mock_scope$web_r40_bound_cli<-function(...,check_lease){stopifnot(check_lease());completed$native}
  out<-mock_scope$srma_run(settings,"synthetic",web_r40_full_operation(),list(),job,work,"engine",root,version)
  receipt<-out$artifacts[[1L]]$path
  register<-function() {
    archived<-out
    archived$artifacts<-list(list(logical_name=web_r40_full_artifact(),artifact_id="receipt",checksum=web_sha(receipt)))
    archived$provenance$project_version_id<-version
    web_exec(con,"UPDATE jobs SET state='COMPLETE',result_json=?r WHERE id=?j",r=as.character(web_json(archived)),j=job)
  }
  mock_scope$web_c3_artifact<-function(con,settings,actor,project_id,id,checksum,job) {
    stopifnot(identical(web_sha(receipt),checksum))
    list(row=list(id=id,version_id=version,checksum=checksum),path=receipt)
  }
  register()
  persisted<-mock_scope$web_r40_full_job(con,settings,"owner","synthetic",version,narrative)$inspection
  stopifnot(persisted$validation=="PASS",persisted$approval=="UNREVIEWED",persisted$rules$METHODS_PROTOCOL$result=="PASS",
    is.null(mock_scope$web_r40_execution_proof(con,settings,"owner","synthetic",narrative,bound)))
  # Simulate a trusted live-class receipt for the submission consumer only.
  persisted$native$mode<-"LIVE"
  jsonlite::write_json(persisted,receipt,auto_unbox=TRUE,null="null");register()
  approved_contract$execution$scope$manuscript_version_id<-version
  after_review<-make_plan("NARRATIVE_SWIM",approved_contract)
  stopifnot(identical(after_review$execution_scope$fingerprint,narrative$execution_scope$fingerprint))
  proof<-mock_scope$web_r40_execution_proof(con,settings,"owner","synthetic",after_review,approved_contract)
  if(is.null(proof)) {
    status<-mock_scope$web_r40_full_job(con,settings,"owner","synthetic",version,after_review)
    print(list(execution=status$execution,valid=status$inspection$validation,mode=status$inspection$native$mode,
      inputs_match=identical(status$inspection$execution_inputs_sha256,after_review$execution_scope$inputs_sha256)))
  }
  stopifnot(is.list(proof),proof$receipt$version=="approved",proof$rules$METHODS_PROTOCOL$execution=="REUSED")
  gate<-web_r40_apply_input_contract(list(blockers=list(),adaptive=list(r40_required=TRUE),finding_sha256=digest),
    bound,narrative,proof)
  stopifnot(gate$r40_integration$R40_VALIDATION_COMPLETE,invalid(web_c3_require_submission(gate)))
  release_inputs<-web_r40_apply_input_contract(list(blockers=list(),adaptive=list(r40_required=TRUE),finding_sha256=digest),
    approved_contract,after_review,proof)
  stopifnot(!length(release_inputs$blockers));web_c3_require_submission(release_inputs)
  cat("Phase C Worker / submission PASS: persisted rules, Mock excluded, reuse after unchanged author approval, draft cannot submit\n")
})
