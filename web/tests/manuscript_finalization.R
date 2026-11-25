# 6.3.15 Phase B. Disposable source, approvals and registry boundaries only.
engine<-normalizePath(getwd(),winslash="/")
for(module in c("core","evidence","manuscript","manuscript_render","manuscript_revision","statistical_methods",
  "qc_r40","qc_r40_inputs","qc_r40_full","qc_r40_execute","qc_r40_ai"))
  source(file.path("web/R",paste0(module,".R")),encoding="UTF-8")
source("web/qc/runtime.R",encoding="UTF-8")
if("--web" %in% commandArgs(TRUE)) local({
  .libPaths(c(file.path(engine,"web/.library"),.libPaths()))
  source("web/ui/manuscript.R",encoding="UTF-8")
  ready<-FALSE;approved<-FALSE;last<-list()
  view<-list(versions=data.frame(id=c("original","approved"),version=1:2),jobs=data.frame())
  packet_fixture<-list(stage="REVIEW_REQUIRED",docx=list(artifact_id="word"),current=TRUE,
    revision_editor=list(default="Discussion",canonical=TRUE,
      sections=list(Discussion=list(text="Original discussion.",sha256=web_c2_hash("Original discussion.")))))
  request<-function(action,args) {
    last[[action]]<<-args
    switch(action,view=view,
      manuscript_view=if(identical(args$version_id,"approved"))utils::modifyList(packet_fixture,list(stage="APPROVED")) else packet_fixture,
      r40_status=list(bound_ready=TRUE,bound_worker=list(execution=if(ready)"COMPLETE" else "NOT_RUN"),
        integration=list(R40_VALIDATION_COMPLETE=ready)),
      r40_bound_start={ready<<-TRUE;list(integration=list(R40_VALIDATION_COMPLETE=TRUE),bound_worker=list(execution="COMPLETE"))},
      manuscript_author_ready=ready,manuscript_approve={
        stopifnot(ready,args$confirmed,identical(args$version_id,"original"),nzchar(args$rationale))
        approved<<-TRUE;list(project_id="p",version_id="approved")
      },list())
  }
  shiny::testServer(function(input,output,session) {
    safe<-identity;notice<-function(...)NULL;refresh<-shiny::reactiveVal(0L)
    eval(body(web_c2_server),environment())
  },{
    session$setInputs(page="Manuscript",project="p",version="original")
    packet(packet_fixture);packet_version("original");session$flushReact()
    stopifnot(grepl("Check manuscript",output$manuscript_revision_validation$html,fixed=TRUE),
      grepl("after the required scientific checks",output$manuscript_author_review$html,fixed=TRUE),
      grepl("review DOCX",output$manuscript_production_download$html,fixed=TRUE))
    session$setInputs(manuscript_revision_section="Discussion",manuscript_revision_text_Discussion="Revised discussion.",
      manuscript_revision_reason="Clarify the summary.",manuscript_revision_preview=1)
    stopifnot(identical(revision_preview()$section,"Discussion"),identical(revision_preview()$text,"Revised discussion."))
    session$setInputs(manuscript_revision_verify=1)
    stopifnot(last$r40_bound_start$version_id=="original",
      grepl("Approve author-reviewed manuscript",output$manuscript_author_review$html,fixed=TRUE))
    session$setInputs(manuscript_confirm=TRUE,manuscript_review_reason="I checked the evidence.",manuscript_approve=1)
    stopifnot(approved,identical(packet_version(),"approved"))
    session$setInputs(version="approved")
    stopifnot(grepl("author-approved DOCX",output$manuscript_production_download$html,fixed=TRUE))
  })
  cat("Shiny section compare, first R/40 action, approval unlock and approved download PASS\n")
}) else local({
  root<-tempfile("manuscript-finalization-");dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  settings<-list(data_root=root,engine_root=engine)
  put<-function(where,file,lines) {
    path<-file.path(where,file);dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
    writeLines(lines,path,useBytes=TRUE);path
  }
  bad<-function(expr)inherits(tryCatch(force(expr),error=identity),"error")
  parent_text<-paste(c("# Research review","## Abstract","Results: Data were reported.","## Introduction",
    "The objective was specified [1].","## Methods","### Statistical Analysis","The studies were summarized separately.",
    "## Results","A total of 27 participants contributed data.","## Discussion","The results are uncertain [1].",
    "## References","1. Methodological report. PMID:12345678."),collapse="\n\n")
  x<-list(stage="HUMAN_EDITED",manuscript_id="m",evidence_version="e",source_sha256=web_c2_hash("source"),
    reference_sha256=web_c2_hash("references"))
  staged<-file.path(root,"config/generated/manuscript_revision")
  original_md<-put(staged,"manuscript.md",parent_text)
  mapping<-put(staged,"citation_map.json",'[{"citation_number":1,"pmid":"12345678"}]')
  figure<-file.path(root,"figure.png")
  grDevices::png(figure,width=420,height=300);graphics::plot(1:3,1:3);grDevices::dev.off()
  assets<-list(tables=list(list(title="Table 4. Recorded results",data=data.frame(Study="A",Results="Recorded"),origin="hash-bound")),
    figures=list(list(title="Figure 1. Study flow",path=figure,origin="hash-bound")))
  # Results text and Methods use different section addresses. Preserve default
  # citation numbering while refusing citation edits and unsupported syntax.
  edited<-"The verified findings remain uncertain [1]."
  old<-web_revision_markdown(parent_text,"Discussion")$section
  rev<-list(section="Discussion",canonical=TRUE,text=edited,original_section_sha256=web_c2_hash(old),
    section_sha256=web_c2_hash(edited),fingerprint=web_c2_hash("discussion edit"),
    source_sha256=x$source_sha256,reference_sha256=x$reference_sha256)
  updated<-web_revision_apply(parent_text,rev)
  stopifnot(identical(web_revision_markdown(updated,"Discussion")$section,edited),
    identical(web_revision_markdown(updated)$section,web_revision_markdown(parent_text)$section),
    !web_revision_plain("An *emphasis* change."),!web_revision_plain("An equation $x=y$."),
    bad(web_revision_apply(parent_text,utils::modifyList(rev,list(text="Other [2].",section_sha256=web_c2_hash("Other [2]."))))))
  layout<-web_c2_source_layout(assets,staged)
  canonical<-web_c2_source_write(staged,x,layout)
  word<-file.path(staged,"manuscript.docx")
  web_c2_render_word(parent_text,engine,word,assets)
  parent_word_hash<-web_sha(word)
  rev$parent_archive<-setNames(lapply(list.files(staged,full.names=TRUE),function(p)list(sha256=web_sha(p))),list.files(staged))
  x$revision<-rev;x$sections<-list(Discussion=list(text=edited))
  out<-file.path(root,"derived");dir.create(out)
  result<-web_revision_render(root,out,x,list(),engine)
  source_path<-file.path(out,"manuscript_source.json")
  source<-web_c2_source_read(source_path,x,file.path(out,"manuscript.md"),file.path(out,"citation_map.json"))
  rendered<-officer::docx_summary(officer::read_docx(file.path(out,"manuscript.docx")))
  stopifnot(all(c(edited,"Recorded","1. Methodological report. PMID:12345678.") %in% rendered$text),
    identical(canonical,web_sha(file.path(staged,"manuscript_source.json"))),
    !identical(result$canonical_source_sha256,canonical),identical(result$docx_sha256,web_sha(file.path(out,"manuscript.docx"))),
    identical(parent_word_hash,web_sha(word)),identical(web_sha(mapping),web_sha(file.path(out,"citation_map.json"))),
    identical(web_c2_hash(source$layout),web_c2_hash(layout)),
    identical(web_sha(file.path(out,layout$figures[[1L]]$file)),layout$figures[[1L]]$sha256))
  wrong<-put(root,"wrong.md",sub("27","28",updated,fixed=TRUE))
  stopifnot(bad(web_c2_source_read(source_path,x,wrong,mapping)))
  word_check<-web_r40_check_word
  word_env<-new.env(parent=globalenv())
  environment(word_check)<-word_env
  archive<-list("manuscript.md"=list(path=file.path(out,"manuscript.md"),sha256=web_sha(file.path(out,"manuscript.md"))),
    "citation_map.json"=list(path=file.path(out,"citation_map.json")),
    "manuscript_source.json"=list(path=source_path),
    "manuscript.docx"=list(path=file.path(out,"manuscript.docx"),sha256=result$docx_sha256))
  word_env$web_revision_archive<-function(...)archive
  assessed<-word_check(x,settings,NULL,NULL,NULL)
  stopifnot(identical(assessed$result,"PASS"),identical(assessed$source$docx_sha256,result$docx_sha256))
  archive[["manuscript.docx"]]$path<-word
  stopifnot(identical(word_check(x,settings,NULL,NULL,NULL)$result,"FAIL"))
  # Native staging fixture with the declared pinned R/40 paths and a genuine
  # original numbered/token pair. Record bytes before creating the attempt.
  paths<-web_r40_cli_paths(engine)
  native<-file.path(root,"native")
  token_body<-sub("(?m)^## References[\\s\\S]*$","",parent_text,perl=TRUE)
  token_body<-trimws(gsub("[1]","[[PMID:12345678]]",token_body,fixed=TRUE))
  put(native,paths[["manuscript"]],parent_text)
  put(native,paths[["token_manuscript"]],token_body)
  put(native,paths[["citation_map"]],c("pmid,citation_number","12345678,1"))
  delta<-file.path(out,"manuscript.md");sha<-web_sha(delta)
  stopifnot(bad(web_r40_stage_revision(native,paths,delta,web_sha(wrong))),
    identical(parent_text,web_revision_read_md(file.path(native,paths[["manuscript"]]))))
  stopifnot(web_r40_stage_revision(native,paths,delta,sha),
    identical(sha,web_sha(file.path(native,paths[["manuscript"]]))),
    grepl("The verified findings remain uncertain [[PMID:12345678]].",
      web_revision_read_md(file.path(native,paths[["token_manuscript"]])),fixed=TRUE))
  put(native,"config/project_config.yml",c("project:","  id: fixture","analysis:","  manuscript:","    reverify_pubmed_references: false"))
  put(native,paths[["r38_state"]],c("manuscript_production:","  status: FINAL_EVIDENCE_READY"))
  put(native,paths[["r38_results"]],c("## Results","- A total of 27 participants contributed data."))
  put(native,paths[["r38_registry"]],c("metric,status","result,PASS"))
  put(native,paths[["r38_artifact_registry"]],paste(c("artifact_id","artifact_type","title","manuscript_role",
    "manuscript_priority","coverage_required","coverage_token","source_relative_path"),collapse=","))
  put(native,paths[["r39_state"]],c("manuscript_drafting:","  drafting_status: DRAFT_COMPLETE_READY_FOR_R40_VALIDATION"))
  put(native,paths[["citation_audit"]],c("citation_number,pmid,status","1,12345678,PASS"))
  put(native,paths[["literature_selected"]],c("pmid,title,doi","12345678,Methodological report,10.1234/methods-fixture"))
  policy<-srma_qc_policy_load(engine)
  scope<-list(fingerprint=web_c2_hash("initial run"),engine_sha256=web_r40_script_sha(engine),policy_sha256=policy$sha256)
  ids<-c("CITATION_INTEGRITY","IDENTIFIER_INTEGRITY","ARTIFACT_COVERAGE","NUMERIC_CLAIMS","CROSS_SECTION_SEMANTICS",
    "STATISTICAL_METHODS_REPORTING","DOCUMENT_STRUCTURE")
  rules<-setNames(lapply(ids,function(id)web_r40_rule(id,id)),ids)
  rules$DOCUMENT_STRUCTURE<-assessed
  method<-"The studies were summarized separately."
  plan<-list(execution_scope=scope,rules=rules,statistical_records=list(synthesis=list(field="synthesis",text=method)))
  scientific<-list(rscript=file.path(R.home("bin"),"Rscript.exe"),engine_library=Sys.getenv("SRMA_R40_TEST_LIBRARY"),timeout=90)
  stopifnot(dir.exists(scientific$engine_library))
  word_runtime<-web_r40_word_runtime;environment(word_runtime)<-word_env
  word_env$web_science_settings<-function(...)c(scientific,list(runtime_acceptance="EXACT_CANONICAL_RUNTIME"))
  word_from_native<-word_runtime(settings,root,file.path(out,"manuscript.docx"),delta,
    list(docx_sha256=assessed$source$docx_sha256,markdown_sha256=sha,tables=1L,figures=1L))
  stopifnot(identical(web_c2_hash(assessed$source),web_c2_hash(word_from_native)))
  result<-web_r40_bridge_run(scientific,engine,native,scope,plan,settings,policy,function()TRUE,
    transport=function(...)stop("No provider dispatch authorized"),mode="MOCK")
  found<-web_r40_execution_rules(plan,result,sha,"fixture",list(source="staged revision"))
  stopifnot(identical(result$state,"AI_APPROVAL_REQUIRED"),identical(result$dispatched,0L),
    identical(found$CITATION_INTEGRITY$result,"PASS"),identical(found$IDENTIFIER_INTEGRITY$result,"PASS"),
    identical(found$NUMERIC_CLAIMS$result,"NOT_ASSESSED"),!web_r40_execution_complete(found,result))
  scope$fingerprint<-web_c2_hash("initial run mock authorized")
  web_r40_ai_grant(settings,scope,list(operator="test-reviewer",model="fixture-model",
    expires_at=ceiling(as.numeric(Sys.time())+3600),budget_microusd=1000000L,max_input_tokens=500000L,
    max_output_tokens=12000L,input_microusd_per_million=100000L,cached_input_microusd_per_million=50000L,
    output_microusd_per_million=200000L),"MOCK")
  received<-new.env();received$calls<-0L
  mock<-function(body) {
    received$calls<-received$calls+1L
    if(grepl("MANUSCRIPT SENTENCES WITH NUMBERS:",body$user,fixed=TRUE)) {
      csv<-read.csv(file.path(native,"results/manuscript/production/qc/canonical_numeric_fact_registry.csv"))
      id<-csv$fact_id[which(csv$canonical_value==27)[1L]]
      response<-list(claims=list(list(sentence_id="RES_001",classification="CURRENT_REVIEW",fact_ids=list(id),
        reason="Exact participant count")))
    } else {
      stopifnot(grepl("Full manuscript:",body$user,fixed=TRUE),grepl(edited,body$user,fixed=TRUE))
      w<-list(id="synthesis",result="PASS",evidence_quote=method,manuscript_quote=method)
      response<-list(overall="PASS",issues=list(),checks=list(list(id="STATISTICAL_METHODS_REPORTING",
        result="PASS",unsupported_claims=list(),witnesses=list(w))))
    }
    list(text=as.character(jsonlite::toJSON(response,auto_unbox=TRUE)),model=body$model,provider_id="mock-receipt",
      usage=list(input_tokens=100L,cached_input_tokens=0L,output_tokens=10L))
  }
  completed<-web_r40_bridge_run(scientific,engine,native,scope,plan,settings,policy,function()TRUE,transport=mock,mode="MOCK")
  found<-web_r40_execution_rules(plan,completed,sha,"fixture",list(source="staged revision"))
  stopifnot(identical(completed$state,"COMPLETE"),received$calls==2L,identical(completed$mode,"MOCK"),
    identical(found$STATISTICAL_METHODS_REPORTING$result,"PASS"),web_r40_execution_complete(found,completed))
  # The registered Worker receives a revision without prior approval. It must
  # dispatch native verification and retain completed and pending rule results.
  env<-new.env(parent=globalenv());worker<-web_r40_full_worker_execute;environment(worker)<-env
  plan$revision<-list(selective=FALSE)
  plan$execution_scope$inputs_sha256<-web_c2_hash("inputs");plan$execution_scope$manuscript_version_id<-"revised"
  contract<-list(binding=list(scope=list(manuscript_version_id="revised"),inputs=list(
    revision=list(binding=list(revised_archive=list("manuscript.md"=list(sha256=sha)))))),execution=list(ready=TRUE))
  env$web_r40_worker_scope<-function(...)list(actor="reviewer",row=list(project_id="p",idempotency_key=web_r40_full_key(plan)))
  env$web_r40_full_preflight<-function(...)list(packet=x,publication=list(r40_validation_plan=plan,r40_input_contract=contract))
  env$web_r40_check_word<-function(...)assessed
  env$web_c3_collect_inputs<-function(...)list()
  env$web_revision_worker_rules<-function(...)stop("A first review cannot use selective-only execution")
  env$web_r40_bound_cli<-function(...)result
  work<-file.path(root,"job");dir.create(work)
  job<-worker(settings,"fixture","revised","job",work,"fixture-engine")
  inspection<-jsonlite::read_json(job$artifacts[[1L]]$path,simplifyVector=FALSE)
  stopifnot(identical(job$status,"USER_ACTION_REQUIRED"),identical(inspection$validation,"NOT_ASSESSED"),
    identical(inspection$approval,"UNREVIEWED"),identical(inspection$rules$DOCUMENT_STRUCTURE$result,"PASS"),
    identical(inspection$rules$CITATION_INTEGRITY$result,"PASS"))
  proof<-web_r40_execution_proof;environment(proof)<-env
  env$web_r40_full_job<-function(...)list(execution="COMPLETE",inspection=list(validation="PASS",native=completed,
    execution_inputs_sha256=plan$execution_scope$inputs_sha256,rules=found))
  stopifnot(is.null(proof(NULL,settings,"reviewer","p",plan,contract)))
  cat("Canonical Word re-render, frozen assets and citations, R/39 stage, native R/40 without approval and with an isolated Mock PASS\n")
})
