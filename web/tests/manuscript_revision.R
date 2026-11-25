args<-commandArgs(TRUE)
if("--word" %in% args) {
  source("web/R/core.R",encoding="UTF-8")
  source("web/R/manuscript.R",encoding="UTF-8")
  source("web/R/manuscript_revision.R",encoding="UTF-8")
  root<-tempfile("word-revision-test-");dir.create(root)
  out<-file.path(root,"results");dir.create(out)
  input<-file.path(root,"config/generated/manuscript_revision");dir.create(input,recursive=TRUE)
  original<-"We compared outcomes within their approved strata [1]."
  revised<-"Outcomes were compared within the approved strata [1].\n\nWe retained the study-level results."
  md<-paste("# Review","## Methods","### Sources","Source details.",
    "### Statistical analysis and synthesis method",original,"## Results","Observed study results.",
    "## References","1. Registered reference.",sep="\n\n")
  writeLines(md,file.path(input,"manuscript.md"),useBytes=TRUE)
  writeLines('[{"citation_number":1,"pmid":"12345"}]',file.path(input,"citation_map.json"),useBytes=TRUE)
  plot<-file.path(root,"figure.png")
  grDevices::png(plot,width=420,height=280);graphics::plot(1:3,1:3);grDevices::dev.off()
  word<-officer::read_docx()
  add<-function(word,text,style="Normal")officer::body_add_par(word,text,style=style)
  word<-add(word,"Review","heading 1");word<-add(word,"Methods","heading 1")
  word<-add(word,"Sources","heading 2");word<-add(word,"Source details.")
  word<-add(word,"Statistical analysis and synthesis method","heading 2");word<-add(word,original)
  word<-add(word,"Results","heading 1");word<-add(word,"Observed study results.")
  word<-add(word,"References","heading 1");word<-add(word,"1. Registered reference.")
  word<-officer::body_add_table(word,data.frame(study="A",result="Preserved"))
  word<-officer::body_add_img(word,plot,width=2,height=1.5)
  path<-file.path(input,"manuscript.docx");print(word,target=path)
  hash<-web_sha(path)
  archive<-setNames(lapply(list.files(input,full.names=TRUE),function(p)list(sha256=web_sha(p))),list.files(input))
  r<-list(text=revised,original_section_sha256=web_c2_hash(original),section_sha256=web_c2_hash(revised),
    parent_archive=archive,source_sha256="source",reference_sha256="refs",fingerprint="revision")
  x<-list(stage="HUMAN_EDITED",source_sha256="source",reference_sha256="refs",revision=r,
    sections=list(Methods=list(text=revised)))
  proof<-web_revision_render(root,out,x,list())
  saved<-web_revision_read_md(file.path(out,"manuscript.md"))
  stopifnot(identical(web_revision_markdown(saved)$section,revised),identical(web_sha(path),hash),
    identical(web_sha(file.path(out,"citation_map.json")),archive[["citation_map.json"]]$sha256),
    !identical(proof$docx_sha256,hash),identical(proof$scope,NULL),
    identical(proof$revision$section_sha256,r$section_sha256))
  edited<-officer::docx_summary(officer::read_docx(file.path(out,"manuscript.docx")))
  stopifnot(all(web_revision_paragraphs(revised) %in% edited$text),"Preserved" %in% edited$text,
    "1. Registered reference." %in% edited$text,nzchar(proof$revision$scope$outside_sha256))
  bad<-x;bad$revision$parent_archive[["manuscript.docx"]]$sha256<-strrep("0",64L)
  failed<-file.path(root,"failed");dir.create(failed)
  stopifnot(inherits(tryCatch(web_revision_render(root,failed,bad,list()),error=identity),"srma_web_error"),
    !file.exists(file.path(failed,"manuscript.docx")),identical(web_sha(path),hash))
  cat("Real Word revision, table, figure and reference preservation, and fail-closed artifact handling PASS\n")
} else {
  source("web/load.R",encoding="UTF-8")
  source("web/qc/runtime.R",encoding="UTF-8")
  settings<-list(engine_root=normalizePath(getwd(),winslash="/"))
  sha<-strrep("a",64L)
  section<-"Random-effects models were used."
  old<-paste("# Review","## Methods","### Statistical analysis",section,"## Results","Bound results.",sep="\n\n")
  rev<-list(text="We used random-effects models.",original_section_sha256=web_c2_hash(section))
  rev$section_sha256<-web_c2_hash(rev$text)
  updated<-web_revision_apply(old,rev)
  stopifnot(identical(web_revision_markdown(updated)$section,rev$text),
    identical(web_revision_markdown(updated)$after,web_revision_markdown(old)$after),
    inherits(tryCatch(web_revision_apply(old,utils::modifyList(rev,list(original_section_sha256=sha))),error=identity),"srma_web_error"))
  report<-list(status="PASS",records=list(a=list(field="model",text=section)))
  part<-list(fingerprint=sha,original_section=section,section=rev$text,section_sha256=rev$section_sha256,
    unchanged_sections_sha256=sha,results="Bound results.")
  inputs<-list(protocol=web_r40_item("PASS","Protocol"),analyses=web_r40_item("PASS","Analyses"),
    qc_policy=web_r40_item("PASS","Policy",list(sha256=sha,r40_script_sha256=sha)),
    statistical_methods=web_r40_item("PASS","Statistical evidence",report))
  base<-list(binding=list(inputs=inputs),sha256=sha,ready=TRUE,execution=list(ready=TRUE,scope=list(),sha256=sha))
  make_plan<-function(input)web_r40_validation_plan(settings,input,list(profiles=list("CORE","PAIRWISE_META")),list(),list())
  plan0<-make_plan(base)
  parent<-list(r40_validation_plan=plan0,r40_input_contract=base,r40_rule_matrix=plan0$rules,r40=list(list(id="approved")))
  preserved<-c("ARTIFACT_COVERAGE","NUMERIC_CLAIMS","CITATION_INTEGRITY","PAIRWISE_ESTIMATES_CI")
  for(id in c(preserved,"STATISTICAL_METHODS_REPORTING"))parent$r40_rule_matrix[[id]]$result<-"PASS"
  base$binding$inputs$revision<-web_r40_item("PASS","Derived archive",part)
  plan<-web_revision_selective_plan(settings,make_plan(base),base,parent)
  stopifnot(all(vapply(plan$rules[preserved],function(r)identical(r$execution,"REUSED"),logical(1))),
    identical(plan$rules$STATISTICAL_METHODS_REPORTING$result,"NOT_ASSESSED"),
    !is.null(plan$execution_scope$revision_sha256))
  p<-web_r40_apply_input_contract(list(blockers=list(),finding_sha256=sha),base,plan)
  stopifnot(inherits(tryCatch(web_c3_require_submission(p),error=identity),"srma_web_error"))
  bad<-parent;bad$r40_validation_plan$binding$web_sha256<-sha
  rejected<-web_revision_selective_plan(settings,make_plan(base),base,bad)
  stopifnot(identical(rejected$rules$NUMERIC_CLAIMS$result,"NOT_ASSESSED"),
    identical(rejected$rules$ARTIFACT_COVERAGE$result,"NOT_ASSESSED"))
  tests<-list(checks=list(list(id="STATISTICAL_METHODS_REPORTING",result="FAIL")))
  stopifnot(identical(web_revision_semantic_results(tests,plan,"We used fixed-effect models.",section)$STATISTICAL_METHODS_REPORTING$result,"FAIL"),
    !length(web_revision_semantic_results(list(checks=list(list(id="STATISTICAL_METHODS_REPORTING",result="PASS"))),
      plan,rev$text,section)))

  # Use the real SQLite registry, transaction and immutable snapshot writer.
  # The fixture substitutes only engine inspection and its registered baseline.
  s<-web_settings(data_root=tempfile("manuscript-revision-db-"))
  con<-web_db(s);web_migrate(con,s);web_local_identity(con)
  policy<-list(sha256=sha,text="Operator fixture policy")
  calls<-0L
  fake_provider<-function(body) {
    calls<<-calls+1L
    checks<-lapply(plan$revision$pending,function(id)list(id=id,result="PASS",unsupported_claims=list(),
      manuscript_quote=rev$text,evidence_quote=section,witnesses=list(list(id="a",result="PASS",
        manuscript_quote=rev$text,evidence_quote=section))))
    list(model="fixture-model",provider_id="fixture-reply",text=as.character(web_json(list(checks=checks))),
      usage=list(input_tokens=11L,cached_input_tokens=0L,output_tokens=10L))
  }
  worker<-function()web_revision_worker_rules(s,plan,base,policy,function()TRUE,fake_provider,"MOCK")
  stopifnot(identical(worker()$state,"AI_APPROVAL_REQUIRED"),calls==0L)
  web_r40_ai_grant(s,plan$execution_scope,list(operator="operator",model="fixture-model",budget_microusd=0L,
    max_input_tokens=90000L,max_output_tokens=3000L,input_microusd_per_million=0L,
    cached_input_microusd_per_million=0L,output_microusd_per_million=0L,expires_at=floor(as.numeric(Sys.time()))+600),"MOCK")
  checked<-worker()
  stopifnot(identical(checked$rules$STATISTICAL_METHODS_REPORTING$result,"PASS"),calls==1L,
    identical(worker()$call$reused,TRUE),calls==1L)
  db<-function(sql)DBI::dbExecute(con,sql)
  db("INSERT INTO projects (id,workspace_id,canonical_id,title,created_at) VALUES('p','local-workspace','fixture','Review','time')")
  db("INSERT INTO engine_releases VALUES('engine','commit','sha','{}','time')")
  db("INSERT INTO project_versions VALUES('e0','p',1,'sha','snapshots/e0','{}','time')")
  db("INSERT INTO project_versions VALUES('e','p',2,'sha','snapshots/e','{}','time')")
  db("INSERT INTO evidence_versions VALUES('e','p','e0','READY',NULL,'local-researcher','time')")
  root<-file.path(s$data_root,"snapshots/v");dir.create(file.path(root,"config/generated"),recursive=TRUE)
  source<-list(route="NARRATIVE",references=list(records=list()),analyses=list())
  x<-list(stage="REVIEW_REQUIRED",manuscript_id="m",source=source,source_sha256=web_c2_hash(source),
    reference_sha256=web_c2_hash(source$references),sections=list(),spec=list(title="Review"),evidence_version="e",analysis_watermark=0L)
  files<-c("manuscript.md","manuscript.docx","citation_map.json")
  stage<-file.path(s$data_root,files)
  for(i in seq_along(stage))writeLines(c(old,"Standalone Writer fixture input","[]")[[i]],stage[[i]])
  ids<-c("markdown","word","map")
  arts<-lapply(seq_along(files),function(i) {
    store<-web_store(s)$put(stage[[i]],web_c2_hash(ids[[i]]))
    list(logical_name=files[[i]],checksum=store$checksum,locator=store$locator,artifact_id=ids[[i]])
  })
  x$docx<-list(version_id="v",job_id="job",artifact_id="word",docx_sha256=arts[[2L]]$checksum)
  web_c2_write(root,x)
  writeLines("project: {project_id: fixture}",file.path(root,"config/project_config.yml"))
  manifest<-list("config/generated/web_manuscript.json"=web_sha(file.path(root,"config/generated/web_manuscript.json")),
    "config/project_config.yml"=web_sha(file.path(root,"config/project_config.yml")))
  web_exec(con,"INSERT INTO project_versions VALUES('v','p',3,'sha','snapshots/v',?m,'time')",m=as.character(web_json(manifest)))
  result<-list(provenance=list(project_version_id="v",source_sha256=x$source_sha256),artifacts=arts)
  web_exec(con,paste("INSERT INTO jobs (id,project_id,version_id,engine_id,operation,idempotency_key,state,message,progress,created_at,updated_at,result_json)",
    "VALUES ('job','p','v','engine','generate_final_manuscript','fixture','COMPLETE','ready',100,'time','time',?result)"),
    result=as.character(web_json(result)))
  for(a in arts)web_exec(con,paste("INSERT INTO artifacts (id,project_id,version_id,job_id,logical_name,artifact_type,version,status,checksum,storage_locator,engine_id,approval_state,created_at)",
    "VALUES (?i,'p','v','job',?n,'manuscript',1,'AVAILABLE',?h,?k,'engine','UNREVIEWED','time')"),
    i=a$artifact_id,n=a$logical_name,h=a$checksum,k=a$locator)
  web_exec(con,"INSERT INTO manuscript_versions VALUES('v','p','m','e','e','REVIEW_REQUIRED',?s,?r,?h,'job','engine','local-researcher','time')",
    s=x$source_sha256,r=x$reference_sha256,h=web_c2_hash(x$spec))
  env<-new.env(parent=globalenv())
  env$web_register_engine<-function(...)"engine"
  env$web_engine_inspect<-function(settings,source_root,configuration_only) {
    inputs<-list.files(source_root,recursive=TRUE)
    list(project_id="fixture",title="Review",inputs=as.list(inputs),manifest=setNames(lapply(
      file.path(source_root,inputs),web_sha),inputs))
  }
  for(k in c("web_import_project","web_c2_save","web_revision_save")) {
    fn<-get(k);environment(fn)<-env;assign(k,fn,env)
  }
  save<-function(text,type="EDITORIAL")env$web_revision_save(con,s,"local-researcher","p","v",web_c2_hash(section),type,text,"Author rationale")
  route<-save("",type="SCIENTIFIC_ANALYSIS")
  stopifnot(identical(route$route,"ANALYSIS_APPROVAL"),identical(route$version_id,"e"))
  first<-save(rev$text)
  second<-save(rev$text)
  stale<-tryCatch(save("A concurrent revision."),error=identity)
  child<-web_c2_context(con,s,"local-researcher","p",first$version_id)$packet
  stopifnot(!identical(first$version_id,"v"),identical(first,second),identical(stale$code,"STALE_UPSTREAM_VERSION"),
    identical(child$source,x$source),identical(child$revision$origin$docx,x$docx),is.null(child$docx),
    identical(child$revision$reviewer,"local-researcher"),identical(child$revision$section_sha256,rev$section_sha256),
    nrow(web_query(con,"SELECT * FROM audit_events WHERE event_type='MANUSCRIPT_SECTION_REVISED'"))==1L,
    identical(web_c2_context(con,s,"local-researcher","p","v")$packet$docx,x$docx))

  # Register a bounded Worker result in the isolated database, then exercise
  # the real accept transaction and the R/39 derivation verifier.
  completed<-file.path(s$data_root,"completed");dir.create(completed)
  new_files<-file.path(completed,files)
  writeLines(updated,new_files[[1L]]);writeLines("Revised Writer fixture output",new_files[[2L]])
  file.copy(stage[[3L]],new_files[[3L]])
  revised_proof<-list(fingerprint=child$revision$fingerprint,section_sha256=child$revision$section_sha256,
    markdown_sha256=web_sha(new_files[[1L]]))
  draft_id<-first$version_id
  worker_result<-list(version_id=draft_id,job_id="rendered",source_sha256=child$source_sha256,
    reference_sha256=child$reference_sha256,evidence_version="e",content_sha256=web_c2_hash(child$sections),
    docx_sha256=web_sha(new_files[[2L]]),revision=revised_proof)
  jsonlite::write_json(worker_result,file.path(completed,"result.json"),auto_unbox=TRUE)
  assets<-lapply(c(files,"result.json"),function(name) {
    id<-paste0("revised-",name)
    registered<-web_store(s)$put(file.path(completed,name),web_c2_hash(id))
    list(artifact_id=id,logical_name=name,checksum=registered$checksum,storage_locator=registered$locator,
      relative_path=paste0("results/web_manuscript/",name))
  })
  job_result<-list(provenance=list(project_version_id=draft_id,source_sha256=child$source_sha256),artifacts=assets)
  web_exec(con,paste("INSERT INTO jobs (id,project_id,version_id,engine_id,operation,idempotency_key,state,message,progress,created_at,updated_at,result_json)",
    "VALUES ('rendered','p',?v,'engine','generate_manuscript_docx','revision','COMPLETE','ready',100,'time','time',?result)"),
    v=draft_id,result=as.character(web_json(job_result)))
  for(a in assets)web_exec(con,paste("INSERT INTO artifacts (id,project_id,version_id,job_id,logical_name,artifact_type,version,status,checksum,storage_locator,engine_id,approval_state,created_at)",
    "VALUES (?i,'p',?v,'rendered',?n,'manuscript',2,'AVAILABLE',?h,?k,'engine','UNREVIEWED','time')"),
    i=a$artifact_id,v=draft_id,n=a$logical_name,h=a$checksum,k=a$storage_locator)
  accept<-web_c2_accept;environment(accept)<-env
  made<-accept(con,s,"local-researcher","p",draft_id,"rendered")
  stopifnot(identical(accept(con,s,"local-researcher","p",draft_id,"rendered"),made))
  registered<-web_c2_context(con,s,"local-researcher","p",made$version_id)$packet
  bound<-web_revision_binding(con,s,"local-researcher","p",registered)
  stopifnot(identical(registered$stage,"REVIEW_REQUIRED"),identical(bound$origin_version,"v"),
    identical(bound$origin_docx$docx_sha256,x$docx$docx_sha256),
    identical(bound$revised_archive[["manuscript.md"]]$sha256,revised_proof$markdown_sha256),
    identical(bound$reference_sha256,x$reference_sha256))
  DBI::dbDisconnect(con)

  # Reuse the live server's automatic Word acceptance and download binding.
  source("web/ui/manuscript.R",encoding="UTF-8")
  ui_draft<-list(stage="REVIEW_REQUIRED",docx=list(artifact_id="new-word"),current=TRUE,
    revision=list(type="EDITORIAL"),revision_editor=list(text=rev$text,sha256=rev$section_sha256))
  view<-list(versions=data.frame(id=c("old","draft","new"),version=1:3),
    jobs=data.frame(id="job",version_id="draft",operation="generate_manuscript_docx",state="COMPLETE",created_at="time"))
  request<-function(action,args) {
    switch(action,view=view,manuscript_view=if(args$version_id=="new")ui_draft else list(),
      manuscript_accept=list(project_id="p",version_id="new"),list())
  }
  shiny::testServer(function(input,output,session) {
    safe<-identity;notice<-function(...)NULL;refresh<-shiny::reactiveVal(0L)
    eval(body(web_c2_server),environment())
  },{
    session$setInputs(project="p",version="old")
    production(list(state="QUEUED",project_id="p",version_id="draft",job_id="job",operation="generate_manuscript_docx"))
    session$elapse(4000L)
    stopifnot(identical(production()$state,"COMPLETE"),identical(download_source()$artifact_id,"new-word"),
      identical(download_source()$version_id,"new"),grepl("review DOCX",output$manuscript_production_download$html,fixed=TRUE))
  })
  cat("Revision scope, SQLite version and audit, same-screen download, rule reuse and fail-closed R/40 PASS\n")
}
