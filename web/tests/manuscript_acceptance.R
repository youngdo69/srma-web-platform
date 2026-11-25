# Phase C integration. Upstream approvals and semantic verdicts are fixture inputs.
# The database, object store, Worker, editor, R/41 and final QC are exercised here.
argv<-commandArgs(TRUE)
if(length(argv) && argv[[1L]]=="--canonical-child") {
  call<-jsonlite::read_json(argv[[2L]],simplifyVector=FALSE)
  for(module in c("core","evidence","analysis","manuscript","manuscript_render","manuscript_revision","final_qc","final_qc_checks"))
    source(file.path(call$engine_root,"web/R",paste0(module,".R")),encoding="UTF-8")
  if(identical(call$action,"fixture_archive")) {
    where<-call$project_dir
    figure<-file.path(where,"figure.png")
    grDevices::png(figure,width=400,height=300);graphics::plot(1:3,1:3);grDevices::dev.off()
    assets<-list(tables=list(list(title="Table 1. Study report",data=data.frame(Study="A",Report="Recorded"),origin="Fixture evidence")),
      figures=list(list(title="Figure 1. Study flow",path=figure,origin="Fixture evidence")))
    web_c2_source_write(where,call$base,web_c2_source_layout(assets,where))
    web_c2_render_word(web_revision_read_md(file.path(where,"manuscript.md")),call$engine_root,file.path(where,"manuscript.docx"),assets)
    jsonlite::write_json(list(status="COMPLETE"),argv[[3L]],auto_unbox=TRUE)
    quit(save="no")
  }
  if(identical(call$action,"inspect_fixture_docx")) {
    word<-call$path
    entries<-unzip(word,list=TRUE)$Name
    read_part<-function(part) {
      handle<-unz(word,part,open="rb");on.exit(close(handle))
      xml2::read_xml(handle,options="NONET")
    }
    doc<-read_part("word/document.xml");ns<-xml2::xml_ns(doc)
    body<-xml2::xml_find_first(doc,"/w:document/w:body",ns)
    text_nodes<-function(p)paste(xml2::xml_text(xml2::xml_find_all(p,".//w:t",ns)),collapse="")
    paras<-vapply(xml2::xml_find_all(body,"./w:p",ns),text_nodes,character(1))
    required<-c("Abstract","Introduction","Methods","Results","Discussion","References","Tables","Figures")
    stopifnot(all(required %in% paras),!is.unsorted(match(required,paras)),
      all(vapply(c("Background:","Methods:","Results:","Conclusions:"),function(label)any(startsWith(paras,label)),logical(1))),
      all(vapply(required,function(k)sum(paras==k)==1L,logical(1))),
      !any(grepl("SRMA_|R/40|canonical_source_sha256",paras)),
      all(vapply(strsplit(call$text,"\n\n",fixed=TRUE)[[1L]],function(p)p %in% paras,logical(1))))
    margins<-xml2::xml_find_first(body,"./w:sectPr/w:pgMar",ns)
    page<-xml2::xml_find_first(body,"./w:sectPr/w:pgSz",ns)
    twips<-function(node,name)as.numeric(xml2::xml_attr(node,paste0("w:",name),ns=ns))
    width<-twips(page,"w")-twips(margins,"left")-twips(margins,"right")
    cols<-xml2::xml_find_all(body,"./w:tbl/w:tblGrid/w:gridCol",ns)
    stopifnot(is.finite(width),length(cols)>0L,sum(twips(cols,"w"))<=width)
    figure_entries<-grep("^word/media/.*[.]png$",entries,value=TRUE)
    stopifnot(length(figure_entries)==1L)
    img<-unzip(word,files=figure_entries,exdir=tempdir())
    stopifnot(identical(web_sha(img),call$figure_sha))
    jsonlite::write_json(list(status="PASS"),argv[[3L]],auto_unbox=TRUE)
    quit(save="no")
  }
  action<-switch(call$action,generate_manuscript_docx="manuscript",run_final_qc="final_qc",stop("Unsupported fixture action"))
  source(file.path(call$engine_root,"web/adapter",paste0(action,".R")),encoding="UTF-8")
  result<-if(action=="manuscript")web_c2_canonical(call,NULL,NULL,call$project_dir,list()) else
    web_c3_canonical(call,NULL,NULL,call$project_dir,list())
  jsonlite::write_json(result,argv[[3L]],auto_unbox=TRUE,null="null",digits=NA)
  quit(save="no")
}

source("web/load.R",encoding="UTF-8")
source("web/qc/runtime.R",encoding="UTF-8")
source("web/ui/manuscript.R",encoding="UTF-8")
local({
  s<-web_settings(data_root=tempfile("manuscript-acceptance-"),backend="sqlite")
  on.exit(unlink(s$data_root,recursive=TRUE),add=TRUE)
  stopifnot(identical(s$mode,"development"),nzchar(Sys.getenv("SRMA_SCIENTIFIC_RSCRIPT")),
    dir.exists(Sys.getenv("SRMA_SCIENTIFIC_LIBRARY")))
  con<-web_db(s);on.exit(DBI::dbDisconnect(con),add=TRUE,after=FALSE)
  web_migrate(con,s);web_local_identity(con)
  s$r40_worker_connection<-con
  actor<-"local-researcher";pid<-web_id();eid<-"evidence";original<-"original";review0<-"original-review"
  engine<-web_register_engine(con,s);root<-s$engine_root
  policy<-srma_qc_policy_load(root)
  sql<-function(q,...)web_exec(con,q,...)
  query<-function(q,...)web_query(con,q,...)
  denied<-function(expr)inherits(tryCatch(force(expr),error=identity),"srma_web_error")
  json<-function(x)as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null="null",digits=NA))
  sql("INSERT INTO projects(id,workspace_id,canonical_id,title,created_at) VALUES(?p,'local-workspace','generic-review','Review','time')",p=pid)
  put<-function(where,name,text) {
    p<-file.path(where,name);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE)
    writeLines(text,p,useBytes=TRUE);p
  }
  snapshot<-function(id,n,write) {
    rel<-paste0("snapshots/",id);path<-file.path(s$data_root,rel)
    put(path,"config/project_config.yml",c("project:","  id: generic-review","  title: Review"))
    dir.create(file.path(path,"config/generated"),showWarnings=FALSE)
    write(path)
    files<-list.files(path,recursive=TRUE)
    manifest<-setNames(lapply(file.path(path,files),web_sha),files)
    sql("INSERT INTO project_versions VALUES(?v,?p,?n,?s,?l,?m,'time')",
      v=id,p=pid,n=n,s=web_c2_hash(manifest),l=rel,m=json(manifest))
    manifest
  }
  job<-function(id,vid,op,files=character(),approval="UNREVIEWED",provenance=list(),status="COMPLETE",ids=NULL) {
    result<-web_result(op,"generic-review",id,status,"Fixture upstream input",engine,
      provenance=c(list(project_version_id=vid,R="R version 4.6.1"),provenance))
    if(is.null(ids))ids<-replicate(length(files),web_id())
    result$artifacts<-lapply(seq_along(files),function(i)list(artifact_id=ids[[i]],logical_name=basename(files[[i]]),
      checksum=web_sha(files[[i]]),relative_path=paste0("results/web_manuscript/",basename(files[[i]]))))
    sql(paste("INSERT INTO jobs(id,project_id,version_id,engine_id,operation,idempotency_key,state,message,progress,created_at,updated_at,result_json)",
      "VALUES(?j,?p,?v,?e,?o,?j,?s,'Fixture',100,'time','time',?r)"),j=id,p=pid,v=vid,e=engine,o=op,r=json(result),s=status)
    for(i in seq_along(files)) {
      a<-result$artifacts[[i]];key<-paste0(pid,"/",a$artifact_id)
      web_store(s)$put(files[[i]],key)
      ver<-query("SELECT COALESCE(MAX(version),0)+1 AS n FROM artifacts WHERE project_id=?p AND logical_name=?l",p=pid,l=a$logical_name)$n
      sql(paste("INSERT INTO artifacts VALUES(?a,?p,?v,?j,?l,'manuscript',?n,'AVAILABLE',?h,?k,?e,?state,'time')"),
        a=a$artifact_id,p=pid,v=vid,j=id,l=a$logical_name,n=ver,h=a$checksum,k=key,e=engine,state=approval)
    }
    result
  }
  gate<-function(id,vid,type,operation,status)sql(paste("INSERT INTO gates(id,project_id,workspace_id,version_id,gate_type,required_operation,status,",
    "decision,reviewer_id,decided_at,rationale,created_at) VALUES(?g,?p,'local-workspace',?v,?t,?o,?s,?a,?u,?at,'Fixture','time')"),
    g=id,p=pid,v=vid,t=type,o=operation,s=status,a=if(status=="APPROVED")status else NA_character_,
    u=if(status=="APPROVED")actor else NA_character_,at=if(status=="APPROVED")"time" else NA_character_)
  snapshot("selection",1L,function(...)NULL)
  job("finalize", "selection","finalize_evidence")
  gate("rob","selection","b2_rob","finalize_evidence","APPROVED")
  ev<-list(stage="READY",.synthesis_execution_fresh=TRUE,
    synthesis_plan=list(status="HUMAN_CONFIRMED",decisions=list(list(analysis_group="descriptive",mode="NARRATIVE"))),
    synthesis_execution=list(source_digest=web_c2_hash("frozen evidence"),sets=list(list(analysis_group="descriptive",
      route="NARRATIVE",common_variable="Endpoint",timepoint="Follow-up"))))
  ev_manifest<-snapshot(eid,2L,function(path)put(path,"config/generated/web_evidence.json",json(ev)))
  sql("INSERT INTO evidence_versions VALUES(?v,?p,'selection','READY','finalize',?a,'time')",v=eid,p=pid,a=actor)
  protocol<-list(r40_required=TRUE,statistical_methods=list(narrative=list(
    rationale="Definitions differed between the protocol strata.",comparison_method="We compared directions and precision by stratum.",
    missing_data="PRESERVE_UNREPORTED")))
  # Bind one recorded quantitative execution through its real Analysis PLAN,
  # RESULT, approval gate, and approved snapshot, then check both routes.
  config<-protocol
  config$statistical_methods$missing_data<-"AVAILABLE_VERIFIED_INPUTS"
  config$statistical_methods$significance<-list(alpha=.05,sidedness="TWO_SIDED")
  config$statistical_methods$analyses<-list(pooled=list(effect_direction="INTERVENTION_VS_COMPARATOR",reference_group="Comparator"))
  config$statistical_methods$mixed_synthesis_relationship<-"We pooled eligible estimates and described incompatible outcomes separately."
  model<-web_c1_methods()
  plan_a<-list(group="pooled",outcome="Other endpoint",definition="Prespecified event",timepoint="Follow-up",design="Randomized trials",
    population="Eligible participants",estimate_basis="RAW",effect_measure="RR",role="PRIMARY",methods=model,diagnostics=TRUE,exploratory=FALSE)
  script_name<-"27_run_universal_meta_analysis.R"
  var<-c(.1,.2,.3);weights<-(1/(var+.05))/sum(1/(var+.05))*100
  temporary<-file.path(s$data_root,"execution-fixture")
  diagnostic<-'results/analysis/meta_analysis_diagnostics_summary.csv'
  summary<-put(temporary,"results/analysis/table.csv",c("measure,estimate,lower,upper","RR,0.9,0.6,1.3"))
  forest<-put(temporary,"results/analysis/figure-source.csv",c("measure,estimate,lower,upper,position","RR,0.9,0.6,1.3,summary"))
  diagfile<-put(temporary,diagnostic,c("diagnostic_status,leave_one_out_performed","PASS,Yes"))
  outputs<-setNames(lapply(c(summary,forest,diagfile),web_sha),c("results/analysis/table.csv","results/analysis/figure-source.csv",diagnostic))
  a<-list(analysis_id="recorded-analysis",evidence_version=eid,stage="PLAN",plan=plan_a)
  snapshot("analysis-plan",3L,function(path)web_c1_write(path,a))
  analysis_row<-function(id,parent,stage,job_id=NULL)sql(paste("INSERT INTO analysis_versions VALUES",
    "(?v,?p,'recorded-analysis',?parent,?e,NULL,?s,?h,?j,?a,'time')"),v=id,p=pid,parent=parent,e=eid,s=stage,
    h=web_c1_hash(plan_a),j=if(is.null(job_id))NA_character_ else job_id,a=actor)
  analysis_row("analysis-plan",eid,"PLAN")
  job("analysis-executed","analysis-plan","run_meta_analysis")
  a$result<-list(status="PASS",job_id="analysis-executed",project_version="analysis-plan",evidence_version=eid,
    plan_sha256=web_c1_hash(plan_a),R="R version 4.6.1",packages=list(metafor="4.8-0"),
    canonical_scripts=setNames(list(web_sha(file.path(root,"R",script_name))),script_name),
    summary=list(effect_measure="RR",model_name=model$model,tau2_method=model$estimator,inference_test=model$inference,
      tau2=.05,i2=17,q=1.2,q_df=2,q_p_value=.31),
    study_effects=lapply(seq_along(var),function(i)list(meta_input_type="raw_dichotomous",vi=var[[i]],random_effect_weight_percent=weights[[i]])),
    consistency=list(status="PASS",figure_sha256=web_sha(forest),figure_annotation_source_sha256=web_sha(summary)),output_hashes=outputs)
  a$stage<-"RESULT";snapshot("analysis-result",4L,function(path)web_c1_write(path,a))
  analysis_row("analysis-result","analysis-plan","RESULT","analysis-executed")
  gate("analysis-review","analysis-result","analysis_results","approve_analysis","APPROVED")
  a$stage<-"APPROVED";a$result_review<-list(gate_id="analysis-review")
  snapshot("analysis-approved",5L,function(path) {
    web_c1_write(path,a)
    dir.create(file.path(path,"results/analysis"),recursive=TRUE)
    stopifnot(all(file.copy(c(summary,forest,diagfile),file.path(path,"results/analysis"))))
  })
  analysis_row("analysis-approved","analysis-result","APPROVED","analysis-executed")
  quantitative<-list(evidence_version=eid,source=list(analyses=list("analysis-approved"=a)))
  stat_registry<-function(x,profiles)web_stat_registry_report(con,s,actor,pid,x,ev,profiles,config,srma_qc_adaptive_analysis_ids(con,pid,x))
  pair<-stat_registry(quantitative,"PAIRWISE_META")
  mixed<-stat_registry(quantitative,c("PAIRWISE_META","NARRATIVE_SWIM"))
  narrative_marker<-"without pooling"
  generated<-web_stat_methods_prose(pair)
  stopifnot(identical(pair$status,"PASS"),identical(mixed$status,"PASS"),
    all(vapply(c("RR","Comparator","Random-effects","REML","Knapp-Hartung","Heterogeneity","inverse","0.5","metafor","each study omitted"),
      function(k)grepl(k,generated,fixed=TRUE),logical(1))),
    !grepl("Subgroup|Sensitivity|Egger|without pooling",generated),
    grepl(narrative_marker,web_stat_methods_prose(mixed),fixed=TRUE),"mixed/allocation_criteria" %in% names(mixed$records))
  bad_analysis<-quantitative;bad_analysis$source$analyses[[1L]]$result$summary$model_name<-"Fixed-effects"
  stopifnot(identical(stat_registry(bad_analysis,"PAIRWISE_META")$status,"NOT_ASSESSED"),
    identical(web_stat_report(bad_analysis,ev,"PAIRWISE_META",config,"analysis-approved")$status,"FAIL"))
  source<-list(route="NARRATIVE",project_version="selection",protocol=protocol,references=list(records=list()),analyses=list(),
    narrative=list(source_manifest=ev_manifest))
  methods<-web_stat_report(list(evidence_version=eid,source=source),ev,"NARRATIVE_SWIM",protocol)
  stopifnot(identical(methods$status,"PASS"))
  source$statistical_methods<-methods
  method_text<-web_stat_methods_prose(methods)
  x<-list(stage="REVIEW_REQUIRED",manuscript_id="m",source=source,source_sha256=web_c2_hash(source),
    reference_sha256=web_c2_hash(source$references),sections=list(),spec=list(title="Generic review"),
    evidence_version=eid,analysis_watermark=5L,production=list(qc=list(publication_9cd=list(embedded_figures=1L,scholarly_tables=1L))))
  md<-paste(c("# Generic review","## Abstract","Background: Evidence was reviewed.","Methods: Findings were summarized.",
    "Results: Results varied across strata.","Conclusions: Findings were uncertain.","## Introduction","The protocol defined the question [1].",
    "## Methods","### Statistical analysis",method_text,"## Results","The strata differed in their study reports (Table 1 and Figure 1).",
    "## Discussion","These findings warrant cautious interpretation [1].","## References","1. Registered methodological source."),collapse="\n\n")
  fixture<-file.path(s$data_root,"upstream");dir.create(fixture)
  put(fixture,"manuscript.md",md);put(fixture,"citation_map.json",'[{"pmid":"12345","citation_number":1}]')
  scientific<-function(settings)utils::modifyList(settings,list(rscript=Sys.getenv("SRMA_SCIENTIFIC_RSCRIPT"),
    engine_library=Sys.getenv("SRMA_SCIENTIFIC_LIBRARY"),runtime_acceptance="EXACT_CANONICAL_RUNTIME"))
  child<-function(settings,action,...,base=NULL) {
    stopifnot(identical(settings$rscript,Sys.getenv("SRMA_SCIENTIFIC_RSCRIPT")))
    input<-tempfile("engine-input-",s$data_root);output<-tempfile("engine-output-",s$data_root)
    writeLines(json(c(list(action=action,engine_root=root,base=base),list(...))),input)
    st<-withr::with_envvar(c(R_LIBS=settings$engine_library),system2(settings$rscript,
      shQuote(c("--vanilla",file.path(root,"web/tests/manuscript_acceptance.R"),"--canonical-child",input,output)),stdout=TRUE,stderr=TRUE))
    if(!file.exists(output))stop(paste(st,collapse="\n"))
    jsonlite::read_json(output,simplifyVector=FALSE)
  }
  child(scientific(s),"fixture_archive",project_dir=fixture,base=x)
  names0<-c("manuscript.md","manuscript.docx","citation_map.json","manuscript_source.json","render-figure-1.png")
  original_files<-file.path(fixture,names0)
  archive_ids<-replicate(length(original_files),web_id())
  x$docx<-list(version_id=review0,job_id="archive",artifact_id=archive_ids[[2L]],docx_sha256=web_sha(original_files[[2L]]),
    canonical_source_sha256=web_sha(original_files[[4L]]),content_sha256=web_c2_hash(x$sections))
  snapshot(review0,6L,function(path)web_c2_write(path,x))
  job("archive",review0,"generate_final_manuscript",original_files,provenance=list(source_sha256=x$source_sha256),ids=archive_ids)
  mrow<-function(v,parent,packet)sql("INSERT INTO manuscript_versions VALUES(?v,?p,'m',?parent,?e,?stage,?s,?r,?spec,'archive',?engine,?a,'time')",
    v=v,p=pid,parent=parent,e=eid,stage=packet$stage,s=packet$source_sha256,r=packet$reference_sha256,
    spec=web_c2_hash(packet$spec),engine=engine,a=actor)
  mrow(review0,eid,x)
  gate("author-original",review0,"manuscript_review","approve_manuscript","APPROVED")
  x$stage<-"APPROVED";x$review<-list(gate="author-original",actor=actor)
  snapshot(original,7L,function(path)web_c2_write(path,x));mrow(original,review0,x)

  # Isolate at upstream verification and AI semantics. Never replace the
  # Worker, R/40 receipt reader, approval transactions, or submission predicate.
  env<-new.env(parent=globalenv())
  for(name in c("web_submit","web_import_project","web_revision_save","web_c2_save","web_c2_guard","web_c2_accept",
    "web_c2_approve","web_c2_mutate","web_qc_author_review_ready","web_c3_guard","web_c3_collect","web_c3_start",
    "web_c3_context","web_c3_accept","web_c3_resolve","web_c3_approve","web_c3_view","web_worker_once","srma_run",
    "web_r40_full_preflight","web_r40_full_worker_execute","web_r40_status","web_revision_base_receipt","web_r40_word_runtime")) {
    fn<-get(name);environment(fn)<-env;assign(name,fn,env)
  }
  env$web_engine_inspect<-function(settings,root,configuration_only) {
    files<-list.files(root,recursive=TRUE)
    list(project_id="generic-review",title="Review",inputs=as.list(files),manifest=setNames(lapply(file.path(root,files),web_sha),files))
  }
  env$web_science_settings<-scientific
  force_docx_failure<-FALSE
  env$web_engine_call<-function(settings,request,diagnostic_dir) {
    if(force_docx_failure && identical(request$action,"generate_manuscript_docx"))
      return(web_result(request$action,request$project_id,request$job_id,"FAILED","Fixture render failure",engine,"DOCUMENT_GENERATION_FAILED"))
    do.call(child,c(list(settings),request))
  }
  approved_link<-list(stage="VERIFIED_SOURCE",approval="APPROVED")
  links<-setNames(rep(list(approved_link),4L),c("protocol","prisma","references","rob"))
  exec_input<-web_c2_hash(list(ev_manifest,protocol,methods))
  approval<-function(packet,vid)web_r40_manuscript_review(con,pid,vid,packet)
  plan_for<-function(packet,vid) {
    stopifnot(identical(web_c1_evidence(con,s,actor,pid,eid,current=FALSE)$version$manifest,ev_manifest),
      identical(packet$source_sha256,web_c2_hash(packet$source)))
    lineage<-if(is.list(packet$revision))web_revision_binding(con,s,actor,pid,packet)
    author<-approval(packet,vid)
    item<-function(...)web_r40_item("PASS",...)
    inputs<-list(protocol=item("Approved protocol",list(sha256=web_c2_hash(protocol))),
      study_selection=item("Frozen evidence",ev_manifest),outcome_harmonization=item("Bound outcome"),
      analyses=web_r40_item("NOT_APPLICABLE","Approved narrative plan specifies no pooling"),
      r38=item("Synthetic approved data fixture"),r39=item("Synthetic approved drafting fixture"),
      statistical_methods=item("Frozen Methods records",methods),qc_policy=item("Pinned policy",list(sha256=policy$sha256,
        r40_script_sha256=web_r40_script_sha(root))),human_confirmation=if(is.list(author))item("Author approval",author) else
        web_r40_item("NOT_ASSESSED","Author decision pending"))
    if(is.list(lineage))inputs$revision<-item("Exact edited archive",lineage)
    adaptive<-list(profiles=list("CORE","NARRATIVE_SWIM"),source_sha256=packet$source_sha256,evidence_version=eid,
      protocol_requirements_approved=TRUE,r40_required=TRUE)
    scope<-list(manuscript_version_id=if(is.list(author))author$version_id else vid,docx_sha256=packet$docx$docx_sha256,
      project_id=pid,evidence_version_id=eid)
    execution<-list(ready=TRUE,scope=scope,sha256=web_c2_hash(list(scope,exec_input,lineage)))
    binding<-list(scope=list(manuscript_version_id=vid),inputs=inputs)
    contract<-list(binding=binding,ready=is.list(author),sha256=web_c2_hash(binding),execution=execution)
    plan<-web_r40_validation_plan(s,contract,adaptive,links,list(narrative_final=TRUE),protocol)
    if(is.list(lineage))plan<-web_revision_selective_plan(s,plan,contract,env$web_revision_base_receipt(con,s,actor,pid,original))
    list(plan=plan,contract=contract,adaptive=adaptive)
  }
  baseline<-plan_for(x,original)
  proof_file<-put(fixture,"baseline.json",json(list(project=pid,version=original,rule_fingerprint=baseline$plan$fingerprint)))
  # This fixture supplies approved pre-existing R/40 evidence to the real reuse matcher.
  proof_ref<-job("baseline",original,"validate_bound_r40",proof_file,"APPROVED")$artifacts[[1L]]
  previous<-list(id=proof_ref$artifact_id,version=original,sha256=proof_ref$checksum,status="PASS",family="r40",
    docx_sha256=x$docx$docx_sha256,input_contract_sha256=baseline$contract$sha256,
    validation_fingerprint=baseline$plan$fingerprint,validation_ruleset=baseline$plan$binding$ruleset)
  pending<-Filter(function(r)r$applicability=="REQUIRED" && r$result!="PASS",baseline$plan$rules)
  previous$validation_checks<-lapply(pending,function(r)list(result="PASS",source_ref=r$id,artifact_id=proof_ref$artifact_id,sha256=proof_ref$checksum))
  previous$validation_checks$STATISTICAL_METHODS_REPORTING$records_sha256<-web_stat_records_digest(methods$records)
  previous$validation_checks$STATISTICAL_METHODS_REPORTING$record_ids<-as.list(names(methods$records))
  env$web_c3_submission<-function(con,settings,actor,project_id,version_id,x) {
    f<-plan_for(x,version_id)
    reusable<-if(identical(version_id,original))web_r40_reuse_matrix(f$plan,list(previous),function(row) {
      ar<-web_c3_artifact(con,s,actor,pid,row$artifact_id,row$sha256,"baseline")
      identical(ar$row$version_id,original)
    }) else web_r40_execution_proof(con,s,actor,pid,f$plan,f$contract)
    snapshot<-list(links=links,adaptive=f$adaptive,meta=list(narrative_final=TRUE,approved_artifacts=if(is.list(reusable))list(reusable$receipt)))
    finding<-list(status="PASS",ai=list(checks=list(list(rule_id="FIXTURE-01",status="PASS"))))
    web_r40_apply_input_contract(srma_qc_submission_evaluate(x,finding,snapshot),f$contract,f$plan,reusable)
  }
  # The simulated semantic reply is scoped to this disposable DB. Real MOCK
  # mode is separately rejected before the accepted receipt can be consumed.
  verdict<-"PASS";run_mode<-"LIVE"
  env$web_revision_worker_rules<-function(settings,plan,contract,policy,check_lease) {
    stopifnot(check_lease(),plan$revision$selective)
    part<-contract$binding$inputs$revision$binding
    rows<-plan$rules
    for(id in c(plan$revision$pending,"IDENTIFIER_INTEGRITY")) {
      rows[[id]]$result<-if(id=="STATISTICAL_METHODS_REPORTING")verdict else "PASS"
      rows[[id]]$execution<-"EXECUTED"
      rows[[id]]$source<-list(section_sha256=part$section_sha256,
        check=list(records_sha256=web_stat_records_digest(plan$statistical_records)))
    }
    list(state="COMPLETE",mode=run_mode,rules=rows)
  }
  env$web_r40_check_word<-function(x,settings,con,actor,pid,work) {
    check<-web_r40_check_word;environment(check)<-env
    check(x,settings,con,actor,pid,work)
  }
  stopifnot(env$web_c3_submission(con,s,actor,pid,original,x)$r40_rule_matrix$NUMERIC_CLAIMS$execution=="REUSED")

  deny_receipt<-function(job_record,change,expect,approval_attempt) {
    registered<-web_parse(job_record$result_json)
    old<-registered$artifacts[[1L]]
    path<-web_c3_artifact(con,s,actor,pid,old$artifact_id,old$checksum,registered$job_id)$path
    inspection<-jsonlite::read_json(path,simplifyVector=FALSE)
    changed<-change(registered,inspection)
    file<-tempfile("negative-",s$data_root);writeLines(json(changed$inspection),file,useBytes=TRUE)
    id<-web_id();loc<-web_store(s)$put(file,paste0(pid,"/",id))
    DBI::dbBegin(con);on.exit(DBI::dbRollback(con),add=TRUE)
    sql(paste("INSERT INTO artifacts SELECT ?a,project_id,version_id,job_id,logical_name,artifact_type,",
      "(SELECT MAX(version)+1 FROM artifacts WHERE project_id=?p AND logical_name=?n),status,?h,?k,engine_id,approval_state,created_at",
      "FROM artifacts WHERE id=?i"),a=id,p=pid,n=old$logical_name,h=loc$checksum,k=loc$locator,i=old$artifact_id)
    changed$result$artifacts[[1L]]$artifact_id<-id;changed$result$artifacts[[1L]]$checksum<-loc$checksum
    sql("UPDATE jobs SET result_json=?r WHERE id=?j",r=json(changed$result),j=registered$job_id)
    stopifnot(isTRUE(expect()),denied(approval_attempt()))
  }
  replace_check<-function(status)function(r,z) {
    z$rules$STATISTICAL_METHODS_REPORTING$result<-status
    # Even an inconsistent outer PASS label cannot waive a failed rule.
    list(result=r,inspection=z)
  }

  invoke<-function(action,args) {
    fn<-switch(action,view=web_view,download=web_download,manuscript_view=web_c2_view,
      manuscript_revision_save=env$web_revision_save,submit=env$web_submit,manuscript_accept=env$web_c2_accept,
      manuscript_author_ready=env$web_qc_author_review_ready,manuscript_approve=env$web_c2_approve,r40_status=env$web_r40_status)
    if(is.null(fn))return(NULL)
    do.call(fn,c(list(con=con,actor=actor),if(action!="view")list(settings=s),args))
  }
  revised<-paste("We structured synthesis by the protocol strata.",method_text,sep="\n\n")
  result<-new.env()
  shiny::testServer(function(input,output,session) {
    safe<-identity;notice<-function(...)NULL;refresh<-shiny::reactiveVal(0L);request<-invoke
    eval(body(web_c2_server),environment())
  },{
    session$setInputs(page="Manuscript",project=pid,version=original)
    stopifnot(identical(packet()$stage,"APPROVED"))
    session$setInputs(manuscript_revision_section="Statistical Analysis",manuscript_revision_type="EDITORIAL",
      manuscript_revision_text_Statistical_Analysis=revised,manuscript_revision_reason="Clarify the structure.",manuscript_revision_preview=1)
    stopifnot(identical(revision_preview()$text,revised))
    session$setInputs(manuscript_revision_confirm=1)
    draft<-production()$version_id
    queued<-production()$job_id
    force_docx_failure<<-TRUE;env$web_worker_once(con,s);force_docx_failure<<-FALSE
    stopifnot(denied(env$web_c2_accept(con,s,actor,pid,draft,queued)),
      nrow(query("SELECT id FROM artifacts WHERE job_id=?j",j=queued))==0L)
    # Submit a fresh attempt with the same immutable draft, after the failure.
    retry<-env$web_submit(con,s,actor,pid,draft,"generate_manuscript_docx","fixture-word-retry")
    run<-production();run$job_id<-retry;production(run)
    env$web_worker_once(con,s)
    j<-query("SELECT state,result_json FROM jobs WHERE id=?j",j=retry)
    if(j$state!="COMPLETE")stop(web_parse(j$result_json)$technical$diagnostic)
    session$elapse(4000L)
    review<-packet_version();stopifnot(!identical(review,original),!identical(review,draft),
      identical(download_source()$version_id,review),!download_source()$approved)
    session$setInputs(version=review)
    result$review<-review;result$draft<-draft
    copied<-web_c2_context(con,s,actor,pid,review)$packet
    bound<-web_revision_binding(con,s,actor,pid,copied)
    stopifnot(identical(bound$origin_version,original),identical(bound$section,revised),
      identical(bound$evidence_version,eid),identical(bound$source_sha256,x$source_sha256),
      identical(web_c2_hash(copied$source$statistical_methods$records),web_c2_hash(methods$records)),
      denied(env$web_c2_approve(con,s,actor,pid,review,TRUE,"Required checks are missing")))
    f<-plan_for(copied,review)
    j40<-env$web_submit(con,s,actor,pid,review,web_r40_full_operation(),web_r40_full_key(f$plan))
    env$web_worker_once(con,s)
    state40<-query("SELECT state,result_json FROM jobs WHERE id=?j",j=j40)
    if(state40$state!="COMPLETE")stop(web_parse(state40$result_json)$technical$diagnostic)
    complete<-env$web_c3_submission(con,s,actor,pid,review,copied)
    stopifnot(complete$r40_integration$R40_VALIDATION_COMPLETE,denied(web_c3_require_submission(complete)),
      identical(complete$r40_rule_matrix$INPUT_ANALYSES$applicability,"NOT_APPLICABLE"),
      identical(complete$r40_rule_matrix$NUMERIC_CLAIMS$execution,"REUSED"))
    expect_blocked<-function()!env$web_qc_author_review_ready(con,s,actor,pid,review)
    try_approve<-function()env$web_c2_approve(con,s,actor,pid,review,TRUE,"Negative fixture")
    for(change in list(replace_check("NOT_ASSESSED"),replace_check("FAIL"),
      function(r,z){z$native$mode<-"MOCK";list(result=r,inspection=z)},
      function(r,z){z$execution_inputs_sha256<-web_c2_hash("other inputs");list(result=r,inspection=z)},
      function(r,z){z$rules$DOCUMENT_STRUCTURE$source$docx_sha256<-web_c2_hash("other Word");list(result=r,inspection=z)},
      function(r,z){r$provenance$project_version_id<-original;list(result=r,inspection=z)}))
      deny_receipt(state40,change,expect_blocked,try_approve)
    session$setInputs(manuscript_confirm=TRUE,manuscript_review_reason="Read the fixture document and provenance.",manuscript_approve=1)
    result$approved<-packet_version()
    # Browser has not yet acknowledged the programmatic selector update.
    d<-download_source();stopifnot(d$approved,identical(d$version_id,result$approved),identical(as.character(input$version),review))
    result$docx<-d$artifact_id
    result$download<-file.path(s$data_root,"approved-download.docx")
    stopifnot(grepl("author-approved DOCX",output$manuscript_production_download$html,fixed=TRUE),
      file.copy(output$manuscript_download,result$download))
  })
  approved<-web_c2_context(con,s,actor,pid,result$approved)$packet
  stopifnot(identical(approved$stage,"APPROVED"),identical(web_sha(result$download),approved$docx$docx_sha256),
    identical(approval(approved,result$approved)$version_id,result$review))
  preview<-Sys.getenv("SRMA_TEST_WORD_PREVIEW","")
  if(nzchar(preview)) {
    preview<-web_inside(preview,file.path(root,"tmp"),exists=FALSE)
    stopifnot(endsWith(preview,".docx"),!file.exists(preview),file.copy(result$download,preview))
  }
  stopifnot(identical(child(scientific(s),"inspect_fixture_docx",path=result$download,text=revised,
    figure_sha=web_sha(original_files[[5L]]))$status,"PASS"))
  submission<-env$web_c3_submission(con,s,actor,pid,result$approved,approved)
  web_c3_require_submission(submission)
  stopifnot(all(vapply(Filter(function(r)r$applicability=="REQUIRED",submission$r40_rule_matrix),function(r)r$result=="PASS",logical(1))))
  qc<-env$web_c3_start(con,s,actor,pid,result$approved)
  env$web_worker_once(con,s)
  qjob<-query("SELECT state,result_json FROM jobs WHERE operation='run_final_qc'")
  if(qjob$state!="COMPLETE")stop(web_parse(qjob$result_json)$technical$diagnostic)
  qc<-env$web_c3_accept(con,s,actor,pid,qc$qc_id)
  q<-env$web_c3_view(con,s,actor,pid,qc$qc_id)
  stopifnot(isTRUE(q$result$machine_pass),isTRUE(q$result$canonical_sources),denied(env$web_c3_approve(con,s,actor,pid,qc$qc_id,TRUE,"Pending visual disposition")))
  for(f in q$result$findings)qc<-env$web_c3_resolve(con,s,actor,pid,qc$qc_id,f$id,"CONFIRMED","Checked in the isolated fixture")
  qc<-env$web_c3_approve(con,s,actor,pid,qc$qc_id,TRUE,"Approved the frozen fixture document")
  final<-env$web_c3_view(con,s,actor,pid,qc$qc_id)
  stopifnot(final$eligibility$SCIENTIFIC_PUBLICATION_QC_CLEARED,!final$eligibility$RELEASE_ELIGIBLE)
  lock<-web_sha(file.path(root,"renv.lock"));acceptance_json<-json(list(web_sha256=web_c3_code_hash(s)))
  for(kind in c("CANONICAL_RUNTIME","POSTGRESQL"))
    sql("INSERT INTO release_acceptances VALUES(?i,?e,?k,?h,?x,'time')",i=web_id(),e=engine,k=kind,h=lock,x=acceptance_json)
  simulated_deployment<-s;simulated_deployment$backend<-"postgres"
  stopifnot(env$web_c3_view(con,simulated_deployment,actor,pid,qc$qc_id)$eligibility$RELEASE_ELIGIBLE)
  n<-nrow(query("SELECT version_id FROM manuscript_versions"))
  next_edit<-function(text)env$web_revision_save(con,s,actor,pid,result$approved,web_c2_hash(revised),"EDITORIAL",text,"Later revision")
  stopifnot(denied(next_edit(sub("[1]","[2]",paste0(revised," [1]"),fixed=TRUE))),
    denied(next_edit(paste0(revised,"\n\nUnsupported *emphasis* or $x=y$."))),
    nrow(query("SELECT version_id FROM manuscript_versions"))==n)
  latest<-next_edit(paste("A new editorial revision awaits verification.",revised,sep="\n\n"))
  historical<-env$web_c3_view(con,simulated_deployment,actor,pid,qc$qc_id)
  stopifnot(identical(historical$effective_status,"INVALIDATED"),!historical$eligibility$RELEASE_ELIGIBLE,
    is.null(web_c2_context(con,s,actor,pid,latest$version_id)$packet$docx),
    identical(web_sha(result$download),approved$docx$docx_sha256),
    identical(web_c2_context(con,s,actor,pid,original)$packet$docx$docx_sha256,x$docx$docx_sha256),
    nrow(query("SELECT id FROM audit_events WHERE event_type='FINAL_QC_INVALIDATED'"))==1L)
  cat("End-to-end registry, Word Worker, scoped R/40, Shiny approval and exact approved download PASS\n")
})
