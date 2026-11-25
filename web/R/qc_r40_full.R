web_r40_full_operation <- function() "validate_bound_r40"
web_r40_full_ruleset <- function() "r40-submission-matrix-v2"
web_r40_full_artifact <- function() "r40_bound_inspection.json"

web_r40_rule <- function(id, basis, outcome="NOT_ASSESSED", execution="BLOCKED", source=NULL,
                         applicability="REQUIRED") {
  list(id=id,applicability=applicability,result=outcome,execution=execution,basis=basis,source=source)
}

web_r40_validation_plan <- function(settings,contract,adaptive,links,meta,protocol=NULL,analyses=NULL) {
  inputs<-contract$binding$inputs
  rules<-lapply(names(inputs),function(k) {
    a<-inputs[[k]]
    verified<-a$status %in% c("PASS","NOT_APPLICABLE")
    web_r40_rule(paste0("INPUT_",toupper(k)),a$reason,
      if(a$status %in% c("PASS","FAIL"))a$status else "NOT_ASSESSED",
      if(verified)"REUSED" else if(identical(a$status,"FAIL"))"EXECUTED" else "BLOCKED",a$binding,
      if(identical(a$status,"NOT_APPLICABLE"))"NOT_APPLICABLE" else "REQUIRED")
  })
  link_rule<-function(id,k,reason,conflict=FALSE) {
    a<-links[[k]]
    valid<-identical(a$stage,"VERIFIED_SOURCE") && identical(a$approval,"APPROVED")
    web_r40_rule(id,reason,if(conflict)"FAIL" else if(valid)"PASS" else "NOT_ASSESSED",
      if(conflict)"EXECUTED" else if(valid)"REUSED" else "BLOCKED",a)
  }
  methods<-setdiff(unlist(adaptive$profiles,use.names=FALSE),"CORE")
  no_pool<-identical(methods,"NARRATIVE_SWIM") && identical(inputs$analyses$status,"NOT_APPLICABLE")
  supported<-length(methods)>0L && all(methods %in% c("NARRATIVE_SWIM","PAIRWISE_META"))
  rules<-c(rules,list(
    web_r40_rule("SYNTHESIS_ROUTE","Use the synthesis route selected by the approved Protocol and frozen evidence.",
      if(supported && identical(inputs$protocol$status,"PASS"))"PASS" else "NOT_ASSESSED",
      if(supported && identical(inputs$protocol$status,"PASS"))"REUSED" else "BLOCKED",adaptive$methods),
    link_rule("PRISMA_SELECTION","prisma","The approved PRISMA flow must match the Human-finalized selection.",
      isTRUE(meta$prisma_conflict) || isTRUE(meta$selection_conflict)),
    link_rule("REFERENCE_APPROVAL","references","Reuse only approval of the exact reference set.")))
  protocol_bound<-identical(inputs$protocol$status,"PASS")
  exempt<-function(item)is.list(item) && identical(item$applicable,FALSE) && nzchar(srma_qc_one(item$rationale))
  add_approval<-function(id,link,reason,applicability) {
    a<-link_rule(id,link,reason)
    a$applicability<-applicability
    if(applicability=="NOT_APPLICABLE") {
      a$result<-"NOT_ASSESSED";a$execution<-"REUSED";a$source<-inputs$protocol$binding
    }
    a
  }
  rob<-if(protocol_bound && exempt(protocol$risk_of_bias))"NOT_APPLICABLE" else "REQUIRED"
  grade<-if(protocol_bound && exempt(protocol$certainty))"NOT_APPLICABLE" else
    if(!protocol_bound || isTRUE(adaptive$certainty_required) || isTRUE(protocol$certainty) ||
       is.list(protocol$certainty) && (isTRUE(protocol$certainty$required) || nzchar(srma_qc_one(protocol$certainty$method))))
      "REQUIRED" else "OPTIONAL"
  rules<-c(rules,list(add_approval("ROB_APPROVAL","rob","Use the study-design-specific RoB assessment required by Protocol.",rob),
    add_approval("GRADE_APPROVAL","certainty","Respect the Protocol's certainty-of-evidence requirements.",grade)))
  cli<-c(STUDY_AND_PARTICIPANT_COUNTS="Match manuscript study and participant counts to the frozen evidence.",
    METHODS_PROTOCOL="Compare reported Methods with the approved Protocol and executed procedures.",
    NUMERIC_CLAIMS="Run canonical R/40 claim mapping and numeric comparison.",
    CITATION_INTEGRITY="Check manuscript citations against the frozen citation map and literature set.",
    ARTIFACT_COVERAGE="Check every required R/38 table or figure against R/39 coverage.",
    IDENTIFIER_INTEGRITY="Check the submission prose for internal identifiers.",
    DOCUMENT_STRUCTURE="Verify the exact editable Word archive has its required sections and registered tables and figures.",
    CROSS_SECTION_SEMANTICS="Check for contradictions and unsupported interpretations across sections.")
  rules<-c(rules,lapply(names(cli),function(id)web_r40_rule(id,cli[[id]])))
  if(!is.null(inputs$statistical_methods))rules<-c(rules,list(web_r40_rule("STATISTICAL_METHODS_REPORTING",
    "Verify every required Statistical Analysis description against the approved Protocol and actual execution; check omissions, contradictions, and claims about analyses not performed against Results, Tables and Figures.")))
  if("NARRATIVE_SWIM" %in% methods)rules<-c(rules,list(
    web_r40_rule("NARRATIVE_STUDY_RESULTS","Match reported narrative results against finalized study-level evidence."),
    web_r40_rule("NARRATIVE_INTERPRETATION","Check narrative conclusions against the actual study evidence.")))
  pooling<-c(PAIRWISE_STUDY_COUNTS="Verify the included-study count for each pooled outcome.",
    PAIRWISE_ESTIMATES_CI="Verify the pooled effect, confidence interval, model and heterogeneity.",
    PAIRWISE_ADDITIONAL_ANALYSES="Verify required or performed subgroup and sensitivity analyses.",
    PAIRWISE_ARTIFACT_NUMERICS="Compare numeric values across the manuscript, tables and figures.",
    PAIRWISE_DIRECTION="Check the interpretation and direction of each effect.")
  additional<-protocol_bound && exempt(protocol$additional_analyses) && identical(inputs$analyses$status,"PASS") &&
    !any(vapply(analyses,function(a)a$plan$role %in% c("SUBGROUP","SENSITIVITY") || isTRUE(a$plan$exploratory),logical(1)))
  rules<-c(rules,lapply(names(pooling),function(id) {
    na<-no_pool || (id=="PAIRWISE_ADDITIONAL_ANALYSES" && additional)
    web_r40_rule(id,if(no_pool)inputs$analyses$reason else if(na)protocol$additional_analyses$rationale else pooling[[id]],
      execution=if(na)"REUSED" else "BLOCKED",source=if(na)inputs$analyses$binding,
      applicability=if(na)"NOT_APPLICABLE" else "REQUIRED")
  }))
  if(all(c("PAIRWISE_META","NARRATIVE_SWIM") %in% methods))rules<-c(rules,list(
    web_r40_rule("MIXED_CONSISTENCY","Compare both synthesis strands for incompatible numbers, citations or interpretations.")))
  ids<-vapply(rules,`[[`,character(1),"id")
  rules<-setNames(rules,ids)
  spec<-lapply(rules,function(a)a[c("id","applicability","basis")])
  policy<-inputs$qc_policy$binding
  binding<-list(inputs_sha256=contract$sha256,ruleset=web_r40_full_ruleset(),
    rules_sha256=web_c2_hash(spec),policy_sha256=policy$sha256,profiles=as.list(sort(methods)),
    reporting_approvals_sha256=web_c2_hash(links[c("prisma","references")]),
    canonical_sha256=policy$r40_script_sha256,web_sha256=web_c3_code_hash(settings),
    runtime_lock_sha256=web_sha(file.path(settings$engine_root,"renv.lock")))
  plan<-list(binding=binding,fingerprint=web_c2_hash(binding),supported=supported,
    ready=isTRUE(contract$ready) && supported && srma_qc_link_sha(contract$sha256),rules=rules,
    native_ready=isTRUE(contract$execution$ready) && supported,
    statistical_records=inputs$statistical_methods$binding$records)
  plan$execution_scope<-web_r40_execution_scope(plan,contract)
  plan
}

# Approval was verified by the Canonical Registry reader before entering here.
# Extra or missing rules, an old build, or an unevidenced PASS cannot clear QC.
web_r40_reuse_matrix <- function(plan,receipts,verify_check) {
  if(!isTRUE(plan$ready))return(NULL)
  required<-Filter(function(a)identical(a$applicability,"REQUIRED"),plan$rules)
  needs<-Filter(function(a)!identical(a$result,"PASS"),required)
  if(!length(required) || any(vapply(required,function(a)identical(a$result,"FAIL"),logical(1))))return(NULL)
  direct<-c("PRISMA_SELECTION","REFERENCE_APPROVAL","ROB_APPROVAL","GRADE_APPROVAL","INPUT_STATISTICAL_METHODS")
  if(any(direct %in% names(needs)))return(NULL)
  for(z in receipts) {
    if(!identical(z$status,"PASS") || !identical(z$input_contract_sha256,plan$binding$inputs_sha256) ||
       !identical(z$validation_fingerprint,plan$fingerprint) || !identical(z$validation_ruleset,plan$binding$ruleset))next
    checks<-z$validation_checks
    if(!is.list(checks) || anyDuplicated(names(checks)) || !setequal(names(checks),names(needs)))next
    stat<-checks$STATISTICAL_METHODS_REPORTING
    if(!is.null(stat) && (!identical(stat$records_sha256,web_stat_records_digest(plan$statistical_records)) ||
       !identical(unlist(stat$record_ids,use.names=FALSE),names(plan$statistical_records))))next
    valid<-vapply(checks,function(a)is.list(a) && identical(a$result,"PASS") &&
      nzchar(srma_qc_one(a$source_ref)) && srma_qc_link_sha(a$sha256) &&
      isTRUE(verify_check(a)),logical(1))
    if(!all(valid))next
    rules<-plan$rules
    for(k in names(checks)) {
      rules[[k]]$result<-"PASS"
      rules[[k]]$execution<-"REUSED"
      rules[[k]]$source<-list(receipt_id=z$id,source_version=z$version,receipt_sha256=z$sha256,
        evidence=checks[[k]])
    }
    return(list(rules=rules,receipt=z))
  }
  NULL
}

web_r40_verify_check <- function(sources,vid,a) {
  matches<-Filter(function(z)identical(z$id,a$artifact_id) && identical(z$version,vid) &&
    identical(z$checksum,a$sha256),sources$artifacts)
  length(matches)==1L && isTRUE(tryCatch(identical(web_sha(sources$read_artifact(matches[[1L]])),a$sha256),
    error=function(e)FALSE))
}

web_r40_full_key <- function(plan) paste0("r40-bound-",plan$execution_scope$fingerprint %||b2% plan$fingerprint)

web_r40_full_preflight <- function(con,settings,actor,project_id,version_id) {
  x<-web_r40_candidate(con,settings,actor,project_id,version_id)
  publication<-web_c3_submission(con,settings,actor,project_id,version_id,x)
  plan<-publication$r40_validation_plan
  web_assert(isTRUE(plan$supported) && is.list(plan$execution_scope),
    "A supported synthesis route and frozen inputs must be identified for R/40.","USER_ACTION_REQUIRED")
  list(packet=x,publication=publication)
}

web_r40_full_start <- function(con,settings,actor,project_id,version_id) {
  pre<-web_r40_full_preflight(con,settings,actor,project_id,version_id)
  plan<-pre$publication$r40_validation_plan
  if(!isTRUE(pre$publication$r40_integration$R40_VALIDATION_COMPLETE)) {
    job<-web_submit(con,settings,actor,project_id,version_id,web_r40_full_operation(),idempotency_key=web_r40_full_key(plan))
    prior<-web_r40_full_job(con,settings,actor,project_id,version_id,plan)
    if(identical(prior$execution,"USER_ACTION_REQUIRED") && isTRUE(plan$native_ready) &&
       file.exists(file.path(web_r40_ai_dir(settings,plan$execution_scope$fingerprint),"approval","grant.json")))
      web_retry(con,settings,actor,project_id,job)
  }
  web_r40_status(con,settings,actor,project_id,version_id)
}

web_r40_full_job <- function(con,settings,actor,project_id,version_id,plan) {
  j<-web_query(con,paste0("SELECT id,state,result_json,engine_id FROM jobs WHERE project_id=?p AND version_id=?v ",
    "AND operation=?o AND idempotency_key=?k"),p=project_id,v=version_id,
    o=web_r40_full_operation(),k=web_r40_full_key(plan))
  if(nrow(j)!=1L)return(list(registration="NOT_REGISTERED",execution="NOT_RUN"))
  status<-list(registration="REGISTERED",execution=j$state,job_id=j$id)
  if(!j$state %in% c("COMPLETE","USER_ACTION_REQUIRED"))return(status)
  record<-tryCatch({
    result<-web_parse(j$result_json)
    a<-Filter(function(a)identical(a$logical_name,web_r40_full_artifact()),result$artifacts)
    fingerprint<-plan$execution_scope$fingerprint %||b2% plan$fingerprint
    stopifnot(length(a)==1L,identical(result$status,j$state),identical(result$job_id,j$id),
      identical(result$operation,web_r40_full_operation()),
      identical(result$engine_version,j$engine_id),identical(result$provenance$fingerprint,fingerprint),
      identical(result$provenance$project_version_id,version_id),srma_qc_link_sha(a[[1L]]$checksum))
    a<-web_c3_artifact(con,settings,actor,project_id,a[[1L]]$artifact_id,a[[1L]]$checksum,j$id)
    stopifnot(identical(a$row$version_id,version_id),isTRUE(file.info(a$path)$size<=1e6))
    z<-jsonlite::read_json(a$path,simplifyVector=FALSE)
    stopifnot(identical(z$job_id,j$id),identical(z$fingerprint,fingerprint),identical(z$approval,"UNREVIEWED"),
      z$semantic %in% c("PASS","FAIL","NOT_ASSESSED"),z$validation %in% c("PASS","FAIL","NOT_ASSESSED"))
    z$receipt<-list(id=a$row$id,version=version_id,sha256=a$row$checksum)
    z
  },error=function(e)NULL)
  status$inspection<-record
  status
}

web_r40_inspection_rules <- function(record) {
  checks<-record$deterministic_subset$checks
  lapply(names(checks),function(k) {
    value<-checks[[k]]
    web_r40_rule(paste0("WEB_SUBSET_",toupper(k)),"The existing deterministic Web inspection checked this scoped item.",
      if(value %in% c("PASS","FAIL"))value else "NOT_ASSESSED",
      if(value %in% c("PASS","FAIL"))"EXECUTED" else "BLOCKED",record$receipt,"OPTIONAL")
  })
}

web_r40_r27_files <- function(plan_result,job_result) {
  hashes<-plan_result$output_hashes
  paths<-names(hashes)
  summary<-paths[grepl("^results/meta_analysis/.*/meta_analysis_summary[.]csv$",paths) &
    !grepl("/tables/",paths,fixed=TRUE) & !grepl("(^|/)[.]{1,2}(/|$)",paths)]
  if(length(summary)!=1L)return(NULL)
  selected<-c(summary,paste0(dirname(summary),"/study_effects_used.csv"))
  files<-lapply(selected,function(path) {
    sha<-hashes[[path]]
    if(!srma_qc_link_sha(sha))return(NULL)
    hits<-Filter(function(a)identical(chartr("\\","/",srma_qc_one(a$relative_path)),path) &&
      identical(a$checksum,sha) && nzchar(srma_qc_one(a$artifact_id)),job_result$artifacts)
    if(length(hits)==1L)hits[[1L]]
  })
  if(all(lengths(files)>0L))setNames(files,selected)
}

web_r40_stage_bound_cli <- function(con,settings,actor,project_id,pre,proof,root) {
  inputs<-pre$publication$r40_input_contract$binding$inputs
  copy<-function(path,sha,relative) {
    dest<-web_inside(file.path(root,relative),root,FALSE)
    web_assert(!file.exists(dest) && identical(web_sha(path),sha),"R/40 staging hash or path failed.","ARTIFACT_INTEGRITY_FAILED")
    dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
    web_assert(file.copy(path,dest) && identical(web_sha(dest),sha),"R/40 staging copy failed.","ARTIFACT_INTEGRITY_FAILED")
  }
  config<-web_version_root(con,settings,actor,project_id,inputs$protocol$binding$version)
  path<-"config/project_config.yml"
  copy(web_inside(file.path(config$root,path),config$root),inputs$protocol$binding$sha256,path)
  for(k in c("r38","r39")) {
    set<-inputs[[k]]$binding
    for(path in names(set$artifacts)) {
      a<-set$artifacts[[path]]
      ar<-web_c3_artifact(con,settings,actor,project_id,a$id,a$sha256,set$job)
      web_assert(identical(ar$row$version_id,set$version) && identical(ar$row$approval_state,"APPROVED"),
        "R/40 staged dossier approval changed.","STALE_UPSTREAM_VERSION")
      copy(ar$path,a$sha256,path)
    }
  }
  if(is.list(inputs$revision$binding)) {
    revised<-inputs$revision$binding$revised_archive[["manuscript.md"]]
    a<-web_c3_artifact(con,settings,actor,project_id,revised$id,revised$sha256,pre$packet$docx$job_id)
    web_assert(identical(a$row$version_id,pre$packet$docx$version_id),"R/40 revised Markdown is from another archive.")
    if(!isTRUE(tryCatch(web_r40_stage_revision(root,web_r40_cli_paths(settings$engine_root),a$path,revised$sha256),
      error=function(e)FALSE)))return("REVISED_R39_CITATION_BINDING_REQUIRED")
  }
  pending<-list()
  for(id in names(proof$analyses)) {
    a<-proof$analyses[[id]]
    j<-web_scoped_job(con,actor,project_id,a$version,a$job,"run_meta_analysis")
    result<-web_parse(j$result_json)
    if(!identical(j$state,"COMPLETE") || !identical(result$provenance$plan_sha256,a$plan_sha256) ||
       !identical(result$provenance$project_version_id,a$version))return(FALSE)
    paths<-web_r40_r27_files(pre$packet$source$analyses[[id]]$result,result)
    if(!length(paths) || any(names(paths) %in% names(pending)))return(FALSE)
    for(path in names(paths)) {
      entry<-paths[[path]]
      ar<-web_c3_artifact(con,settings,actor,project_id,entry$artifact_id,entry$checksum,a$job)
      if(!identical(ar$row$version_id,a$version))return(FALSE)
      pending[[path]]<-list(path=ar$path,sha=entry$checksum)
    }
  }
  if(identical(inputs$analyses$status,"PASS") && !length(pending))return(FALSE)
  for(path in names(pending))copy(pending[[path]]$path,pending[[path]]$sha,path)
  invisible(TRUE)
}

web_r40_stage_revision <- function(root,paths,revised,sha) {
  relative<-function(id)web_inside(file.path(root,paths[[id]]),root)
  read<-web_revision_read_md
  original<-read(relative("manuscript")); child<-read(revised)
  tokens<-read(relative("token_manuscript"))
  split_manuscript<-function(text) {
    cut<-regexpr("(?m)^## References[[:blank:]]*$",text,perl=TRUE)[[1L]]
    web_assert(cut>0L,"The staged R/39 bibliography boundary is missing.")
    list(body=trimws(substr(text,1L,cut-1L)),references=trimws(substr(text,cut,nchar(text))))
  }
  mapping<-utils::read.csv(relative("citation_map"),colClasses="character")
  pmid<-mapping$pmid;number<-mapping$citation_number
  web_assert(identical(web_sha(revised),sha) && length(pmid)==length(number) &&
    !anyNA(c(pmid,number)) && all(grepl("^[0-9]+$",c(pmid,number))) &&
    !anyDuplicated(pmid) && !anyDuplicated(number),"The revised native source or citation map is invalid.")
  project<-function(text) {
    for(i in seq_along(pmid))text<-gsub(paste0("[",number[[i]],"]"),
      paste0("[[PMID:",pmid[[i]],"]]"),text,fixed=TRUE)
    text
  }
  old<-split_manuscript(original);new<-split_manuscript(child)
  web_assert(identical(old$references,new$references) && identical(project(old$body),trimws(tokens)) &&
    identical(web_revision_citations(old$body),web_revision_citations(new$body)),
    "The revised numbered text cannot be bound to the frozen R/39 citation tokens.","ARTIFACT_INTEGRITY_FAILED")
  web_assert(file.copy(revised,relative("manuscript"),overwrite=TRUE) && identical(web_sha(relative("manuscript")),sha),
    "The revised manuscript could not be staged.")
  writeLines(project(new$body),relative("token_manuscript"),useBytes=TRUE)
  invisible(TRUE)
}

web_r40_bound_cli <- function(con,settings,actor,project_id,pre,proof,work,check_lease) {
  plan<-pre$publication$r40_validation_plan
  if(!isTRUE(plan$native_ready))return(list(state="CANONICAL_DOSSIER_BINDING_REQUIRED"))
  scientific<-tryCatch(web_science_settings(settings),error=function(e)NULL)
  if(!is.list(scientific) || !identical(scientific$runtime_acceptance,"EXACT_CANONICAL_RUNTIME"))
    return(list(state="CANONICAL_SCIENTIFIC_RUNTIME_REQUIRED",ai_calls=0L))
  web_r40_cli_paths(settings$engine_root)
  root<-tempfile("bound-r40-",tmpdir=work)
  web_assert(dir.create(root,mode="0700"),"Cannot isolate R/40 sources.","SYSTEM_FAILED")
  staged<-web_r40_stage_bound_cli(con,settings,actor,project_id,pre,proof,root)
  if(!isTRUE(staged))return(list(state=if(is.character(staged))staged else "CANONICAL_R27_WORKSPACE_NOT_BOUND",ai_calls=0L))
  policy<-srma_qc_policy_load(settings$engine_root)
  web_r40_bridge_run(scientific,settings$engine_root,root,plan$execution_scope,plan,settings,policy,check_lease)
}

web_r40_word_content <- function(word,markdown,engine,criteria) {
  stopifnot(identical(web_sha(word),criteria$docx_sha256),
    is.null(markdown) || identical(web_sha(markdown),criteria$markdown_sha256))
  inventory<-utils::unzip(word,list=TRUE)
  required<-c("[Content_Types].xml","word/document.xml","word/styles.xml")
  stopifnot(!anyDuplicated(tolower(inventory$Name)),all(required %in% inventory$Name),
    isTRUE(inventory$Length[match("word/document.xml",inventory$Name)]<=20e6))
  handle<-unz(word,"word/document.xml",open="rb");on.exit(close(handle),add=TRUE)
  doc<-xml2::read_xml(handle,options="NONET")
  ns<-c(w="http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  body<-xml2::xml_find_first(doc,"/w:document/w:body",ns)
  headings<-xml2::xml_find_all(body,"./w:p[w:pPr/w:pStyle]",ns)
  labels<-vapply(headings,function(p)paste(xml2::xml_text(xml2::xml_find_all(p,"./w:r/w:t",ns)),collapse=""),character(1))
  required<-c("Abstract","Introduction","Methods","Results","Discussion","References")
  stopifnot(all(vapply(required,function(k)sum(labels==k)==1L,logical(1))),!is.unsorted(match(required,labels)))
  if(!is.null(markdown)) {
    blocks<-web_c2_defs(engine,41)$parse_markdown_blocks(web_revision_read_md(markdown))
    expected<-vapply(blocks,function(b)paste0(if(b$type=="bullet")"\u2022 " else "",b$text),character(1))
    visible<-vapply(xml2::xml_find_all(body,"./w:p",ns),function(p)
      paste(xml2::xml_text(xml2::xml_find_all(p,".//w:t",ns)),collapse=""),character(1))
    normalize<-function(p)trimws(gsub("\u00a0"," ",p,fixed=TRUE))
    visible<-normalize(visible);visible<-visible[nzchar(visible)]
    stopifnot(identical(visible[seq_along(expected)],normalize(expected)))
  }
  assembled<-web_c2_9cd_docx_integrity(word,min_figures=criteria$figures,min_tables=criteria$tables)
  c(list(docx_sha256=criteria$docx_sha256),assembled[c("embedded_figures","scholarly_tables","table_grids")])
}

web_r40_word_runtime <- function(settings,work,word,markdown,criteria) {
  scientific<-tryCatch(web_science_settings(settings),error=function(e)NULL)
  if(!is.list(scientific) || !identical(scientific$runtime_acceptance,"EXACT_CANONICAL_RUNTIME"))return(NULL)
  wd<-tempfile("bound-word-",tmpdir=work)
  if(!dir.create(wd,mode="0700"))return(NULL)
  if(!file.copy(word,file.path(wd,"manuscript.docx")) ||
    (!is.null(markdown) && !file.copy(markdown,file.path(wd,"manuscript.md"))))return(NULL)
  jsonlite::write_json(criteria,file.path(wd,"criteria.json"),auto_unbox=TRUE,null="null")
  vars<-c(R_LIBS=scientific$engine_library)
  withr::with_envvar(vars,{
    launcher<-file.path(settings$engine_root,"web/adapter/r40_bound_preflight.R")
    status<-suppressWarnings(system2(scientific$rscript,
      shQuote(c("--vanilla",launcher,"--word",settings$engine_root,wd)),
      stdout=nullfile(),stderr=nullfile(),timeout=min(60,scientific$timeout)))
    out<-file.path(wd,"inspection.json")
    if(!identical(as.integer(status),0L) || !isTRUE(file.info(out)$size<=16384L))return(NULL)
    response<-jsonlite::read_json(out,simplifyVector=FALSE)
    if(identical(response$state,"FAIL"))return(FALSE)
    if(identical(response$proof$docx_sha256,criteria$docx_sha256))response$proof
  })
}

web_r40_check_word <- function(x,settings,con,actor,pid,work=NULL) {
  id<-"DOCUMENT_STRUCTURE"
  reason<-"Verify the exact editable Word archive has its required sections and registered tables and figures."
  checked<-tryCatch({
    archive<-web_revision_archive(con,settings,actor,pid,x)
    word<-archive[["manuscript.docx"]]
    md<-archive[["manuscript.md"]]
    counts<-x$docx$production_qc$publication_9cd %||b2% x$production$qc$publication_9cd
    layout<-if(is.list(archive[["manuscript_source.json"]]))web_c2_source_read(
      archive[["manuscript_source.json"]]$path,x,md$path,archive[["citation_map.json"]]$path)$layout
    criteria<-list(docx_sha256=word$sha256,markdown_sha256=if(is.list(layout))md$sha256,
      figures=if(isTRUE(layout$publication))length(layout$figures) else counts$embedded_figures %||b2% 0L,
      tables=if(isTRUE(layout$publication))length(layout$tables) else counts$scholarly_tables %||b2% 0L)
    markdown<-if(is.list(layout))md$path
    if(requireNamespace("xml2",quietly=TRUE))web_r40_word_content(word$path,markdown,settings$engine_root,criteria)
    else if(!is.null(work))web_r40_word_runtime(settings,work,word$path,markdown,criteria)
  },error=function(e)FALSE)
  if(is.null(checked))return(web_r40_rule(id,reason))
  web_r40_rule(id,reason,if(is.list(checked))"PASS" else "FAIL","EXECUTED",if(is.list(checked))checked)
}

web_r40_full_worker_execute <- function(settings,canonical_id,version_id,job_id,work,engine_id) {
  attempt<-web_r40_worker_scope(settings,canonical_id,version_id,job_id,work,engine_id,web_r40_full_operation())
  con<-settings$r40_worker_connection
  j<-attempt$row
  pre<-web_r40_full_preflight(con,settings,attempt$actor,j$project_id,version_id)
  publication<-pre$publication
  plan<-publication$r40_validation_plan
  web_assert(identical(j$idempotency_key,web_r40_full_key(plan)),
    "R/40 inputs or rules changed after the request was registered.","STALE_UPSTREAM_VERSION")
  scope<-plan$execution_scope
  check_lease<-function()nrow(web_query(con,paste0("SELECT id FROM jobs WHERE id=?j AND project_id=?p AND version_id=?v ",
    "AND state='RUNNING' AND worker_token=?w"),j=job_id,p=j$project_id,v=version_id,w=j$worker_token))==1L
  contract<-publication$r40_input_contract
  inspection<-list(verification="NOT_ASSESSED",checks=list())
  word<-web_r40_check_word(pre$packet,settings,con,attempt$actor,j$project_id,work)
  plan$rules$DOCUMENT_STRUCTURE<-word
  if(identical(word$result,"FAIL")) {
    native<-list(state="DOCUMENT_STRUCTURE_FAILED",mode="LIVE")
    rules<-plan$rules;complete<-FALSE
  } else if(isTRUE(plan$revision$selective)) {
    policy<-srma_qc_policy_load(settings$engine_root)
    native<-web_revision_worker_rules(settings,plan,contract,policy,check_lease)
    rules<-native$rules;native$rules<-NULL
    part<-contract$binding$inputs$revision$binding
    native$revision_sha256<-part$fingerprint;native$section_sha256<-part$section_sha256
    complete<-web_r40_revision_complete(rules,native)
  } else {
    proof<-web_c3_collect_inputs(con,settings,attempt$actor,j$project_id,version_id,pre$packet,work)
    methods<-unlist(plan$binding$profiles,use.names=FALSE)
    if(is.null(plan$revision) && (identical(methods,"NARRATIVE_SWIM") ||
        (identical(methods,"PAIRWISE_META") && is.null(pre$packet$production))))
      inspection<-web_r40_inspect(pre$packet,proof,work,settings$engine_root)
    native<-web_r40_bound_cli(con,settings,attempt$actor,j$project_id,pre,proof,work,check_lease=check_lease)
    declared<-web_r40_cli_paths(settings$engine_root)[["manuscript"]]
    hash<-contract$binding$inputs$revision$binding$revised_archive[["manuscript.md"]]$sha256 %||b2%
      contract$binding$inputs$r39$binding$artifacts[[declared]]$sha256
    rules<-web_r40_execution_rules(plan,native,hash,canonical_id,list(job_id=job_id,version_id=version_id,
      manuscript_sha256=hash,native_sha256=web_c2_hash(native)))
    complete<-web_r40_execution_complete(rules,native)
  }
  failed<-any(vapply(rules,function(a)identical(a$result,"FAIL"),logical(1)))
  record<-list(job_id=job_id,fingerprint=scope$fingerprint,deterministic_subset=inspection,
    execution_inputs_sha256=scope$inputs_sha256,native=native,rules=rules,
    semantic=rules$CROSS_SECTION_SEMANTICS$result,validation=if(complete)"PASS" else if(failed)"FAIL" else "NOT_ASSESSED",
    approval="UNREVIEWED")
  dest<-web_inside(file.path(work,web_r40_full_artifact()),work,exists=FALSE)
  jsonlite::write_json(record,dest,auto_unbox=TRUE,null="null")
  wait<-native$state %in% c("AI_APPROVAL_REQUIRED","AI_LIVE_DISABLED","AI_PROVIDER_CREDENTIAL_REQUIRED",
    "CANONICAL_SCIENTIFIC_RUNTIME_REQUIRED")
  web_result(web_r40_full_operation(),canonical_id,job_id,if(wait)"USER_ACTION_REQUIRED" else "COMPLETE",
    "Bound inspection finished. Open the recorded checks for the validation status.",engine_id,
    artifacts=list(list(path=dest,logical_name=web_r40_full_artifact(),type="r40_bound_inspection")),
    provenance=list(fingerprint=scope$fingerprint,validation=record$validation))
}
