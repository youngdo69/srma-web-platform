web_c2_hash <- function(x) digest::digest(as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=NA,dataframe="rows")),algo="sha256",serialize=FALSE)
# WEB-012 state and authorization; canonical statistics are never calculated here.
web_c2_operations <- function() c(generate_manuscript_docx="HUMAN_EDITED",
  generate_manuscript_section="DRAFT", generate_final_manuscript="PLAN")
web_c2_sections <- function() c("Abstract","Keywords","Introduction","Methods","Results","Discussion","Limitations","Conclusions","Declarations")
web_c2_read <- function(root) jsonlite::read_json(file.path(root,"config/generated/web_manuscript.json"),simplifyVector=FALSE)
web_c2_write <- function(root,x) jsonlite::write_json(x,file.path(root,"config/generated/web_manuscript.json"),auto_unbox=TRUE,null="null",pretty=TRUE,digits=NA)
web_c2_defs <- function(engine,n) {
 if(!exists("web_b2_exports",mode="function"))source(file.path(engine,"web/adapter/evidence.R"))
 path<-list.files(file.path(engine,"R"),pattern=paste0("^",n,"_"),full.names=TRUE);web_assert(length(path)==1,"Canonical manuscript component unavailable.")
 web_b2_exports(path,new.env(parent=globalenv()),if(n==40)"numeric_token_pattern" else character())
}
web_c2_text <- function(x,engine) {
 web_assert(is.character(x)&&length(x)==1&&!is.na(x)&&nchar(x)<=50000,"Enter bounded manuscript text.")
 web_no_secrets(x,engine)
 web_assert(!grepl("(/workspace/|/home/|[A-Za-z]:[/\\\\]|<script|file://|data:|https?://)",x,ignore.case=TRUE),"Remove paths, raw links or executable markup; use verified citation tokens.")
 x
}
# RC28.25.5: PubMed networking/XML parsing runs in a CLEAN, separate Rscript.
# Shiny may already have loaded rlang 1.1.3; httr2 in the scientific library
# requires >=1.3.0. .libPaths() cannot replace an already-loaded namespace.
# No canonical R/39 source is executed top-level: child selectively exports
# only known R/39 function definitions using its parsed AST.
web_c2_pubmed_child_main <- function() {
  args <- commandArgs(trailingOnly=TRUE)
  if(length(args)!=4L)stop("Expected input, output, engine, scientific library paths.")
  input_path<-args[[1L]];output_path<-args[[2L]]
  engine<-normalizePath(args[[3L]],winslash="/",mustWork=TRUE)
  library<-normalizePath(args[[4L]],winslash="/",mustWork=TRUE)
  .libPaths(unique(c(library,.libPaths())))
  output<-tryCatch({
    required<-c("rlang","httr2","xml2","jsonlite","stringr","tibble",
      "dplyr","digest","yaml","readr","openxlsx")
    for(pkg in required)loadNamespace(pkg)
    if(utils::packageVersion("rlang")<package_version("1.3.0"))
      stop(paste("Scientific library rlang version",as.character(utils::packageVersion("rlang")),
        "is below httr2 minimum 1.3.0"))
    # Build an isolated namespace using R/39 functions, but do NOT source
    # its top-level 25-reference gate, LLM calls, or manuscript generation.
    canonical<-file.path(engine,"R","39_generate_manuscript_draft_with_verified_literature.R")
    if(!file.exists(canonical))stop("Canonical R/39 is missing.")
    allow<-c("safe_chr","ncbi_request","pubmed_search","pubmed_fetch_xml",
      "xml_text_one","xml_text_all","parse_pubmed_articles")
    e<-new.env(parent=baseenv())
    seen<-character()
    for(expr in parse(canonical,keep.source=FALSE)) {
      if(!is.call(expr)||length(expr)!=3L||!identical(expr[[1L]],as.name("<-")))next
      if(!is.symbol(expr[[2L]]))next
      nm<-as.character(expr[[2L]])
      if(!(nm %in% allow))next
      rhs<-expr[[3L]]
      if(!is.call(rhs)||!identical(rhs[[1L]],as.name("function")))
        stop(paste("Unexpected canonical function binding:",nm))
      if(nm %in% seen)stop(paste("Duplicate canonical function binding:",nm))
      eval(expr,envir=e);seen<-c(seen,nm)
    }
    if(!setequal(seen,allow))stop("R/39 PubMed function inventory changed; update isolation contract.")
    e$ncbi_base<-"https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
    e$ncbi_email<-Sys.getenv("NCBI_EMAIL",unset="")
    e$ncbi_api_key<-Sys.getenv("NCBI_API_KEY",unset="")
    req<-jsonlite::read_json(input_path,simplifyVector=FALSE)
    op<-req$operation
    if(identical(op,"preflight")) {
      # Completely offline parser test; no user data, PubMed, or AI requests.
      fixture<-paste0("<PubmedArticleSet><PubmedArticle><MedlineCitation><PMID>12345678</PMID>",
        "<Article><ArticleTitle>Isolated parser fixture</ArticleTitle><Journal><Title>Test Journal</Title></Journal>",
        "<AuthorList><Author><LastName>Tester</LastName><Initials>A</Initials></Author></AuthorList>",
        "</Article></MedlineCitation></PubmedArticle></PubmedArticleSet>")
      parsed<-e$parse_pubmed_articles(fixture)
      if(nrow(parsed)!=1L||!isTRUE(parsed$metadata_verified[[1L]])||
         !identical(as.character(parsed$pmid[[1L]]),"12345678"))
        stop("R/39 XML parser offline fixture failed.")
      list(ok=TRUE,operation=op,rlang_version=as.character(utils::packageVersion("rlang")),
        httr2_version=as.character(utils::packageVersion("httr2")),
        parser_fixture="PASS",request_made=FALSE)
    } else if(identical(op,"search")) {
      query<-req$query
      n<-suppressWarnings(as.integer(req$retmax))
      if(!is.character(query)||length(query)!=1L||is.na(query)||
         !nzchar(trimws(query))||nchar(query)>800L||
         length(n)!=1L||is.na(n)||n<1L||n>100L)stop("Invalid bounded PubMed query.")
      result<-e$pubmed_search(query,n)
      list(ok=TRUE,operation=op,ids=as.list(result$ids),
        count=result$count,query_translation=result$query_translation)
    } else if(identical(op,"fetch")) {
      ids<-unique(as.character(unlist(req$pmids,use.names=FALSE)))
      if(!length(ids)||length(ids)>100L||anyNA(ids)||
         !all(grepl("^[0-9]{1,9}$",ids)))stop("Invalid bounded PubMed ID batch.")
      raw<-e$pubmed_fetch_xml(ids)
      parsed<-e$parse_pubmed_articles(raw)
      records<-if(nrow(parsed))lapply(seq_len(nrow(parsed)),
        function(i)as.list(parsed[i,,drop=FALSE])) else list()
      list(ok=TRUE,operation=op,records=records,
        response_sha256=digest::digest(raw,algo="sha256",serialize=FALSE))
    } else stop("Unsupported isolated PubMed operation.")
  },error=function(err) {
    msg<-conditionMessage(err)
    token<-Sys.getenv("NCBI_API_KEY",unset="")
    if(nzchar(token))msg<-gsub(token,"[REDACTED]",msg,fixed=TRUE)
    list(ok=FALSE,error=substr(msg,1L,1000L))
  })
  # Explicit file IPC: return raw verified records only, never API keys.
  jsonlite::write_json(output,output_path,auto_unbox=TRUE,null="null",na="null")
  if(!isTRUE(output$ok))quit(status=2L,save="no")
  invisible(TRUE)
}

web_c2_pubmed_isolated <- function(engine,settings=NULL,operation,request=list()) {
  explicit<-trimws(Sys.getenv("SRMA_SCIENTIFIC_LIBRARY",unset=""))
  configured<-if(!is.null(settings))settings$engine_library else NULL
  configured<-if(!is.null(configured)&&length(configured)==1L&&
    !is.na(configured[[1L]]))trimws(as.character(configured[[1L]])) else ""
  library<-if(nzchar(explicit))explicit else configured
  web_assert(nzchar(library)&&dir.exists(library),
    "Set SRMA_SCIENTIFIC_LIBRARY to the verified scientific R library in Terminal 2.",
    "DEPENDENCY_FAILED")
  library<-normalizePath(library,winslash="/",mustWork=TRUE)
  rscript<-trimws(Sys.getenv("SRMA_SCIENTIFIC_RSCRIPT",unset=""))
  if(!nzchar(rscript))rscript<-file.path(R.home("bin"),
    if(.Platform$OS.type=="windows")"Rscript.exe" else "Rscript")
  web_assert(file.exists(rscript),
    paste0("Rscript executable unavailable: ",rscript),"DEPENDENCY_FAILED")
  # Keep data on disk only inside a private disposable temp directory. Do not
  # reuse stale results, and do not send sensitive PDF evidence to the child.
  work<-tempfile("srma-pubmed-isolated-")
  dir.create(work,recursive=TRUE,showWarnings=FALSE,mode="0700")
  on.exit(unlink(work,recursive=TRUE,force=TRUE),add=TRUE)
  worker<-file.path(work,"worker.R")
  input<-file.path(work,"request.json")
  output<-file.path(work,"response.json")
  stdout<-file.path(work,"stdout.log")
  stderr<-file.path(work,"stderr.log")
  # Deparse an isolated, self-contained function from APPROVED Web source;
  # no new/untracked persistent R script is added to the 18-source overlay.
  writeLines(c("main <-",deparse(web_c2_pubmed_child_main,width.cutoff=500L),
    "main()"),worker,useBytes=TRUE)
  jsonlite::write_json(c(list(operation=operation),request),input,
    auto_unbox=TRUE,null="null",na="null")
  args<-c("--vanilla",shQuote(worker),shQuote(input),shQuote(output),
    shQuote(normalizePath(engine,winslash="/",mustWork=TRUE)),shQuote(library))
  status<-tryCatch(suppressWarnings(system2(rscript,args=args,
    stdout=stdout,stderr=stderr,wait=TRUE,timeout=180L)),
    error=function(err)err)
  web_assert(!inherits(status,"error"),
    "Isolated R/39 PubMed worker could not start; check SRMA_SCIENTIFIC_RSCRIPT.",
    "DEPENDENCY_FAILED")
  web_assert(file.exists(output),
    "Isolated R/39 PubMed worker exited without a response; check Rscript or child startup.",
    "DEPENDENCY_FAILED")
  result<-tryCatch(jsonlite::read_json(output,simplifyVector=FALSE),
    error=function(err)NULL)
  web_assert(is.list(result),"Invalid isolated R/39 PubMed response.","PROVIDER_UNAVAILABLE")
  if(!isTRUE(result$ok))web_error(if(identical(operation,"preflight"))
    "DEPENDENCY_FAILED" else "PROVIDER_UNAVAILABLE",
    paste0("Isolated R/39 worker: ",as.character(result$error)))
  web_assert(identical(as.integer(status),0L),
    "Isolated R/39 worker exited unsuccessfully.","PROVIDER_UNAVAILABLE")
  result
}

web_c2_fetch_references <- function(engine,pmids,settings=NULL) {
  pmids<-unique(as.character(pmids))
  web_assert(length(pmids)>0L&&length(pmids)<=100L&&
    all(grepl("^[0-9]{1,9}$",pmids)),"Invalid PubMed reference IDs.",
    "CITATION_VALIDATION_FAILED")
  reply<-web_c2_pubmed_isolated(engine,settings,"fetch",list(pmids=as.list(pmids)))
  rows<-reply$records
  web_assert(is.list(rows)&&setequal(vapply(rows,`[[`,character(1),"pmid"),pmids)&&
    all(vapply(rows,function(r)isTRUE(r$metadata_verified),logical(1))),
    "Some requested references could not be verified.","CITATION_VALIDATION_FAILED")
  list(records=rows,provider="PubMed EFetch / isolated R/39",
    response_sha256=reply$response_sha256,verified_at=web_now())
}
web_c2_normalize_refs <- function(refs,engine) {
 e<-web_c2_defs(engine,40);out<-list();seen<-character();aliases<-list()
 for(r in refs$records) {
  web_assert(isTRUE(r$metadata_verified)&&grepl("^[0-9]+$",r$pmid)&&all(vapply(r[c("title","authors","year","journal")],function(a)is.character(a)&&length(a)==1&&!is.na(a)&&nzchar(a),logical(1))),"Reference metadata is incomplete or unverified.","CITATION_VALIDATION_FAILED")
  doi<-e$normalize_doi(r$doi %||b2% "");key<-if(nzchar(doi))paste0("doi:",doi) else paste0("pmid:",r$pmid)
  hit<-which(vapply(out,function(a)a$pmid==r$pmid||nzchar(doi)&&identical(e$normalize_doi(a$doi),doi),logical(1)))
  if(length(hit)){aliases[[r$pmid]]<-out[[hit[1]]]$pmid;next}
  r$doi<-doi;out[[length(out)+1L]]<-r;seen<-c(seen,key)
 }
 refs$records<-out;refs$aliases<-aliases;refs
}
web_c2_current <- function(con,project_id,x,version_id=NULL,latest=FALSE) {
 web_c1_current(con,project_id,x$evidence_version)
 # Any later analysis plan/result is a material downstream revision; historical files are retained.
 newer<-web_query(con,"SELECT a.version_id FROM analysis_versions a JOIN project_versions v ON v.id=a.version_id WHERE a.project_id=?p AND v.version>?n",p=project_id,n=x$analysis_watermark)
 web_assert(!nrow(newer),"Analyses changed. Create and approve a new manuscript plan.","STALE_UPSTREAM_VERSION")
 refs<-web_query(con,"SELECT reference_sha256 FROM manuscript_versions WHERE project_id=?p AND stage='PLAN' ORDER BY created_at DESC,version_id DESC",p=project_id)
 if(nrow(refs)) {
  # Creation ordering uses ProjectVersion monotonic sequence, not wall-clock ties.
  refs<-web_query(con,"SELECT m.reference_sha256 FROM manuscript_versions m JOIN project_versions v ON v.id=m.version_id WHERE m.project_id=?p AND m.stage='PLAN' ORDER BY v.version DESC LIMIT 1",p=project_id)
  web_assert(refs$reference_sha256==x$reference_sha256,"Reference library changed. Review a new manuscript plan.","STALE_UPSTREAM_VERSION")
 }
 if(latest) {
  v<-web_query(con,"SELECT m.version_id FROM manuscript_versions m JOIN project_versions v ON v.id=m.version_id WHERE m.project_id=?p AND m.manuscript_id=?m ORDER BY v.version DESC LIMIT 1",p=project_id,m=x$manuscript_id)
  web_assert(nrow(v)==1&&v$version_id==version_id,"A newer manuscript edit exists. Load it; old jobs cannot replace Human edits.","STALE_UPSTREAM_VERSION")
 }
 invisible(TRUE)
}
web_c2_context <- function(con,settings,actor,project_id,version_id,current=FALSE,latest=FALSE) {
 v<-web_version_root(con,settings,actor,project_id,version_id)
 row<-web_query(con,"SELECT * FROM manuscript_versions WHERE project_id=?p AND version_id=?v",p=project_id,v=version_id)
 web_assert(nrow(row)==1,"Select a manuscript version in this project.")
 x<-web_c2_read(v$root)
 web_assert(x$manuscript_id==row$manuscript_id&&x$stage==row$stage&&web_c2_hash(x$source)==row$source_sha256&&web_c2_hash(x$spec)==row$spec_sha256&&web_c2_hash(x$source$references)==row$reference_sha256,"Manuscript source integrity failed.")
 if(current)web_c2_current(con,project_id,x,version_id,latest)
 list(version=v,row=row,packet=x)
}
web_c2_save <- function(con,settings,actor,project_id,parent,root,x,job=NULL,new_plan=FALSE) {
 p<-web_authorize(con,actor,project_id);x$source_sha256<-web_c2_hash(x$source);x$reference_sha256<-web_c2_hash(x$source$references);x$actor<-actor;x$updated_at<-web_now();x$engine<-web_register_engine(con,settings);web_c2_write(root,x)
 jsonlite::write_json(x$source,file.path(root,"config/generated/manuscript_source.json"),auto_unbox=TRUE,null="null",pretty=TRUE,digits=NA)
 web_tx(con,function(){
  web_exec(con,"UPDATE projects SET title=title WHERE id=?p",p=project_id)
  if(identical(x$stage,"HUMAN_EDITED") && is.list(x$revision)) {
    duplicate<-web_revision_existing(con,settings,actor,project_id,parent,x$revision$fingerprint)
    if(is.list(duplicate))return(duplicate)
  }
  if(identical(x$stage,"REVIEW_REQUIRED") && is.list(x$revision) && !is.null(job)) {
    duplicate<-web_revision_registered(con,project_id,parent,job)
    if(is.list(duplicate))return(duplicate)
  }
  if(!new_plan)web_c2_current(con,project_id,x,parent,TRUE) else web_c1_current(con,project_id,x$evidence_version)
  if(identical(x$stage,"APPROVED")) {
    web_assert(web_qc_author_review_ready(con,settings,actor,project_id,parent),
      "The scientific validation changed. Refresh the manuscript before approving.","STALE_UPSTREAM_VERSION")
    review<-x$review
    gate<-web_query(con,paste("SELECT status,reviewer_id,decided_at,rationale FROM gates",
      "WHERE id=?g AND project_id=?p AND version_id=?v AND gate_type='manuscript_review'"),
      g=review$gate,p=project_id,v=parent)
    web_assert(nrow(gate)==1L,"The author review gate changed.","STALE_UPSTREAM_VERSION")
    if(identical(gate$status,"PENDING")) {
      n<-web_exec(con,paste("UPDATE gates SET status='APPROVED',decision='APPROVED',reviewer_id=?a,",
        "decided_at=?t,rationale=?r WHERE id=?g AND status='PENDING'"),a=actor,t=review$at,r=review$rationale,g=review$gate)
      web_assert(n==1L,"The author review gate changed.","STALE_UPSTREAM_VERSION")
      web_audit(con,p,"GATE_DECIDED",review$gate,actor,list(decision="APPROVED",rationale=review$rationale))
    } else web_assert(identical(gate$status,"APPROVED") && identical(gate$reviewer_id,actor) &&
      identical(gate$rationale,review$rationale) && identical(gate$decided_at,review$at),
      "The author review gate changed.","STALE_UPSTREAM_VERSION")
  }
  v<-web_import_project(con,settings,actor,p$workspace_id,root,configuration_only=TRUE,transaction=function(con,f)f())
  web_assert(v$project_id==project_id,"Manuscript project changed.")
  web_exec(con,"INSERT INTO manuscript_versions VALUES(?v,?p,?m,?parent,?e,?stage,?source,?refs,?spec,?job,?engine,?actor,?now)",v=v$version_id,p=project_id,m=x$manuscript_id,parent=parent,e=x$evidence_version,stage=x$stage,source=x$source_sha256,refs=x$reference_sha256,spec=web_c2_hash(x$spec),job=job %||b2% NA_character_,engine=x$engine,actor=actor,now=web_now())
  # Automatic final manuscript drafting requires NO Human reference/plan decision.
  # A REVIEW_REQUIRED manuscript still gets the final Author Review gate.
  # Manual historical workflows keep their old PLAN gate semantics.
  create_gate <- identical(x$stage,"REVIEW_REQUIRED") ||
    (identical(x$stage,"PLAN") && !identical(x$review_mode,"FINAL_MANUSCRIPT_ONLY"))
  if(create_gate)web_exec(con,"INSERT INTO gates(id,project_id,workspace_id,version_id,gate_type,required_operation,status,created_at) VALUES(?g,?p,?w,?v,?k,?o,'PENDING',?now)",g=web_id(),p=project_id,w=p$workspace_id,v=v$version_id,k=if(x$stage=="PLAN")"manuscript_plan" else "manuscript_review",o=if(x$stage=="PLAN")"prepare_manuscript" else "approve_manuscript",now=web_now())
  web_c3_invalidate_audit(con,p,actor,v$version_id)
  web_audit(con,p,paste0("MANUSCRIPT_",x$stage),v$version_id,actor,list(source=x$source_sha256,reference=x$reference_sha256,job=job))
  if(identical(x$stage,"HUMAN_EDITED") && is.list(x$revision))
    web_audit(con,p,"MANUSCRIPT_SECTION_REVISED",v$version_id,actor,x$revision[c("parent_version","section","type",
      "original_section_sha256","section_sha256","fingerprint","rationale","affected")])
  if(new_plan)web_audit(con,p,"MANUSCRIPT_SOURCE_REVISION",v$version_id,actor,list(reference=x$reference_sha256))
  v
 })
}
web_c2_catalog <- function(con,settings,actor,project_id,version_id) {
 c<-web_c1_evidence(con,settings,actor,project_id,version_id)
 a<-web_query(con,"SELECT a.version_id,a.source_job_id,v.version FROM analysis_versions a JOIN project_versions v ON v.id=a.version_id WHERE a.project_id=?p AND a.evidence_version_id=?e AND a.stage='APPROVED' ORDER BY v.version",p=project_id,e=version_id)
 a$label<-vapply(a$version_id,function(v){x<-web_c1_context(con,settings,actor,project_id,v)$packet;paste(x$plan$role,x$plan$outcome,x$plan$timepoint)},character(1))
 arts<-lapply(seq_len(nrow(a)),function(i){t<-web_query(con,"SELECT id,logical_name,checksum FROM artifacts WHERE project_id=?p AND job_id=?j AND status='AVAILABLE'",p=project_id,j=a$source_job_id[i]);result<-web_c1_context(con,settings,actor,project_id,a$version_id[i])$packet$result;jr<-web_parse(web_query(con,"SELECT result_json FROM jobs WHERE id=?j",j=a$source_job_id[i])$result_json);allowed<-vapply(Filter(function(z)z$relative_path %in% c("results/meta_analysis/tables/meta_analysis_summary.csv",result$forest_path),jr$artifacts),`[[`,character(1),"artifact_id");t<-t[t$id %in% allowed,,drop=FALSE];t$analysis_version<-rep(a$version_id[i],nrow(t));t})
 list(analyses=a,artifacts=if(length(arts))do.call(rbind,arts) else data.frame(),evidence_version=version_id)
}
web_c2_plan <- function(con,settings,actor,project_id,version_id,spec) {
 web_assert(is.list(spec)&&setequal(names(spec),c("title","language","analyses","artifacts","pmids","rationale")),"Unsupported manuscript specification fields.")
 for(k in c("title","language","rationale"))web_assert(nzchar(trimws(web_c2_text(spec[[k]],settings$engine_root))),"Complete the manuscript plan.")
 ids<-unlist(spec$analyses,use.names=FALSE);pmids<-unique(unlist(spec$pmids,use.names=FALSE));arts<-unlist(spec$artifacts,use.names=FALSE)
 web_assert(length(ids)>0&&!anyDuplicated(ids)&&length(pmids)>0&&length(pmids)<=100&&all(grepl("^[0-9]{1,9}$",pmids)),"Choose approved analyses and valid PubMed IDs (maximum one hundred).")
 c<-web_c1_evidence(con,settings,actor,project_id,version_id);catalog<-web_c2_catalog(con,settings,actor,project_id,version_id)
 web_assert(all(ids %in% catalog$analyses$version_id)&&length(arts)>0&&!anyDuplicated(arts)&&all(arts %in% catalog$artifacts$id),"Choose registered figures/tables belonging to approved analyses.")
 a<-setNames(lapply(ids,function(v)web_c1_context(con,settings,actor,project_id,v,TRUE)$packet),ids)
 web_assert(any(vapply(a,function(x)x$plan$role=="PRIMARY",logical(1))),"Select an approved primary result.")
 chosen<-catalog$artifacts[match(arts,catalog$artifacts$id),,drop=FALSE];web_assert(all(chosen$analysis_version %in% ids),"An artifact belongs to an unselected analysis.")
 web_assert(all(ids %in% chosen$analysis_version[grepl("[.]csv$",chosen$logical_name)]),"Select the canonical result table for every included analysis.")
 for(t in a)if(t$plan$role %in% c("SENSITIVITY","SUBGROUP","EXPLORATORY"))web_assert(t$plan$parent_version %in% ids,"Include the approved parent of each linked analysis.")
 refs<-web_c2_normalize_refs(web_c2_fetch_references(settings$engine_root,pmids,settings=settings),settings$engine_root)
 root<-tempfile("manuscript-plan-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root)
 cfg<-yaml::read_yaml(file.path(root,"config/project_config.yml"));policy<-cfg$analysis$manuscript
 limits<-list(total=policy$minimum_unique_references %||b2% 25,introduction=policy$minimum_intro_references %||b2% 8,discussion=policy$minimum_discussion_references %||b2% 15)
 web_assert(all(vapply(limits,function(v)is.numeric(v)&&length(v)==1&&is.finite(v)&&v>=1&&v==as.integer(v),logical(1))),"Invalid canonical reference-count policy.")
 frozen<-list()
 for(i in seq_len(nrow(chosen))) {
  ar<-web_query(con,"SELECT * FROM artifacts WHERE id=?i AND project_id=?p",i=chosen$id[i],p=project_id)
  dest<-file.path("config/generated/manuscript_assets",paste0(ar$id,".",tools::file_ext(ar$logical_name)));dir.create(dirname(file.path(root,dest)),recursive=TRUE,showWarnings=FALSE)
  web_download(con,settings,actor,project_id,ar$id,file.path(root,dest))
  frozen[[ar$id]]<-list(id=ar$id,analysis_version=chosen$analysis_version[i],name=ar$logical_name,sha256=ar$checksum,path=dest,type=if(grepl("[.]csv$",ar$logical_name))"table" else "figure")
 }
 # Evidence tables and source-file hashes retain original record provenance; unavailable totals stay unavailable.
 source<-list(project_version=version_id,evidence_version=version_id,project=cfg$project,research_question=cfg$research_question,protocol=cfg$protocol,registration=cfg$registration,search=cfg$search,screening=cfg$screening,evidence=c$packet,evidence_manifest=c$version$manifest,analyses=a,artifacts=frozen,references=refs,reference_policy=limits)
 source$evidence_outputs<-list()
 for(path in names(c$version$manifest))if(grepl("(prisma|summary|crosswalk|included_studies|rob_final).*\\.csv$",path,ignore.case=TRUE)) {
  d<-web_c1_csv(file.path(root,path));source$evidence_outputs[[path]]<-list(version=version_id,sha256=c$version$manifest[[path]],rows=lapply(seq_len(nrow(d)),function(i)as.list(d[i,,drop=FALSE])))
 }
 source$facts<-web_c2_facts(source,settings$engine_root)
 watermark<-web_query(con,"SELECT MAX(v.version) AS n FROM analysis_versions a JOIN project_versions v ON v.id=a.version_id WHERE a.project_id=?p",p=project_id)$n
 x<-list(manuscript_id=web_id(),evidence_version=version_id,analysis_watermark=watermark,stage="PLAN",review_mode="FINAL_MANUSCRIPT_ONLY",spec=spec,source=source,sections=list(),created_at=web_now())
 x$source$statistical_methods<-tryCatch(web_stat_draft_source(con,settings,actor,project_id,x,c$packet,c$version),
   error=function(e)list(status="NOT_ASSESSED",gaps=list("source_verification"),records=list()))
 x$source$facts<-c(x$source$facts,web_stat_methods_facts(x$source$statistical_methods,version_id))
 web_c2_save(con,settings,actor,project_id,version_id,root,x,new_plan=TRUE)
}
web_c2_plan_gate <- function(con,project_id,x) {
 g<-web_query(con,"SELECT id FROM gates WHERE project_id=?p AND version_id=?v AND gate_type='manuscript_plan' AND status='APPROVED'",p=project_id,v=x$plan_version)
 web_assert(nrow(g)==1,"Approve the exact manuscript plan and reference library first.","USER_ACTION_REQUIRED")
}
web_c2_begin <- function(con,settings,actor,project_id,version_id) {
 c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE);x<-c$packet;web_assert(x$stage=="PLAN","Select a manuscript plan.");x$plan_version<-version_id;web_c2_plan_gate(con,project_id,x)
 methods<-web_stat_methods_editor_text(x)
 x$stage<-"DRAFT";x$sections<-setNames(lapply(web_c2_sections(),function(k)list(id=k,source_sha256=x$source_sha256,source="SYSTEM",original="[AUTHOR REQUIRED]",text="[AUTHOR REQUIRED]",status="REVIEW_REQUIRED",actor=actor,at=web_now(),history=list())),web_c2_sections())
 if(nzchar(methods))x$sections$Methods$text<-x$sections$Methods$original<-methods
 web_c2_mutate(con,settings,actor,project_id,version_id,c,x)
}
web_c2_mutate <- function(con,settings,actor,project_id,version_id,c,x,job=NULL) {
 root<-tempfile("manuscript-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root)
 web_c2_save(con,settings,actor,project_id,version_id,root,x,job)
}
web_c2_edit <- function(con,settings,actor,project_id,version_id,section,text,decision,rationale) {
 c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE);x<-c$packet
 web_assert(!is.list(x$revision),"Use the versioned Statistical Analysis revision editor for this manuscript.","USER_ACTION_REQUIRED")
 if(identical(x$stage,"PLAN") && identical(section,"Methods") &&
    identical(x$review_mode,"FINAL_MANUSCRIPT_ONLY")) {
  web_assert(identical(decision,"SAVE") && identical(x$source$statistical_methods$status,"PASS"),
    "Save a Methods draft after its statistical evidence is verified. Final approval follows R/40.","USER_ACTION_REQUIRED")
  text<-web_c2_text(text,settings$engine_root)
  rationale<-web_c2_text(rationale,settings$engine_root)
  web_assert(nzchar(trimws(text)) && nzchar(trimws(rationale)),"Enter revised prose and a rationale.")
  sec<-x$sections$Methods
  x$sections$Methods<-list(id=section,source_sha256=x$source_sha256,source="HUMAN",status="REVIEW_REQUIRED",
    original=sec$original %||b2% web_stat_methods_editor_text(x),text=text,actor=actor,at=web_now(),rationale=rationale,
    history=if(is.list(sec))c(sec$history,list(sec[setdiff(names(sec),"history")])) else list())
  return(web_c2_mutate(con,settings,actor,project_id,version_id,c,x))
 }
 web_c2_plan_gate(con,project_id,x)
 web_assert(x$stage %in% c("DRAFT","HUMAN_EDITED","REVIEW_REQUIRED")&&section %in% web_c2_sections()&&decision %in% c("SAVE","APPROVE","REJECT"),"Select an editable section and explicit action.")
 text<-web_c2_text(text,settings$engine_root);web_assert(nzchar(trimws(web_c2_text(rationale,settings$engine_root))),"Record a Human editing/review rationale.")
 sec<-x$sections[[section]];sec$history<-c(sec$history,list(sec[setdiff(names(sec),"history")]));sec$text<-text;sec$source<-"HUMAN";sec$status<-if(decision=="SAVE")"REVIEW_REQUIRED" else if(decision=="APPROVE")"APPROVED" else "REJECTED";sec$actor<-actor;sec$at<-web_now();sec$rationale<-rationale;x$sections[[section]]<-sec
 x$stage<-"HUMAN_EDITED";x$docx<-NULL;x$generation<-NULL;x$review<-NULL
 web_c2_mutate(con,settings,actor,project_id,version_id,c,x)
}
web_c2_prepare_ai <- function(con,settings,actor,project_id,version_id,section) {
 c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE);x<-c$packet
 methods_plan<-identical(x$stage,"PLAN") && identical(section,"Methods") &&
   identical(x$review_mode,"FINAL_MANUSCRIPT_ONLY") && identical(x$source$statistical_methods$status,"PASS")
 if(!methods_plan)web_c2_plan_gate(con,project_id,x)
 web_assert(methods_plan || (section %in% web_c2_sections() && x$stage %in% c("DRAFT","HUMAN_EDITED","REVIEW_REQUIRED")),"Select an editable section.")
 web_assert(nzchar(Sys.getenv("OPENAI_API_KEY")),"AI drafting is unavailable; enter Human-authored sections or ask the operator to configure credentials.","PROVIDER_UNAVAILABLE")
 x$generation<-list(section=section,source_sha256=x$source_sha256,model=Sys.getenv("SRMA_MANUSCRIPT_MODEL","gpt-5.6"),provider="OpenAI",template="web-manuscript-section-v1");x$stage<-"DRAFT";x$docx<-NULL
 if(methods_plan)x$generation$return_stage<-"PLAN"
 web_c2_mutate(con,settings,actor,project_id,version_id,c,x)
}
web_c2_guard <- function(con,settings,actor,project_id,version_id,operation) {
 if(!operation %in% names(web_c2_operations()))return(invisible(TRUE))
 c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE);x<-c$packet
 if(identical(operation,"generate_manuscript_docx") && identical(x$stage,"HUMAN_EDITED") && is.list(x$revision)) {
   web_science_settings(settings);return(invisible(TRUE))
 }
 if(identical(operation,"generate_final_manuscript")) {
   # This is a machine-generated AUTHOR-REVIEW DRAFT from finalized scientific
   # evidence. The editorial manuscript_plan gate is not falsely APPROVED;
   # source-specific Human scientific verification remains mandatory.
   web_assert(identical(x$stage,"PLAN") && length(x$source$references$records)>0L,
     "Select a current, source-verified Manuscript PLAN before production.",
     "USER_ACTION_REQUIRED")
   if(identical(x$source$route,"NARRATIVE"))
     web_assert(identical(x$source$narrative$finalization_scope,
       "NARRATIVE_ANALYSIS_LOCKED; NOT_CANONICAL_R26"),
       "Human-confirmed narrative synthesis is not finalized.","USER_ACTION_REQUIRED")
   else web_assert(length(x$source$analyses)>0L,
     "No approved quantitative analysis was found.","USER_ACTION_REQUIRED")
   web_science_settings(settings)
   return(invisible(TRUE))
 }
 methods_plan<-identical(operation,"generate_manuscript_section") && identical(x$generation$return_stage,"PLAN") &&
   identical(x$review_mode,"FINAL_MANUSCRIPT_ONLY") && identical(x$generation$section,"Methods") &&
   identical(x$generation$source_sha256,x$source_sha256) && identical(x$source$statistical_methods$status,"PASS")
 if(!methods_plan)web_c2_plan_gate(con,project_id,x)
 if(operation=="generate_manuscript_docx") {
    if(identical(x$source$route,"NARRATIVE"))
      web_assert(x$stage %in% c("DRAFT","HUMAN_EDITED","REVIEW_REQUIRED") &&
        length(x$sections)==length(web_c2_sections()),
        "Prepare Manuscript sections before requesting a narrative working Word draft.",
        "USER_ACTION_REQUIRED")
    else web_assert(web_c2_check(x,settings$engine_root)$status=="PASS",
        "Resolve manuscript pre-check findings and approve all sections before Word generation.",
        "USER_ACTION_REQUIRED")
  }
 else web_assert(!is.null(x$generation)&&nzchar(Sys.getenv("OPENAI_API_KEY")),"Prepare an exact section request with available provider credentials.","PROVIDER_UNAVAILABLE")
 web_science_settings(settings);invisible(TRUE)
}
web_c2_accept <- function(con,settings,actor,project_id,version_id,job_id) {
 c<-web_c2_context(con,settings,actor,project_id,version_id);x<-c$packet
 j<-web_scoped_job(con,actor,project_id,version_id,job_id,names(web_c2_operations()));web_assert(j$state=="COMPLETE","Only a completed job for this exact manuscript can be accepted.")
 if(is.list(x$revision)) {
   prior<-web_revision_registered(con,project_id,version_id,job_id)
   if(is.list(prior))return(prior)
 }
 web_c2_current(con,project_id,x,version_id,TRUE)
 root<-tempfile("manuscript-result-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root);web_overlay_job(con,settings,actor,project_id,version_id,job_id,root)
 result<-jsonlite::read_json(file.path(root,"results/web_manuscript/result.json"),simplifyVector=FALSE)
 web_assert(result$version_id==version_id&&result$source_sha256==x$source_sha256&&result$job_id==job_id&&result$content_sha256==web_c2_hash(x$sections),"Manuscript result/version binding failed.")
 if(j$operation %in% c("generate_manuscript_docx","generate_final_manuscript")){
  a<-web_query(con,"SELECT id,checksum FROM artifacts WHERE project_id=?p AND job_id=?j AND logical_name='manuscript.docx' AND status='AVAILABLE'",p=project_id,j=job_id)
  web_assert(nrow(a)==1&&a$checksum==result$docx_sha256,"DOCX checksum binding failed.");web_store(settings)$get(web_query(con,"SELECT storage_locator FROM artifacts WHERE id=?i",i=a$id)$storage_locator,a$checksum)
  x$docx<-c(result,list(artifact_id=a$id));x$stage<-"REVIEW_REQUIRED"
  if(srma_qc_link_sha(result$canonical_source_sha256))
    web_revision_archive(con,settings,actor,project_id,x)
  if(is.list(x$revision)) {
    web_assert(identical(result$reference_sha256,x$reference_sha256) && identical(result$evidence_version,x$evidence_version) &&
      identical(result$revision$fingerprint,x$revision$fingerprint) &&
      identical(result$revision$section_sha256,x$revision$section_sha256) &&
      identical(web_sha(file.path(root,"results/web_manuscript/manuscript.md")),result$revision$markdown_sha256),
      "The revised Word job does not match the saved revision.","ARTIFACT_INTEGRITY_FAILED")
  }
  if(identical(j$operation,"generate_final_manuscript"))
    x$production<-list(origin="RC28.25.8B_WEB_COMPATIBILITY_ADAPTER",
      qc=result$production_qc,author_review="REQUIRED",at=web_now())
 } else {
  k<-x$generation$section;web_assert(result$section==k,"Wrong generated section.")
  method_plan<-identical(x$generation$return_stage,"PLAN") && identical(k,"Methods")
  sec<-x$sections[[k]]
  if(is.list(sec))sec$history<-c(sec$history,list(sec[setdiff(names(sec),"history")]))
  sec$id<-k;sec$source_sha256<-x$source_sha256
  sec$original<-web_c2_text(result$text,settings$engine_root);sec$text<-sec$original;sec$source<-"AI";sec$provenance<-result$generation
  sec$status<-"REVIEW_REQUIRED";sec$actor<-actor;sec$at<-web_now();x$sections[[k]]<-sec
  x$generation<-NULL;x$stage<-if(method_plan)"PLAN" else "DRAFT"
 }
 web_c2_save(con,settings,actor,project_id,version_id,root,x,job_id)
}
web_c2_approve <- function(con,settings,actor,project_id,version_id,confirmed,rationale) {
 c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE);x<-c$packet
 web_assert(isTRUE(confirmed)&&nzchar(trimws(web_c2_text(rationale,settings$engine_root))),"Explicitly confirm text, analyses, references, warnings and matching Word document.")
 web_assert(x$stage=="REVIEW_REQUIRED"&&!is.null(x$docx),"Complete Word generation before author review.")
 web_assert(web_qc_author_review_ready(con,settings,actor,project_id,version_id,x),
   "Complete the required scientific validation before approving the manuscript.","USER_ACTION_REQUIRED")
 if(is.list(x$revision)) {
   derived<-web_revision_archive(con,settings,actor,project_id,x)
   web_assert(identical(x$docx$revision$fingerprint,x$revision$fingerprint) &&
     identical(derived[["manuscript.md"]]$sha256,x$docx$revision$markdown_sha256),
     "The revised archive cannot be verified.","ARTIFACT_INTEGRITY_FAILED")
 } else if(identical(x$source$route,"NARRATIVE") && is.list(x$production)) {
  # Automatic production drafts contain a completed Word artifact instead of
  # editable section packets. This remains the existing draft author decision.
  j<-web_scoped_job(con,actor,project_id,x$docx$version_id,x$docx$job_id,"generate_final_manuscript")
  result_path<-"results/web_manuscript/result.json"
  refs_path<-"results/web_manuscript/verified_reference_expansion.json"
  web_assert(identical(j$state,"COMPLETE") && identical(j$id,c$row$source_job_id[[1L]]) &&
    all(c(result_path,refs_path) %in% names(c$version$manifest)),
    "The completed narrative production job and its reference evidence are required.")
  result<-jsonlite::read_json(web_inside(file.path(c$version$root,result_path),c$version$root),simplifyVector=FALSE)
  web_assert(identical(result$job_id,j$id) && identical(result$version_id,j$version_id) &&
    identical(result$evidence_version,x$evidence_version) &&
    identical(result$source_sha256,x$source_sha256) &&
    identical(result$reference_sha256,x$reference_sha256) &&
    identical(result$content_sha256,x$docx$content_sha256) &&
    identical(result$docx_sha256,x$docx$docx_sha256) &&
    identical(result$verified_literature_sha256,c$version$manifest[[refs_path]]) &&
    identical(result$production_qc$publication_9cd$status,"PUBLICATION_ASSEMBLY_STRUCTURE_PASS"),
    "The narrative production record does not match this saved Word and reference version.")
 } else {
  web_assert(web_c2_check(x,settings$engine_root)$status=="PASS","Complete section review and pre-check before approval.")
 }
 web_assert(x$docx$content_sha256==web_c2_hash(x$sections),"Word document no longer matches reviewed text.")
 a<-web_query(con,"SELECT * FROM artifacts WHERE id=?a AND project_id=?p AND status='AVAILABLE'",a=x$docx$artifact_id,p=project_id);web_assert(nrow(a)==1&&a$checksum==x$docx$docx_sha256,"Word artifact mismatch.");web_store(settings)$get(a$storage_locator,a$checksum)
 g<-web_query(con,"SELECT id,status,reviewer_id,decided_at,rationale FROM gates WHERE project_id=?p AND version_id=?v AND gate_type='manuscript_review'",p=project_id,v=version_id)
 prior<-nrow(g)==1L && identical(g$status,"APPROVED") && identical(g$reviewer_id,actor) &&
   identical(g$rationale,rationale) && !is.na(g$decided_at)
 web_assert(nrow(g)==1L && (identical(g$status,"PENDING") || prior),"Review gate unavailable.")
 x$stage<-"APPROVED";x$review<-list(actor=actor,at=if(prior)g$decided_at else web_now(),
   rationale=rationale,gate=g$id,meaning="Author-reviewed draft; NOT final release")
 web_c2_mutate(con,settings,actor,project_id,version_id,c,x,x$docx$job_id)
}
web_c2_view <- function(con,settings,actor,project_id,version_id) {
 c<-web_c2_context(con,settings,actor,project_id,version_id);x<-c$packet;x$current<-tryCatch({web_c2_current(con,project_id,x);TRUE},error=function(e)FALSE)
 if(!is.null(x$docx$artifact_id))x$revision_editor<-tryCatch(web_revision_view(con,settings,actor,project_id,x),error=function(e)NULL)
 if(!is.null(x$production) && !is.null(x$docx$artifact_id)) {
   x$precheck<-list(status=if(isTRUE(x$current))"AUTHOR_REVIEW_REQUIRED" else "STALE_UPSTREAM_VERSION",
     label="Automated source/citation preflight; standalone R/40 PASS not claimed",
     issues=if(isTRUE(x$current))list() else list("Evidence or references changed"))
   x$preview<-"The generated editable DOCX is available below. Scientific/author final review remains required."
   return(x)
 }
 x$precheck<-web_c2_check(x,settings$engine_root);if(!x$current){x$precheck$status<-"FAIL";x$precheck$issues<-c(x$precheck$issues,"Stale upstream sources")}
 x$preview<-tryCatch(web_c2_render(x,settings$engine_root,strict=FALSE)$text,error=function(e)"Resolve citation/placeholder issues to preview.")
 x
}

web_c2_invalidate_audit <- function(con,project,actor,new_version,reason) {
 web_c3_invalidate_audit(con,project,actor,new_version)
 a<-web_query(con,"SELECT version_id FROM manuscript_versions WHERE project_id=?p AND stage='APPROVED'",p=project$id)
 if(nrow(a))web_audit(con,project,"DOWNSTREAM_MANUSCRIPT_INVALIDATED",new_version,actor,list(historical_versions=as.list(a$version_id),reason=reason))
}


# RC28.23 | Narrative Evidence-to-Manuscript auto-handoff.
# Intentionally additive: the C1 READY/ROB/canonical R/26 path remains intact.
# Source facts are built only from exact, saved, Human-verified PDF records.

web_c2_handoff_one <- function(x) {
  if (is.null(x)) return("")
  v <- as.character(unlist(x, use.names=FALSE))
  if (!length(v) || is.na(v[[1L]])) "" else trimws(v[[1L]])
}
web_c2_handoff_vec <- function(x) as.character(unlist(x, use.names=FALSE))
web_c2_handoff_narrative <- function(x) {
  is.list(x$narrative_source_finalization) &&
    identical(web_c2_handoff_one(x$narrative_source_finalization$status),"FINALIZED")
}
web_c2_handoff_refs <- function(x) {
  # Only previously verified metadata can be reused without a provider call.
  candidates <- list(x$reference_registry, x$verified_reference_registry,
    x$references, x$pubmed_reference_registry)
  for (z in candidates) {
    rec <- if (is.list(z)) z$records else NULL
    if (!is.list(rec) || !length(rec)) next
    valid <- Filter(function(r) is.list(r) && isTRUE(r$metadata_verified) &&
      grepl("^[0-9]{1,9}$",web_c2_handoff_one(r$pmid)),rec)
    if (length(valid)) {
      ids <- vapply(valid,function(r)web_c2_handoff_one(r$pmid),character(1))
      return(valid[!duplicated(ids)])
    }
  }
  list()
}
# RC28.24: collect *candidate identifiers*, never claim verified citation metadata.
# All candidates are bound to source records for the finalized selected studies.
# R/39 PubMed EFetch remains the sole verification path when saving a plan.
web_c2_handoff_pmid_candidates <- function(x, verified_rows) {
  selected <- unique(vapply(verified_rows,function(r)
    web_c2_handoff_one(r$screening_id),character(1)))
  selected <- selected[nzchar(selected)]
  collections <- c("Author_Final_Review","Reviewer_1","Reviewer_2","AI_Screening")
  sources <- if(is.list(x$tables)) x$tables else list()
  candidates <- list()
  conflicts <- list()
  normalize <- function(v) {
    p <- web_c2_handoff_one(v)
    p <- sub("^PMID[[:space:]]*:[[:space:]]*","",p,ignore.case=TRUE)
    if(!grepl("^[0-9]{1,9}$",p)) return("")
    p
  }
  for(sid in selected) {
    ids <- character()
    names_found <- character()
    for(n in collections) {
      rs <- sources[[n]]
      if(!is.list(rs)) next
      for(rec in rs) {
        if(!is.list(rec) ||
           !identical(web_c2_handoff_one(rec$screening_id),sid)) next
        p <- normalize(rec$pmid)
        if(!nzchar(p)) next
        ids <- c(ids,p)
        names_found <- c(names_found,n)
      }
    }
    unique_ids <- unique(ids)
    if(length(unique_ids)>1L) {
      conflicts[[length(conflicts)+1L]] <- list(screening_id=sid,
        reason="CONFLICTING_SOURCE_PMIDS",sources=unique(names_found))
      next
    }
    if(length(unique_ids)==1L)
      candidates[[length(candidates)+1L]] <- list(screening_id=sid,
        pmid=unique_ids[[1L]],source_tables=unique(names_found),
        metadata_verified=FALSE,review_relevance="UNREVIEWED")
  }
  list(records=candidates,conflicts=conflicts)
}

web_c2_handoff_validate <- function(con,settings,actor,project_id,version_id) {
  c <- web_b2_context(con,settings,actor,project_id,version_id)
  x <- c$packet
  web_assert(identical(web_c2_handoff_one(x$stage),"EXTRACTION") &&
    identical(web_c2_handoff_one(c$row$stage),"EXTRACTION"),
    "Use the exact saved narrative Evidence Version.","USER_ACTION_REQUIRED")
  web_assert(identical(web_c2_handoff_one(x$final_study_selection$status),"HUMAN_CONFIRMED") &&
    identical(web_c2_handoff_one(x$synthesis_plan$status),"HUMAN_CONFIRMED") &&
    identical(web_c2_handoff_one(x$synthesis_execution$status),"AUTO_PREPARED"),
    "Narrative selection, synthesis plan and execution must be Human-confirmed.","USER_ACTION_REQUIRED")
  digest_value <- web_c2_handoff_one(x$synthesis_execution$source_digest)
  web_assert(nzchar(digest_value) &&
    identical(digest_value,web_b2_synthesis_source_digest(x)),
    "Narrative synthesis source is stale.","USER_ACTION_REQUIRED")
  batch <- x$narrative_batch_verification
  final <- x$narrative_source_finalization
  web_assert(is.list(batch) && is.list(final) &&
    identical(web_c2_handoff_one(batch$status),"HUMAN_VERIFIED_EDITABLE") &&
    identical(web_c2_handoff_one(final$status),"FINALIZED") &&
    identical(web_c2_handoff_one(batch$source_digest),digest_value) &&
    identical(web_c2_handoff_one(final$source_digest),digest_value) &&
    grepl("NOT_CANONICAL_R26",web_c2_handoff_one(final$scope),fixed=TRUE) &&
    nzchar(web_c2_handoff_one(batch$actor)) &&
    nzchar(web_c2_handoff_one(final$actor)),
    "The exact narrative result set lacks Human verification/finalization.",
    "USER_ACTION_REQUIRED")
  rows <- x$synthesis_execution$rows
  web_assert(is.list(rows) && length(rows)>0L,
    "Final narrative result set is empty.","USER_ACTION_REQUIRED")
  ids <- vapply(rows,function(r)web_c2_handoff_one(r$result_id),character(1))
  batch_ids <- web_c2_handoff_vec(batch$result_ids)
  web_assert(all(nzchar(ids)) && !anyDuplicated(ids) &&
    length(batch_ids)==length(ids) && !anyDuplicated(batch_ids) &&
    setequal(ids,batch_ids),
    "Final batch verification does not match the exact synthesis results.",
    "USER_ACTION_REQUIRED")
  selected <- web_c2_handoff_vec(x$final_study_selection$study_ids)
  web_assert(length(selected)>0L && !anyDuplicated(selected),
    "Final study selection is invalid.","USER_ACTION_REQUIRED")
  verified <- vector("list",length(rows))
  checked <- new.env(parent=emptyenv())
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    rid <- ids[[i]]
    sid <- web_c2_handoff_one(r$study_id)
    screening <- web_c2_handoff_one(r$screening_id)
    web_assert(nzchar(sid) && sid %in% selected && nzchar(screening),
      "An excluded/unknown study reached finalized narrative evidence.",
      "USER_ACTION_REQUIRED")
    d <- web_b2_nr_detail(x,rid)
    rev <- web_b2_nr_review_get(x,rid,digest_value)
    web_assert(is.list(d) && is.list(rev) &&
      identical(web_c2_handoff_one(d$row$study_id),sid) &&
      identical(web_c2_handoff_one(d$row$screening_id),screening) &&
      identical(web_c2_handoff_one(rev$result_id),rid) &&
      identical(web_c2_handoff_one(rev$study_id),sid) &&
      identical(web_c2_handoff_one(rev$screening_id),screening) &&
      identical(web_c2_handoff_one(rev$pdf_sha256),d$checksum) &&
      identical(web_c2_handoff_one(rev$source_digest),digest_value) &&
      isTRUE(rev$batch_verification) &&
      nzchar(web_c2_handoff_one(rev$verified_by)),
      "Every manuscript result needs its exact PDF-bound Human decision.",
      "USER_ACTION_REQUIRED")
    authors <- Filter(function(z)
      identical(web_c2_handoff_one(z$screening_id),screening),
      x$tables$Author_Final_Review)
    web_assert(length(authors)==1L &&
      identical(web_c2_handoff_one(authors[[1L]]$.pdf_sha256),d$checksum),
      "Original report registration does not match narrative result.",
      "USER_ACTION_REQUIRED")
    rel <- web_c2_handoff_one(authors[[1L]]$.pdf_relative)
    web_assert(nzchar(rel) && rel %in% names(c$version$manifest) &&
      identical(web_c2_handoff_one(c$version$manifest[[rel]]),d$checksum),
      "Source report is not present in the immutable Evidence manifest.",
      "USER_ACTION_REQUIRED")
    if (!exists(screening,envir=checked,inherits=FALSE)) {
      pdf <- web_inside(file.path(c$version$root,rel),c$version$root)
      web_assert(identical(web_sha(pdf),d$checksum),
        "Saved source PDF checksum mismatch.","USER_ACTION_REQUIRED")
      assign(screening,TRUE,envir=checked)
    }
    decision <- web_c2_handoff_one(rev$decision)
    web_assert(decision %in% c("CONFIRMED","CORRECTED","NOT_REPORTED") &&
      nzchar(web_c2_handoff_one(rev$source_location)) &&
      nzchar(web_c2_handoff_one(rev$source_evidence)),
      "Human PDF review decision or source citation is missing.",
      "USER_ACTION_REQUIRED")
    summary <- web_b2_nr_summary(rev)
    if (identical(decision,"NOT_REPORTED")) {
      num <- rev$values
      if (!is.list(num)) num <- list()
      web_assert(!nzchar(web_c2_handoff_one(rev$reported_result)) &&
        !any(vapply(num,function(v)nzchar(web_c2_handoff_one(v)),logical(1))) &&
        !nzchar(summary),
        "NOT_REPORTED cannot carry numerical claims.","USER_ACTION_REQUIRED")
    } else {
      web_assert(nzchar(summary),
        "A Human-confirmed narrative numerical result has no source-bound value.",
        "USER_ACTION_REQUIRED")
    }
    name <- web_c2_handoff_one(r$original_outcome)
    common <- web_c2_handoff_one(r$common_variable)
    pdf_time <- web_c2_handoff_one(r$reported_timepoint)
    timepoint <- web_c2_handoff_one(r$analysis_timepoint)
    web_assert(nzchar(name) && nzchar(common) &&
      identical(name,web_c2_handoff_one(d$pdf$reported_name)) &&
      identical(pdf_time,web_c2_handoff_one(d$pdf$reported_timepoint)) &&
      nzchar(timepoint),
      "Original endpoint or timepoint no longer matches its PDF inventory.",
      "USER_ACTION_REQUIRED")
    au <- authors[[1L]]
    choose_first <- function(keys) {
      for(k in keys) {
        value <- web_c2_handoff_one(au[[k]])
        if(nzchar(value))return(value)
      }
      ""
    }
    author <- choose_first(c("first_author","author","study_author","authors","publication_author"))
    year <- choose_first(c("publication_year","year","pub_year"))
    article_title <- choose_first(c("article_title","study_title","title"))
    label <- if (nzchar(author) && nzchar(year)) paste(author,year) else
      if (nzchar(author)) author else if(nzchar(article_title)) article_title else
      "[AUTHOR REQUIRED study identification]"
    key <- if (decision=="NOT_REPORTED") "" else
      paste0("N",substr(digest::digest(rid,algo="sha256"),1L,24L))
    verified[[i]] <- list(result_id=rid,study_id=sid,screening_id=screening,
      study_label=label,analysis_group=web_c2_handoff_one(r$analysis_group),
      common_variable=common,original_outcome=name,
      reported_timepoint=pdf_time,analysis_timepoint=timepoint,
      source_location=web_c2_handoff_one(rev$source_location),
      source_evidence=web_c2_handoff_one(rev$source_evidence),
      pdf_relative=rel,pdf_sha256=d$checksum,source_digest=digest_value,
      decision=decision,verified_by=web_c2_handoff_one(rev$verified_by),
      verified_at=web_c2_handoff_one(rev$verified_at),
      fact_id=key,numeric_summary=summary)
  }
  keys <- vapply(verified,function(z)z$fact_id,character(1))
  keys <- keys[nzchar(keys)]
  web_assert(!anyDuplicated(keys),"Narrative source-fact token collision.")
  sets <- x$synthesis_execution$sets
  if (!is.list(sets)) sets <- list()
  modes <- vapply(x$synthesis_plan$decisions,function(z)
    web_c2_handoff_one(z$mode),character(1))
  web_assert(length(modes)>0 && all(modes=="NARRATIVE"),
    "Mixed/quantitative results require their own approved analysis route; no canonical R/26 approval is inferred.",
    "USER_ACTION_REQUIRED")
  list(context=c,rows=verified,sets=sets,source_digest=digest_value,
    unresolved_rob=isTRUE(final$unresolved_rob),
    source_version=version_id,refs=web_c2_handoff_refs(x))
}
web_c2_narrative_catalog <- function(con,settings,actor,project_id,version_id) {
  a <- web_c2_handoff_validate(con,settings,actor,project_id,version_id)
  sets <- a$sets
  ids <- vapply(sets,function(s)web_c2_handoff_one(s$analysis_group),character(1))
  if (!length(ids)) ids <- unique(vapply(a$rows,function(r)r$analysis_group,character(1)))
  ids <- ids[nzchar(ids)]
  web_assert(length(ids)>0L && !anyDuplicated(ids),
    "Narrative analysis groups are not uniquely defined.","USER_ACTION_REQUIRED")
  groups <- vapply(ids,function(id){
    rows <- Filter(function(r)identical(r$analysis_group,id),a$rows)
    web_assert(length(rows)>0L,"Narrative analysis group has no verified source rows.","USER_ACTION_REQUIRED")
    paste0(rows[[1L]]$common_variable," — ",rows[[1L]]$analysis_timepoint)
  },character(1))
  cfg <- yaml::read_yaml(file.path(a$context$version$root,"config/project_config.yml"))
  pr <- web_query(con,"SELECT title FROM projects WHERE id=?p",p=project_id)
  title <- web_c2_handoff_one(cfg$project$title)
  if (!nzchar(title)) title <- web_c2_handoff_one(pr$title)
  references <- a$refs
  source_candidates <- web_c2_handoff_pmid_candidates(a$context$packet,a$rows)
  verified_pmids <- vapply(references,function(r)
    web_c2_handoff_one(r$pmid),character(1))
  candidate_pmids <- vapply(source_candidates$records,function(r)
    web_c2_handoff_one(r$pmid),character(1))
  # Candidate PMIDs still require PubMed R/39 verification on plan save.
  # Inclusion in this field is NOT an approval of relevance for the manuscript.
  auto_pmids <- unique(c(verified_pmids,candidate_pmids))
  list(route="NARRATIVE",evidence_version=version_id,source_digest=a$source_digest,
    analyses=data.frame(version=rep("Narrative",length(ids)),
      version_id=ids,label=groups,stringsAsFactors=FALSE),
    artifacts=data.frame(id=character(),logical_name=character(),
      analysis_version=character(),stringsAsFactors=FALSE),
    title=title,language="English",
    pmids=auto_pmids,verified_pmids=as.list(verified_pmids),
    candidate_pmids=as.list(setdiff(candidate_pmids,verified_pmids)),
    conflicting_source_pmids=source_candidates$conflicts,
    candidate_provenance=source_candidates$records,
    rationale="Report the finalized, Human-verified source-indexed narrative synthesis; retain the original PDF outcome definitions and timepoints. No pooled effects were calculated.",
    unresolved_rob=a$unresolved_rob,
    selected_results=length(a$rows),verified_reference_records=references)
}
web_c2_narrative_plan <- function(con,settings,actor,project_id,version_id,spec) {
  web_assert(is.list(spec) && setequal(names(spec),
    c("title","language","analyses","artifacts","pmids","rationale")),
    "Invalid manuscript plan fields.","VALIDATION_FAILED")
  for (k in c("title","language","rationale"))
    web_assert(nzchar(trimws(web_c2_text(spec[[k]],settings$engine_root))),
      paste("Specify manuscript",k),"USER_ACTION_REQUIRED")
  a <- web_c2_handoff_validate(con,settings,actor,project_id,version_id)
  cat <- web_c2_narrative_catalog(con,settings,actor,project_id,version_id)
  ids <- unique(web_c2_handoff_vec(spec$analyses))
  web_assert(length(ids)==nrow(cat$analyses) &&
    setequal(ids,cat$analyses$version_id),
    "Keep every Human-approved narrative analysis group in the manuscript plan.",
    "USER_ACTION_REQUIRED")
  web_assert(length(web_c2_handoff_vec(spec$artifacts))==0L,
    "No canonical forest plot or result table exists for this narrative-only route.",
    "VALIDATION_FAILED")
  pmids <- unique(web_c2_handoff_vec(spec$pmids))
  pmids <- pmids[nzchar(pmids)]
  web_assert(length(pmids)<=100L && all(grepl("^[0-9]{1,9}$",pmids)),
    "Supply valid verified PubMed identifiers, up to 100.","VALIDATION_FAILED")
  cached <- a$refs
  cached_ids <- vapply(cached,function(r)web_c2_handoff_one(r$pmid),character(1))
  from_cache <- cached[match(intersect(pmids,cached_ids),cached_ids)]
  missing <- setdiff(pmids,cached_ids)
  fetched <- if(length(missing)) web_c2_fetch_references(settings$engine_root,missing,settings=settings)$records else list()
  refs <- list(records=c(from_cache,fetched),provider="Saved verified registry / PubMed EFetch",
    verified_at=web_now())
  if(length(refs$records)) refs <- web_c2_normalize_refs(refs,settings$engine_root)
  root <- tempfile("narrative-manuscript-plan-",settings$data_root)
  dir.create(root)
  on.exit(unlink(root,recursive=TRUE))
  web_copy_version(con,settings,actor,project_id,version_id,root)
  cfg <- yaml::read_yaml(file.path(root,"config/project_config.yml"))
  policy <- cfg$analysis$manuscript
  limits <- list(total=policy$minimum_unique_references %||b2% 25L,
    introduction=policy$minimum_intro_references %||b2% 8L,
    discussion=policy$minimum_discussion_references %||b2% 15L)
  web_assert(all(vapply(limits,function(v)is.numeric(v)&&length(v)==1L&&
    is.finite(v)&&v>=1&&v==as.integer(v),logical(1))),
    "Canonical manuscript reference-count policy is invalid.")
  # Verified PMID library and topic-filtered candidates are handled automatically.
  # Author evaluates evidence/citation suitability in the complete Word draft.
  # A reference-library count is never misrepresented as manuscript citations.
  discovery <- list(status="NOT_REQUIRED",review_status="EXISTING_LIBRARY",
    reason="Previously verified references reused; no separate pre-draft Human reference review",
    added=0L,verified_total=length(refs$records),candidates=refs$records,
    protected_pmids=as.list(vapply(refs$records,function(r)web_c2_handoff_one(r$pmid),character(1))),
    queries=list())
  if (length(refs$records) < as.integer(limits$total)) {
    source_records <- refs$records
    d <- web_c2_ref_discover(cfg,refs$records,settings$engine_root,
      minimum=as.integer(limits$total),settings=settings)
    refs$records <- c(refs$records,d$records)
    refs <- web_c2_normalize_refs(refs,settings$engine_root)
    web_assert(length(refs$records)>=as.integer(limits$total),
      "Insufficient unique PubMed references after DOI deduplication; no manuscript plan saved.",
      "USER_ACTION_REQUIRED")
    discovery <- d$report
    discovery$candidates <- c(source_records,d$candidates)
    discovery$protected_pmids <- as.list(vapply(source_records,function(r)web_c2_handoff_one(r$pmid),
      character(1)))
    discovery$verified_total <- length(refs$records)
    discovery$review_status <- "AUTOMATED_VERIFIED_FOR_DRAFT"
    discovery$human_review_required <- FALSE
    discovery$author_review_stage <- "FINAL_MANUSCRIPT"
  }
  frozen <- list(route="NARRATIVE",source_version=version_id,
    source_digest=a$source_digest,source_manifest=a$context$version$manifest,
    human_final_selection=web_c2_9cd_freeze_selection(
      a$context$packet,version_id,a$source_digest,a$context$version$manifest),
    finalization_scope="NARRATIVE_ANALYSIS_LOCKED; NOT_CANONICAL_R26",
    unresolved_rob=a$unresolved_rob,
    analysis_groups=as.list(ids),rows=a$rows,
    publication_status="WORKING_DRAFT; HUMAN_AUTHOR_REVIEW_REQUIRED")
  source <- list(route="NARRATIVE",project_version=version_id,
    evidence_version=version_id,project=cfg$project,
    research_question=cfg$research_question,protocol=cfg$protocol,
    registration=cfg$registration,search=cfg$search,screening=cfg$screening,
    analyses=list(),artifacts=list(),narrative=frozen,
    references=refs,reference_policy=limits,reference_discovery=discovery)
  source$facts <- web_c2_facts(source,settings$engine_root)
  watermark <- web_query(con,"SELECT MAX(v.version) AS n FROM analysis_versions a JOIN project_versions v ON v.id=a.version_id WHERE a.project_id=?p",p=project_id)$n
  if(!length(watermark) || is.na(watermark[[1L]])) watermark <- 0L
  x <- list(manuscript_id=web_id(),evidence_version=version_id,
    analysis_watermark=watermark[[1L]],stage="PLAN",
    review_mode="FINAL_MANUSCRIPT_ONLY",spec=spec,
    source=source,sections=list(),created_at=web_now())
  x$source$statistical_methods<-tryCatch(web_stat_draft_source(con,settings,actor,project_id,x,a$context$packet,a$context$version),
    error=function(e)list(status="NOT_ASSESSED",gaps=list("source_verification"),records=list()))
  x$source$facts<-c(x$source$facts,web_stat_methods_facts(x$source$statistical_methods,version_id))
  web_c2_save(con,settings,actor,project_id,version_id,root,x,new_plan=TRUE)
}

# RC28.25.3: Reference Discovery runs synchronously in the Shiny App process,
# unlike canonical scientific jobs which inherit SRMA_SCIENTIFIC_LIBRARY via
# their private R subprocess. Temporarily expose that approved R library to
# R/39's PubMed XML parser. Restore App library search paths on EVERY exit.
# No package installation, renv.lock change, or scientific file mutation.
web_c2_reference_library_scope <- function(settings) {
  # Deliberately no .libPaths() manipulation or loadNamespace() in the App.
  # Loaded Shiny rlang may be older than isolated Scientific R/39 httr2.
  # The child Rscript preflight verifies all dependencies safely.
  explicit<-trimws(Sys.getenv("SRMA_SCIENTIFIC_LIBRARY",unset=""))
  configured<-settings$engine_library
  configured<-if(!is.null(configured)&&length(configured)==1L&&
    !is.na(configured[[1L]]))trimws(as.character(configured[[1L]])) else ""
  library<-if(nzchar(explicit))explicit else configured
  web_assert(nzchar(library)&&dir.exists(library),
    "Configure a valid scientific library for isolated R/39 PubMed verification.",
    "DEPENDENCY_FAILED")
  invisible(function() invisible(TRUE))
}

# Dispatch only finalized narrative Evidence to the new route. The existing
# canonical manuscript catalog/plan retain all READY/ROB/R26 guards unchanged.
web_c2_catalog_canonical <- web_c2_catalog
web_c2_plan_canonical <- web_c2_plan
web_c2_catalog <- function(con,settings,actor,project_id,version_id) {
  c <- web_b2_context(con,settings,actor,project_id,version_id)
  if(web_c2_handoff_narrative(c$packet))
    return(web_c2_narrative_catalog(con,settings,actor,project_id,version_id))
  web_c2_catalog_canonical(con,settings,actor,project_id,version_id)
}
# RC28.25.9BCD.2: an already generated manuscript may be rebuilt under the
# current publication engine, but NEVER overwrite the original source or DOCX.
# Reuse the frozen verified evidence, analysis, PMID library and asset hashes.
# The existing manuscript_plan API performs the action; no new endpoint, DB
# column, scientific execution or PubMed verification is needed to reissue.
web_c2_reissue_plan <- function(con,settings,actor,project_id,version_id,spec) {
  c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE)
  x<-c$packet
  web_assert(x$stage %in% c("REVIEW_REQUIRED","APPROVED") &&
    !is.null(x$docx$artifact_id) && length(x$docx$artifact_id)>0L,
    "Choose an existing manuscript with a registered DOCX to create a new version.",
    "USER_ACTION_REQUIRED")
  web_assert(is.list(spec) && identical(web_c2_hash(spec),web_c2_hash(x$spec)),
    "Regeneration must use the EXACT frozen manuscript specification. Select the original Evidence Version for changed inputs.",
    "STALE_UPSTREAM_VERSION")
  web_assert(length(x$source$references$records)>0L,
    "The existing manuscript has no verified references.","USER_ACTION_REQUIRED")
  # No pre-draft Human reference selection; this is an explicit publication
  # governance policy. Verify EVERY reused PMID record instead of silently
  # treating legacy HUMAN_SELECTION_REQUIRED as a scientific approval.
  d<-x$source$reference_discovery
  web_assert(all(vapply(x$source$references$records,web_c2_ref_valid,logical(1))),
    "Stored reference metadata is missing or unverified; auto-reuse refused.",
    "CITATION_VALIDATION_FAILED")
  web_assert(identical(web_c2_hash(x$source),x$source_sha256) &&
    identical(web_c2_hash(x$source$references),x$reference_sha256),
    "Saved manuscript sources changed. No regeneration was queued.",
    "STALE_UPSTREAM_VERSION")
  # web_c2_context(...,current=TRUE,latest=TRUE) already checks same-project,
  # newest immutable manuscript, current Evidence and analysis watermark.
  y<-x
  if(identical(x$source$route,"NARRATIVE")) {
    ev<-web_b2_context(con,settings,actor,project_id,x$evidence_version)
    web_assert(identical(web_c2_hash(ev$version$manifest),
      web_c2_hash(x$source$narrative$source_manifest)),
      "Human Finalized Evidence manifest differs from the frozen manuscript source.",
      "STALE_UPSTREAM_VERSION")
    y$source$narrative$human_final_selection<-web_c2_9cd_freeze_selection(
      ev$packet,x$evidence_version,x$source$narrative$source_digest,
      ev$version$manifest)
  }
  y$manuscript_id<-web_id()
  y$stage<-"PLAN"
  y$review_mode<-"FINAL_MANUSCRIPT_ONLY"
  y$created_at<-web_now()
  y$sections<-list()
  y$docx<-NULL
  # Keep an exact NULL entry: $review otherwise partially matches review_mode.
  y["review"]<-list(NULL)
  y$production<-NULL
  y$generation<-NULL
  y$plan_version<-NULL
  y$precheck<-NULL
  y$preview<-NULL
  y$current<-NULL
  y$regeneration<-list(parent_manuscript_version=version_id,
    parent_docx_sha256=x$docx$docx_sha256,
    source_sha256=x$source_sha256,
    reference_sha256=x$reference_sha256,
    policy="AUTOMATED_REFERENCES_FINAL_AUTHOR_REVIEW",
    prior_reference_review_status=if(is.list(d)) d$review_status else NULL,
    reference_policy="VERIFIED_PMID_METADATA_AND_AUTOMATED_CITATION_VALIDATION",
    author_review_stage="FINAL_MANUSCRIPT",
    at=web_now())
  root<-tempfile("manuscript-regeneration-",tmpdir=settings$data_root)
  dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  web_copy_version(con,settings,actor,project_id,version_id,root)
  # Historical results are versioned in the datastore. Do not carry their
  # generated DOCX/result files into the new, not-yet-executed PLAN snapshot.
  unlink(file.path(root,"results","web_manuscript"),recursive=TRUE)
  web_c2_save(con,settings,actor,project_id,version_id,root,y,new_plan=TRUE)
}

web_c2_plan <- function(con,settings,actor,project_id,version_id,spec) {
  # An existing manuscript PLAN can be revised with explicit per-PMID choices,
  # using the same authorized, versioned manuscript_plan operation.
  plan <- web_query(con,"SELECT version_id,stage FROM manuscript_versions WHERE project_id=?p AND version_id=?v",
    p=project_id,v=version_id)
  if(nrow(plan)==1L) {
    if(identical(as.character(plan$stage[[1L]]),"PLAN"))
      return(web_c2_ref_review_plan(con,settings,actor,project_id,version_id,spec))
    return(web_c2_reissue_plan(con,settings,actor,project_id,version_id,spec))
  }
  # Ensure xml2 is resolved from the same explicit scientific library as
  # the worker; do not depend on the Shiny process' ambient renv activation.
  restore_libraries <- web_c2_reference_library_scope(settings)
  on.exit(restore_libraries(),add=TRUE)
  # Catch R/39/Scientific library incompatibility before any PubMed request.
  web_c2_pubmed_isolated(settings$engine_root,settings,"preflight")
  c <- web_b2_context(con,settings,actor,project_id,version_id)
  if(web_c2_handoff_narrative(c$packet))
    return(web_c2_narrative_plan(con,settings,actor,project_id,version_id,spec))
  web_c2_plan_canonical(con,settings,actor,project_id,version_id,spec)
}



# RC28.25.2 -- topic-agnostic, provenance-bounded discovery and explicit
# candidate-level Human selection. R/38, R/39, R/40 and R/37 remain frozen.
web_c2_ref_words <- function(...) {
  value <- paste(unlist(list(...),recursive=TRUE,use.names=FALSE),collapse=" ")
  if(!nzchar(trimws(value)))return(character())
  words <- unlist(strsplit(tolower(value),"[^[:alnum:]]+",perl=TRUE),use.names=FALSE)
  stopwords <- c("the","and","for","with","from","that","this","into","among",
    "between","versus","using","based","during","after","before","study","studies",
    "review","systematic","meta","analysis","evidence","effect","effects","outcomes",
    "outcome","clinical","patients","patient","adult","adults","comparison",
    "comparative","treatment","evaluating","assessment","approach","era","pure",
    "dedicated","device","devices","versus","trial","trials","methods","results",
    "relative","impact","risk","association","current","contemporary","evaluation",
    "primary","secondary","overall","observational","controlled","randomized")
  unique(words[nchar(words)>=4L & !(words %in% stopwords) & !grepl("^[0-9]+$",words)])
}
web_c2_ref_config_text <- function(cfg,keys) {
  node <- cfg
  for(key in keys) {
    if(!is.list(node) || is.null(node[[key]]))return("")
    node <- node[[key]]
  }
  vals <- unlist(node,recursive=TRUE,use.names=FALSE)
  if(!length(vals))return("")
  vals <- as.character(vals[!is.na(vals)])
  paste(vals[nzchar(trimws(vals))],collapse=" ")
}
web_c2_ref_concept <- function(text,max_words=4L) {
  # A clinical/epidemiologic concept is kept as an anchored phrase rather than
  # choosing the first generic two words of a long study title.
  value <- trimws(as.character(text))
  if(!length(value) || is.na(value[[1L]]) || !nzchar(value[[1L]]))return("")
  value <- value[[1L]]
  parts <- unlist(strsplit(value,"(?i)\\b(?:versus|vs\\.?|compared with|among|for|with|in patients with|in|and)\\b|[,;:()]",perl=TRUE))
  options <- lapply(parts,web_c2_ref_words)
  options <- Filter(function(v)length(v)>=2L,options)
  if(!length(options))options <- list(web_c2_ref_words(value))
  if(!length(options[[1L]]))return("")
  # Prefer a succinct distinct clause. If there are no structured dimensions,
  # trailing disease/construct phrases often follow 'for' in review titles.
  best <- options[[which.max(vapply(options,function(v)min(length(v),max_words),numeric(1)))]]
  paste(head(best,max_words),collapse=" ")
}
web_c2_ref_dimensions <- function(cfg) {
  get <- function(...)web_c2_ref_config_text(cfg,c(...))
  population <- paste(c(get("research_question","population"),
    get("eligibility","population"),get("condition")),collapse=" ")
  main <- c(get("research_question","intervention"),get("research_question","exposure"),
    get("research_question","index_test"),get("research_question","prognostic_factor"),
    get("research_question","outcomes"),get("research_question","comparator"))
  main <- main[nzchar(trimws(main))]
  title <- get("project","title")
  if(!nzchar(title))title <- get("research_question","question")
  if(!nzchar(trimws(population))) {
    # Prefer the clause after 'for' in review titles, otherwise a title clause.
    chunks <- strsplit(title,"(?i)\\bfor\\b",perl=TRUE)[[1L]]
    population <- if(length(chunks)>=2L)chunks[[2L]] else title
  }
  primary <- web_c2_ref_concept(population,4L)
  secondary <- if(length(main))web_c2_ref_concept(main[[1L]],4L) else ""
  if(!nzchar(secondary))secondary <- web_c2_ref_concept(title,4L)
  # Avoid two identical anchors; use a distinct secondary title clause.
  if(identical(primary,secondary)) {
    alt <- web_c2_ref_words(title)
    alt <- setdiff(alt,web_c2_ref_words(primary))
    secondary <- paste(head(alt,3L),collapse=" ")
  }
  list(primary=primary,secondary=secondary,
       terms=unique(c(web_c2_ref_words(primary),web_c2_ref_words(secondary))),
       title=title)
}
web_c2_ref_queries <- function(cfg,year=as.integer(format(Sys.Date(),"%Y"))) {
  policy <- cfg$analysis$manuscript
  configured <- if(is.list(policy))policy$reference_discovery_queries else NULL
  if(is.character(configured) && length(configured)) {
    q <- unique(trimws(configured))
    web_assert(all(nzchar(q)) && all(nchar(q)<=800L) && length(q)<=8L,
      "Provide 1-8 bounded, nonempty project-specific PubMed queries.","VALIDATION_FAILED")
    return(data.frame(query_id=paste0("Q",seq_along(q)),query=q,
      purpose=rep("PROJECT_CONFIG",length(q)),stringsAsFactors=FALSE))
  }
  dim <- web_c2_ref_dimensions(cfg)
  web_assert(length(web_c2_ref_words(dim$primary))>=2L,
    "A topic-specific literature search requires a population/condition/construct anchor; configure reference_discovery_queries if necessary.",
    "USER_ACTION_REQUIRED")
  quote <- function(value)paste0('"',gsub('"','',value,fixed=TRUE),'"[Title/Abstract]')
  core <- quote(dim$primary)
  short_terms <- web_c2_ref_words(dim$primary)
  broad <- quote(paste(tail(short_terms,min(length(short_terms),2L)),collapse=" "))
  secondary <- if(nzchar(dim$secondary))quote(dim$secondary) else ""
  # A broad query is allowed only if it is still anchored by the topic construct.
  combined <- if(nzchar(secondary))paste0("(",core,") AND (",secondary,")") else core
  recent_year <- max(1900L,year-8L)
  q <- c(combined,
    paste0("(",core,") AND (systematic review[Publication Type] OR meta-analysis[Publication Type])"),
    paste0("(",core,") AND (guideline[Publication Type] OR practice guideline[Publication Type] OR consensus[Title/Abstract])"),
    paste0("(",combined,") AND (",recent_year,":",year,"[Date - Publication])"),
    paste0("(",broad,") AND (cohort[Title/Abstract] OR trial[Title/Abstract] OR observational[Title/Abstract])"),
    broad)
  q <- unique(q[nzchar(q)])
  web_assert(length(q)>0L && length(q)<=8L && all(nchar(q)<=800L),
    "Invalid bounded PubMed reference search plan.","VALIDATION_FAILED")
  data.frame(query_id=paste0("Q",seq_along(q)),query=q,
    purpose=c("CORE","REVIEWS","GUIDELINES","RECENT","PRIMARY","LANDMARK")[seq_along(q)],
    stringsAsFactors=FALSE)
}
web_c2_ref_valid <- function(r) {
  is.list(r) && isTRUE(r$metadata_verified) &&
    grepl("^[0-9]{1,9}$",web_c2_handoff_one(r$pmid)) &&
    all(vapply(c("title","authors","year","journal"),function(k)
      nzchar(web_c2_handoff_one(r[[k]])),logical(1)))
}
web_c2_ref_relevance <- function(records,cfg) {
  dim <- web_c2_ref_dimensions(cfg)
  p <- web_c2_ref_words(dim$primary)
  s <- web_c2_ref_words(dim$secondary)
  lapply(records,function(r) {
    title <- web_c2_ref_words(r$title)
    abstract <- web_c2_ref_words(r$abstract)
    all <- unique(c(title,abstract))
    m_primary <- sum(p %in% all)
    m_second <- sum(s %in% all)
    # Both topic and a distinctive detail are required when possible;
    # metadata verification is not equivalent to evidence-level relevance.
    enough_primary <- length(p)>=2L && m_primary>=min(2L,length(p))
    enough_secondary <- !length(s) || m_second>=1L
    ptype <- tolower(web_c2_handoff_one(r$publication_types))
    # A consensus/guideline may legitimately address the central construct
    # without discussing the secondary intervention/test by name.
    authoritative <- grepl("guideline|consensus|practice guideline",ptype)
    eligible <- enough_primary && (enough_secondary || authoritative || m_primary>=3L)
    score <- as.integer(5L*sum(p %in% title)+3L*sum(s %in% title)+
      2L*sum(p %in% abstract)+sum(s %in% abstract)+
      if(grepl("guideline|systematic review|meta-analysis",ptype))2L else 0L)
    list(eligible=eligible,score=score,primary_hits=m_primary,
      secondary_hits=m_second,reason=if(eligible)"TOPIC_MATCH_REQUIRES_HUMAN_REVIEW" else
        "INSUFFICIENT_TOPIC_MATCH")
  })
}
web_c2_ref_doi <- function(v) {
  x <- tolower(trimws(web_c2_handoff_one(v)))
  x <- sub("^https?://(?:dx\\.)?doi\\.org/","",x,perl=TRUE)
  sub("^doi:","",x)
}
web_c2_ref_discover <- function(cfg,existing,engine,minimum=25L,
    search_fn=NULL,fetch_fn=NULL,now=web_now(),settings=NULL) {
  web_assert(is.list(existing) && all(vapply(existing,web_c2_ref_valid,logical(1))),
    "Existing reference library is not PubMed-verified.","CITATION_VALIDATION_FAILED")
  minimum <- as.integer(minimum)
  web_assert(length(minimum)==1L && !is.na(minimum) && minimum>=1L && minimum<=100L,
    "Invalid bounded manuscript reference minimum.","VALIDATION_FAILED")
  policy <- cfg$analysis$manuscript
  if(!is.list(policy))policy <- list()
  target <- suppressWarnings(as.integer(policy$literature_selected_target))
  if(length(target)!=1L || is.na(target))target <- 35L
  target <- min(100L,max(minimum,target))
  cap <- suppressWarnings(as.integer(policy$literature_candidate_target))
  if(length(cap)!=1L || is.na(cap))cap <- 70L
  cap <- min(200L,max(target,cap))
  queries <- web_c2_ref_queries(cfg)
  if(is.null(search_fn))search_fn <- function(q,n) {
    reply<-web_c2_pubmed_isolated(engine,settings,"search",
      list(query=q,retmax=as.integer(n)))
    as.character(unlist(reply$ids,use.names=FALSE))
  }
  if(is.null(fetch_fn))fetch_fn <- function(ids) {
    reply<-web_c2_pubmed_isolated(engine,settings,"fetch",list(pmids=as.list(ids)))
    reply$records
  }
  existing_ids <- unique(vapply(existing,function(r)web_c2_handoff_one(r$pmid),character(1)))
  existing_dois <- vapply(existing,function(r)web_c2_ref_doi(r$doi),character(1))
  pool <- character()
  query_ids <- list()
  query_buckets <- vector("list",nrow(queries))
  logs <- list()
  per_query <- max(10L,min(50L,as.integer(ceiling(cap/nrow(queries)))*2L))
  for(i in seq_len(nrow(queries))) {
    q <- queries$query[[i]]
    ids <- tryCatch(search_fn(q,per_query),error=function(err)err)
    if(inherits(ids,"error")) {
      logs[[length(logs)+1L]] <- list(query_id=queries$query_id[[i]],status="FAILED",
        count=0L,reason=substr(conditionMessage(ids),1L,200L))
      next
    }
    ids <- as.character(unlist(ids,use.names=FALSE))
    ids <- unique(ids[!is.na(ids) & grepl("^[0-9]{1,9}$",ids)])
    for(id in ids) query_ids[[id]] <- unique(c(query_ids[[id]],queries$query_id[[i]]))
    query_buckets[[i]] <- ids
    logs[[length(logs)+1L]] <- list(query_id=queries$query_id[[i]],status="PASS",
      count=length(ids),reason="")
  }
  # Round-robin across search purposes (context, reviews, guidelines,
  # recent, primary, older) rather than allowing the first broad query to
  # occupy every candidate slot. Preserve PMID/query provenance.
  largest <- if(length(query_buckets))max(lengths(query_buckets)) else 0L
  if(largest>0L)for(j in seq_len(largest)) {
    values <- vapply(query_buckets,function(ids)
      if(length(ids)>=j)ids[[j]] else "",character(1))
    pool <- unique(c(pool,values[nzchar(values)]))
  }
  pool <- head(setdiff(pool,existing_ids),cap)
  web_assert(length(pool)>0L,
    "PubMed discovery returned no new candidates. Check or broaden the topic-specific search.",
    "USER_ACTION_REQUIRED")
  fetched <- list()
  for(b in split(pool,ceiling(seq_along(pool)/50L))) {
    records <- tryCatch(fetch_fn(b),error=function(err)err)
    web_assert(!inherits(records,"error") && is.list(records),
      "PubMed metadata retrieval failed; no manuscript plan saved.","PROVIDER_UNAVAILABLE")
    fetched <- c(fetched,records)
  }
  valid <- Filter(web_c2_ref_valid,fetched)
  if(length(valid)) {
    ids <- vapply(valid,function(r)web_c2_handoff_one(r$pmid),character(1))
    valid <- valid[!duplicated(ids) & ids %in% pool]
  }
  # Keep one bibliographic record for each DOI even when PubMed returns aliases.
  if(length(valid)) {
    doi <- vapply(valid,function(r)web_c2_ref_doi(r$doi),character(1))
    key <- ifelse(nzchar(doi),paste0("doi:",doi),paste0("pmid:",
      vapply(valid,function(r)web_c2_handoff_one(r$pmid),character(1))))
    valid <- valid[!duplicated(key) & !(nzchar(doi) & doi %in% existing_dois)]
  }
  matches <- web_c2_ref_relevance(valid,cfg)
  eligible <- if(length(matches))vapply(matches,function(x)isTRUE(x$eligible),logical(1)) else logical(0)
  vetted <- valid[eligible]
  if(length(vetted)) {
    scores <- vapply(matches[eligible],function(x)x$score,integer(1))
    vetted <- vetted[order(-scores,seq_along(scores))]
    matches <- matches[eligible][order(-scores,seq_along(scores))]
    for(i in seq_along(vetted)) {
      vetted[[i]]$web_relevance_score <- matches[[i]]$score
      vetted[[i]]$web_relevance_reason <- matches[[i]]$reason
      vetted[[i]]$web_query_ids <- paste(query_ids[[web_c2_handoff_one(vetted[[i]]$pmid)]],collapse=";")
      vetted[[i]]$web_review_status <- "UNREVIEWED_CANDIDATE"
    }
  }
  chosen <- head(vetted,max(0L,target-length(existing)))
  web_assert(length(existing)+length(chosen)>=minimum,
    paste0("Only ",length(existing)+length(chosen),
      " topic-matched PubMed records were verified (minimum ",minimum,
      "). Search was not padded with unrelated references. Add focused manuscript reference queries."),
    "USER_ACTION_REQUIRED")
  report <- list(status="PUBMED_METADATA_VERIFIED_RELEVANCE_FILTERED",
    review_status="AUTOMATED_VERIFIED_FOR_DRAFT",provider="Canonical R/39 PubMed ESearch/EFetch parser",
    searched_at=now,selected_count=length(chosen),original_count=length(existing),
    added=length(chosen),candidate_count=length(valid),
    topic_matched_count=length(vetted),relevance_rejected=length(valid)-length(vetted),
    verified_total=length(existing)+length(chosen),
    queries=lapply(seq_len(nrow(queries)),function(i)
      list(query_id=queries$query_id[[i]],purpose=queries$purpose[[i]],query=queries$query[[i]])),
    search_log=logs,pmids=as.list(vapply(chosen,function(r)web_c2_handoff_one(r$pmid),character(1))),
    caveat="Metadata verification and topic matching do not prove citation relevance. The AI drafts only from the verified library; PMID/coverage checks must pass automatically, and the author assesses appropriateness in the complete Word manuscript.",
    human_review_required=FALSE,author_review_stage="FINAL_MANUSCRIPT")
  list(records=chosen,candidates=vetted,report=report)
}
web_c2_ref_review_plan <- function(con,settings,actor,project_id,version_id,spec) {
  web_assert(is.list(spec) && setequal(names(spec),
    c("title","language","analyses","artifacts","pmids","rationale")),
    "Use the existing manuscript plan specification for Human reference review.","VALIDATION_FAILED")
  c <- web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE)
  x <- c$packet
  web_assert(identical(x$stage,"PLAN") && identical(x$source$route,"NARRATIVE"),
    "Reference review is supported only for the current Narrative manuscript plan.",
    "USER_ACTION_REQUIRED")
  d <- x$source$reference_discovery
  web_assert(is.list(d) && identical(d$review_status,"HUMAN_SELECTION_REQUIRED"),
    "No unreviewed discovered references in this plan.","USER_ACTION_REQUIRED")
  reason <- web_c2_text(spec$rationale,settings$engine_root)
  web_assert(nzchar(trimws(reason)),"Provide the reason for this Human reference selection.","USER_ACTION_REQUIRED")
  wanted <- unique(web_c2_handoff_vec(spec$pmids))
  web_assert(length(wanted)<=100L && all(grepl("^[0-9]{1,9}$",wanted)),
    "Selected reference PMIDs are malformed.","VALIDATION_FAILED")
  candidates <- d$candidates
  web_assert(is.list(candidates) && all(vapply(candidates,web_c2_ref_valid,logical(1))),
    "Candidate metadata integrity failed.","CITATION_VALIDATION_FAILED")
  ids <- vapply(candidates,function(r)web_c2_handoff_one(r$pmid),character(1))
  web_assert(!anyDuplicated(ids) && all(wanted %in% ids),
    "Only verified, displayed candidates can be selected.","CITATION_VALIDATION_FAILED")
  protected <- web_c2_handoff_vec(d$protected_pmids)
  web_assert(all(protected %in% wanted),
    "Included-study/source-registry PMID removal requires correction in the upstream Evidence source.",
    "USER_ACTION_REQUIRED")
  refs <- list(records=candidates[match(wanted,ids)],provider="R/39 EFetch and Human selection",
    verified_at=web_now())
  refs <- web_c2_normalize_refs(refs,settings$engine_root)
  minimum <- as.integer(x$source$reference_policy$total)
  web_assert(length(refs$records)>=minimum,
    paste0("Only ",length(refs$records)," references selected (minimum ",minimum,
      "). Select additional verified candidates, or expand the search. No revision saved."),
    "USER_ACTION_REQUIRED")
  revised <- x
  revised$source$references <- refs
  revised$source$reference_discovery$review_status <- "HUMAN_SELECTED_PENDING_PLAN_GATE"
  revised$source$reference_discovery$human_selection <- list(
    actor=actor,at=web_now(),rationale=reason,
    included_pmids=as.list(wanted),excluded_pmids=as.list(setdiff(ids,wanted)),
    candidate_version=version_id,selected_unique=length(refs$records))
  revised$source$reference_discovery$verified_total <- length(refs$records)
  revised$source$reference_discovery$human_review_required <- FALSE
  revised$spec$pmids <- as.list(wanted)
  revised$spec$rationale <- paste0(x$spec$rationale," | Reference selection: ",reason)
  revised$created_at <- web_now()
  root <- tempfile("narrative-ref-review-",settings$data_root)
  dir.create(root)
  on.exit(unlink(root,recursive=TRUE))
  web_copy_version(con,settings,actor,project_id,version_id,root)
  web_c2_save(con,settings,actor,project_id,version_id,root,revised,new_plan=TRUE)
}
# Auto-production is source-verified and final-author-reviewed only. Historical
# manual section tools keep their original editorial plan gate where applicable.
web_c2_plan_gate_original <- web_c2_plan_gate
web_c2_plan_gate <- function(con,project_id,x) {
  if(identical(x$review_mode,"FINAL_MANUSCRIPT_ONLY"))return(invisible(TRUE))
  web_c2_plan_gate_original(con,project_id,x)
}
# Display-only citation count: R/39 extraction and web_c2_check are authoritative
# for final gating. The same PMID can appear in both sections or repeatedly.
web_c2_ref_citation_summary <- function(x) {
  extract <- function(name) {
    sec <- x$sections[[name]]
    if(!is.list(sec) || !is.character(sec$text) || length(sec$text)!=1L)
      return(character())
    hits <- regmatches(sec$text,gregexpr("\\[\\[PMID:[0-9]+\\]\\]",sec$text,perl=TRUE))[[1L]]
    if(!length(hits))return(character())
    unique(sub("^\\[\\[PMID:([0-9]+)\\]\\]$","\\1",hits,perl=TRUE))
  }
  section_ids <- lapply(web_c2_sections(),extract)
  names(section_ids) <- web_c2_sections()
  list(total=length(unique(unlist(section_ids,use.names=FALSE))),
    introduction=length(section_ids$Introduction),
    discussion=length(section_ids$Discussion))
}

# RC28.25.8E: Private, source-bound retry checkpoints.  The worker stores only
# validated draft text, never credentials or provider request/response logs.
# Cache is OUTSIDE the scientific engine, workspace and approved Web sources.
web_c2_8e_cache_dir <- function(x, engine, model) {
  root <- Sys.getenv("SRMA_WEB_DATA", unset="")
  if(!nzchar(root)) root <- tools::R_user_dir("srma-web", "data")
  root <- normalizePath(root, winslash="/", mustWork=FALSE)
  dir.create(root, recursive=TRUE, showWarnings=FALSE, mode="0700")
  web_assert(dir.exists(root),"Manuscript checkpoint data root is unavailable.",
    "DEPENDENCY_FAILED")
  root <- normalizePath(root, winslash="/", mustWork=TRUE)
  # Reject path traversal through user-supplied credentials or identifiers: all
  # subdirectory names are calculated SHA-256 values, never raw user fields.
  key <- web_c2_hash(list(schema="RC28.25.8E-DRAFT-V1",
    manuscript_id=x$manuscript_id, evidence_version=x$evidence_version,
    source_sha256=x$source_sha256, reference_sha256=x$reference_sha256,
    title=x$spec$title, model=model,
    canonical_R39=web_sha(list.files(file.path(engine,"R"),
      pattern="^39_",full.names=TRUE))))
  dir <- file.path(root,"manuscript-draft-checkpoints",key)
  dir.create(dir,recursive=TRUE,showWarnings=FALSE,mode="0700")
  web_assert(dir.exists(dir),"Unable to create private manuscript checkpoint directory.",
    "DEPENDENCY_FAILED")
  dir
}
web_c2_8e_cached_section <- function(cache_dir,label,prompt,context,e,engine,max_tokens) {
  # Label is a fixed internal name (introduction, discussion, etc), not a path.
  web_assert(grepl("^[a-z0-9_]+$",label),"Invalid checkpoint label.",
    "SOURCE_INTEGRITY_FAILED")
  file <- file.path(cache_dir,paste0(label,".json"))
  prompt_sha <- web_c2_hash(list(prompt=prompt,context=context,
    max_output_tokens=max_tokens))
  link <- if(file.exists(file)) suppressWarnings(Sys.readlink(file)) else ""
  old <- if(file.exists(file) && (is.na(link) || !nzchar(link)))
    tryCatch(jsonlite::read_json(file,simplifyVector=FALSE),error=function(e) NULL) else NULL
  if(is.list(old) && identical(old$prompt_sha256,prompt_sha) &&
     is.character(old$text) && length(old$text)==1L &&
     identical(old$text_sha256,digest::digest(old$text,algo="sha256",serialize=FALSE))) {
    web_c2_text(old$text,engine)
    return(old$text)
  }
  draft <- e$call_openai(prompt,context,label,max_output_tokens=as.integer(max_tokens))
  draft <- trimws(gsub("^```[[:alnum:]_-]*|```$","",trimws(draft),perl=TRUE))
  web_c2_text(draft,engine)
  web_assert(nzchar(draft),"AI returned an empty manuscript section.",
    "DOCUMENT_GENERATION_FAILED")
  # Save only a complete, validated text response. Atomic new-file creation.
  record <- list(prompt_sha256=prompt_sha,
    text_sha256=digest::digest(draft,algo="sha256",serialize=FALSE),text=draft)
  tmp <- tempfile(".section-",tmpdir=cache_dir,fileext=".json")
  on.exit(unlink(tmp),add=TRUE)
  jsonlite::write_json(record,tmp,auto_unbox=TRUE,null="null",pretty=FALSE)
  web_assert(file.rename(tmp,file) || (file.copy(tmp,file,overwrite=TRUE)),
    "Cannot persist manuscript draft checkpoint.","DEPENDENCY_FAILED")
  draft
}
web_c2_8e_coverage <- function(intro,discussion,extract) {
  a <- unique(as.character(extract(intro)))
  b <- unique(as.character(extract(discussion)))
  list(intro=a,discussion=b,all=unique(c(a,b)))
}
# RC28.25.8G: phase reporting uses only the job id and public counts/stage.
# The filename is an opaque job ID, never a user-provided path. This is a
# best-effort UI status sidecar, NOT a scientific result or status verdict.
web_c2_8g_progress_file <- function(job_id) {
  id<-if(length(job_id)==1L && !is.na(job_id))as.character(job_id) else ""
  if(!grepl("^[A-Za-z0-9_-]{16,64}$",id))return("")
  root<-Sys.getenv("SRMA_WEB_DATA",unset="")
  if(!nzchar(root))root<-tools::R_user_dir("srma-web","data")
  file.path(root,"manuscript-production-progress",paste0(id,".json"))
}
web_c2_8g_progress_write <- function(job_id,phase,message,coverage=NULL,round=NULL) {
  file<-web_c2_8g_progress_file(job_id)
  if(!nzchar(file))return(invisible(FALSE))
  # 9BCD.1: register the two source-bound publication phases while preserving
  # the existing RC28.25.8G progress schema and UI reader compatibility.
  valid_phase<-c("CHECKING_PUBLICATION_SOURCES","BUILDING_PUBLICATION_ASSETS",
    "DRAFTING","COVERAGE_SHORTFALL","EXPANDING_DISCUSSION",
    "SEARCHING_PUBMED","CHECKING_CITATIONS","CREATING_DOCX","COMPLETE",
    "EVIDENCE_REVIEW_REQUIRED")
  if(!phase %in% valid_phase)return(invisible(FALSE))
  # Progress is informative only: I/O errors must not fail manuscript science.
  tryCatch({
    dir.create(dirname(file),recursive=TRUE,showWarnings=FALSE,mode="0700")
    record<-list(schema="RC28.25.8G-PROGRESS-V1",phase=phase,
      message=substr(as.character(message),1L,360L),
      coverage=coverage,round=round,at=web_now())
    tmp<-tempfile(".progress-",tmpdir=dirname(file),fileext=".json")
    on.exit(unlink(tmp),add=TRUE)
    jsonlite::write_json(record,tmp,auto_unbox=TRUE,null="null",na="null")
    if(!file.rename(tmp,file))file.copy(tmp,file,overwrite=TRUE)
    invisible(TRUE)
  },error=function(err)invisible(FALSE))
}
web_c2_8g_progress_read <- function(job_id) {
  file<-web_c2_8g_progress_file(job_id)
  if(!nzchar(file) || !file.exists(file))return(NULL)
  x<-tryCatch(jsonlite::read_json(file,simplifyVector=FALSE),error=function(e)NULL)
  if(!is.list(x) || !identical(x$schema,"RC28.25.8G-PROGRESS-V1") ||
     !is.character(x$message) || length(x$message)!=1L ||
     !is.character(x$phase) || length(x$phase)!=1L)return(NULL)
  x
}
web_c2_8g_progress_coverage <- function(cov,limits) {
  list(introduction=length(cov$intro),introduction_min=as.integer(limits$introduction),
       discussion=length(cov$discussion),discussion_min=as.integer(limits$discussion),
       unique_body=length(cov$all),unique_min=as.integer(limits$total))
}
web_c2_8g_coverage_message <- function(cov,limits) {
  paste0("Introduction ",length(cov$intro),"/",as.integer(limits$introduction),
    " | Discussion ",length(cov$discussion),"/",as.integer(limits$discussion),
    " | Unique references ",length(cov$all),"/",as.integer(limits$total),
    " (additional ",max(0L,as.integer(limits$total)-length(cov$all))," needed)")
}
# One source-bound automatic PubMed expansion. Query generation and relevance
# filtering reuse the approved topic-agnostic Web wrapper around Canonical R/39.
# Prefer older studies, reviews and landmark work; do not infer clinical claims
# from publication metadata alone. Both the result and query log are retained.
web_c2_8g_discover_older <- function(x,refs,engine,needed,cache,settings=NULL) {
  prior_ids<-vapply(refs,function(r)web_c2_handoff_one(r$pmid),character(1))
  path<-file.path(cache,"verified_supplemental_references.json")
  old<-if(file.exists(path))tryCatch(jsonlite::read_json(path,simplifyVector=FALSE),
    error=function(e)NULL) else NULL
  base_hash<-web_c2_hash(lapply(refs,function(r)r[c("pmid","title","doi")]))
  if(is.list(old) && identical(old$base_hash,base_hash) &&
     is.list(old$records) && all(vapply(old$records,web_c2_ref_valid,logical(1))) &&
     identical(old$records_sha256,web_c2_hash(old$records))) {
    new_ids<-vapply(old$records,function(r)web_c2_handoff_one(r$pmid),character(1))
    if(!any(new_ids %in% prior_ids) && !anyDuplicated(new_ids))
      return(list(records=old$records,report=old$report,reused=TRUE))
  }
  cfg<-list(project=x$source$project,research_question=x$source$research_question,
    analysis=list(manuscript=list()))
  year<-as.integer(format(Sys.Date(),"%Y"))
  cutoff<-max(1900L,year-8L)
  # The search is based only on the project configuration, never a fixed disease.
  queries<-web_c2_ref_queries(cfg,year=year)
  core<-head(queries$query,2L)
  older<-paste0("(",core,") AND (1900:",cutoff,"[Date - Publication])")
  fallback<-tail(queries$query,1L)
  cfg$analysis$manuscript$reference_discovery_queries<-unique(c(older,fallback))
  cfg$analysis$manuscript$literature_candidate_target<-70L
  cfg$analysis$manuscript$literature_selected_target<-
    min(100L,length(refs)+max(8L,min(16L,as.integer(needed)+5L)))
  found<-web_c2_ref_discover(cfg,refs,engine,
    minimum=length(refs)+1L,settings=settings)
  fresh<-found$records
  ids<-vapply(fresh,function(r)web_c2_handoff_one(r$pmid),character(1))
  web_assert(all(vapply(fresh,web_c2_ref_valid,logical(1))) &&
    !anyDuplicated(ids) && !any(ids %in% prior_ids),
    "Additional PubMed references failed verification.","CITATION_VALIDATION_FAILED")
  record<-list(schema="RC28.25.8G-VERIFIED-EXTRA-V1",base_hash=base_hash,
    records_sha256=web_c2_hash(fresh),records=fresh,report=found$report)
  tmp<-tempfile(".extra-",tmpdir=cache,fileext=".json")
  on.exit(unlink(tmp),add=TRUE)
  jsonlite::write_json(record,tmp,auto_unbox=TRUE,null="null",pretty=FALSE)
  web_assert(file.rename(tmp,path) || file.copy(tmp,path,overwrite=TRUE),
    "Cannot persist verified PubMed expansion checkpoint.","DEPENDENCY_FAILED")
  list(records=fresh,report=found$report,reused=FALSE)
}
web_c2_8e_revise_section <- function(target,original,other,context,refs,
   need_intro,need_discussion,need_total,e,cache,engine,round=1L) {
  cov <- web_c2_8e_coverage(if(identical(target,"Introduction"))original else other,
    if(identical(target,"Discussion"))original else other,e$extract_pmids)
  # Relevant, verified metadata/abstracts ONLY. We do not manufacture PMIDs.
  unused <- Filter(function(r) !(as.character(r$pmid) %in% cov$all),refs)
  citations <- lapply(unused,function(r)list(pmid=r$pmid,title=r$title,
    year=r$year,abstract=substr(as.character(r$abstract %||b2% ""),1L,1300L)))
  budget <- list(introduction=as.integer(need_intro),
    discussion=as.integer(need_discussion),unique_body=as.integer(need_total),
    current_introduction=length(cov$intro),current_discussion=length(cov$discussion),
    current_unique_body=length(cov$all))
  prompt <- paste(
    "Revise ONLY the requested Introduction or Discussion of a systematic review.",
    "Preserve existing accurately supported content and exact [[PMID:12345]] citations.",
    "Improve substantive evidence comparison, nuance and scholarly depth by citing",
    "additional RELEVANT VERIFIED source records from the supplied metadata and abstracts.",
    "For each newly added citation, use it only where its actual title/abstract supports the claim.",
    "DO NOT append citation-only paragraphs, pad the reference count, or add",
    "unsupported claims merely to meet a numerical threshold.",
    "Return the entire revised section, with exact PMID tokens, no heading or reference list.",
    "No unsourced digits, numeric findings, invented endpoint/timepoint, or pooled claim",
    "when current evidence is narrative-only. Keep unmodified paragraphs if no pertinent",
    "additional references exist. Failure to achieve the requested count is acceptable",
    "and will be truthfully recorded as a blocked scientific source-coverage gate.")
  content <- as.character(jsonlite::toJSON(list(section=target,
    current_text=original,other_section=substr(other,1L,12000L),
    section_target=budget,verified_uncited_literature=citations,
    study_context=list(title=context$title,research_question=context$research_question,
      protocol=context$protocol,route=context$route,
      methods=substr(context$methods,1L,8000L),
      results=substr(context$results,1L,10000L))),
    auto_unbox=TRUE,null="null",digits=NA))
  web_assert(nchar(content)<=120000L,
    "Targeted manuscript citation improvement exceeds bounded context.",
    "USER_ACTION_REQUIRED")
  web_assert(length(round)==1L && is.finite(round) && round>=1L && round<=5L,
    "Invalid bounded citation expansion round.","VALIDATION_FAILED")
  label<-paste0(if(identical(target,"Discussion"))"discussion_refinement_" else
    "introduction_refinement_",as.integer(round))
  web_c2_8e_cached_section(cache,label,prompt,content,e,engine,11500L)
}
# RC28.25.8E — Web compatibility production uses only verified PMID and
# section/body reference coverage gates, followed by numeric-token checks.
# Do not run per-paragraph AI citation-review or JSON decision repair.
# The Canonical R/40 standalone publication QC is not claimed by this route.
# RC28.25.9B: source-bound publication prose (no provider, R/26, or pooled recalculation).
# Helpers use only the frozen PLAN's verified scientific records. Unavailable
# methodology is labeled as unverified; no PRISMA or PROSPERO value is invented.
web_c2_9b_one <- function(x) {
  if(is.null(x))return("")
  v <- as.character(unlist(x,use.names=FALSE))
  if(!length(v) || is.na(v[[1L]]))return("")
  trimws(gsub("[\\r\\n\\t]+"," ",v[[1L]],perl=TRUE))
}
web_c2_9b_plain <- function(x) {
  y<-web_c2_9b_one(x)
  # Registered PDF content is data, never Markdown structure or UI instructions.
  y<-gsub("[[:cntrl:]]+"," ",y,perl=TRUE)
  trimws(gsub("[[:space:]]+"," ",y,perl=TRUE))
}
web_c2_9b_registration <- function(source) {
  if(is.null(source$registration))return("")
  raw<-paste(as.character(unlist(source$registration,use.names=FALSE)),collapse=" ")
  hits<-regmatches(raw,gregexpr("CRD[0-9]{9,15}",raw,perl=TRUE))[[1L]]
  hits<-unique(hits)
  if(length(hits)==1L)hits[[1L]] else ""
}
web_c2_9b_databases <- function(source) {
  ds<-source$search$databases
  if(!is.list(ds) || !length(ds) || is.null(names(ds)))return(character())
  names(ds)[vapply(ds,function(z)identical(z,TRUE) ||
    identical(tolower(web_c2_9b_one(z)),"true"),logical(1))]
}
web_c2_9b_rationale <- function(x) {
  question<-x$source$research_question
  if(is.list(question)) {
    p<-web_c2_9b_one(question$population %||b2% question$P)
    i<-web_c2_9b_one(question$intervention %||b2% question$I)
    comparator<-web_c2_9b_one(question$comparator %||b2% question$comparison %||b2% question$C)
    o<-web_c2_9b_one(question$outcomes %||b2% question$outcome %||b2% question$O)
    if(all(nzchar(c(p,i,comparator,o))))return(paste0(
      "Among ",p,", the review evaluated ",i," versus ",comparator,
      ", with outcomes including ",o,"."))
  }
  title<-web_c2_9b_plain(x$spec$title)
  if(nzchar(title))paste0("The objective was to address the approved review question: ",title,".")
  else "The approved research question requires source verification."
}
web_c2_9b_title <- function(x) {
  title<-web_c2_9b_plain(x$spec$title)
  if(!nzchar(title))return("Systematic review — title requires author confirmation")
  route<-web_c2_9b_one(x$source$route)
  if(!grepl("systematic[[:space:]]+review",title,ignore.case=TRUE))
    title<-paste0(title,": a systematic review")
  if(identical(route,"QUANTITATIVE") &&
     !grepl("meta[- ]analysis",title,ignore.case=TRUE))
    title<-paste0(title," and meta-analysis")
  title
}
web_c2_9b_methods <- function(x,narrative=TRUE,original="") {
  source<-x$source
  reg<-web_c2_9b_registration(source)
  reg_text<-if(nzchar(reg))paste0("The protocol carries verified PROSPERO identifier ",reg,".")
    else "The PROSPERO identifier was not verifiable from the frozen manuscript source; the author must confirm the registration status before submission."
  db<-web_c2_9b_databases(source)
  db_text<-if(length(db))paste0("Configured database sources were ",
      paste(toupper(db),collapse=", "),". Their final executed search dates and search strings require confirmation from the same-review canonical search record.")
    else "The database names, dates and exact search strings were not transferred in verified form to this manuscript package and require same-review source binding."
  protocol<-paste0("### Protocol and registration\n",web_c2_9b_rationale(x)," ",reg_text)
  search<-paste0("### Information sources and search strategy\n",db_text,
    " Only the actual executed searches and their dates should be stated in the submitted manuscript; the reproducible search strings belong in supplementary material.")
  selection<-paste0("### Eligibility criteria and study selection\n",
    "Eligibility and final inclusion are governed by the approved review protocol and the saved final study-selection decisions. ",
    "The review population, comparator eligibility, and permitted designs must be confirmed against those immutable records. ",
    "Literature added to support the Discussion was not treated as newly included outcome evidence.")
  extraction<-paste0("### Data extraction and synthesis\n",
    "Study-level results were taken from the saved source-PDF-bound outcome and timepoint inventory and its explicit Human verification/finalization. ",
    "The original reported outcome, analysis mapping and source-reported timepoint were preserved separately. ",
    "This manuscript does not assume two independent Human extractors unless that workflow is recorded in the project evidence.")
  rob<-if(narrative && isTRUE(source$narrative$unresolved_rob))
    "Risk-of-bias adjudication remains unresolved. No completed RoB assessment or certainty rating is claimed; scientific acceptance is pending."
    else "Risk-of-bias tools, judgments and adjudication are described only when their source-verified records are available; author verification remains required."
  assessment<-paste0("### Risk of bias and certainty assessment\n",rob)
  edited<-x$sections$Methods
  stat_text<-if(isTRUE(edited$source %in% c("HUMAN","AI")) && identical(edited$source_sha256,x$source_sha256))
    web_c2_expand(edited$text,x) else web_stat_methods_prose(source$statistical_methods)
  stats<-paste0("### Statistical analysis and synthesis method\n",stat_text)
  paste(c(protocol,search,selection,extraction,assessment,stats),collapse="\n\n")
}
web_c2_9b_verified_rows <- function(x) {
  rows<-x$source$narrative$rows
  if(!is.list(rows))return(list())
  Filter(function(r)is.list(r) &&
    web_c2_9b_one(r$decision) %in% c("CONFIRMED","CORRECTED") &&
    nzchar(web_c2_9b_one(r$numeric_summary)) &&
    nzchar(web_c2_9b_one(r$study_id)) &&
    nzchar(web_c2_9b_one(r$original_outcome)),rows)
}
web_c2_9b_study_count <- function(x) {
  if(identical(web_c2_9b_one(x$source$route),"NARRATIVE") &&
     is.list(x$source$narrative$human_final_selection))
    return(length(web_c2_9cd_human_ids(x)))
  # Legacy draft-only behavior. 9BCD.4 publication acceptance separately
  # REQUIRES a signed Human Finalized Study Selection; this cannot publish.
  rows<-x$source$narrative$rows
  if(!is.list(rows))return(0L)
  length(unique(Filter(nzchar,vapply(rows,function(r)web_c2_9b_one(r$study_id),character(1)))))
}
web_c2_9b_group_title <- function(r) {
  name<-web_c2_9b_plain(r$common_variable)
  if(!nzchar(name))name<-web_c2_9b_plain(r$original_outcome)
  name<-gsub("[#*_`]+","",name,perl=TRUE)
  if(!nzchar(name))"Study-reported outcomes" else name
}
web_c2_9b_numeric_snippet <- function(summary,limit=260L) {
  raw<-web_c2_9b_plain(summary)
  if(!nzchar(raw))return("")
  clauses<-strsplit(raw,";[[:space:]]*",perl=TRUE)[[1L]]
  clauses<-clauses[nzchar(clauses)]
  # For Abstract only: discard long adjustment-method metadata, not the
  # numerical outcome text. Every retained clause is verbatim source-derived.
  clauses<-clauses[!grepl("^Adjustment:[[:space:]]*",clauses,ignore.case=TRUE)]
  numbered<-clauses[grepl("[0-9]",clauses)]
  if(!length(numbered))return("")
  # Select complete semicolon-delimited units (never cut an estimate/CI).
  selected<-character()
  for(z in numbered) {
    if(nchar(z)>limit)next
    if(length(selected) && nchar(paste(c(selected,z),collapse="; "))>limit)break
    selected<-c(selected,z)
  }
  if(!length(selected))return("")
  paste(selected,collapse="; ")
}
web_c2_9b_publication_summary <- function(raw) {
  original<-web_c2_9b_plain(raw)
  clauses<-strsplit(original,";[[:space:]]*",perl=TRUE)[[1L]]
  # Discard verbose provenance/adjustment covariate lists only when there is
  # no numeric finding inside; retain all quantified claims, verbatim.
  keep<-!grepl("^Adjustment:[[:space:]]*",clauses,ignore.case=TRUE) |
    grepl("[0-9]",clauses)
  selected<-clauses[keep]
  if(!length(selected))original else paste(selected,collapse="; ")
}

# RC28.25.9BCD.5 — publication prose uses first author/year, never the
# entire report-author field. Preserve original labels in the frozen source.
web_c2_9e_study_short_label <- function(label) {
  z<-web_c2_9b_plain(label)
  z<-trimws(strsplit(z,";",fixed=TRUE)[[1L]][[1L]])
  z<-trimws(gsub("[[:space:]]+"," ",z))
  # Original formats may be 'Shimoda TM' or 'Shimoda TM, Ueyama HA'.
  z<-strsplit(z,",",fixed=TRUE)[[1L]][[1L]]
  words<-strsplit(z,"[[:space:]]+",perl=TRUE)[[1L]]
  first<-if(length(words))words[[1L]] else ""
  yy<-regmatches(label,regexpr("(19|20)[0-9]{2}",label,perl=TRUE))
  year<-if(length(yy) && nzchar(yy))yy else ""
  if(!nzchar(first) || !nzchar(year))
    return("the included study")
  paste0(first," et al. (",year,")")
}
web_c2_9e_study_pmid <- function(r,refs) {
  # Match source-verified first author + year against PubMed-verified metadata.
  # Do not infer a PMID from the topic, title similarity or author list alone.
  label<-web_c2_9b_plain(r$study_label)
  short<-web_c2_9e_study_short_label(label)
  web_assert(!identical(short,"the included study"),
    "Included-study first author/year missing; citation cannot be verified.",
    "CITATION_VALIDATION_FAILED")
  surname<-tolower(sub(" et al[.].*$","",short))
  year<-sub("^.*[(]([0-9]{4})[)].*$","\\1",short)
  hits<-Filter(function(z) {
    auth<-web_c2_9b_one(z$authors)
    lead<-tolower(strsplit(trimws(auth),"[ ,;]",perl=TRUE)[[1L]][[1L]])
    identical(lead,surname) && identical(web_c2_9b_one(z$year),year) &&
      web_c2_ref_valid(z)
  },refs)
  web_assert(length(hits)==1L,
    paste0("Included study citation is not uniquely present in verified References: ",
      short," (matches=",length(hits),"). Never drop a finalized included study."),
    "CITATION_VALIDATION_FAILED")
  web_c2_9b_one(hits[[1L]]$pmid)
}
web_c2_9e_narrative_pmids <- function(x,refs) {
  rows<-web_c2_9b_verified_rows(x)
  studies<-unique(vapply(rows,function(r)web_c2_9b_one(r$study_id),character(1)))
  studies<-studies[nzchar(studies)]
  human_ids<-web_c2_9cd_human_ids(x)
  # Included-but-not-extractable studies are retained in the final selection;
  # require study bibliography mapping if such a case arises.
  web_assert(setequal(human_ids,studies),
    "Some Human Finalized included studies lack study-specific bibliographic anchors; do not silently omit citations.",
    "CITATION_VALIDATION_FAILED")
  setNames(vapply(studies,function(id) {
    row<-Filter(function(z)identical(web_c2_9b_one(z$study_id),id),rows)[[1L]]
    web_c2_9e_study_pmid(row,refs)
  },character(1)),studies)
}
web_c2_9e_quantitative_pmids <- function(assets,refs) {
  tabs<-Filter(function(z)is.list(z) && startsWith(web_c2_9b_one(z$title),"Table 1.") &&
    is.data.frame(z$data),assets$tables)
  web_assert(length(tabs)>=1L,
    "Quantitative included-study reference check needs same-version Table 1.",
    "CITATION_VALIDATION_FAILED")
  tab<-tabs[[1L]]$data
  nm<-tolower(gsub("[^a-z]","",names(tab)))
  ai<-match(TRUE,nm %in% c("firstauthor","study","author","studyname"))
  yi<-match(TRUE,nm %in% c("year","publicationyear"))
  web_assert(!is.na(ai) && nrow(tab)>0L,
    "Included study identifiers missing from quantitative Table 1.",
    "CITATION_VALIDATION_FAILED")
  pmids<-vapply(seq_len(nrow(tab)),function(i) {
    label<-web_c2_9b_one(tab[[ai]][[i]])
    if(!is.na(yi))label<-paste(label,web_c2_9b_one(tab[[yi]][[i]]))
    web_c2_9e_study_pmid(list(study_label=label),refs)
  },character(1))
  web_assert(!anyDuplicated(pmids),
    "Multiple quantitative included studies mapped to the same PMID.",
    "CITATION_VALIDATION_FAILED")
  pmids
}

web_c2_9e_reference_token <- function(pmid) {
  web_assert(grepl("^[0-9]{1,9}$",pmid),"Invalid included study PMID.",
    "CITATION_VALIDATION_FAILED")
  paste0("[[PMID:",pmid,"]]")
}

web_c2_9b_results <- function(x,original="") {
  if(!identical(web_c2_9b_one(x$source$route),"NARRATIVE")) {
    if(!nzchar(trimws(original)))return("### Quantitative synthesis\nThe approved quantitative result block is not bound to this plan.")
    return(paste0("### Approved quantitative syntheses\n",original))
  }
  rows<-web_c2_9b_verified_rows(x)
  n_studies<-web_c2_9b_study_count(x)
  headline<-paste0("### Study selection and synthesis scope\n",
    "The finalized Human-confirmed narrative evidence contains ",n_studies,
    " distinct included study records and ",length(rows),
    " source-verified, reportable endpoint records. ",
    "The exact identification, screening and exclusion counts, together with Figure 1 (PRISMA), require the same-review R/35 source binding; no PRISMA counts are inferred here. ",
    "Study and patient characteristics require the corresponding source-backed Tables before publication.")
  rb<-if(isTRUE(x$source$narrative$unresolved_rob))
    "### Risk of bias\nRisk-of-bias assessment was not finalized; no study-level judgments are fabricated in this draft."
    else "### Risk of bias\nThe author must verify the saved source-bound risk-of-bias assessments before scientific acceptance."
  if(!length(rows))return(paste(headline,rb,
    "### Study-reported outcomes\nNo confirmed numeric endpoint is bound to this manuscript.",sep="\n\n"))
  groups<-unique(vapply(rows,web_c2_9b_group_title,character(1)))
  results<-character()
  refmap<-web_c2_9e_narrative_pmids(x,x$source$references$records)
  for(g in groups) {
    subset<-rows[vapply(rows,function(r)identical(web_c2_9b_group_title(r),g),logical(1))]
    lines<-vapply(subset,function(r) {
      study<-web_c2_9e_study_short_label(r$study_label)
      name<-web_c2_9b_plain(r$original_outcome)
      time<-web_c2_9b_plain(r$reported_timepoint)
      summary<-web_c2_9b_publication_summary(r$numeric_summary)
      # Reference numbers use the same verified PubMed records as References.
      pmid<-refmap[[web_c2_9b_one(r$study_id)]]
      paste0(study," reported ",name,
        if(nzchar(time))paste0(" at ",time) else "",
        ". ",summary,if(grepl("[.!?]$",summary))" " else ". ",
        web_c2_9e_reference_token(pmid))
    },character(1))
    results<-c(results,paste0("### ",g,"\n",paste(lines,collapse="\n\n")))
  }
  note<-"### Interpretation of the synthesis\nThese are separate study-level findings, not pooled treatment effects. Differences in reported timepoints, outcome definitions and comparator composition must be considered before comparing results."
  paste(c(headline,rb,results,note),collapse="\n\n")
}
web_c2_9b_abstract <- function(x,methods,results) {
  narrative<-identical(web_c2_9b_one(x$source$route),"NARRATIVE")
  background<-paste0("Background: ",web_c2_9b_rationale(x))
  db<-web_c2_9b_databases(x$source)
  search<-if(length(db))paste(toupper(db),collapse=", ") else
    "database/search details pending same-review source verification"
  search_status<-if(length(db))paste0("Configured databases were ",search,
    "; final executed search details require the canonical search record.")
    else "Database and search details require same-review source verification."
  method<-if(narrative)
    paste0("Methods: ",search_status,
      " A finalized, PDF-source-bound, Human-verified narrative synthesis was used; no pooled effect estimate was calculated.")
    else paste0("Methods: ",search_status,
      " Frozen approved quantitative analyses were used; analytical model details are reported in Methods.")
  if(narrative) {
    rows<-web_c2_9b_verified_rows(x)
    selected<-head(rows,4L)
    snippets<-vapply(selected,function(r)web_c2_9b_numeric_snippet(r$numeric_summary),character(1))
    keep<-which(nzchar(snippets))
    findings<-if(length(keep))paste(vapply(keep,function(i)paste0(
      "For ",web_c2_9b_plain(selected[[i]]$original_outcome),", ",
      web_c2_9e_study_short_label(selected[[i]]$study_label),
      " reported ",snippets[[i]]),character(1)),
      collapse="; ") else "No numerical outcome was available for abstract reporting"
    contributors<-length(unique(vapply(rows,function(r)web_c2_9b_one(r$study_id),character(1))))
    res<-paste0("Results: ",web_c2_9b_study_count(x),
      " studies were finally included; ",contributors,
      " studies contributed ",length(rows),
      " verified reportable endpoint records. ",findings,". ",
      "All estimates were study-specific, without statistical pooling.")
    conclusion<-"Conclusions: The available study-level evidence does not by itself establish pooled comparative effectiveness. Clinical implications remain provisional until risk of bias, study eligibility, and applicability have been confirmed."
  } else {
    # The quantitative report is reproduced from the verified canonical
    # result block; no estimates or heterogeneity statistics are invented.
    lines<-strsplit(web_c2_9b_plain(results),"[.]\\s+",perl=TRUE)[[1L]]
    lines<-lines[grepl("[0-9]",lines)]
    if(length(lines))lines<-head(lines,3L)
    res<-paste0("Results: ",if(length(lines))paste(lines,collapse=". ") else
      "Quantitative results await source-bound numerical summary.")
    conclusion<-"Conclusions: Interpret the approved effect estimates in light of study design, heterogeneity, risk of bias and limitations; final author assessment is required."
  }
  reg<-web_c2_9b_registration(x$source)
  prospective<-if(nzchar(reg))paste0("PROSPERO: ",reg)
    else "PROSPERO: registration identifier not verified in frozen source; author review required."
  paste(background,method,res,conclusion,prospective,sep="\n\n")
}
web_c2_9b_publication_qc <- function(abstract,methods,results,x) {
  labels<-c("Background:","Methods:","Results:","Conclusions:","PROSPERO:")
  pos<-vapply(labels,function(z)regexpr(z,abstract,fixed=TRUE)[1L],integer(1))
  stopifnot(all(pos>0L),!is.unsorted(pos,strictly=TRUE))
  if(identical(web_c2_9b_one(x$source$route),"NARRATIVE")) {
    rows<-web_c2_9b_verified_rows(x)
    # Require each source-bound numerical summary to appear *unaltered*
    # within the prose Results; no silent omissions or recalculated effect.
    missing<-vapply(rows,function(r)!grepl(web_c2_9b_publication_summary(r$numeric_summary),
      results,fixed=TRUE),logical(1))
    stopifnot(!any(missing),!grepl("PDF_location|Human-approved analysis variable|fact_id:",results,fixed=FALSE))
  }
  list(status="STRUCTURAL_AND_SOURCE_PARITY_PRECHECK_PASS",
    structured_abstract=TRUE,verified_rows=if(identical(web_c2_9b_one(x$source$route),"NARRATIVE"))length(web_c2_9b_verified_rows(x)) else NA_integer_,
    registered_prospero=nzchar(web_c2_9b_registration(x$source)),
    source_prisma="PENDING_RC28.25.9C",source_tables="PENDING_RC28.25.9C",
    standalone_R40="NOT_RUN",author_scientific_review="REQUIRED")
}

# RC28.25.9BCD.5.1 — A source-bound numerical preflight for AI prose.
# Canonical R/40's zero-token shortcut rejected genuine Human-verified estimates.
# This token-sequence gate accepts only values witnessed in a frozen verified
# result; it is NOT the standalone R/40 semantic or release acceptance check.
web_c2_9f_numeric_signature <- function(text, parser) {
  value <- paste(as.character(unlist(text,use.names=FALSE)),collapse=" ")
  if(!nzchar(trimws(value)))return(character())
  tokens <- parser$extract_numeric_tokens(value)
  if(!is.data.frame(tokens) || !nrow(tokens))return(character())
  vapply(seq_len(nrow(tokens)), function(i) {
    z <- suppressWarnings(as.numeric(tokens$value[[i]]))
    web_assert(is.finite(z),"Unparseable numerical manuscript token.",
      "NUMERIC_CONSISTENCY_FAILED")
    # A percentage is not interchangeable with a nonpercentage estimate.
    end <- as.integer(tokens$end[[i]])
    suffix <- if(end<nchar(value))substr(value,end+1L,end+1L) else ""
    paste0(sprintf("%.12g",z),if(identical(suffix,"%"))"%" else "")
  },character(1))
}
web_c2_9f_approved_numeric_sources <- function(x,parser) {
  narrative <- identical(web_c2_9b_one(x$source$route),"NARRATIVE")
  if(narrative) {
    rows <- web_c2_9b_verified_rows(x)
    web_assert(length(rows)>0L,
      "No Human-verified numerical sources for AI prose.",
      "NUMERIC_CONSISTENCY_FAILED")
    facts <- list()
    for(r in rows) {
      # Each outcome is individually Human verified and PDF-bound in the PLAN.
      for(field in c("numeric_summary","reported_timepoint","analysis_timepoint")) {
        sig <- web_c2_9f_numeric_signature(r[[field]],parser)
        if(length(sig))facts[[length(facts)+1L]]<-sig
      }
    }
    return(facts)
  }
  # Quantitative review: ONLY executed and approved canonical analysis results.
  analyses<-x$source$analyses
  if(!is.list(analyses))return(list())
  facts<-list()
  for(a in analyses) {
    summary<-a$result$summary
    if(is.null(summary))next
    texts<-as.character(unlist(summary,use.names=FALSE))
    for(value in texts) {
      sig<-web_c2_9f_numeric_signature(value,parser)
      if(length(sig))facts[[length(facts)+1L]]<-sig
    }
  }
  facts
}
web_c2_9f_numeric_source_gate <- function(part,section,x,parser) {
  web_assert(is.character(part) && length(part)==1L &&
    section %in% c("Introduction","Discussion"),
    "Invalid numeric preflight input.","NUMERIC_CONSISTENCY_FAILED")
  facts<-web_c2_9f_approved_numeric_sources(x,parser)
  refs<-x$source$references$records
  if(!is.list(refs))refs<-list()
  # Check each paragraph independently. This cannot combine unrelated values
  # spread across different source claims to manufacture an effect estimate.
  paragraphs<-strsplit(part,"\\n[[:space:]]*\\n",perl=TRUE)[[1L]]
  checked<-0L
  for(p in paragraphs) {
    # PMID citation IDs and Table/Figure labels are not scientific quantities.
    clean<-gsub("\\[\\[PMID:[0-9]+\\]\\]","",p,perl=TRUE)
    clean<-gsub("(?i)\\b(?:supplementary[[:space:]]+)?(?:figure|fig\\.|table)[[:space:]]+[0-9]+[a-z]?\\b","",clean,perl=TRUE)
    values<-web_c2_9f_numeric_signature(clean,parser)
    if(!length(values))next
    checked<-checked+length(values)
    cursor<-1L
    while(cursor<=length(values)) {
      longest<-0L
      for(sig in facts) {
        if(!length(sig))next
        at<-which(sig==values[[cursor]])
        if(!length(at))next
        for(start in at) {
          m<-0L
          while(cursor+m<=length(values) && start+m<=length(sig) &&
             identical(values[[cursor+m]],sig[[start+m]]))m<-m+1L
          if(m>longest)longest<-m
        }
      }
      # Publication years are permitted only when the paragraph cites the
      # specific verified PubMed record with that exact year.
      if(longest==0L && grepl("^(19|20)[0-9]{2}$",values[[cursor]])) {
        yr<-values[[cursor]]
        for(ref in refs) {
          if(identical(web_c2_9b_one(ref$year),yr) &&
              grepl(paste0("[[PMID:",web_c2_9b_one(ref$pmid),"]]"),p,fixed=TRUE)) {
            longest<-1L;break
          }
        }
      }
      web_assert(longest>0L,paste0(
        "Unverified numeric token in AI ",section,
        ": ",values[[cursor]],". Frozen numerical sources do not support this token. ",
        "Existing cached draft retained; no DOCX was produced."),
        "NUMERIC_CONSISTENCY_FAILED")
      cursor<-cursor+longest
    }
  }
  list(section=section,approved_numeric_tokens=as.integer(checked),
    source_groups=length(facts),gate="SOURCE_BOUND_TOKEN_SEQUENCE_PRECHECK",
    standalone_R40="NOT_RUN")
}

web_c2_auto_draft <- function(x, engine, out, job_id="", scientific_library="", publication_assets=NULL) {
  web_assert(identical(x$stage,"PLAN"),
    "Choose a saved, source-verified Manuscript PLAN.","USER_ACTION_REQUIRED")
  web_assert(identical(web_c2_hash(x$source),x$source_sha256) &&
    identical(web_c2_hash(x$source$references),x$reference_sha256),
    "Reference or evidence package checksum mismatch.","SOURCE_INTEGRITY_FAILED")
  route<-x$source$route %||b2% "QUANTITATIVE"
  web_assert(route %in% c("NARRATIVE","QUANTITATIVE"),
    "This mixed synthesis requires a separately verified mixed evidence adapter.",
    "USER_ACTION_REQUIRED")
  narrative <- identical(route,"NARRATIVE")
  if(narrative) {
    s<-x$source$narrative
    web_assert(identical(s$source_version,x$evidence_version) &&
      identical(s$finalization_scope,"NARRATIVE_ANALYSIS_LOCKED; NOT_CANONICAL_R26") &&
      length(s$rows)>0L, "Narrative synthesis is not frozen and Human-verified.",
      "USER_ACTION_REQUIRED")
    web_assert(all(vapply(s$rows,function(r) {
      if(identical(r$decision,"NOT_REPORTED"))
        return(!nzchar(r$numeric_summary) && !nzchar(r$fact_id))
      identical(r$decision,"CONFIRMED") || identical(r$decision,"CORRECTED")
    },logical(1))),"An unverified numeric result entered the narrative source.",
    "SOURCE_INTEGRITY_FAILED")
  } else {
    web_assert(length(x$source$analyses)>0L && length(x$source$artifacts)>0L,
      "Approved quantitative analyses and real result artifacts are required.",
      "USER_ACTION_REQUIRED")
  }
  refs <- x$source$references$records
  web_assert(length(refs)>0L && all(vapply(refs,web_c2_ref_valid,logical(1))),
      "A PubMed-verified reference library is required.","CITATION_VALIDATION_FAILED")
  ids <- vapply(refs,function(r)as.character(r$pmid),character(1))
  web_assert(!anyDuplicated(ids),"Duplicate verified PMIDs in the reference library.",
    "CITATION_VALIDATION_FAILED")
  limits <- x$source$reference_policy
  total <- as.integer(limits$total);intro_min <- as.integer(limits$introduction)
  discussion_min <- as.integer(limits$discussion)
  web_assert(all(is.finite(c(total,intro_min,discussion_min))) &&
    min(total,intro_min,discussion_min)>=1L && length(ids)>=total,
    "Reference discovery has not supplied enough verified sources.",
    "USER_ACTION_REQUIRED")
  # The Shiny and Worker sessions have separate environments. Resolve only
  # the operator-controlled provider variable from the verified engine root.
  # Never persist this secret in snapshots, job requests or diagnostics.
  if(!nzchar(Sys.getenv("OPENAI_API_KEY",unset=""))) {
    env_path<-file.path(engine,".env")
    if(file.exists(env_path)) {
      lines<-readLines(env_path,warn=FALSE,encoding="UTF-8")
      hits<-grep("^[[:space:]]*(export[[:space:]]+)?OPENAI_API_KEY[[:space:]]*=",lines)
      if(length(hits)) {
        key<-sub("^[[:space:]]*(export[[:space:]]+)?OPENAI_API_KEY[[:space:]]*=[[:space:]]*","",
          lines[[tail(hits,1L)]])
        key<-trimws(key)
        if(nchar(key)>=2L && (startsWith(key,"\"") && endsWith(key,"\"") ||
           startsWith(key,"'") && endsWith(key,"'")))
          key<-substr(key,2L,nchar(key)-1L)
        if(nzchar(key))Sys.setenv(OPENAI_API_KEY=key)
      }
    }
  }
  web_assert(nzchar(Sys.getenv("OPENAI_API_KEY",unset="")),
    "Configure OPENAI_API_KEY in the scientific Worker or operator-controlled engine .env before drafting.",
    "PROVIDER_UNAVAILABLE")
  e <- web_c2_defs(engine,39)
  n <- web_c2_defs(engine,40)
  e$model_name <- Sys.getenv("SRMA_MANUSCRIPT_MODEL",unset="gpt-5.6")
  e$openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  private <- tempfile("srma-automanuscript-raw-")
  dir.create(private,mode="0700")
  on.exit(unlink(private,recursive=TRUE,force=TRUE),add=TRUE)
  e$raw_root <- private
  blocks <- web_c2_system_blocks(x,engine)
  # 9B: the publication prose is assembled from the exact frozen evidence
  # rather than copying PDF/Human audit rows into the journal Results.
  methods <- web_c2_9b_methods(x,narrative,
    original=web_c2_expand(blocks$Methods,x))
  results <- web_c2_9b_results(x,
    original=web_c2_expand(blocks$Results,x))
  if(!is.null(publication_assets))
    results<-web_c2_9cd_results(results,publication_assets)
  if(!narrative && !is.null(publication_assets)) {
    # Every study in the approved Table 1 must be present in the verified
    # bibliography. The IDs are anchored to author/year, never guessed.
    included<-web_c2_9e_quantitative_pmids(publication_assets,refs)
    results<-paste0(results,"\n\n### Included-study references\n",
      "The studies contributing to the approved quantitative synthesis are detailed in Table 1 ",
      paste(vapply(included,web_c2_9e_reference_token,character(1)),collapse=" "),".")
  }
  web_assert(nzchar(trimws(methods)) && nzchar(trimws(results)),
    "Frozen Methods/Results source facts are incomplete.","SOURCE_INTEGRITY_FAILED")
  # A bounded bibliographic prompt: title/abstract remain source-verified, not
  # AI-provided PMID suggestions. Do not allow any new source into the draft.
  library <- lapply(refs,function(r)list(pmid=r$pmid,title=r$title,
    year=r$year,journal=r$journal,abstract=substr(as.character(r$abstract %||b2% ""),1L,900L)))
  context <- list(title=x$spec$title,research_question=x$source$research_question,
    protocol=x$source$protocol,route=if(narrative)"NARRATIVE" else "QUANTITATIVE",
    methods=substr(methods,1L,20000L),results=substr(results,1L,30000L),
    verified_literature=library,reference_policy=limits)
  context_text <- as.character(jsonlite::toJSON(context,auto_unbox=TRUE,null="null",digits=NA))
  web_assert(nchar(context_text)<=140000L,
    "The frozen source package exceeds the bounded manuscript drafting context.",
    "USER_ACTION_REQUIRED")
  base_rules <- paste(
    "You are drafting an academic systematic review manuscript for author review.",
    "Use only the frozen source findings for current-review Results and Methods.",
    "Never calculate new statistics, infer a PDF timepoint, or fabricate a study outcome.",
    "Narrative-only sources MUST NOT be described as pooled meta-analysis results.",
    "Every literature claim must cite an EXACT [[PMID:12345]] token from the supplied library.",
    "Do not insert any digits (years, study sizes, outcomes or percentages) outside PMID tokens in AI-authored prose.",
    "A PMID is NOT evidence for an unstated claim; prefer removing unsupported claims.",
    "No URLs, no fake citations, no new numeric claims not present in frozen evidence.",
    "Do not include a heading, numbered reference list, code fence, or commentary.",
    "Use ordinary paragraphs. Mark author-required information explicitly if unavailable.")
  cache <- web_c2_8e_cache_dir(x,engine,e$model_name)
  progress<-function(phase,message,cov=NULL,round=NULL)
    web_c2_8g_progress_write(job_id,phase,message,
      if(is.null(cov))NULL else web_c2_8g_progress_coverage(cov,limits),round)
  progress("DRAFTING","Writing Introduction and Discussion from frozen evidence")
  intro_prompt <- paste(base_rules,
    paste0("Write a rigorous academic Introduction with at least ",intro_min,
    " distinct justified verified PMIDs cited within its literature paragraphs."))
  intro <- web_c2_8e_cached_section(cache,"introduction",intro_prompt,
    context_text,e,engine,6500L)
  ci <- e$extract_pmids(intro)
  web_assert(all(ci %in% ids),
    "AI Introduction contains an unverified PMID; no DOCX generated.",
    "CITATION_VALIDATION_FAILED")
  # Expose the actual Introduction citations to the Discussion drafting prompt;
  # do not count the same PMID twice toward the manuscript-wide gate.
  discussion_context<-context
  discussion_context$already_cited_in_introduction<-as.list(ci)
  need_new<-max(0L,total-length(ci))
  discussion_prompt <- paste(base_rules,
    paste0("Write a balanced, detailed Discussion with at least ",discussion_min,
    " distinct justified verified PMIDs, ideally including ",need_new,
    " pertinent PMIDs not already cited in Introduction. Do not pad citations. ",
    "Explain study-level limitations and avoid unsupported comparative inferences."))
  discussion_input<-as.character(jsonlite::toJSON(discussion_context,
    auto_unbox=TRUE,null="null",digits=NA))
  discussion <- web_c2_8e_cached_section(cache,"discussion",discussion_prompt,
    discussion_input,e,engine,10500L)
  cd <- e$extract_pmids(discussion)
  web_assert(all(cd %in% ids),
    "AI Discussion contains an unverified PMID; no DOCX generated.",
    "CITATION_VALIDATION_FAILED")
  cov<-web_c2_8e_coverage(intro,discussion,e$extract_pmids)
  coverage_ok <- function(z) length(z$intro)>=intro_min &&
    length(z$discussion)>=discussion_min && length(z$all)>=total
  # Automatic, bounded source-coverage recovery. A shortfall triggers a new
  # phase within THE SAME Worker job, not an immediate FAILED transition.
  # Only the Discussion is expanded by default; Introduction is touched only
  # if its own threshold is still not met. No citation-only padding.
  extra<-list(records=list(),report=list(status="NOT_NEEDED"))
  initial_cov<-cov
  if(!coverage_ok(cov)) {
    progress("COVERAGE_SHORTFALL",paste0(
      web_c2_8g_coverage_message(cov,limits),
      " — continuing automatically with evidence-supported Discussion expansion"),cov)
  }
  accept_discussion<-function(candidate,current) {
    cited_candidate<-e$extract_pmids(candidate)
    web_assert(all(cited_candidate %in% ids),
      "Discussion expansion introduced an unverified PMID.",
      "CITATION_VALIDATION_FAILED")
    newcov<-web_c2_8e_coverage(intro,candidate,e$extract_pmids)
    # Keep existing source anchors and never sacrifice valid citation coverage.
    if(!all(current$discussion %in% newcov$discussion) ||
       length(newcov$all)<length(current$all) ||
       length(newcov$discussion)<length(current$discussion))
      return(NULL)
    list(text=candidate,coverage=newcov)
  }
  # First reuse the literature that the plan has already verified.
  if(!coverage_ok(cov)) {
    progress("EXPANDING_DISCUSSION",paste0(
      "Expanding Discussion with previously verified literature: ",
      web_c2_8g_coverage_message(cov,limits)),cov,round=1L)
    candidate<-web_c2_8e_revise_section("Discussion",discussion,intro,context,refs,
      intro_min,discussion_min,total,e,cache,engine,round=1L)
    accepted<-accept_discussion(candidate,cov)
    if(!is.null(accepted)) { discussion<-accepted$text; cov<-accepted$coverage }
  }
  # If a true deficit remains, discover and verify additional relevant prior
  # studies using isolated Canonical R/39 PubMed functions. Never invent PMIDs.
  if(!coverage_ok(cov)) {
    progress("SEARCHING_PUBMED",paste0(
      web_c2_8g_coverage_message(cov,limits),
      " — searching PubMed for relevant earlier studies and reviews"),cov)
    pubmed_settings<-if(length(scientific_library)==1L &&
      !is.na(scientific_library) && nzchar(as.character(scientific_library)))
        list(engine_library=as.character(scientific_library)) else NULL
    extra<-tryCatch(web_c2_8g_discover_older(x,refs,engine,
      max(1L,total-length(cov$all)),cache,settings=pubmed_settings),error=function(err)
        list(records=list(),report=list(status="UNAVAILABLE",
          note=substr(conditionMessage(err),1L,280L))))
    if(length(extra$records)) {
      refs<-c(refs,extra$records)
      ids<-vapply(refs,function(r)web_c2_handoff_one(r$pmid),character(1))
      web_assert(!anyDuplicated(ids) && all(vapply(refs,web_c2_ref_valid,logical(1))),
        "Verified PubMed library expansion failed integrity checks.",
        "CITATION_VALIDATION_FAILED")
      context$verified_literature<-lapply(refs,function(r)list(
        pmid=r$pmid,title=r$title,year=r$year,journal=r$journal,
        abstract=substr(as.character(r$abstract %||b2% ""),1L,900L)))
    }
  }
  # Continue without a page reload. Two bounded Discussion continuation passes,
  # each tied to a distinct checkpoint. Source records and counts are auditable.
  for(round in 2:3) {
    if(coverage_ok(cov))break
    progress("EXPANDING_DISCUSSION",paste0(
      "Reviewing prior studies and expanding Discussion (pass ",round,"): ",
      web_c2_8g_coverage_message(cov,limits)),cov,round=round)
    candidate<-web_c2_8e_revise_section("Discussion",discussion,intro,context,refs,
      intro_min,discussion_min,total,e,cache,engine,round=round)
    accepted<-accept_discussion(candidate,cov)
    if(!is.null(accepted)) {discussion<-accepted$text;cov<-accepted$coverage}
  }
  if(!coverage_ok(cov) && length(cov$intro)<intro_min) {
    progress("EXPANDING_DISCUSSION",paste0(
      "Introduction is also below its minimum; refining source-supported background. ",
      web_c2_8g_coverage_message(cov,limits)),cov,round=4L)
    candidate<-web_c2_8e_revise_section("Introduction",intro,discussion,context,refs,
      intro_min,discussion_min,total,e,cache,engine,round=1L)
    cited_candidate<-e$extract_pmids(candidate)
    web_assert(all(cited_candidate %in% ids),
      "Introduction refinement introduced an unverified PMID.",
      "CITATION_VALIDATION_FAILED")
    newcov<-web_c2_8e_coverage(candidate,discussion,e$extract_pmids)
    if(all(cov$intro %in% newcov$intro) &&
       length(newcov$all)>=length(cov$all) &&
       length(newcov$intro)>=length(cov$intro)) {
      intro<-candidate;cov<-newcov
    }
  }
  # Critical: a meaningful, PubMed-verified body remains the source of truth;
  # never add arbitrary citations or lower the 25/8/15 policy for a PASS.
  if(!coverage_ok(cov))progress("EVIDENCE_REVIEW_REQUIRED",paste0(
    "Automatic Discussion expansion completed, but relevant literature is still insufficient: ",
    web_c2_8g_coverage_message(cov,limits),
    ". Draft checkpoint retained; author/source review required"),cov)
  web_assert(coverage_ok(cov),
    paste0("Automatic literature expansion exhausted: Introduction ",length(cov$intro),"/",intro_min,
      ", Discussion ",length(cov$discussion),"/",discussion_min,
      ", unique body ",length(cov$all),"/",total,
      ". Relevant sources could not justify the remaining citations; saved draft available for audit. No DOCX generated.") ,
    "CITATION_VALIDATION_FAILED")
  progress("CHECKING_CITATIONS",paste0(
    "Reference coverage achieved: ",web_c2_8g_coverage_message(cov,limits),
    ". Checking citation and numeric integrity"),cov)
  # Abstract is requested AFTER adequate body citations, so unsuccessful
  # citation coverage does not waste an additional provider call.
  # 9B: deterministic Structured Abstract ensures the actual source-verified
  # numerical results are present; no AI may rewrite or invent numerical facts.
  abstract <- web_c2_9b_abstract(x,methods,results)
  if(!is.null(publication_assets))
    abstract<-web_c2_9cd_abstract(abstract,publication_assets)
  publication_qc <- web_c2_9b_publication_qc(abstract,methods,results,x)
  web_assert(all(nzchar(c(intro,discussion,abstract))),
    "Manuscript generation returned an empty required section.",
    "DOCUMENT_GENERATION_FAILED")
  # Strict automated reference gate. This is intentionally unchanged in
  # scientific meaning: counts are actual body citations, not library size.
  ci<-e$extract_pmids(intro);cd<-e$extract_pmids(discussion)
  cited<-unique(c(ci,cd,e$extract_pmids(abstract)))
  web_assert(all(cited %in% ids),
    "AI draft cites a PMID not in the PubMed-verified library.",
    "CITATION_VALIDATION_FAILED")
  web_assert(length(ci)>=intro_min && length(cd)>=discussion_min &&
    length(unique(c(ci,cd)))>=total,
    paste0("Generated body reference gate FAILED: Introduction ",length(ci),"/",intro_min,
      ", Discussion ",length(cd),"/",discussion_min,
      ", unique body ",length(unique(c(ci,cd))),"/",total,
      ". No DOCX has been generated."),"CITATION_VALIDATION_FAILED")
  # No unconditional per-paragraph citation gate: review-specific objectives,
  # interpretations and limitations may legitimately have uncited paragraphs.
  # The actual PMID whitelist and 25/8/15 (or configured) coverage gates above
  # remain fail-closed. The scientific author reviews evidence/claim fit in Word.
  # Canonical R/40 numeric-token parser is used as the Web adapter's
  # automated preflight, NOT described as a full R/40 PASS.
  # The structured Abstract and Methods/Results are deterministic, source-
  # bound text and legitimately contain source-reported numerics. The zero-new-
  # numbers rule still applies to AI-generated Introduction/Discussion only.
  # 9BCD.5.1: Human-verified numerical signatures in AI prose are allowed;
  # unbound values still fail closed. Standalone R/40 release QC is separate.
  numeric_precheck<-list(
    Introduction=web_c2_9f_numeric_source_gate(intro,"Introduction",x,n),
    Discussion=web_c2_9f_numeric_source_gate(discussion,"Discussion",x,n))
  # Enforce stable source provenance: all study results appear verbatim in
  # deterministic Results or their linked editable source-indexed table.
  limitations<-if(narrative)
    "The findings are a source-indexed narrative synthesis, not pooled estimates. Study designs, endpoint definitions, and timepoints may differ. Author assessment of risk of bias and applicability is required before submission." else
    "Interpretation is limited by the included studies, analysis assumptions, heterogeneity and risk-of-bias judgments. Author review is required before submission."
  conclusions<-if(narrative)
    "The verified study-level results have been summarized narratively. No pooled efficacy or safety claim can be made from this synthesis alone." else
    "The analysis findings should be interpreted in the context of the approved pooled estimates and their documented limitations."
  declarations<-"Funding, conflicts of interest, author contributions and ethics declarations: [AUTHOR REVIEW REQUIRED]."
  # Discussion closes with an integrated limitations paragraph and a distinct
  # concluding paragraph, rather than standalone numbered manuscript sections.
  # The AI Discussion already contains source-supported limitations and a
  # closing synthesis where present. Do not duplicate them by adding stock prose.
  discussion_end<-discussion
  if(!grepl("limitation",tolower(discussion),fixed=TRUE))
    discussion_end<-paste(discussion_end,paste0("This review has several limitations. ",limitations),sep="\n\n")
  ending<-tolower(substr(discussion,max(1L,nchar(discussion)-2300L),nchar(discussion)))
  if(!grepl("overall|in conclusion|taken together|collectively|in summary",ending,perl=TRUE))
    discussion_end<-paste(discussion_end,
      paste0("Taken together, ",sub("^[Tt]he ","the ",conclusions)),sep="\n\n")
  pieces<-c(paste0("# ",web_c2_9b_title(x)),"## Abstract",abstract,
    "## Keywords",paste("systematic review;",if(narrative)"narrative synthesis" else "meta-analysis"),
    "## Introduction",intro,"## Methods",methods,"## Results",results,
    "## Discussion",discussion_end,"## Declarations",declarations)
  body<-paste(pieces,collapse="\n\n")
  if(narrative) {
    included_pmids<-unname(web_c2_9e_narrative_pmids(x,refs))
    web_assert(all(included_pmids %in% e$extract_pmids(body)),
      "A Human Finalized included study is missing from body citations.",
      "CITATION_VALIDATION_FAILED")
  }
  all_order<-e$extract_pmids(body)
  numbers<-data.frame(pmid=all_order,citation_number=seq_along(all_order),stringsAsFactors=FALSE)
  numbered<-e$replace_tokens_with_numbers(body,numbers)
  indexed<-setNames(refs,ids)
  bib<-vapply(seq_along(all_order),function(i)
    paste0(i,". ",e$format_vancouver_reference(indexed[[all_order[[i]]]])),character(1))
  labels<-unname(web_c2_artifact_labels(x))
  doc_text<-paste(c(numbered,"## References",bib),collapse="\n\n")
  qc<-list(status="AUTOMATED_DRAFT_CHECKS_PASS",
    mode=if(narrative)"NARRATIVE" else "QUANTITATIVE",
    intro_unique=length(ci),discussion_unique=length(cd),
    body_unique=length(unique(c(ci,cd))),reference_minimum=total,
    canonical_R40_status="NOT_RUN: standalone R/38-R/40 production evidence not present in this Web route",
    author_review="REQUIRED",risk_of_bias_pending=if(narrative)isTRUE(x$source$narrative$unresolved_rob) else FALSE,
    source_sha256=x$source_sha256,reference_sha256=x$reference_sha256,
    citation_review_mode="CANONICAL_REFERENCE_GATES_ONLY",
    initial_body_unique=length(initial_cov$all),
    additional_verified_pmids=as.list(vapply(extra$records,function(r)
      web_c2_handoff_one(r$pmid),character(1))),
    additional_verified_count=length(extra$records),
    expansion_method="BOUNDED_DISCUSSION_FIRST_WITH_OPTIONAL_OLDER_PUBMED",
    publication_9b=publication_qc,
    numeric_source_preflight=numeric_precheck)
  list(text=doc_text,citations=numbers,legends=labels,blocks=blocks,qc=qc,
    verification=list(base_library_sha256=x$reference_sha256,
      supplementary=extra,all_records=refs))
}



# RC28.25.9BCD.4 — immutable Human Final Study Selection provenance.
# Stored separately from the subset of studies with extractable outcome rows.
web_c2_9cd_freeze_selection <- function(packet,version_id,source_digest,manifest) {
  f<-packet$final_study_selection
  ids<-if(is.list(f))as.character(unlist(f$study_ids,use.names=FALSE)) else character()
  ids<-trimws(ids)
  web_assert(is.list(f) && identical(web_c2_9cd_stem(f$status),"HUMAN_CONFIRMED") &&
    length(ids)>0L && all(!is.na(ids) & nzchar(ids)) && !anyDuplicated(ids),
    "Human Final Study Selection is not finalized or has invalid Study IDs.",
    "USER_ACTION_REQUIRED")
  actual_digest<-web_b2_synthesis_source_digest(packet)
  web_assert(identical(web_c2_9cd_stem(actual_digest),
    web_c2_9cd_stem(source_digest)) && nzchar(web_c2_9cd_stem(source_digest)),
    "Human Final Study Selection and narrative synthesis have different source digests.",
    "STALE_UPSTREAM_VERSION")
  list(status="HUMAN_CONFIRMED",study_ids=as.list(ids),
    source_version=version_id,source_digest=source_digest,
    source_manifest_sha256=web_c2_hash(manifest),
    count=length(ids),authority="FINAL_HUMAN_STUDY_SELECTION")
}
web_c2_9cd_human_ids <- function(x) {
  s<-x$source$narrative$human_final_selection
  ids<-if(is.list(s))as.character(unlist(s$study_ids,use.names=FALSE)) else character()
  ids<-trimws(ids)
  web_assert(is.list(s) && identical(web_c2_9cd_stem(s$status),"HUMAN_CONFIRMED") &&
    identical(web_c2_9cd_stem(s$source_version),web_c2_9cd_stem(x$evidence_version)) &&
    identical(web_c2_9cd_stem(s$source_digest),web_c2_9cd_stem(x$source$narrative$source_digest)) &&
    identical(web_c2_9cd_stem(s$source_manifest_sha256),
      web_c2_hash(x$source$narrative$source_manifest)) &&
    length(ids)>0L && all(!is.na(ids) & nzchar(ids)) && !anyDuplicated(ids) &&
    identical(as.integer(s$count),as.integer(length(ids))),
    "Missing or inconsistent Human Final Study Selection binding. Regenerate a new immutable PLAN from the finalized Evidence Version.",
    "USER_ACTION_REQUIRED")
  rows<-x$source$narrative$rows
  if(is.list(rows) && length(rows)) {
    observed<-unique(vapply(rows,function(r)web_c2_9cd_stem(r$study_id),character(1)))
    web_assert(all(nzchar(observed)) && all(observed %in% ids),
      "Narrative outcomes contain a Study ID outside Human Final Study Selection.",
      "SOURCE_INTEGRITY_FAILED")
  }
  ids
}
# Reconciliation operates ONLY on checksum-verified copies in the private
# publication staging directory. R/24/R/35 originals and immutable Evidence stay untouched.
web_c2_9cd_reconcile_prisma <- function(stage,x,original_paths,audit_path=NULL) {
  humans<-web_c2_9cd_human_ids(x)
  p_inc<-original_paths[["included_studies.csv"]]
  p_ft<-original_paths[["final_full_text_eligibility.csv"]]
  p_exc<-original_paths[["excluded_after_full_text.csv"]]
  read_tab<-function(p) {
    web_assert(is.character(p) && length(p)==1L && nzchar(p) && file.exists(p),
      "Frozen canonical PRISMA input is missing.","SOURCE_INTEGRITY_FAILED")
    utils::read.csv(p,stringsAsFactors=FALSE,check.names=FALSE,
      fileEncoding="UTF-8-BOM",colClasses="character")
  }
  inc<-read_tab(p_inc);ft<-read_tab(p_ft);exc<-read_tab(p_exc)
  web_assert(nrow(inc)>0L && all(c("screening_id","final_decision") %in% names(inc)) &&
    all(c("screening_id","final_decision") %in% names(ft)) &&
    all(c("screening_id","final_exclusion_reason") %in% names(exc)),
    "Canonical PRISMA inputs lack stable Screening IDs or decision/reason columns; automatic reconciliation refused.",
    "USER_ACTION_REQUIRED")
  ids<-function(d)trimws(as.character(d$screening_id))
  inc_sid<-ids(inc); ft_sid<-ids(ft); exc_sid<-ids(exc)
  web_assert(all(nzchar(inc_sid) & !is.na(inc_sid)) && !anyDuplicated(inc_sid) &&
    all(nzchar(ft_sid) & !is.na(ft_sid)) && !anyDuplicated(ft_sid) &&
    !anyDuplicated(exc_sid[nzchar(exc_sid)]),
    "PRISMA Study/Screening ID mapping is missing or ambiguous.",
    "USER_ACTION_REQUIRED")
  # Prefer direct Study ID identity; otherwise use exact Human-verified
  # (Study ID, Screening ID) bindings from the frozen narrative evidence.
  if("study_id" %in% names(inc) &&
     all(humans %in% trimws(as.character(inc$study_id)))) {
    inc_study<-trimws(as.character(inc$study_id))
    chosen<-inc_study %in% humans
    match_mode<-"R24_STUDY_ID"
  } else {
    rows<-x$source$narrative$rows
    study<-vapply(rows,function(r)web_c2_9cd_stem(r$study_id),character(1))
    screening<-vapply(rows,function(r)web_c2_9cd_stem(r$screening_id),character(1))
    web_assert(all(humans %in% study) && all(nzchar(screening)) &&
      all(vapply(humans,function(h)length(unique(screening[study==h]))==1L,logical(1))),
      "Some Human Finalized Study IDs cannot be uniquely linked to the R/24 Screening IDs; refuse inferred study counts.",
      "USER_ACTION_REQUIRED")
    chosen_ids<-unique(screening[study %in% humans])
    chosen<-inc_sid %in% chosen_ids
    match_mode<-"PDF_VERIFIED_STUDY_SCREENING_BINDING"
  }
  matched_humans<-if(identical(match_mode,"R24_STUDY_ID"))inc_study[chosen]
    else unique(study[study %in% humans])
  web_assert(sum(chosen)==length(humans) && setequal(humans,matched_humans),
    "Human Finalized study set cannot be matched one-to-one to R/24 included rows.",
    "USER_ACTION_REQUIRED")
  # Where narrative PDF bindings exist, require the exact paired Screening ID
  # even if R/24 Study IDs happen to match. No cross-review identity mixing.
  evidence_rows<-x$source$narrative$rows
  if(is.list(evidence_rows))for(r in evidence_rows) {
    sid<-web_c2_9cd_stem(r$study_id)
    scr<-web_c2_9cd_stem(r$screening_id)
    if(!sid %in% humans)next
    expected<-if(identical(match_mode,"R24_STUDY_ID"))
      inc_sid[trimws(as.character(inc$study_id))==sid] else
      inc_sid[inc_sid==scr]
    web_assert(nzchar(scr) && length(expected)==1L &&
      identical(expected[[1L]],scr),
      "Frozen PDF Evidence and R/24 map a selected Study ID to different Screening IDs.",
      "SOURCE_INTEGRITY_FAILED")
  }
  # An included record must not already be excluded at the full-text stage.
  in_dec<-tolower(trimws(as.character(inc$final_decision)))
  web_assert(all(in_dec=="include"),
    "R/24 included_studies.csv has non-Include decisions.",
    "SOURCE_INTEGRITY_FAILED")
  removed<-inc[!chosen,,drop=FALSE]
  removed_ids<-ids(removed)
  web_assert(all(inc_sid %in% ft_sid) && !any(removed_ids %in% exc_sid),
    "Canonical full-text decisions do not match R/24 or contain conflicting exclusions.",
    "USER_ACTION_REQUIRED")
  original_sha<-vapply(original_paths[c("included_studies.csv",
    "final_full_text_eligibility.csv","excluded_after_full_text.csv")],
    web_c2_9cd_sha,character(1))
  if(length(removed_ids)) {
    pos<-match(removed_ids,ft_sid)
    web_assert(length(pos)==length(removed_ids) && all(!is.na(pos)) &&
      all(tolower(trimws(as.character(ft$final_decision[pos])))=="include"),
      "R/24 inclusion cannot be traced to a single previous full-text Include decision.",
      "USER_ACTION_REQUIRED")
    ft$final_decision[pos]<-"Exclude"
    reason<-"Excluded at Human Final Study Selection (specific reason not recorded)"
    for(i in seq_along(removed_ids)) {
      record<-as.list(rep("",ncol(exc)));names(record)<-names(exc)
      shared<-intersect(names(exc),names(ft))
      for(k in shared)record[[k]]<-as.character(ft[[k]][pos[[i]]])
      record[["screening_id"]]<-removed_ids[[i]]
      record[["final_exclusion_reason"]]<-reason
      exc<-rbind(exc,as.data.frame(record,stringsAsFactors=FALSE,
        check.names=FALSE))
    }
    write_stage<-function(d,p)utils::write.csv(d,p,row.names=FALSE,
      na="",fileEncoding="UTF-8")
    write_stage(inc[chosen,,drop=FALSE],p_inc)
    write_stage(ft,p_ft)
    write_stage(exc,p_exc)
  }
  web_assert(nrow(read_tab(p_inc))==length(humans) &&
    sum(tolower(trimws(as.character(read_tab(p_ft)$final_decision)))=="exclude") ==
      sum(tolower(trimws(as.character(ft$final_decision)))=="exclude"),
    "Staged PRISMA reconciliation did not preserve the Human Finalized study count.",
    "SOURCE_INTEGRITY_FAILED")
  audit<-list(status=if(length(removed_ids))"STAGED_HUMAN_FINAL_RECONCILED"
      else "ALREADY_HUMAN_FINAL_CONSISTENT",
    evidence_version=x$evidence_version,
    human_final_study_ids=as.list(humans),
    human_final_included_count=length(humans),
    original_R24_included_count=nrow(inc),
    staged_R24_included_count=nrow(read_tab(p_inc)),
    original_R24_screening_ids=as.list(inc_sid),
    staged_included_screening_ids=as.list(ids(read_tab(p_inc))),
    newly_excluded_screening_ids=as.list(removed_ids),
    exclusion_reason_provenance="Human Final Study Selection; detailed reason unavailable",
    mapping_method=match_mode,original_source_sha256=as.list(original_sha),
    no_source_or_database_mutation=TRUE)
  if(!is.null(audit_path))
    jsonlite::write_json(audit,audit_path,auto_unbox=TRUE,pretty=TRUE,null="null")
  audit
}

# RC28.25.9C/9D — source-pinned publication assets and Word assembly.
# No modification of the canonical scientific code or saved evidence.
web_c2_9cd_sha <- function(path) digest::digest(file=path,algo="sha256",serialize=FALSE)
web_c2_9cd_stem <- function(x) {
  v<-as.character(unlist(x,use.names=FALSE))
  if(!length(v) || is.na(v[[1L]]))"" else trimws(v[[1L]])
}
web_c2_9cd_manifest <- function(x) {
  m<-if(identical(web_c2_9cd_stem(x$source$route),"NARRATIVE"))
    x$source$narrative$source_manifest else x$source$evidence_manifest
  if(!is.list(m) || is.null(names(m)))return(list())
  m
}
web_c2_9cd_signed_file <- function(root,manifest,filename) {
  matches<-names(manifest)[basename(gsub(intToUtf8(92),"/",names(manifest),fixed=TRUE))==filename]
  # Ambiguous registered outputs are not resolved by arbitrary directory order.
  if(length(matches)!=1L)return("")
  rel<-matches[[1L]]
  if(grepl("(^/|^[A-Za-z]:|(^|[/\\\\])\\.\\.([/\\\\]|$))",rel,perl=TRUE))return("")
  abs<-normalizePath(file.path(root,rel),winslash="/",mustWork=FALSE)
  base<-normalizePath(root,winslash="/",mustWork=TRUE)
  if(!startsWith(abs,paste0(base,"/")) || !file.exists(abs) || dir.exists(abs))return("")
  want<-web_c2_9cd_stem(manifest[[rel]])
  if(!grepl("^[a-f0-9]{64}$",want))return("")
  if(!identical(web_c2_9cd_sha(abs),want))return("")
  abs
}
web_c2_9cd_copy_signed <- function(root,stage,manifest,filename) {
  from<-web_c2_9cd_signed_file(root,manifest,filename)
  if(!nzchar(from))return("")
  rel<-names(manifest)[basename(gsub(intToUtf8(92),"/",names(manifest),fixed=TRUE))==filename][[1L]]
  dest<-file.path(stage,rel)
  dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
  stopifnot(isTRUE(file.copy(from,dest,overwrite=FALSE)))
  dest
}
web_c2_9cd_run_script <- function(script,wd,tag) {
  rscript<-file.path(R.home("bin"),if(.Platform$OS.type=="windows")"Rscript.exe" else "Rscript")
  if(!file.exists(rscript))return(list(ok=FALSE,error=paste0(tag," Rscript not available")))
  before<-getwd();on.exit(setwd(before),add=TRUE);setwd(wd)
  output<-tryCatch(suppressWarnings(system2(rscript,
    args=c("--vanilla",shQuote(normalizePath(script,winslash="/",mustWork=TRUE))),
    stdout=TRUE,stderr=TRUE,wait=TRUE)),error=function(e)e)
  if(inherits(output,"error"))return(list(ok=FALSE,error=substr(conditionMessage(output),1L,350L)))
  status<-attr(output,"status");if(is.null(status))status<-0L
  list(ok=identical(as.integer(status),0L),
    error=if(status==0L)"" else paste(tag,"failed:",substr(paste(tail(output,4L),collapse=" "),1L,300L)))
}
web_c2_9cd_read_tab <- function(path) {
  if(!nzchar(path) || !file.exists(path))return(NULL)
  t<-tryCatch(utils::read.csv(path,check.names=FALSE,stringsAsFactors=FALSE,
    fileEncoding="UTF-8-BOM"),error=function(e)NULL)
  if(is.null(t) || !ncol(t) || !nrow(t))return(NULL)
  t[]<-lapply(t,function(z)ifelse(is.na(z),"NR",as.character(z)))
  t
}
web_c2_9cd_canonical_inputs <- function() c(
  "all_database_records_before_deduplication.csv",
  "deduplicated_records_for_screening.csv","removed_duplicate_records.csv",
  "final_title_abstract_screening.csv","included_for_full_text_review.csv",
  "full_text_registry.csv","final_full_text_eligibility.csv",
  "excluded_after_full_text.csv","included_studies.csv")
# 9BCD.1: a lightweight, no-provider source preflight that is performed
# BEFORE reporting BUILDING_PUBLICATION_ASSETS. Running R/34/R/35 remains in
# web_c2_9cd_prepare(), after this check and still before any AI calls.
web_c2_9cd_source_preflight <- function(root,x) {
  manifest<-web_c2_9cd_manifest(x)
  inputs<-web_c2_9cd_canonical_inputs()
  verified<-vapply(inputs,function(name)
    nzchar(web_c2_9cd_signed_file(root,manifest,name)),logical(1))
  missing<-inputs[!verified]
  web_assert(!length(missing),paste0(
    "Publication source preflight blocked: missing or hash-invalid same-version canonical CSV: ",
    paste(missing,collapse=", "),
    ". No AI or PubMed call was started. Correct the Evidence source binding first."),
    "USER_ACTION_REQUIRED")
  invisible(list(status="SOURCE_PREFLIGHT_PASS",files=length(inputs),
    source_manifest_sha256=web_c2_hash(manifest)))
}

web_c2_9cd_study_table <- function(x) {
  rows<-web_c2_9b_verified_rows(x)
  if(!length(rows))return(NULL)
  ids<-unique(vapply(rows,function(r)web_c2_9b_one(r$study_id),character(1)))
  entries<-lapply(ids,function(id) {
    matched<-Filter(function(r)identical(web_c2_9b_one(r$study_id),id),rows)
    data.frame(Study=web_c2_9e_study_short_label(matched[[1L]]$study_label),
      `Source-reported endpoints`=paste(unique(vapply(matched,function(r)
        web_c2_9b_plain(r$original_outcome),character(1))),collapse="; "),
      `Reported follow-up/timepoints`=paste(unique(Filter(nzchar,vapply(matched,function(r)
        web_c2_9b_plain(r$reported_timepoint),character(1)))),collapse="; "),
      check.names=FALSE,stringsAsFactors=FALSE)
  })
  do.call(rbind,entries)
}
web_c2_9cd_outcome_table <- function(x) {
  rows<-web_c2_9b_verified_rows(x)
  if(!length(rows))return(NULL)
  data.frame(Study=vapply(rows,function(r)web_c2_9e_study_short_label(r$study_label),character(1)),
    Outcome=vapply(rows,function(r)web_c2_9b_plain(r$original_outcome),character(1)),
    `Source timepoint`=vapply(rows,function(r)web_c2_9b_plain(r$reported_timepoint),character(1)),
    `Verified numerical result`=vapply(rows,function(r)
      web_c2_9b_publication_summary(r$numeric_summary),character(1)),
    check.names=FALSE,stringsAsFactors=FALSE)
}
web_c2_9cd_evidence_chart <- function(x,path) {
  rows<-web_c2_9b_verified_rows(x)
  if(!length(rows))return(FALSE)
  titles<-unique(vapply(rows,web_c2_9b_group_title,character(1)))
  if(!length(titles))return(FALSE)
  counts<-vapply(titles,function(t)length(unique(vapply(Filter(function(r)
    identical(web_c2_9b_group_title(r),t),rows),function(r)
    web_c2_9b_one(r$study_id),character(1)))),integer(1))
  # This is an evidence-coverage figure, never a forest plot or effect estimate.
  cap<-min(13L,length(counts))
  ord<-order(counts,decreasing=TRUE)[seq_len(cap)]
  counts<-rev(counts[ord]);titles<-rev(titles[ord])
  titles<-substr(gsub("[\r\n]+"," ",titles),1L,65L)
  okay<-tryCatch({
    grDevices::png(path,width=1500,height=max(700L,115L*length(titles)),res=130)
    graphics::par(mar=c(5,19,4,3))
    graphics::barplot(counts,horiz=TRUE,names.arg=titles,las=1,
      xlab="Number of distinct included studies reporting the endpoint",
      main="Study-level evidence coverage (not pooled effects)",
      col="gray65",border="gray40",xlim=c(0,max(1,max(counts))*1.25))
    grDevices::dev.off()
    TRUE
  },error=function(e) {try(grDevices::dev.off(),silent=TRUE);FALSE})
  isTRUE(okay) && file.exists(path) && file.info(path)$size>100L
}
web_c2_9cd_prepare <- function(root,engine,out,x) {
  manifest<-web_c2_9cd_manifest(x)
  stage<-tempfile("rc259cd-source-",tmpdir=dirname(out))
  dir.create(stage,recursive=TRUE,showWarnings=FALSE)
  manifest_sha<-if(length(manifest))web_c2_hash(manifest) else "NOT_BOUND"
  inputs<-web_c2_9cd_canonical_inputs()
  paths<-vapply(inputs,function(name)web_c2_9cd_copy_signed(root,stage,manifest,name),character(1))
  missing<-names(paths)[!nzchar(paths)]
  r35<-file.path(engine,"R/35_generate_prisma_flow_diagram.R")
  r34<-file.path(engine,"R/34_generate_manuscript_tables.R")
  scientific_manifest<-file.path(engine,"web/docs/engine-source-manifest.json")
  if(file.exists(scientific_manifest)) {
    em<-jsonlite::read_json(scientific_manifest,simplifyVector=FALSE)
    for(s in c("34_generate_manuscript_tables.R","35_generate_prisma_flow_diagram.R")) {
      entry<-em[[paste0("R/",s)]]
      if(!is.null(entry))web_assert(identical(web_c2_9cd_sha(file.path(engine,"R",s)),
        web_c2_9cd_stem(entry)),paste0("Scientific source changed: ",s),"SOURCE_INTEGRITY_FAILED")
    }
  }
  result<-list(stage=stage,source_manifest_sha256=manifest_sha,source_version=x$evidence_version,
    missing_prisma_inputs=as.list(missing),prisma_status="NOT_BOUND",r34_status="NOT_RUN",
    prisma_png="",studies_included=NA_integer_,tables=list(),figures=list(),
    messages=character())
  if(length(missing)) {
    result$messages<-c(result$messages,paste0("PRISMA source binding incomplete (",length(missing),
      " canonical files missing/unverified): ",paste(missing,collapse=", ")))
    return(result) # Fail before AI, never synthesize PRISMA counts.
  }
  # Human Final Study Selection is authoritative, not the number of outcome
  # rows and not a possibly stale R/24 include CSV. Resolve by exact IDs in
  # this isolated, provenance-audited staging directory before R/35 runs.
  if(identical(web_c2_9cd_stem(x$source$route),"NARRATIVE")) {
    selection_audit<-file.path(out,"human_final_prisma_reconciliation.json")
    reconciled<-web_c2_9cd_reconcile_prisma(stage,x,paths,selection_audit)
    result$human_final_selection_sha256<-web_c2_9cd_sha(selection_audit)
    result$human_final_selection_status<-reconciled$status
    result$human_final_studies<-reconciled$human_final_included_count
  }
  # Canonical R/35 only runs in a private, single-review staging workspace.
  if(!file.exists(r35)) {result$messages<-"Canonical R/35 is unavailable";return(result)}
  run35<-web_c2_9cd_run_script(r35,stage,"Canonical R/35 PRISMA")
  if(!run35$ok) {result$messages<-run35$error;return(result)}
  png<-file.path(stage,"results/prisma/prisma_2020_flow_diagram.png")
  audit<-file.path(stage,"results/prisma/prisma_flow_audit.csv")
  if(!file.exists(png) || !file.exists(audit)) {
    result$messages<-"R/35 did not produce both the PRISMA PNG and count audit"
    return(result)
  }
  csv<-paths[["included_studies.csv"]]
  # Inclusion counts must match the *same* finalized manuscript study universe.
  included<-tryCatch(nrow(utils::read.csv(csv,check.names=FALSE)),error=function(e)NA_integer_)
  if(is.na(included)) {result$messages<-"Could not parse verified included_studies.csv";return(result)}
  if(identical(web_c2_9cd_stem(x$source$route),"NARRATIVE") &&
     included!=length(web_c2_9cd_human_ids(x))) {
    result$messages<-paste0("PRISMA HUMAN FINALIZED COUNT CONFLICT: R/35 staged output includes ",
      included," but immutable Human Final Study Selection has ",
      length(web_c2_9cd_human_ids(x)),". Never substitute an outcome-row count.")
    return(result)
  }
  pinned_png<-file.path(out,"figure1_prisma.png")
  web_assert(isTRUE(file.copy(png,pinned_png,overwrite=TRUE)),
    "Could not stage a PRISMA image for DOCX insertion.","DOCUMENT_GENERATION_FAILED")
  file.copy(audit,file.path(out,"prisma_flow_audit.csv"),overwrite=TRUE)
  result$prisma_png<-pinned_png
  result$prisma_status<-"CANONICAL_R35_PASS"
  result$studies_included<-included
  result$figures[[1L]]<-list(title="Figure 1. PRISMA 2020 flow diagram",path=pinned_png,
    origin="Same-version canonical R/35, source-audited")
  # R/34 is attempted from the SAME source-pinned staging area, not repo global.
  # Only snapshot-registered CSVs and the same config enter the private workspace.
  all_csv<-names(manifest)[grepl("[.]csv$",names(manifest),ignore.case=TRUE)]
  for(rel in all_csv) {
    bn<-basename(gsub(intToUtf8(92),"/",rel,fixed=TRUE))
    if(!bn %in% inputs && length(all_csv)<250L)web_c2_9cd_copy_signed(root,stage,manifest,bn)
  }
  cfg<-file.path(root,"config/project_config.yml")
  if(file.exists(cfg)) {
    # The snapshot is already version-bound to this manuscript, not user's repo.
    dir.create(file.path(stage,"config"),recursive=TRUE,showWarnings=FALSE)
    file.copy(cfg,file.path(stage,"config/project_config.yml"))
  }
  if(file.exists(r34) && file.exists(file.path(stage,"config/project_config.yml"))) {
    run34<-web_c2_9cd_run_script(r34,stage,"Canonical R/34 tables")
    result$r34_status<-if(run34$ok)"CANONICAL_R34_PASS" else "R34_UNAVAILABLE"
    if(!run34$ok)result$messages<-c(result$messages,run34$error)
  } else result$r34_status<-"R34_INPUTS_OR_SCRIPT_UNAVAILABLE"
  for(spec in list(
      list(name="Table 1. Study characteristics",file="table1_study_characteristics.csv"),
      list(name="Table 2. Patient and baseline characteristics",file="table2_patient_characteristics.csv"),
      list(name="Table 3. Procedure/intervention characteristics",file="table3_intervention_characteristics.csv"))) {
    path<-file.path(stage,"results/manuscript/tables",spec$file)
    tab<-web_c2_9cd_read_tab(path)
    if(!is.null(tab))result$tables[[length(result$tables)+1L]]<-list(
      title=spec$name,data=tab,origin="Canonical R/34 from same-version staged inputs")
  }
  if(!any(vapply(result$tables,function(t)startsWith(t$title,"Table 1."),logical(1)))) {
    tab<-web_c2_9cd_study_table(x)
    if(!is.null(tab))result$tables[[length(result$tables)+1L]]<-list(
      title="Table 1. Study characteristics (source-reported endpoints and follow-up)",
      data=tab,origin="Human-verified study records; baseline descriptors unavailable")
    result$messages<-c(result$messages,
      "R/34 study/patient descriptors not source-bound; Table 1 is a limited evidence-derived draft")
  }
  tab4<-web_c2_9cd_outcome_table(x)
  if(!is.null(tab4))result$tables[[length(result$tables)+1L]]<-list(
    title="Table 4. Study-specific verified clinical outcomes",data=tab4,
    origin="Frozen Human-verified PDF numerical summaries")
  # Figure 2 describes evidence availability only, without fabricating pooled statistics.
  fig2<-file.path(out,"figure2_study_level_coverage.png")
  if(web_c2_9cd_evidence_chart(x,fig2))result$figures[[length(result$figures)+1L]]<-
    list(title="Figure 2. Study-level outcome evidence coverage (not pooled effects)",
      path=fig2,origin="Frozen Human-verified outcome inventory")
  result
}
web_c2_9cd_abstract <- function(abstract,assets) {
  if(!identical(assets$prisma_status,"CANONICAL_R35_PASS"))return(abstract)
  regexpr_ <- "Results: "
  replacement<-paste0("Results: PRISMA 2020 selection confirmed ",
    assets$studies_included," included studies (Figure 1). ")
  sub(regexpr_,replacement,abstract,fixed=TRUE)
}
web_c2_9cd_results <- function(results,assets) {
  if(!identical(assets$prisma_status,"CANONICAL_R35_PASS"))return(results)
  paste0(results,"\n\n### Source-verified study selection and presentation\n",
    "The selection flow was verified against the same-review canonical records (Figure 1). ",
    assets$studies_included," studies were included. Study characteristics and endpoint findings ",
    "are presented in the corresponding source-bound Tables; Figure 2 summarizes the number of studies ",
    "reporting individual endpoints and must not be interpreted as a pooled effect.")
}

# RC28.25.9BCD.5 — journal-neutral publication end matter.
# These definitions are Web-only; canonical R/34-R/41 remain unchanged.
web_c2_9e_abbrev_dictionary <- function() {
  c(ACC="aortic cross-clamp",AKI="acute kidney injury",
    CAD="coronary artery disease",CI="confidence interval",
    CKD="chronic kidney disease",COPD="chronic obstructive pulmonary disease",
    CPB="cardiopulmonary bypass",GFR="glomerular filtration rate",
    HR="hazard ratio",IQR="interquartile range",
    LVEF="left ventricular ejection fraction",NR="not reported",
    NYHA="New York Heart Association",OR="odds ratio",
    PG="pressure gradient",PPI="permanent pacemaker implantation",
    PPM="prosthesis-patient mismatch",
    PRISMA="Preferred Reporting Items for Systematic Reviews and Meta-Analyses",
    PVL="paravalvular leak",RR="risk ratio",SD="standard deviation",
    SE="standard error",SMD="standardized mean difference",
    SUAVR="sutureless/rapid-deployment aortic valve replacement",
    TAVR="transcatheter aortic valve replacement",
    TEER="transcatheter edge-to-edge repair")
}
web_c2_9e_abbrev_note <- function(data,extra="") {
  dict<-web_c2_9e_abbrev_dictionary()
  joined<-paste(c(extra,as.character(unlist(data,use.names=FALSE))),collapse=" ")
  found<-names(dict)[vapply(names(dict),function(k)
    grepl(paste0("(^|[^A-Za-z])",k,"([^A-Za-z]|$)"),joined,perl=TRUE),logical(1))]
  if(!length(found))return("")
  found<-found[order(tolower(found))]
  paste(paste0(found,", ",unname(dict[found])),collapse="; ")
}
web_c2_9e_table_panels <- function(tab,max_columns=5L) {
  tab<-as.data.frame(tab,stringsAsFactors=FALSE,check.names=FALSE)
  n<-ncol(tab)
  web_assert(n>0L,"Empty scholarly table.","DOCUMENT_GENERATION_FAILED")
  if(n<=max_columns)return(list(tab))
  # Never squeeze fifteen clinical columns into a six-inch page. Repeat the
  # study identifier in independently readable, lossless column panels.
  anchor<-1L
  cols<-seq.int(2L,n)
  groups<-split(cols,ceiling(seq_along(cols)/(max_columns-1L)))
  lapply(groups,function(i)tab[,c(anchor,i),drop=FALSE])
}
web_c2_9e_word_table <- function(tab,max_width) {
  z<-as.data.frame(tab,stringsAsFactors=FALSE,check.names=FALSE)
  z[]<-lapply(z,function(v) {v<-as.character(v);v[is.na(v)|!nzchar(trimws(v))]<-"NR";v})
  n<-ncol(z)
  web_assert(n<=5L,"Wide table panels were not split before layout.",
    "DOCUMENT_GENERATION_FAILED")
  w<-if(n==4L && grepl("numerical|result|estimate",tail(names(z),1L),ignore.case=TRUE))
    c(1.18,1.16,1.16,2.80) else if(n==5L)c(1.18,rep(1.33,4L)) else
      rep(6.45/n,n)
  w<-min(6.40,max_width)*w/sum(w)
  ft<-flextable::flextable(z)
  ft<-flextable::set_table_properties(ft,layout="fixed")
  ft<-flextable::width(ft,j=seq_len(n),width=w)
  ft<-flextable::fontsize(ft,size=8,part="all")
  ft<-flextable::valign(ft,valign="top",part="all")
  ft<-flextable::padding(ft,padding=2,part="all")
  ft<-flextable::bold(ft,part="header")
  ft
}
web_c2_9e_caption <- function(doc,text,e) {
  officer::body_add_fpar(doc,officer::fpar(
    officer::ftext(text,prop=officer::fp_text(bold=TRUE))),
    style=e$style_map$normal)
}
web_c2_9e_append_endmatter <- function(doc,assets,e) {
  web_assert(is.list(assets),"Publication assets are missing.",
    "DOCUMENT_GENERATION_FAILED")
  page<-officer::docx_dim(doc)
  width<-floor(100*(page$page[["width"]]-sum(page$margins[c("left","right")])))/100
  web_assert(is.finite(width) && width>0,"Publication page width is unavailable.","DOCUMENT_GENERATION_FAILED")
  doc<-officer::body_add_break(doc)
  doc<-officer::body_add_par(doc,"Tables",style=e$style_map$heading1)
  for(tab in assets$tables) {
    panels<-web_c2_9e_table_panels(tab$data)
    for(k in seq_along(panels)) {
      title<-if(length(panels)>1L)
        paste0(tab$title," (part ",k," of ",length(panels),")") else tab$title
      doc<-web_c2_9e_caption(doc,title,e)
      doc<-flextable::body_add_flextable(doc,web_c2_9e_word_table(panels[[k]],width))
      note<-web_c2_9e_abbrev_note(panels[[k]],title)
      if(nzchar(note))doc<-officer::body_add_par(doc,note,style=e$style_map$normal)
    }
  }
  doc<-officer::body_add_break(doc)
  doc<-officer::body_add_par(doc,"Figures",style=e$style_map$heading1)
  for(fig in assets$figures) {
    web_assert(file.exists(fig$path),paste("Missing publication figure:",fig$title),
      "DOCUMENT_GENERATION_FAILED")
    doc<-web_c2_9e_caption(doc,fig$title,e)
    dims<-e$fit_word_image_dimensions(fig$path,max_width=min(6.25,width),max_height=7.0)
    doc<-officer::body_add_img(doc,src=fig$path,
      width=dims[["width"]],height=dims[["height"]])
    note<-web_c2_9e_abbrev_note(character(),fig$title)
    if(nzchar(note))doc<-officer::body_add_par(doc,note,style=e$style_map$normal)
  }
  doc
}
# Modify only Word presentation XML; no scientific calculations, tables or images.
web_c2_9e_format_docx <- function(path) {
  web_assert(all(vapply(c("xml2","zip"),requireNamespace,logical(1),quietly=TRUE)),
    "DOCX formatting requires xml2 and zip packages.","DEPENDENCY_FAILED")
  dir<-tempfile("srma-docx-final-");dir.create(dir,recursive=TRUE)
  on.exit(unlink(dir,recursive=TRUE,force=TRUE),add=TRUE)
  utils::unzip(path,exdir=dir)
  ns<-c(w="http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  # Heading numbering was inherited from the canonical R/41 default template.
  styles_path<-file.path(dir,"word/styles.xml")
  web_assert(file.exists(styles_path),"Word styles missing.","DOCUMENT_GENERATION_FAILED")
  styles<-xml2::read_xml(styles_path)
  sn<-xml2::xml_find_all(styles,
    ".//w:style[w:name[starts-with(@w:val, 'heading ')]]/w:pPr/w:numPr",ns=ns)
  if(length(sn))xml2::xml_remove(sn)
  xml2::write_xml(styles,styles_path)
  doc_path<-file.path(dir,"word/document.xml")
  doc<-xml2::read_xml(doc_path)
  paras<-xml2::xml_find_all(doc,".//w:body/w:p",ns=ns)
  in_refs<-FALSE
  for(p in paras) {
    txt<-paste(xml2::xml_text(xml2::xml_find_all(p,".//w:t",ns=ns)),collapse="")
    txt<-trimws(txt)
    if(identical(txt,"References"))in_refs<-TRUE
    if(identical(txt,"Tables"))in_refs<-FALSE
    sty<-xml2::xml_find_first(p,"./w:pPr/w:pStyle",ns=ns)
    sty_id<-if(inherits(sty,"xml_missing"))"" else {
      v<-xml2::xml_attr(sty,"w:val",ns=ns)
      if(is.na(v))v<-xml2::xml_attr(sty,"val")
      if(is.na(v))"" else as.character(v)
    }
    # No leading indentation on headings/captions/numbered References or notices.
    is_head<-grepl("^([Tt]itre|[Hh]eading)[0-9]*$",sty_id) ||
      txt %in% c("Abstract","Keywords","Introduction","Methods","Results",
        "Discussion","Declarations","References","Tables","Figures")
    is_caption<-grepl("^(Table|Figure)[[:space:]][0-9]+[.]",txt)
    is_note<-startsWith(txt,"AUTO-GENERATED") || startsWith(txt,"WORKING DRAFT")
    ppr<-xml2::xml_find_first(p,"./w:pPr",ns=ns)
    if(inherits(ppr,"xml_missing"))ppr<-xml2::xml_add_child(p,"w:pPr",.where=0)
    spacing<-xml2::xml_find_first(ppr,"./w:spacing",ns=ns)
    if(inherits(spacing,"xml_missing"))spacing<-xml2::xml_add_child(ppr,"w:spacing")
    xml2::xml_set_attr(spacing,"w:line","360")
    xml2::xml_set_attr(spacing,"w:lineRule","auto")
    indent<-xml2::xml_find_first(ppr,"./w:ind",ns=ns)
    if(inherits(indent,"xml_missing"))indent<-xml2::xml_add_child(ppr,"w:ind")
    xml2::xml_set_attr(indent,"w:firstLine",
      if(!is_head && !is_caption && !is_note && !in_refs && nzchar(txt))"480" else "0")
  }
  xml2::write_xml(doc,doc_path)
  out<-tempfile("srma-repack-",fileext=".docx")
  on.exit(unlink(out),add=TRUE)
  entries<-list.files(dir,recursive=TRUE,all.files=TRUE,include.dirs=FALSE)
  web_assert(length(entries)>5L,"DOCX repack contains no files.",
    "DOCUMENT_GENERATION_FAILED")
  zip::zipr(zipfile=out,files=entries,root=dir,mode="mirror",
    include_directories=FALSE)
  web_assert(file.exists(out) && file.info(out)$size>1000L,
    "DOCX repacking failed.","DOCUMENT_GENERATION_FAILED")
  test<-utils::unzip(out,list=TRUE)
  web_assert(all(c("word/document.xml","word/styles.xml","[Content_Types].xml") %in%
    test$Name),"DOCX content missing after repackage.","DOCUMENT_GENERATION_FAILED")
  web_assert(isTRUE(file.copy(out,path,overwrite=TRUE)),
    "Could not install formatted DOCX.","DOCUMENT_GENERATION_FAILED")
  invisible(TRUE)
}

web_c2_9cd_word_table <- function(tab) {
  z<-as.data.frame(tab,stringsAsFactors=FALSE,check.names=FALSE)
  z[]<-lapply(z,function(v) {v<-as.character(v);v[is.na(v)|!nzchar(trimws(v))]<-"NR";v})
  ft<-flextable::flextable(z)
  ft<-flextable::fontsize(ft,size=8,part="all")
  ft<-flextable::autofit(ft)
  ft
}
web_c2_9cd_insert_assets <- function(doc,assets,e) {
  for(tab in assets$tables) {
    doc<-officer::body_add_par(doc,tab$title,style=e$style_map$heading2)
    doc<-flextable::body_add_flextable(doc,web_c2_9cd_word_table(tab$data))
    doc<-officer::body_add_par(doc,paste0("Source: ",tab$origin,". NR = not reported or not verified."),
      style=e$style_map$normal)
  }
  for(fig in assets$figures) {
    web_assert(file.exists(fig$path),paste("Missing generated figure:",fig$title),
      "DOCUMENT_GENERATION_FAILED")
    doc<-officer::body_add_par(doc,fig$title,style=e$style_map$heading2)
    dims<-e$fit_word_image_dimensions(fig$path,max_width=6,max_height=7)
    doc<-officer::body_add_img(doc,src=fig$path,width=dims[["width"]],height=dims[["height"]])
    doc<-officer::body_add_par(doc,paste0("Source: ",fig$origin,"."),style=e$style_map$normal)
  }
  doc
}
web_c2_9cd_docx_integrity <- function(path,min_figures=1L,min_tables=2L) {
  z<-utils::unzip(path,list=TRUE)
  names<-z$Name
  imgs<-sum(grepl("^word/media/.*[.](png|jpg|jpeg)$",names,ignore.case=TRUE))
  web_assert(imgs>=min_figures,
    paste("Publication DOCX lacks required actual embedded figures:",imgs),
    "DOCUMENT_GENERATION_FAILED")
  xml_path<-tempfile(fileext=".xml");on.exit(unlink(xml_path),add=TRUE)
  # Direct single-file extraction in an isolated temp directory.
  dir<-tempfile("rc259cd-docx-");dir.create(dir);on.exit(unlink(dir,recursive=TRUE),add=TRUE)
  utils::unzip(path,files="word/document.xml",exdir=dir)
  xml<-paste(readLines(file.path(dir,"word/document.xml"),warn=FALSE),collapse="")
  tabs<-length(gregexpr("<w:tbl(?:[ >])",xml,perl=TRUE)[[1L]])
  if(tabs==1L && gregexpr("<w:tbl(?:[ >])",xml,perl=TRUE)[[1L]][[1L]]==-1L)tabs<-0L
  grids<-length(gregexpr("<w:tblGrid(?:[ >])",xml,perl=TRUE)[[1L]])
  if(grids==1L && gregexpr("<w:tblGrid(?:[ >])",xml,perl=TRUE)[[1L]][[1L]]==-1L)grids<-0L
  web_assert(tabs>=min_tables && grids>=tabs,
    paste0("Publication DOCX table/grid QC failed: ",tabs," tables, ",grids," column grids"),
    "DOCUMENT_GENERATION_FAILED")
  list(status="PUBLICATION_ASSEMBLY_STRUCTURE_PASS",embedded_figures=imgs,
    scholarly_tables=tabs,table_grids=grids,
    scientific_author_review="REQUIRED")
}

# 9D quantitative path: preserve frozen, approved effects and forest figures.
web_c2_9cd_quant_assets <- function(assets,x,root,engine,out) {
  if(identical(web_c2_9cd_stem(x$source$route),"NARRATIVE"))return(assets)
  fmt<-web_c2_defs(engine,38)
  idx<-length(assets$tables)
  for(v in names(x$source$analyses)) {
    a<-x$source$analyses[[v]]
    if(is.null(a$result$summary))next
    wanted<-c("studies","pooled_effect","lower_ci","upper_ci","p_value","i2","tau2")
    vals<-vapply(wanted,function(k) {v<-a$result$summary[[k]];if(is.null(v))"NR" else fmt$format_num(v)},character(1))
    tab<-data.frame(Statistic=c("Studies","Pooled effect","Lower 95% CI","Upper 95% CI",
      "P value","I-squared","Tau-squared"),Value=vals,stringsAsFactors=FALSE)
    assets$tables[[length(assets$tables)+1L]]<-list(
      title=paste0("Table ",idx+1L,". Approved ",web_c2_9cd_stem(a$plan$outcome),
        " (",web_c2_9cd_stem(a$plan$timepoint),") quantitative synthesis"),
      data=tab,origin="Frozen, approved canonical R/26 analysis; no Web recalculation")
    idx<-idx+1L
  }
  n<-length(assets$figures)
  for(id in names(x$source$artifacts)) {
    art<-x$source$artifacts[[id]]
    if(!identical(art$type,"figure"))next
    from<-file.path(root,art$path)
    if(!file.exists(from) || !identical(web_c2_9cd_sha(from),art$sha256))
      stop("Selected canonical figure checksum mismatch; no DOCX produced.")
    dest<-file.path(out,paste0("canonical_forest_",length(assets$figures)+1L,".png"))
    if(grepl("[.]png$",from,ignore.case=TRUE)) {
      web_assert(isTRUE(file.copy(from,dest,overwrite=TRUE)),
        "Could not stage approved forest figure.","DOCUMENT_GENERATION_FAILED")
    } else if(grepl("[.]pdf$",from,ignore.case=TRUE)) {
      exe<-Sys.which("pdftoppm")
      web_assert(nzchar(exe),
        "Approved PDF forest plots require the pdftoppm renderer.","DEPENDENCY_FAILED")
      prefix<-sub("[.]png$","",dest)
      status<-suppressWarnings(system2(exe,c("-f","1","-singlefile","-scale-to","1600",
        "-png",shQuote(normalizePath(from,winslash="/")),shQuote(prefix)),
        stdout=FALSE,stderr=FALSE))
      web_assert(identical(as.integer(status),0L) && file.exists(dest),
        "Could not render checksum-verified canonical forest PDF.","DOCUMENT_GENERATION_FAILED")
    } else next
    n<-n+1L
    assets$figures[[length(assets$figures)+1L]]<-list(
      title=paste0("Figure ",n,". Approved canonical meta-analysis forest plot"),
      path=dest,origin="Same-review approved R/26 PDF/PNG artifact (checksum matched)")
  }
  assets
}

web_c2_9cd_require_prisma <- function(assets) {
  web_assert(identical(assets$prisma_status,"CANONICAL_R35_PASS"),
    paste0("Publication manuscript PRISMA not ready: ",
      paste(assets$messages,collapse="; "),
      ". No AI call was started. Bind the correct review's R/16-R/24 files to the frozen Evidence version and rerun."),
    "USER_ACTION_REQUIRED")
  web_assert(any(vapply(assets$tables,function(t)
    startsWith(t$title,"Table 1."),logical(1))),
    "Publication Table 1 is not source-bound. Canonical R/34 results are required for this review; no AI call was started.",
    "USER_ACTION_REQUIRED")
  TRUE
}
