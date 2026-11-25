# Execute the pinned CLI without network access. The Worker supplies previously
# journaled responses through this attempt's private directory.
args <- commandArgs(trailingOnly=TRUE)
if(length(args)==3L && identical(args[[1L]],"--word")) {
  engine<-normalizePath(args[[2L]],mustWork=TRUE,winslash="/")
  root<-normalizePath(args[[3L]],mustWork=TRUE,winslash="/")
  for(module in c("core","evidence","manuscript","manuscript_revision","qc_r40_full"))
    source(file.path(engine,"web/R",paste0(module,".R")),encoding="UTF-8")
  criteria<-jsonlite::read_json(web_inside(file.path(root,"criteria.json"),root),simplifyVector=FALSE)
  proof<-tryCatch({
    if(!requireNamespace("xml2",quietly=TRUE))stop("The canonical Word XML dependency is unavailable.")
    word<-web_inside(file.path(root,"manuscript.docx"),root)
    markdown<-if(!is.null(criteria$markdown_sha256))web_inside(file.path(root,"manuscript.md"),root)
    web_r40_word_content(word,markdown,engine,criteria)
  },error=function(e)NULL)
  state<-if(is.list(proof))"PASS" else if(requireNamespace("xml2",quietly=TRUE))"FAIL" else "NOT_ASSESSED"
  jsonlite::write_json(list(state=state,proof=proof),file.path(root,"inspection.json"),auto_unbox=TRUE,null="null")
  quit(save="no")
}
stopifnot(length(args) %in% c(4L,5L))
script <- normalizePath(args[[1L]],mustWork=TRUE)
root <- normalizePath(args[[2L]],mustWork=TRUE)
output <- args[[3L]]
stopifnot(identical(digest::digest(file=script,algo="sha256"),args[[4L]]))
setwd(root)
context <- new.env(parent=globalenv())
offline <- function(state)stop(structure(list(message=state,call=NULL),class=c("r40_offline","error","condition")))
bridge <- if(length(args)==5L)normalizePath(args[[5L]],mustWork=TRUE)
cfg <- if(!is.null(bridge))jsonlite::read_json(file.path(bridge,"rules.json"),simplifyVector=FALSE)
if(length(cfg$statistical_records))
  source(file.path(dirname(dirname(script)),"web/R/statistical_methods.R"),local=TRUE,encoding="UTF-8")
semantic_checks <- list()
pubmed_file <- if(!is.null(bridge))file.path(root,cfg$pubmed_path)
pubmed_replay <- function(pmids) {
  if(!isTRUE(file.info(pubmed_file)$size<=8e6))offline("PUBMED_REVERIFICATION_PENDING")
  document <- xml2::read_xml(pubmed_file,options="NONET")
  articles <- xml2::xml_find_all(document,"/PubmedArticleSet/PubmedArticle")
  ids <- xml2::xml_text(xml2::xml_find_first(articles,"./MedlineCitation/PMID"))
  if(anyDuplicated(ids) || !all(pmids %in% ids))offline("PUBMED_REVERIFICATION_INCOMPLETE")
  paste0("<PubmedArticleSet>",paste(as.character(articles[match(pmids,ids)]),collapse=""),"</PubmedArticleSet>")
}
semantic_request <- function(system_prompt,user_prompt,raw_name,max_output_tokens=12000L) {
  stopifnot(raw_name %in% c("01_numeric_claim_mapping","02_cross_section_consistency"))
  if(is.null(bridge))offline(if(raw_name=="01_numeric_claim_mapping")"NUMERIC_MAPPING_NEEDS_AUTHORIZATION" else "SEMANTICS_NEED_AUTHORIZATION")
  if(isTRUE(context$reverify_pubmed))pubmed_replay(as.character(context$selected_literature$pmid))
  rules <- if(raw_name=="02_cross_section_consistency")cfg$semantic_rules
  if(length(rules)) {
    protocol <- context$read_text_file(context$config_file)
    methods <- context$extract_section(context$manuscript_text,"Methods")
    system_prompt <- paste(system_prompt,"Also audit the requested rules independently. For each rule return an id, result (PASS, FAIL, NOT_ASSESSED), manuscript_quote and evidence_quote. A PASS needs exact excerpts from the supplied manuscript and frozen R/38 evidence respectively. Only report what the excerpts and supplied facts support. Record a contradiction as FAIL and missing or ambiguous information as NOT_ASSESSED. Never infer approval.",sep="\n")
    system_prompt <- paste(system_prompt,"For METHODS_PROTOCOL, use an exact Methods quote and an exact approved Protocol quote. For quantitative checks, include the canonical fact_ids supporting the result. For MIXED_CONSISTENCY, give the narrative evidence in the usual quote fields and both aux_manuscript_quote and aux_evidence_quote for the independent pooled result.",sep="\n")
    system_prompt <- paste(system_prompt,"Extend the consistency audit to all supplied major sections. Compare structured Abstract results with the Results and frozen table and figure evidence, and distinguish selected studies from studies contributing to individual syntheses. Verify reported procedures and flag unsupported analyses or materially contradictory claims.",sep="\n")
    user_prompt <- paste(user_prompt,"Add a top-level checks array for these rules:",
      as.character(jsonlite::toJSON(rules,auto_unbox=TRUE)),"Frozen manuscript Methods:",methods,
      "Approved Protocol configuration:",protocol,"Full manuscript:",context$manuscript_text,sep="\n")
    if("STATISTICAL_METHODS_REPORTING" %in% names(rules)) {
      system_prompt <- paste(system_prompt,
        "For STATISTICAL_METHODS_REPORTING, assess every execution-backed statistical methods passage by its provided ID.",
        "Check completeness and exact meaning against both the Methods and Results including the bound tables and figures.",
        "Explicitly check direction, model, confidence intervals, heterogeneity, sparse data, software, and claims about optional analyses.",
        "Return witnesses with id, result, evidence_quote and manuscript_quote for each record. For PASS, give the full evidence passage and an excerpt of Methods with equivalent meaning, and return unsupported_claims as an empty array. Do not award PASS for a quotation that contradicts its evidence.",sep="\n")
      user_prompt <- paste(user_prompt,"Execution-backed statistical methods passages:",
        as.character(jsonlite::toJSON(cfg$statistical_records,auto_unbox=TRUE)),sep="\n")
    }
  }
  req <- list(step=raw_name,system=system_prompt,user=user_prompt,policy_sha256=cfg$policy_sha256)
  key <- digest::digest(as.character(jsonlite::toJSON(req,auto_unbox=TRUE,null="null")),algo="sha256",serialize=FALSE)
  path <- file.path(bridge,paste0(raw_name,".json"))
  if(!file.exists(path)) {
    jsonlite::write_json(list(request=req,sha256=key),file.path(bridge,"request.json"),auto_unbox=TRUE)
    offline("SEMANTIC_REQUEST_PENDING")
  }
  ans <- jsonlite::read_json(path,simplifyVector=FALSE)
  stopifnot(identical(ans$sha256,key),is.character(ans$text),length(ans$text)==1L)
  if(length(rules)) {
    parsed <- jsonlite::fromJSON(context$strip_markdown_fence(ans$text),simplifyVector=FALSE)
    for(id in names(rules)) {
      checks <- Filter(function(r)is.list(r) && identical(r$id,id),parsed$checks)
      if(length(checks)!=1L)next
      row <- checks[[1L]]
      if(identical(row$result,"FAIL")) {semantic_checks[[id]] <<- list(result="FAIL");next}
      if(id=="STATISTICAL_METHODS_REPORTING") {
        if(identical(row$result,"PASS") && is.list(row$unsupported_claims) && !length(row$unsupported_claims)) {
          proof<-web_stat_methods_witnesses(row$witnesses,cfg$statistical_records,methods)
          if(length(proof))semantic_checks[[id]] <<- c(list(result="PASS"),proof)
        }
        next
      }
      quote <- function(text,source) is.character(text) && length(text)==1L && !is.na(text) &&
        nchar(trimws(text))>=12L && grepl(text,source,fixed=TRUE)
      passage <- if(id=="METHODS_PROTOCOL")methods else context$manuscript_text
      evidence <- if(id=="METHODS_PROTOCOL")protocol else context$r38_results_text
      fact_ids <- unique(as.character(unlist(row$fact_ids,use.names=FALSE)))
      facts <- context$canonical_facts
      metrics <- facts$metric[match(fact_ids,facts$fact_id)]
      required <- switch(id,PAIRWISE_STUDY_COUNTS="n_studies",PAIRWISE_ESTIMATES_CI=c("pooled_effect","pooled_lower_ci","pooled_upper_ci"),
        PAIRWISE_ARTIFACT_NUMERICS="pooled_effect",PAIRWISE_DIRECTION="pooled_effect",MIXED_CONSISTENCY="pooled_effect",character())
      if(length(required) && (!length(fact_ids) || anyNA(metrics) || !all(required %in% metrics)))next
      if(length(required)) {
        quantified <- facts[grepl("meta_analysis_summary[.]csv$",facts$source),,drop=FALSE]
        expected <- unique(quantified$analysis_group)
        chosen <- quantified[quantified$fact_id %in% fact_ids,,drop=FALSE]
        if(!length(expected) || !all(vapply(expected,function(g)all(required %in%
          chosen$metric[chosen$analysis_group==g]),logical(1))))next
      }
      if(id=="MIXED_CONSISTENCY" && (!quote(row$aux_manuscript_quote,passage) || !quote(row$aux_evidence_quote,evidence) ||
          identical(row$manuscript_quote,row$aux_manuscript_quote) || identical(row$evidence_quote,row$aux_evidence_quote)))next
      sha <- function(q)digest::digest(q,algo="sha256",serialize=FALSE)
      if(identical(row$result,"PASS") && quote(row$manuscript_quote,passage) && quote(row$evidence_quote,evidence))
        semantic_checks[[id]] <<- list(result="PASS",fact_ids=as.list(fact_ids),
          evidence_document=if(id=="METHODS_PROTOCOL")"approved_protocol_configuration" else "r38_results_factual_blocks",
          manuscript_quote_sha256=sha(row$manuscript_quote),evidence_quote_sha256=sha(row$evidence_quote),
          second_witness=if(id=="MIXED_CONSISTENCY")list(manuscript_quote_sha256=sha(row$aux_manuscript_quote),
            evidence_quote_sha256=sha(row$aux_evidence_quote)))
    }
  }
  ans$text
}
overrides <- list(
  load_dotenv_simple=function(...)invisible(NULL),
  openai_api_key="offline-preflight",
  call_openai=semantic_request,
  pubmed_fetch_xml=pubmed_replay,
  ncbi_request=function(...)offline("PUBMED_REVERIFICATION_PENDING"))
expressions <- parse(script,encoding="UTF-8",keep.source=TRUE)
target <- vapply(expressions,function(e) {
  if(is.call(e) && length(e)==3L && identical(e[[1L]],as.name("<-")) && is.symbol(e[[2L]]))
    as.character(e[[2L]]) else ""
},character(1))
stopifnot(all(vapply(names(overrides),function(k)sum(target==k)==1L,logical(1))))
state <- tryCatch({
  for(i in seq_along(expressions)) {
    k <- target[[i]]
    if(k %in% names(overrides))assign(k,overrides[[k]],envir=context) else eval(expressions[[i]],context)
  }
  if(is.null(bridge))"UNEXPECTED_COMPLETION" else "COMPLETE"
},r40_offline=function(e)conditionMessage(e),error=function(e)"CANONICAL_PREFLIGHT_INCOMPLETE")
summary <- context$state$manuscript_qc
fields <- c("project_id","manuscript_sha256","overall_status","final_evidence_ready","deterministic_pass",
  "semantic_pass","numeric_claim_failures","citation_integrity_failures","pubmed_reverification_failures",
  "artifact_coverage_failures","machine_identifier_leak_failures","semantic_contradictions_or_unsupported")
partial <- NULL
if(!is.null(bridge) && is.null(summary) && !is.null(context$manuscript_text)) {
  # The citation pass depends on the frozen citation inputs and section parser,
  # independently of the numerical mapping result. Run only that native block.
  if(is.null(context$citation_integrity) && state %in% c("SEMANTIC_REQUEST_PENDING","PUBMED_REVERIFICATION_PENDING","PUBMED_REVERIFICATION_INCOMPLETE")) {
    lines <- readLines(script,encoding="UTF-8",warn=FALSE)
    start <- which(startsWith(lines,"# 11. Citation number parsing"))
    end <- which(startsWith(lines,"# 12. PubMed reference re-verification"))
    positions <- vapply(attr(expressions,"srcref"),function(ref)ref[[1L]],integer(1))
    if(length(start)==1L && length(end)==1L)
      tryCatch(for(i in which(positions>start & positions<end))eval(expressions[[i]],context),error=function(e)NULL)
  }
  counts <- c(numeric_claim_failures="numeric_audit",citation_integrity_failures="citation_integrity",
    artifact_coverage_failures="artifact_coverage_audit",machine_identifier_leak_failures="machine_identifier_audit")
  observed <- lapply(counts,function(k) {
    x <- context[[k]]
    if(is.data.frame(x) && "status" %in% names(x) && !anyNA(x$status))sum(x$status=="FAIL")
  })
  partial <- c(list(project_id=context$project_id,manuscript_sha256=context$sha256_file(context$manuscript_file),
    final_evidence_ready=identical(context$r38_status,"FINAL_EVIDENCE_READY") &&
      identical(context$r39_status,"DRAFT_COMPLETE_READY_FOR_R40_VALIDATION")),observed)
}
jsonlite::write_json(list(state=state,ai_calls=0L,script_sha256=args[[4L]],canonical=if(is.null(summary))partial else summary[fields],
  semantic_checks=semantic_checks),output,auto_unbox=TRUE,null="null")
