# RC28.22 — narrative analysis Web sidecar; canonical R is unchanged.
# Prefer the engine root explicitly provided to the private R subprocess.
# Web App requests can use web_root; neither may depend on the project CWD.
web_b2_narrative_root <- local({
  candidates <- character()
  if (exists("request", inherits = TRUE)) {
    req <- get("request", inherits = TRUE)
    if (is.list(req) && is.character(req$engine_root) &&
        length(req$engine_root) == 1L && !is.na(req$engine_root) &&
        nzchar(req$engine_root)) candidates <- c(candidates, req$engine_root)
  }
  if (exists("web_root", inherits = TRUE)) {
    root <- get("web_root", inherits = TRUE)
    if (is.character(root) && length(root) == 1L &&
        !is.na(root) && nzchar(root)) candidates <- c(candidates, root)
  }
  env_root <- Sys.getenv("SRMA_ENGINE_ROOT", unset = "")
  if (nzchar(env_root)) candidates <- c(candidates, env_root)
  candidates <- unique(c(candidates, getwd()))
  valid <- vapply(candidates, function(root)
    file.exists(file.path(root, "web/R/narrative_analysis.R")), logical(1))
  if (!any(valid))
    stop("Approved Web source root is unavailable for Analysis modules.")
  normalizePath(candidates[[which(valid)[[1L]]]], winslash = "/", mustWork = TRUE)
})
source(file.path(web_b2_narrative_root, "web/R/narrative_analysis.R"), local=TRUE)
rm(web_b2_narrative_root)
# B2 extends ProjectVersion/Job/Gate/Artifact; every edit is a new frozen snapshot.
web_b2_ai_operations <- function() c(recommend_screening="SCREENING",recommend_study_design="SCREENING",recommend_eligibility="FULLTEXT",recommend_extraction="EXTRACTION",discover_pdf_results="EXTRACTION",generate_scope_advisory="EXTRACTION",recommend_risk_of_bias="ROB")
web_b2_operations <- function() c(web_b2_ai_operations(),register_fulltext="FULLTEXT",prepare_screening="INPUT",finalize_screening="SCREENING",finalize_included_studies="FULLTEXT",prepare_extraction="INCLUDED",prepare_targeted_verification="EXTRACTION",finalize_extraction="EXTRACTION",prepare_risk_of_bias="VERIFIED",finalize_evidence="ROB")
web_b2_gates <- function() c(prepare_screening="b2_protocol",finalize_screening="b2_screening",finalize_included_studies="b2_inclusion",finalize_extraction="b2_extraction",finalize_evidence="b2_rob")
web_b2_packet <- function(root) jsonlite::read_json(file.path(root,"config/generated/web_evidence.json"),simplifyVector=FALSE)
web_b2_write <- function(root,x) {dir.create(file.path(root,"config/generated"),recursive=TRUE,showWarnings=FALSE);jsonlite::write_json(x,file.path(root,"config/generated/web_evidence.json"),auto_unbox=TRUE,null="null",pretty=TRUE)}
# R/19 is canonical: Reviewer 1 is the LLM agent; Reviewer 2 is Human.
# An existing AI_Screening packet is bound *by immutable screening_id* without
# another API call. The persisted agent decision log authenticates individual
# GPT-5/Luna provenance (a mixed-model run must never be labelled all Luna).
web_b2_bind_agent_reviewer1 <- function(x,root,manifest=NULL) {
  if (!identical(x$stage,"SCREENING") || !length(x$tables$AI_Screening)) return(x)
  ai <- x$tables$AI_Screening
  r1 <- x$tables$Reviewer_1
  r2 <- x$tables$Reviewer_2
  web_assert(length(ai)>0L && length(ai)==length(r1) && length(r1)==length(r2),
    "AI Reviewer 1 and Human Reviewer 2 must cover the same screening set.","USER_ACTION_REQUIRED")
  ids <- function(rows) vapply(rows,function(z) as.character(z$screening_id %||b2% ""),character(1))
  aid <- ids(ai); rid <- ids(r1); hid <- ids(r2)
  web_assert(all(nzchar(aid)) && !anyDuplicated(aid) &&
    all(nzchar(rid)) && !anyDuplicated(rid) &&
    all(nzchar(hid)) && !anyDuplicated(hid) &&
    setequal(aid,rid) && setequal(aid,hid),
    "Reviewer IDs are incomplete, duplicated or different; refusing AI binding.","USER_ACTION_REQUIRED")
  web_assert(all(vapply(ai,function(z) identical(z$.source,"AI RECOMMENDATION"),logical(1))),
    "AI recommendation provenance is not authenticated in the saved evidence.","USER_ACTION_REQUIRED")
  # The log is saved as an Evidence artifact; never use the live mutable worker checkpoint.
  policy_path <- file.path(root,"config/agent_screening.yml")
  web_assert(file.exists(policy_path),"Versioned agent policy is missing.","USER_ACTION_REQUIRED")
  p <- yaml::read_yaml(policy_path)
  base_name <- as.character(p$agent_screening$output$decisions_basename %||b2% "")
  web_assert(length(base_name)==1L && grepl("^[A-Za-z0-9_-]+$",base_name),
    "Untrusted screening decision-log basename.","USER_ACTION_REQUIRED")
  candidates <- list.files(root,pattern="[.]csv$",recursive=TRUE,full.names=TRUE)
  candidates <- candidates[basename(candidates)==paste0(base_name,".csv")]
  web_assert(length(candidates)==1L,
    "Exactly one saved AI decision log is required to validate each reviewer's provenance.","USER_ACTION_REQUIRED")
  log_path <- web_inside(candidates[[1L]],root)
  if (!is.null(manifest)) {
    root_clean <- normalizePath(root,winslash="/",mustWork=TRUE)
    rel <- substring(normalizePath(log_path,winslash="/",mustWork=TRUE),nchar(root_clean)+2L)
    expected <- manifest[[rel]]
    web_assert(!is.null(expected) && identical(web_sha(log_path),as.character(expected)),
      "The saved AI decision log failed immutable version hash validation.","USER_ACTION_REQUIRED")
  }
  d <- tryCatch(utils::read.csv(log_path,colClasses="character",check.names=FALSE),
                error=function(e) NULL)
  need <- c("screening_id","validation_status","decision","model","provider","response_id")
  web_assert(!is.null(d) && all(need %in% names(d)),
    "Per-record AI decision provenance is missing.","USER_ACTION_REQUIRED")
  valid <- !is.na(d$validation_status) & d$validation_status=="valid"
  valid[is.na(valid)] <- FALSE
  d <- d[valid,,drop=FALSE]
  did <- as.character(d$screening_id)
  web_assert(nrow(d)==length(ai) && all(nzchar(did)) && !anyDuplicated(did) &&
    setequal(did,aid) && all(!is.na(d$model) & nzchar(d$model)) &&
    all(!is.na(d$response_id) & nzchar(d$response_id)),
    "AI screening log is incomplete or missing record-level model/response identity.","USER_ACTION_REQUIRED")
  map <- match(rid,aid)
  provenance_map <- match(rid,did)
  decision_log_hash <- web_sha(log_path)
  for (i in seq_along(r1)) {
    z <- ai[[map[[i]]]]
    log <- d[provenance_map[[i]],,drop=FALSE]
    old_actor <- as.character(r1[[i]]$.actor %||b2% "")
    web_assert(!nzchar(old_actor) || identical(old_actor,"AI_AGENT_REVIEWER_1"),
      "A pre-existing Human Reviewer 1 decision cannot be overwritten by AI.","USER_ACTION_REQUIRED")
    web_assert(!is.null(z$decision) &&
      identical(trimws(as.character(z$decision)),trimws(as.character(log$decision[[1L]]))),
      "Saved AI recommendation disagrees with authenticated decision log.","USER_ACTION_REQUIRED")
    for (key in c("reviewer","decision","exclusion_reason","confidence","notes","screened_at"))
      if (!is.null(z[[key]]) && key %in% names(r1[[i]])) r1[[i]][[key]] <- z[[key]]
    r1[[i]]$.actor <- "AI_AGENT_REVIEWER_1"
    r1[[i]]$.source <- "AI RECOMMENDATION"
    r1[[i]]$.model <- as.character(log$model[[1L]])
    r1[[i]]$.provider <- as.character(log$provider[[1L]])
    r1[[i]]$.response_id <- as.character(log$response_id[[1L]])
    r1[[i]]$.decision_log_sha256 <- decision_log_hash
    ai[[map[[i]]]]$.model <- r1[[i]]$.model
    ai[[map[[i]]]]$.provider <- r1[[i]]$.provider
    ai[[map[[i]]]]$.response_id <- r1[[i]]$.response_id
  }
  x$tables$Reviewer_1 <- r1
  x$tables$AI_Screening <- ai
  x
}

# RC28: the selective workbook contains ONLY AI Include/Unclear cases. AI
# excludes never receive a fabricated Human signature. All row bindings are by
# screening_id, and every import creates a new immutable Evidence Version.
web_b2_selective_review_targets <- function(x) {
  web_assert(identical(x$stage,"SCREENING") && length(x$tables$Reviewer_1)>0L,
    "Complete and save AI screening before Human Excel review.","USER_ACTION_REQUIRED")
  a<-x$tables$Reviewer_1;b<-x$tables$Reviewer_2
  id<-function(rows)vapply(rows,function(r)as.character(r$screening_id %||b2% ""),character(1))
  aid<-id(a);bid<-id(b)
  web_assert(length(a)==length(b) && all(nzchar(aid)) && all(nzchar(bid)) &&
    !anyDuplicated(aid) && !anyDuplicated(bid) && setequal(aid,bid),
    "AI/Human screening IDs are inconsistent.","USER_ACTION_REQUIRED")
  web_assert(all(vapply(a,function(z)identical(z$.actor,"AI_AGENT_REVIEWER_1"),logical(1))),
    "AI Reviewer 1 is not authenticated.","USER_ACTION_REQUIRED")
  dec<-tolower(trimws(vapply(a,function(z)as.character(z$decision %||b2% ""),character(1))))
  groups<-ifelse(dec=="exclude","EXCLUDE",ifelse(dec=="include","INCLUDE",
    ifelse(dec %in% c("unclear","uncertain","unsure","awaiting full-text clarification",
                        "needs clarification","pending","maybe"),"UNCERTAIN","INVALID")))
  web_assert(!any(groups=="INVALID"),"AI screening contains invalid decisions; selective review blocked.","USER_ACTION_REQUIRED")
  list(ai=a,human=b,ids=aid,groups=groups,review_ids=aid[groups!="EXCLUDE"],
    count=list(total=length(a),include=sum(groups=="INCLUDE"),
               uncertain=sum(groups=="UNCERTAIN"),exclude=sum(groups=="EXCLUDE")))
}

# RC28.6 — generic, opt-in study-design triage. The supplementary AI pass
# is restricted to authenticated Reviewer 1 INCLUDE records, never AI Unclear.
# It is a separate AI recommendation, not a replacement R/19 decision.
web_b2_comparative_policy_codes <- function() c("PASS","SINGLE_ARM","AI_UNCLEAR","BOTH")
web_b2_policy_exclusion_label <- function() {
  "Researcher-approved bulk exclusion (individual reason not assessed)"
}
web_b2_design_source_digest <- function(x) {
  z <- web_b2_selective_review_targets(x)
  idx <- which(z$groups=="INCLUDE")
  ids <- z$ids[idx]
  rows <- lapply(sort(ids),function(k) {
    r <- z$ai[[match(k,z$ids)]]
    list(screening_id=k, decision=as.character(r$decision %||b2% ""),
         title=as.character(r$title %||b2% ""),
         abstract=as.character(r$abstract %||b2% ""),
         response_id=as.character(r$.response_id %||b2% ""))
  })
  digest::digest(rows,algo="sha256",serialize=TRUE)
}
web_b2_study_design_validate <- function(x,required=FALSE) {
  z <- web_b2_selective_review_targets(x)
  rows <- x$tables$AI_Study_Design
  include_ids <- z$ids[z$groups=="INCLUDE"]
  if(!length(rows)) {
    web_assert(!isTRUE(required) || !length(include_ids),
      "First run 'Classify AI Include study designs' and save its completed Evidence Version before excluding single-arm studies.","USER_ACTION_REQUIRED")
    return(list(rows=list(),ids=character(),classes=character(),source_digest=web_b2_design_source_digest(x)))
  }
  web_assert(is.list(rows),"Invalid study-design table.","USER_ACTION_REQUIRED")
  ids<-vapply(rows,function(r)as.character(r$screening_id %||b2% ""),character(1))
  web_assert(all(nzchar(ids)) && !anyDuplicated(ids) && setequal(ids,include_ids),
    "Study-design classifications must cover exactly the original AI Include IDs.","USER_ACTION_REQUIRED")
  digest<-web_b2_design_source_digest(x)
  classes<-vapply(rows,function(r)toupper(trimws(as.character(r$classification %||b2% ""))),character(1))
  valid<-vapply(rows,function(r){
    identical(as.character(r$original_ai_decision %||b2% ""),"Include") &&
      identical(as.character(r$.source %||b2% ""),"AI RECOMMENDATION") &&
      identical(as.character(r$.verification %||b2% ""),"UNVERIFIED") &&
      identical(as.character(r$.source_ai_sha256 %||b2% ""),digest) &&
      nzchar(as.character(r$.model %||b2% "")) &&
      nzchar(as.character(r$.provider %||b2% "")) &&
      nzchar(as.character(r$.response_id %||b2% "")) &&
      nzchar(as.character(r$rationale %||b2% ""))
  },logical(1))
  web_assert(all(valid) && all(classes %in% c("COMPARATIVE","NON_COMPARATIVE","UNCLEAR")),
    "Supplementary study-design classification lacks validated provenance or has stale source data.","USER_ACTION_REQUIRED")
  list(rows=rows,ids=ids,classes=classes,source_digest=digest)
}

# Deprecated structured-field guessing is intentionally not used in RC28.6.
# No AI Unclear or Exclude row can become single-arm through this pathway.
web_b2_comparative_policy_targets <- function(x,policy="PASS",rationale="",
                                                reason_single_arm="",reason_ai_unclear="",
                                                require_reasons=FALSE) {
  web_assert(is.character(policy) && length(policy)==1L && !is.na(policy) &&
    policy %in% web_b2_comparative_policy_codes(),
    "Unknown comparative screening policy.","USER_ACTION_REQUIRED")
  z <- web_b2_selective_review_targets(x)
  needs_design <- policy %in% c("SINGLE_ARM","BOTH")
  needs_unclear <- policy %in% c("AI_UNCLEAR","BOTH")
  if(policy!="PASS")web_assert(is.character(rationale) && length(rationale)==1L &&
    !is.na(rationale) && nchar(trimws(rationale))>=15L && nchar(rationale)<=2000L,
    "Researcher justification (15-2000 characters) is required for bulk exclusions.","USER_ACTION_REQUIRED")
  # A supplemental classification is scientifically informational unless the
  # researcher affirmatively chooses to exclude it under this project's protocol.
  design <- web_b2_study_design_validate(x,required=needs_design)
  noncomparative <- design$ids[design$classes=="NON_COMPARATIVE"]
  web_assert(!length(noncomparative) || all(noncomparative %in% z$ids[z$groups=="INCLUDE"]),
    "Non-comparative candidates must come solely from AI Include.","USER_ACTION_REQUIRED")
  selected_design <- if(needs_design) noncomparative else character()
  selected_unclear <- if(needs_unclear) z$ids[z$groups=="UNCERTAIN"] else character()
  selected_ids <- union(selected_design,selected_unclear)
  human_ids <- vapply(z$human,function(r)as.character(r$screening_id %||b2% ""),character(1))
  protected <- vapply(z$human,function(r) {
    a<-as.character(r$.actor %||b2% "")
    nzchar(a)
  },logical(1))
  names(protected) <- human_ids
  auto_ids <- z$ids[z$ids %in% selected_ids & !protected[z$ids]]
  review_ids <- z$review_ids[!z$review_ids %in% auto_ids]
  # Two mutually exclusive screening populations; set union avoids double count.
  codes <- setNames(ifelse(auto_ids %in% selected_design,"SINGLE_ARM","AI_UNCLEAR"),auto_ids)
  configured <- unique(trimws(as.character(unlist(x$choices$reasons,use.names=FALSE))))
  configured <- configured[!is.na(configured) & nzchar(configured)]
  supplied <- c(SINGLE_ARM=as.character(reason_single_arm),AI_UNCLEAR=as.character(reason_ai_unclear))
  web_assert(length(supplied)==2L && !anyNA(supplied),"Invalid legacy exclusion reasons.","USER_ACTION_REQUIRED")
  supplied <- trimws(supplied)
  for(code in names(supplied)) if(nzchar(supplied[[code]]))
    web_assert(supplied[[code]] %in% configured,
      paste("Legacy reason is not configured for this project:",code),"USER_ACTION_REQUIRED")
  reasons <- setNames(vapply(unname(codes),function(code){
    if(nzchar(supplied[[code]])) unname(supplied[[code]]) else web_b2_policy_exclusion_label()
  },character(1)),auto_ids)
  list(policy=policy,rationale=rationale,z=z,review_ids=review_ids,
    auto_ids=auto_ids,auto_types=codes,auto_reasons=reasons,
    reason_single_arm=unname(supplied[["SINGLE_ARM"]]),
    reason_ai_unclear=unname(supplied[["AI_UNCLEAR"]]),
    missing_reason_codes=character(),
    classified_single_arm=length(noncomparative),
    design_classified=length(design$ids),
    design_digest=if(length(design$ids))digest::digest(design$rows,algo="sha256",serialize=TRUE) else "",
    protected_human=sum(z$ids %in% selected_ids & protected[z$ids]),
    counts=c(total=z$count$total,ai_exclude=z$count$exclude,
      ai_include=z$count$include,ai_unclear=z$count$uncertain,
      policy_single_arm=sum(codes=="SINGLE_ARM"),
      policy_ai_unclear=sum(codes=="AI_UNCLEAR"),
      policy_auto_exclude=length(auto_ids),human_review=length(review_ids)))
}

web_b2_selective_excel_data <- function(x,version_id,policy="PASS",rationale="") {
  p<-web_b2_comparative_policy_targets(x,policy,rationale);z<-p$z;aid<-z$ids
  id2<-vapply(z$human,function(r)as.character(r$screening_id),character(1))
  rows<-lapply(p$review_ids,function(id){
    a<-z$ai[[match(id,aid)]];b<-z$human[[match(id,id2)]]
    list(screening_id=id,title=as.character(a$title %||b2% ""),
      abstract=as.character(a$abstract %||b2% ""),
      ai_decision=as.character(a$decision %||b2% ""),
      ai_exclusion_reason=as.character(a$exclusion_reason %||b2% ""),
      ai_rationale=as.character(a$notes %||b2% a$agent_rationale %||b2% ""),
      human_final_decision=if(!is.null(b$.actor))as.character(b$decision %||b2% "") else "",
      human_exclusion_reason=if(!is.null(b$.actor))as.character(b$exclusion_reason %||b2% "") else "",
      human_notes=if(!is.null(b$.actor))as.character(b$notes %||b2% "") else "")
  })
  cols<-c("screening_id","title","abstract","ai_decision","ai_exclusion_reason","ai_rationale",
          "human_final_decision","human_exclusion_reason","human_notes")
  if(length(rows)) return(as.data.frame(setNames(lapply(cols,function(k)
    vapply(rows,function(a)a[[k]],character(1))),cols),stringsAsFactors=FALSE))
  as.data.frame(setNames(rep(list(character()),length(cols)),cols),stringsAsFactors=FALSE)
}

# openxlsx may replace spaces with dots in column headers even when
# check.names=FALSE; normalize only the nine fixed Human_Review headers.
# This does NOT relax protected IDs or AI data validation.
web_b2_selective_header_names <- function(headers) {
  trimws(gsub("[[:space:]]+"," ",gsub("."," ",as.character(headers),fixed=TRUE)))
}

# RC28.2: source links are derived from the saved original screening fields,
# not from a new API call or a guessed bibliographic identifier. These
# supplementary columns are display-only; the first 9 import columns remain
# byte-for-byte compatible with completed RC28.1 review workbooks.
web_b2_selective_source_links <- function(x,policy="PASS",rationale="") {
  p<-web_b2_comparative_policy_targets(x,policy,rationale);z<-p$z
  first_value<-function(row,keys) {
    if(is.null(row))return("")
    for(k in keys) {
      a<-as.character(row[[k]] %||b2% "")
      if(length(a)==1L && nzchar(trimws(a)))return(trimws(a))
    }
    ""
  }
  ai<-x$tables$AI_Screening
  ai_ids<-if(length(ai))vapply(ai,function(r)as.character(r$screening_id %||b2% ""),character(1)) else character()
  if(length(ai_ids))web_assert(all(nzchar(ai_ids))&&!anyDuplicated(ai_ids),
    "AI recommendation source IDs are duplicated or missing.","USER_ACTION_REQUIRED")
  safe_doi<-function(d) {
    d<-trimws(as.character(d))
    d<-sub("^(doi[[:space:]]*:[[:space:]]*|https?://(dx[.])?doi[.]org/)","",d,ignore.case=TRUE,perl=TRUE)
    if(!grepl('^10[.][0-9]{4,9}/[^[:space:]"<>]+$',d,perl=TRUE) || nchar(d)>300L)return("")
    paste0("https://doi.org/",utils::URLencode(d,reserved=FALSE))
  }
  safe_url<-function(u) {
    u<-trimws(as.character(u))
    if(nchar(u)>2048L || !grepl('^https?://[^[:space:]"<>]+$',u,ignore.case=TRUE,perl=TRUE))return("")
    u
  }
  safe_pubmed<-function(p) {
    p<-sub("^[[:space:]]*PMID[[:space:]]*:[[:space:]]*","",trimws(as.character(p)),ignore.case=TRUE)
    if(!grepl("^[0-9]{1,12}$",p))return("")
    paste0("https://pubmed.ncbi.nlm.nih.gov/",p,"/")
  }
  rows<-lapply(p$review_ids,function(id){
    a<-z$ai[[match(id,z$ids)]]
    b<-if(length(ai_ids) && id %in% ai_ids)ai[[match(id,ai_ids)]] else NULL
    val<-function(keys) {
      v<-first_value(a,keys)
      if(nzchar(v))v else first_value(b,keys)
    }
    doi<-val(c("doi"));url<-val(c("url"));pmid<-val(c("pmid"))
    doi_link<-safe_doi(doi);url_link<-safe_url(url);pubmed_link<-safe_pubmed(pmid)
    title<-first_value(a,c("title"))
    lookup<-if(!nzchar(doi_link) && !nzchar(url_link) && !nzchar(pubmed_link) && nzchar(title))
      paste0("https://pubmed.ncbi.nlm.nih.gov/?term=",utils::URLencode(title,reserved=TRUE)) else ""
    list(screening_id=id,doi=doi,url=url,pmid=pmid,doi_link=doi_link,
         url_link=url_link,pubmed_link=pubmed_link,title_search_link=lookup)
  })
  cols<-c("screening_id","doi","url","pmid","doi_link","url_link","pubmed_link","title_search_link")
  if(length(rows))return(as.data.frame(setNames(lapply(cols,function(k)
    vapply(rows,function(a)a[[k]],character(1))),cols),stringsAsFactors=FALSE))
  as.data.frame(setNames(rep(list(character()),length(cols)),cols),stringsAsFactors=FALSE)
}

web_b2_excel_source_link_formula <- function(url,label) {
  if(!length(url) || is.na(url) || !nzchar(url))return("")
  web_assert(grepl("^https?://",url,ignore.case=TRUE),
    "Unsafe source link scheme.","USER_ACTION_REQUIRED")
  paste0('HYPERLINK("',gsub('"','""',url,fixed=TRUE),'","',
         gsub('"','""',label,fixed=TRUE),'")')
}

web_b2_export_selective_excel <- function(x,version_id,path,policy="PASS",rationale="",
                                          reason_single_arm="",reason_ai_unclear="") {
  web_assert(requireNamespace("openxlsx",quietly=TRUE),"openxlsx is not installed.","DEPENDENCY_FAILED")
  web_assert(is.character(version_id)&&length(version_id)==1L&&nzchar(version_id),"Select a version.")
  triage<-web_b2_comparative_policy_targets(x,policy,rationale,
    reason_single_arm,reason_ai_unclear)
  data<-web_b2_selective_excel_data(x,version_id,policy,rationale)
  wb<-openxlsx::createWorkbook(creator="SR-MA Universal Toolkit — RC28")
  openxlsx::addWorksheet(wb,"Guide",gridLines=FALSE)
  openxlsx::addWorksheet(wb,"Human_Review",gridLines=FALSE)
  openxlsx::addWorksheet(wb,"Design_Triage",gridLines=FALSE)
  openxlsx::addWorksheet(wb,"Metadata",visible=FALSE)
  openxlsx::addWorksheet(wb,"Lists",visible=FALSE)
  head<-openxlsx::createStyle(fontColour="#FFFFFF",fgFill="#16324F",textDecoration="bold",wrapText=TRUE,valign="center")
  ai<-openxlsx::createStyle(fgFill="#EEF2F7",wrapText=TRUE,valign="top")
  input<-openxlsx::createStyle(fgFill="#FFF2BC",wrapText=TRUE,valign="top")
  norm<-openxlsx::createStyle(wrapText=TRUE,valign="top")
  warn<-openxlsx::createStyle(fgFill="#FECACA",fontColour="#991B1B")
  openxlsx::writeData(wb,"Guide","Selective Human Reviewer 2 — Title/Abstract Screening",startRow=1)
  openxlsx::addStyle(wb,"Guide",openxlsx::createStyle(fontSize=17,textDecoration="bold",fontColour="#16324F"),rows=1,cols=1)
  guide<-data.frame(Topic=c("Workflow","* Required","Required when Exclude","Decision dropdown","AI Exclude","Protect the record IDs","After editing","Version binding","Finality"),
   Instruction=c("Only AI Include + Unclear records are presented; review every listed row.",
      "Human final decision (G) is mandatory for EVERY row. Yellow cells require action.",
      "When G = Exclude, column H must contain a configured exclusion reason.",
      "Choose Include or Exclude from the dropdown. Leave no G cell blank.",
      "AI Exclude items do not appear here and remain labelled AI-only in audit outputs.",
      "Keep the Toolkit rows, Screening IDs and column headers unchanged. Optional descriptive corrections are recorded as Human annotations; the original AI source remains immutable.",
      "Upload the completed XLSX in the current review. Toolkit validates its source and required decisions; an older template from the same screening set is accepted.",
      paste0("Source Evidence Version: ",version_id),
      "Human Reviewer 2 makes the final choice for listed records. No third adjudicator is required."))
  guide<-rbind(guide,data.frame(Topic="Article access",Instruction="In Human_Review, columns J-M contain clickable article links (DOI, supplied URL, PubMed, or a clearly labelled title search). Columns N-P show the original DOI, URL and PMID. If none was supplied, a PubMed title SEARCH is offered, not a verified full-text link."))
  guide<-rbind(guide,data.frame(Topic="Comparative triage option",Instruction=paste0(
    "Selected option: ",policy,". Bulk exclusions: ",length(triage$auto_ids),
    ". Reasons: ",if(identical(policy,"PASS"))"not applicable" else rationale,
    ". Additional bulk exclusions without individually verified reasons are explicitly labelled: ",
    web_b2_policy_exclusion_label(),
    ". The researcher's justification is preserved in the import audit; no per-study eligibility claim is fabricated.",
     " Only authenticated supplemental classifications of AI Include records can enter the non-comparative group. AI Unclear is not reclassified.")))
  openxlsx::writeData(wb,"Guide",guide,startRow=3,headerStyle=head,withFilter=FALSE)
  # Auditable AI-design recommendations, including cases excluded by this option.
  design <- web_b2_study_design_validate(x,required=policy %in% c("SINGLE_ARM","BOTH"))
  design_audit <- if(length(design$rows)) do.call(rbind,lapply(design$rows,function(r)
    data.frame(screening_id=as.character(r$screening_id),
      original_AI_decision="Include",
      classification=as.character(r$classification),
      rationale=as.character(r$rationale),
      evidence_excerpt=as.character(r$evidence_excerpt %||b2% ""),
      model=as.character(r$.model),
      response_id=as.character(r$.response_id),
      policy_auto_excluded=as.character(r$screening_id) %in% triage$auto_ids,
      stringsAsFactors=FALSE))) else data.frame(
        screening_id=character(),original_AI_decision=character(),classification=character(),
        rationale=character(),evidence_excerpt=character(),model=character(),
        response_id=character(),policy_auto_excluded=logical())
  openxlsx::writeData(wb,"Design_Triage",design_audit,headerStyle=head,withFilter=TRUE)
  openxlsx::setColWidths(wb,"Design_Triage",1:8,c(19,20,22,65,65,24,35,24))
  openxlsx::setColWidths(wb,"Guide",1,26);openxlsx::setColWidths(wb,"Guide",2,102)
  openxlsx::setRowHeights(wb,"Guide",rows=4:(nrow(guide)+3L),heights=33)
  openxlsx::writeData(wb,"Human_Review","HUMAN REVIEWER 2 — SELECTIVE FINAL SCREENING",startRow=1)
  openxlsx::addStyle(wb,"Human_Review",openxlsx::createStyle(fontSize=16,textDecoration="bold",fontColour="#16324F"),rows=1,cols=1)
  openxlsx::writeData(wb,"Human_Review",paste0("Comparative option: ",policy," | Human review: ",nrow(data)," studies | * Required: G decision; H exclusion reason if Exclude | Only yellow cells are editable."),startRow=2)
  openxlsx::writeData(wb,"Human_Review","Completed decisions",startRow=3,startCol=1)
  first<-7L;last<-max(first,first+nrow(data)-1L)
  # Excel recalculates progress automatically. The importer NEVER trusts formula results.
  openxlsx::writeFormula(wb,"Human_Review",paste0('COUNTIF(G',first,':G',last,',"Include")+COUNTIF(G',first,':G',last,',"Exclude")'),startRow=3,startCol=2)
  openxlsx::writeData(wb,"Human_Review","Remaining decisions",startRow=4,startCol=1)
  openxlsx::writeFormula(wb,"Human_Review",paste0(nrow(data),'-B3'),startRow=4,startCol=2)
  display<-data
  names(display)<-c("Screening ID","Title","Abstract","AI Decision","AI Exclusion Reason",
    "AI Rationale","Human Final Decision *","Human Exclusion Reason *","Human Notes")
  openxlsx::writeData(wb,"Human_Review",display,startRow=6,startCol=1,headerStyle=head,withFilter=TRUE,keepNA=FALSE)
  openxlsx::setRowHeights(wb,"Human_Review",6,32)
  if(nrow(data)) {
    openxlsx::addStyle(wb,"Human_Review",ai,rows=first:last,cols=1:6,gridExpand=TRUE,stack=TRUE)
    openxlsx::addStyle(wb,"Human_Review",input,rows=first:last,cols=7:8,gridExpand=TRUE,stack=TRUE)
    openxlsx::addStyle(wb,"Human_Review",norm,rows=first:last,cols=9,gridExpand=TRUE,stack=TRUE)
    openxlsx::dataValidation(wb,"Human_Review",cols=7,rows=first:last,type="list",value="'Lists'!$A$2:$A$3")
    reasons<-as.character(unlist(x$choices$reasons,use.names=FALSE))
    reasons<-unique(reasons[!is.na(reasons) & nzchar(trimws(reasons))])
    web_assert(length(reasons)>0L,"Exclusion reason list is missing.","USER_ACTION_REQUIRED")
    openxlsx::writeData(wb,"Lists",data.frame(Decision=c("Include","Exclude")),startCol=1)
    openxlsx::writeData(wb,"Lists",data.frame(Reason=reasons),startCol=2)
    openxlsx::dataValidation(wb,"Human_Review",cols=8,rows=first:last,type="list",
      value=paste0("'Lists'!$B$2:$B$",length(reasons)+1L))
    openxlsx::conditionalFormatting(wb,"Human_Review",cols=7,rows=first:last,type="expression",
      rule=paste0('$G',first,'=""'),style=warn)
    openxlsx::conditionalFormatting(wb,"Human_Review",cols=8,rows=first:last,type="expression",
      rule=paste0('AND($G',first,'="Exclude",$H',first,'="")'),style=warn)
  } else {
    openxlsx::writeData(wb,"Lists",data.frame(Decision=c("Include","Exclude")),startCol=1)
  }
  metadata<-data.frame(key=c("source_version_id","target_record_count","toolkit_format",
      "comparative_option","comparative_rationale","auto_exclude_count",
       "policy_reason_single_arm","policy_reason_ai_unclear", "design_result_sha256"),
     value=c(version_id,as.character(nrow(data)),"SRMA_SELECTIVE_REVIEW_RC28",
       policy,as.character(rationale),as.character(length(triage$auto_ids)),
       triage$reason_single_arm,triage$reason_ai_unclear,triage$design_digest))
  openxlsx::writeData(wb,"Metadata",metadata)
  openxlsx::setColWidths(wb,"Human_Review",cols=c(1,4,7,8),widths=c(19,17,24,31))
  openxlsx::setColWidths(wb,"Human_Review",cols=c(2,3,5,6,9),widths=c(56,87,28,65,40))
  # Clickable references are outside the 9 authoritative review columns (A:I).
  # R/18 and R/19 already preserve DOI, URL and PMID in reviewer records.
  refs<-web_b2_selective_source_links(x,policy,rationale)
  web_assert(identical(as.character(refs$screening_id),as.character(data$screening_id)),
    "Article references could not be aligned to review IDs.","USER_ACTION_REQUIRED")
  link_headers<-c("Open DOI","Open Article URL","Open PubMed","Search title (PubMed)",
                  "DOI (source)","URL (source)","PMID (source)")
  openxlsx::writeData(wb,"Human_Review",matrix(link_headers,nrow=1L),startRow=6,startCol=10,colNames=FALSE)
  openxlsx::addStyle(wb,"Human_Review",head,rows=6,cols=10:16,gridExpand=TRUE,stack=TRUE)
  linkstyle<-openxlsx::createStyle(fontColour="#1769AA",textDecoration="underline",valign="top")
  if(nrow(refs)){
    raw<-data.frame(doi=refs$doi,url=refs$url,pmid=refs$pmid,stringsAsFactors=FALSE)
    openxlsx::writeData(wb,"Human_Review",raw,startRow=first,startCol=14,
      colNames=FALSE,keepNA=FALSE)
    openxlsx::addStyle(wb,"Human_Review",ai,rows=first:last,cols=14:16,
      gridExpand=TRUE,stack=TRUE)
    links<-c("doi_link","url_link","pubmed_link","title_search_link")
    labels<-c("Open DOI","Open article","Open PubMed","Search by title")
    for(j in seq_along(links)){
      for(i in seq_len(nrow(refs))){
        link<-refs[[links[[j]]]][[i]]
        if(!nzchar(link))next
        openxlsx::writeFormula(wb,"Human_Review",
          x=web_b2_excel_source_link_formula(link,labels[[j]]),
          startRow=first+i-1L,startCol=9L+j)
      }
    }
    openxlsx::addStyle(wb,"Human_Review",linkstyle,rows=first:last,
      cols=10:13,gridExpand=TRUE,stack=TRUE)
  }
  openxlsx::setColWidths(wb,"Human_Review",cols=10:13,widths=c(18,21,19,26))
  openxlsx::setColWidths(wb,"Human_Review",cols=14:16,widths=c(31,58,17))
  openxlsx::freezePane(wb,"Human_Review",firstActiveRow=first,firstActiveCol=3)
  openxlsx::saveWorkbook(wb,path,overwrite=TRUE)
  invisible(path)
}

# Pure ID-frame check used by both legacy and current Toolkit workbooks.
# Only current policy-required Human rows require G/H decisions; additional
# historical rows are retained for audit, never treated as a second review.
web_b2_excel_review_match <- function(ids,template_ids,required_ids,decisions,auto_ids) {
  web_assert(all(nzchar(ids)) && !anyDuplicated(ids) &&
    setequal(ids,template_ids) && all(required_ids %in% ids),
    "Workbook row IDs differ from its Toolkit template, or required Human review rows are missing.",
    "USER_ACTION_REQUIRED")
  # An explicit Human Include takes precedence over the optional bulk policy;
  # the exception is preserved as an individual Human decision and audited.
  intersect(ids[decisions=="Include"],auto_ids)
}

# Universal Human Review Excel: the workbook is a decision-entry interface.
# The operator-approved policy on the upload request is authoritative; workbook
# metadata establishes provenance and the source study-ID set, not write access
# to a frozen Evidence Version. Earlier Toolkit templates can be reused when
# all current required study IDs are covered and AI source decisions agree.
web_b2_import_selective_excel <- function(con,settings,actor,project_id,version_id,upload,upload_root,
                                           comparative_option="PASS",comparative_rationale="",
                                           comparative_approved=FALSE) {
  c<-web_b2_context(con,settings,actor,project_id,version_id)
  x<-web_b2_bind_agent_reviewer1(c$packet,c$version$root,c$version$manifest)
  z<-web_b2_selective_review_targets(x)
  web_assert(is.data.frame(upload)&&nrow(upload)==1L&&all(c("name","datapath","size")%in%names(upload)),
    "Upload one completed XLSX workbook.","USER_ACTION_REQUIRED")
  name<-as.character(upload$name[[1L]])
  web_assert(grepl("^[A-Za-z0-9][A-Za-z0-9_. -]*[.]xlsx$",name,ignore.case=TRUE) && !grepl("..",name,fixed=TRUE),
    "Choose an .xlsx filename without folders.","USER_ACTION_REQUIRED")
  src<-web_inside(upload$datapath[[1L]],upload_root)
  web_assert(!nzchar(Sys.readlink(src)),"Symlink uploads are not accepted.")
  size<-file.info(src)$size
  web_assert(is.finite(size)&&size>0&&size<=20*1024^2&&identical(as.numeric(upload$size[[1L]]),as.numeric(size)),
    "XLSX must be nonempty and 20 MB or smaller.","USER_ACTION_REQUIRED")
  f<-file(src,"rb");sig<-readBin(f,"raw",2);close(f)
  web_assert(identical(sig,charToRaw("PK")),"The selected file is not an XLSX ZIP container.","USER_ACTION_REQUIRED")
  zipinfo<-tryCatch(utils::unzip(src,list=TRUE),error=function(e)NULL)
  web_assert(!is.null(zipinfo)&&nrow(zipinfo)>0L&&nrow(zipinfo)<500L &&
    sum(as.numeric(zipinfo$Length))<=100*1024^2 &&
    !any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))",as.character(zipinfo$Name))) &&
    !any(grepl("externalLinks|vbaProject",zipinfo$Name,ignore.case=TRUE)),
    "Unsafe or oversized Excel archive.","USER_ACTION_REQUIRED")
  sheets<-tryCatch(openxlsx::getSheetNames(src),error=function(e) character())
  web_assert(all(c("Human_Review","Metadata") %in% sheets),"Not a selective Human Review workbook.","USER_ACTION_REQUIRED")
  meta<-tryCatch(openxlsx::read.xlsx(src,sheet="Metadata",colNames=TRUE),error=function(e)NULL)
  web_assert(!is.null(meta)&&all(c("key","value")%in%names(meta)) && !anyDuplicated(meta$key),
    "Selective Excel metadata is incomplete.","USER_ACTION_REQUIRED")
  m<-setNames(as.character(meta$value),as.character(meta$key))
  norm<-function(v){v<-as.character(v);v[is.na(v)]<-"";trimws(v)}
  web_assert(all(c("source_version_id","target_record_count","toolkit_format") %in% names(m)) &&
    identical(as.character(m[["toolkit_format"]]),"SRMA_SELECTIVE_REVIEW_RC28"),
    "This is not a supported Toolkit Human Review Excel template.","USER_ACTION_REQUIRED")
  source_version<-as.character(m[["source_version_id"]])
  web_assert(length(source_version)==1L && nzchar(source_version),
    "Human Review Excel source version is missing.","USER_ACTION_REQUIRED")
  # The source workbook must be from this same project. No data from another
  # project or unrelated screening population can cross this boundary.
  source_ctx<-web_b2_context(con,settings,actor,project_id,source_version)
  source_packet<-web_b2_bind_agent_reviewer1(source_ctx$packet,
    source_ctx$version$root,source_ctx$version$manifest)
  source_z<-web_b2_selective_review_targets(source_packet)
  ai_fence<-function(z){
    ids<-as.character(z$ids)
    decision<-vapply(z$ai,function(r)as.character(r$decision %||b2% ""),character(1))
    response<-vapply(z$ai,function(r)as.character(r$.response_id %||b2% ""),character(1))
    named<-setNames(paste(decision,response,sep=" | "),ids)
    named[sort(names(named))]
  }
  web_assert(identical(ai_fence(source_z),ai_fence(z)),
    "This workbook belongs to a different AI screening dataset; download the current Toolkit template.",
    "USER_ACTION_REQUIRED")
  web_assert(is.character(comparative_option) && length(comparative_option)==1L &&
    comparative_option %in% web_b2_comparative_policy_codes(),
    "Choose one valid comparative-study policy.","USER_ACTION_REQUIRED")
  if(!identical(comparative_option,"PASS"))
    web_assert(isTRUE(comparative_approved),
      "Explicit researcher approval is required for bulk exclusions.","USER_ACTION_REQUIRED")
  policy<-comparative_option
  rationale<-if(identical(comparative_option,"PASS"))"" else as.character(comparative_rationale)
  # Source workbook may have been downloaded before supplementary triage;
  # the selected, explicitly approved UI policy controls *this* import.
  p<-web_b2_comparative_policy_targets(x,comparative_option,rationale)
  d<-tryCatch(openxlsx::read.xlsx(src,sheet="Human_Review",startRow=6,colNames=TRUE,
    skipEmptyRows=FALSE,check.names=FALSE,sep.names=" "),error=function(e)NULL)
  if(!is.null(d))names(d)<-web_b2_selective_header_names(names(d))
  labels<-c("Screening ID","Title","Abstract","AI Decision","AI Exclusion Reason",
    "AI Rationale","Human Final Decision *","Human Exclusion Reason *","Human Notes")
  web_assert(!is.null(d)&&all(labels %in% names(d)) &&
    !anyDuplicated(names(d)) &&
    identical(suppressWarnings(as.integer(m[["target_record_count"]])),as.integer(nrow(d))),
    "Toolkit Excel columns or recorded row count changed. Keep the original template frame.",
    "USER_ACTION_REQUIRED")
  d<-d[,labels,drop=FALSE]
  names(d)<-c("screening_id","title","abstract","ai_decision","ai_exclusion_reason",
              "ai_rationale","human_final_decision","human_exclusion_reason","human_notes")
  ids<-norm(d$screening_id)
  original_policy<-as.character(m[["comparative_option"]] %||b2% "PASS")
  original_rationale<-as.character(m[["comparative_rationale"]] %||b2% "")
  source_template<-web_b2_comparative_policy_targets(source_packet,
    original_policy,original_rationale)
  # Preserve a Toolkit-generated row frame, without forcing users to rewrite
  # an older complete Excel when a later triage policy reduces the workload.
  overrides<-web_b2_excel_review_match(ids,source_template$review_ids,
    p$review_ids,norm(d$human_final_decision),p$auto_ids)
  if(length(overrides)) {
    p$auto_ids<-setdiff(p$auto_ids,overrides)
    p$auto_types<-p$auto_types[names(p$auto_types) %in% p$auto_ids]
    p$auto_reasons<-p$auto_reasons[names(p$auto_reasons) %in% p$auto_ids]
    p$review_ids<-z$review_ids[z$review_ids %in% union(p$review_ids,overrides)]
    p$counts[["policy_auto_exclude"]]<-length(p$auto_ids)
    p$counts[["human_review"]]<-length(p$review_ids)
  }
  # Optional changes to titles, abstracts and AI display columns are captured
  # as researcher corrections. Immutable original AI decisions stay untouched.
  reference<-web_b2_selective_excel_data(x,version_id,"PASS","")
  reference_ids<-norm(reference$screening_id)
  corrections<-list()
  for(key in c("title","abstract","ai_decision","ai_exclusion_reason","ai_rationale")) {
    original<-norm(reference[[key]])[match(ids,reference_ids)]
    submitted<-norm(d[[key]])
    changed<-which(submitted!=original)
    if(length(changed))for(i in changed) {
      corrections[[length(corrections)+1L]]<-list(
        screening_id=ids[[i]],field=key,previous=original[[i]],submitted=submitted[[i]])
    }
  }
  ord<-match(p$review_ids,ids)
  d<-d[ord,,drop=FALSE]
  decision<-norm(d$human_final_decision);reason<-norm(d$human_exclusion_reason)
  web_assert(all(decision%in%c("Include","Exclude")),
    paste0("Human final decision (column G) must be Include or Exclude on every row; incomplete: ",
           paste(head(p$review_ids[!decision%in%c("Include","Exclude")],8),collapse=", ")),
    "USER_ACTION_REQUIRED")
  allowed<-unique(trimws(as.character(unlist(x$choices$reasons,use.names=FALSE))))
  allowed<-allowed[nzchar(allowed)]
  bad<-(decision=="Exclude" & (!nzchar(reason)|!reason%in%allowed))|
       (decision=="Include" & nzchar(reason))
  web_assert(!any(bad),paste0("Invalid/missing exclusion reasons (column H) for screening IDs: ",
    paste(head(p$review_ids[bad],8),collapse=", ")),"USER_ACTION_REQUIRED")
  humanids<-vapply(z$human,function(r)as.character(r$screening_id),character(1))
  idx<-match(p$review_ids,humanids)
  notes<-norm(d$human_notes)
  web_assert(all(nchar(notes)<=20000L),"Review notes exceed 20,000 characters.","USER_ACTION_REQUIRED")
  now<-web_now()
  for(j in seq_along(idx)) {
    i<-idx[[j]];b<-x$tables$Reviewer_2[[i]]
    # An unchanged earlier Human decision keeps its original identity/time.
    if(!is.null(b$.actor) && identical(as.character(b$decision %||b2% ""),decision[[j]]) &&
       identical(as.character(b$exclusion_reason %||b2% ""),reason[[j]]) &&
       identical(as.character(b$notes %||b2% ""),notes[[j]]))next
    b$decision<-decision[[j]];b$exclusion_reason<-reason[[j]]
    b$notes<-notes[[j]];b$reviewer<-actor;b$screened_at<-now
    b$.actor<-actor;b$.timestamp<-now;b$.source<-"HUMAN DECISION — SELECTIVE EXCEL"
    x$tables$Reviewer_2[[i]]<-b
  }
  for(id in p$auto_ids) {
    i<-match(id,humanids);b<-x$tables$Reviewer_2[[i]]
    web_assert(is.null(b$.actor),"An existing Human decision may not be replaced by a bulk-exclusion option.","USER_ACTION_REQUIRED")
    b$decision<-"Exclude";b$exclusion_reason<-unname(p$auto_reasons[[id]])
    b$notes<-paste0("Researcher-selected comparative option: ",policy,
      "; basis: ",unname(p$auto_types[[id]]),"; rationale: ",rationale)
    b$.actor<-NULL;b$.source<-"POLICY-ONLY AUTO EXCLUSION";b$.timestamp<-NULL
    b$.policy<-policy;b$.policy_basis<-unname(p$auto_types[[id]])
    b$.policy_justification<-rationale
    b$.policy_reason_status<-if(identical(b$exclusion_reason,web_b2_policy_exclusion_label()))
      "NOT_INDIVIDUALLY_ASSESSED" else "LEGACY_USER_MAPPED_REASON"
    x$tables$Reviewer_2[[i]]<-b
  }
  for(id in z$ids[z$groups=="EXCLUDE"]) {
    i<-match(id,humanids);a<-z$ai[[match(id,z$ids)]]
    b<-x$tables$Reviewer_2[[i]]
    web_assert(is.null(b$.actor) || identical(b$.source,"AI-ONLY AUTO EXCLUSION"),
      "Pre-existing Human review of an AI Exclude record must not be silently replaced.","USER_ACTION_REQUIRED")
    b$decision<-"Exclude";b$exclusion_reason<-as.character(a$exclusion_reason %||b2% "")
    b$notes<-"AI-only exclusion; Human Reviewer 2 did not screen this record."
    b$.actor<-NULL;b$.source<-"AI-ONLY AUTO EXCLUSION";b$.timestamp<-NULL
    x$tables$Reviewer_2[[i]]<-b
  }
  x$screening_excel_review<-list(source_version_id=source_version,
     import_parent_version_id=version_id,upload_sha256=web_sha(src),
     submitted_display_corrections=corrections,
     human_include_policy_overrides=as.list(overrides),
     human_actor=actor,human_timestamp=now,target_count=length(idx),
     design_result_sha256=p$design_digest,
    ai_only_exclude_count=sum(z$groups=="EXCLUDE"),comparative_option=policy,
    comparative_rationale=rationale,
    policy_reason_single_arm=p$reason_single_arm,
    policy_reason_ai_unclear=p$reason_ai_unclear,
    policy_exclusion_marker=if(length(p$auto_ids))web_b2_policy_exclusion_label() else "",
    policy_auto_exclude_count=length(p$auto_ids),
    policy_excluded_ids=as.list(p$auto_ids),policy_excluded_types=as.list(unname(p$auto_types)),
    human_include=sum(decision=="Include"),
    human_exclude=sum(decision=="Exclude"),workflow="SELECTIVE_HUMAN_FINAL_RC28")
  x$tables$Conflict_Resolution<-NULL # Legacy R/21 technical sheet regenerated ONLY inside a worker attempt.
  x$history<-c(x$history,list(list(event="SELECTIVE_EXCEL_IMPORTED",actor=actor,at=now,
    parent_version=version_id,upload_sha256=web_sha(src),human_rows=length(idx),
     comparative_option=policy,policy_auto_exclude=length(p$auto_ids),
     design_result_sha256=p$design_digest,
    policy_reason_single_arm=p$reason_single_arm,
    policy_reason_ai_unclear=p$reason_ai_unclear,
    policy_exclusion_marker=if(length(p$auto_ids))web_b2_policy_exclusion_label() else "")))
  root<-tempfile("selective-review-",settings$data_root);dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  web_copy_version(con,settings,actor,project_id,version_id,root)
  web_b2_invalidate(root,"SCREENING");web_b2_write(root,x)
  saved<-web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"SCREENING")
  web_audit(con,web_authorize(con,actor,project_id),"SELECTIVE_SCREENING_XLSX_IMPORTED",saved$version_id,
    actor,list(source_version=version_id,upload_sha256=web_sha(src),human_rows=length(idx),
      ai_only_excludes=sum(z$groups=="EXCLUDE"),comparative_option=policy,
      policy_auto_excludes=length(p$auto_ids)))
  saved
}

# Aggregate counts are computed before individual AI votes are hidden from
# Human Reviewer 2. This is presentation-only, not a scientific adjudication.
web_b2_ai_summary_counts <- function(rows) {
  v <- tolower(trimws(vapply(rows,function(z)as.character(z$decision %||b2% ""),character(1))))
  c(Total=length(v),Include=sum(v=="include"),Exclude=sum(v=="exclude"),
    Uncertain=sum(v %in% c("unclear","uncertain","unsure","awaiting full-text clarification","needs clarification","pending","maybe")),
    Pending=sum(!nzchar(v)),Other=sum(nzchar(v) &
      !v %in% c("include","exclude","unclear","uncertain","unsure","awaiting full-text clarification","needs clarification","pending","maybe")))
}

web_b2_table <- function(x) {
 if(!length(x))return(data.frame())
 jsonlite::fromJSON(as.character(web_json(x)),simplifyDataFrame=TRUE)
}
web_b2_context <- function(con,settings,actor,project_id,version_id) {
 v<-web_version_root(con,settings,actor,project_id,version_id)
 row<-web_query(con,"SELECT * FROM evidence_versions WHERE project_id=?p AND version_id=?v",p=project_id,v=version_id)
 web_assert(nrow(row)==1L,"Start an evidence review from this saved input version first.","USER_ACTION_REQUIRED")
 x<-web_b2_packet(v$root);web_assert(identical(x$stage,row$stage),"Evidence state failed integrity verification.")
 list(version=v,row=row,packet=x)
}
web_b2_save_snapshot <- function(con,settings,actor,project_id,parent_version,root,stage,job_id=NULL) {
 project<-web_authorize(con,actor,project_id)
 web_tx(con,function(){
  saved<-web_import_project(con,settings,actor,project$workspace_id,root,configuration_only=TRUE,transaction=function(con,f)f())
  web_assert(identical(saved$project_id,project_id),"Evidence project identity changed.")
  web_exec(con,"INSERT INTO evidence_versions VALUES(?v,?p,?parent,?stage,?j,?actor,?now)",v=saved$version_id,p=project_id,parent=parent_version,stage=stage,j=if(is.null(job_id))NA_character_ else job_id,actor=actor,now=web_now())
  ops<-names(web_b2_operations())[web_b2_operations()==stage];kinds<-web_b2_gates()
  for(op in intersect(ops,names(kinds)))web_exec(con,"INSERT INTO gates(id,project_id,workspace_id,version_id,gate_type,required_operation,status,created_at) VALUES(?g,?p,?w,?v,?k,?op,'PENDING',?now)",g=web_id(),p=project_id,w=project$workspace_id,v=saved$version_id,k=kinds[[op]],op=op,now=web_now())
  if(exists("web_c1_invalidate_audit",mode="function"))web_c1_invalidate_audit(con,project,actor,saved$version_id)
  web_audit(con,project,"EVIDENCE_VERSION_SAVED",saved$version_id,actor,list(parent_version=parent_version,stage=stage,source_job=job_id))
  saved
 })
}
web_b2_start <- function(con,settings,actor,project_id,version_id) {
 web_version_root(con,settings,actor,project_id,version_id)
 j<-web_query(con,"SELECT id,state FROM jobs WHERE project_id=?p AND version_id=?v AND operation='validate_database_inputs' ORDER BY created_at DESC LIMIT 1",p=project_id,v=version_id)
 web_assert(nrow(j)==1L&&j$state=="COMPLETE","Complete database preflight for this exact input version first.","USER_ACTION_REQUIRED")
 existing<-web_query(con,"SELECT version_id FROM evidence_versions WHERE project_id=?p AND parent_version_id=?v AND stage='INPUT' AND source_job_id IS NULL ORDER BY created_at DESC LIMIT 1",p=project_id,v=version_id)
 if(nrow(existing))return(list(project_id=project_id,version_id=existing$version_id[[1L]]))
 root<-tempfile("evidence-start-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE))
 web_copy_version(con,settings,actor,project_id,version_id,root)
 for(id in web_pipeline_sources(con,project_id,version_id,"validate_database_inputs"))web_overlay_job(con,settings,actor,project_id,version_id,id,root)
 web_b2_invalidate(root,"SCREENING")
 web_b2_write(root,list(stage="INPUT",source_version=version_id,preflight_job=j$id,tables=list(),history=list()))
 web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"INPUT")
}
web_b2_guard <- function(con,settings,actor,project_id,version_id,operation,accepting_completed_job=FALSE) {
 if(!operation %in% names(web_b2_operations()))return(invisible(TRUE))
  c<-web_b2_context(con,settings,actor,project_id,version_id)
  if(identical(operation,"finalize_extraction")) {
    web_b2_final_active_reports(c$packet,TRUE)
    if(!is.null(c$packet$result_inventory_selection)) {
      web_assert(identical(c$packet$human_outcome_harmonization$status,"HUMAN_CONFIRMED"),
        "Confirm Human Outcome Harmonization before R/26 finalization.","USER_ACTION_REQUIRED")
      web_assert(identical(c$packet$synthesis_plan$status,"HUMAN_CONFIRMED"),
        "Save Step 4 synthesis choices before R/26. Narrative decisions remain outside quantitative pooling.","USER_ACTION_REQUIRED")
      plan<-c$packet$synthesis_plan$decisions %||b2% list()
      quant<-vapply(Filter(function(a)identical(a$mode,"QUANTITATIVE_CANDIDATE"),plan),
        function(a)as.character(a$analysis_group),character(1))
      web_assert(length(quant)>0L,
        "There is no quantitative meta-analysis candidate. R/26 needs at least one quantitative target; narrative-only evidence remains preserved.",
        "USER_ACTION_REQUIRED")
      active<-Filter(function(r)isTRUE(r$.web_harmonized_result)&&
        identical(as.character(r$human_include_in_planned_analysis %||b2% ""),"Yes"),
        c$packet$tables$Target_Outcome_Verification %||b2% list())
      quant_active<-Filter(function(r)as.character(r$analysis_group %||b2% "") %in% quant,active)
      web_assert(length(quant_active)>0L,
        "No quantitative candidate has Human-approved numeric input. Verify its values before R/26.",
        "USER_ACTION_REQUIRED")
    }
  }
  # Server-side safeguard: a validated Include-only design pass has already
  # been saved as part of this immutable Evidence Version. Re-queuing it would
  # charge the provider again without producing a needed scientific change.
  if(identical(operation,"recommend_study_design")&&
     length(c$packet$tables$AI_Study_Design %||b2% list())) {
    web_b2_study_design_validate(c$packet,required=TRUE)
    web_assert(FALSE,
      "AI Include study-design classification is already saved in this Evidence Version. Proceed to comparative triage; do not rerun the AI job.",
      "USER_ACTION_REQUIRED")
  }
  if(operation %in% names(web_b2_ai_operations())) {
  web_provider_ready(settings)
  web_assert(
   nzchar(Sys.getenv(web_b2_credential_name(settings$engine_root,operation),"")),
   "AI assistance requires an operator credential. Manual Human review remains available.",
   "USER_ACTION_REQUIRED"
  )
 }
  web_science_settings(settings)
  if(identical(operation,"discover_pdf_results")) {
    web_b2_final_active_reports(c$packet,TRUE)
    # Block duplicate new AI submissions, not acceptance of this completed job.
    # Completion/identity/version are independently checked by web_scoped_job().
    if(!isTRUE(accepting_completed_job)) {
      complete_jobs<-web_query(con,
        "SELECT id FROM jobs WHERE project_id=?p AND version_id=?v AND operation='discover_pdf_results' AND state='COMPLETE' LIMIT 1",
        p=project_id,v=version_id)
      web_assert(!nrow(complete_jobs),
        "A completed PDF Discovery job already exists. Save its result instead of running AI again.",
        "USER_ACTION_REQUIRED")
      prior<-c$packet$pdf_result_discovery
      if(is.list(prior)&&identical(prior$status,"COMPLETE")) {
        active<-web_b2_final_active_reports(c$packet,TRUE)
        covered<-unique(vapply(prior$results %||b2% list(),
          function(r)as.character(r$screening_id %||b2% ""),character(1)))
        web_assert(!all(active %in% covered),
          "The selected PDFs already have a saved discovery inventory. Do not repeat an AI call.",
          "USER_ACTION_REQUIRED")
      }
    }
  }
  if(identical(operation,"generate_scope_advisory")) {
    path<-file.path(c$version$root,"results/extraction/research_scope_advisory/research_scope_advisory.json")
    web_assert(!file.exists(path),
      "The R/25A advisory is already saved in this Evidence Version; do not pay for another AI run.","USER_ACTION_REQUIRED")
    web_assert(length(c$packet$tables$AI_outcome_data %||b2% list())>0L,
      "Save the R/25 AI Extraction result first.","USER_ACTION_REQUIRED")
  }
  if(identical(operation,"prepare_targeted_verification")) {
    path<-file.path(c$version$root,"results/extraction/research_scope_advisory/research_scope_advisory.json")
    web_assert(file.exists(path),
      "R/25B requires a saved R/25A Research Scope Advisory; run it explicitly first.","USER_ACTION_REQUIRED")
    web_assert(is.null(c$packet$targeted_verification_source),
      "The R/25B verification plan is already saved; edit the existing Human worksheet instead.","USER_ACTION_REQUIRED")
    already<-unlist(lapply(c$packet$tables[c("Target_Study_Verification","Target_Outcome_Verification")],
      function(rows)vapply(rows %||b2% list(),function(a)!is.null(a$.actor)||!is.null(a$.verified_by),logical(1))),use.names=FALSE)
    web_assert(!any(already),
      "Existing Human extraction entries must not be overwritten by the R/25B targeted plan.","USER_ACTION_REQUIRED")
  }
  if(identical(operation,"recommend_study_design")) {
    authenticated<-web_b2_bind_agent_reviewer1(c$packet,c$version$root,c$version$manifest)
    web_b2_selective_review_targets(authenticated)
    web_assert(is.null(c$packet$screening_excel_review),
      "Supplementary AI study-design classification must precede Human Excel import.",
      "USER_ACTION_REQUIRED")
  }
  web_assert(identical(c$row$stage,unname(web_b2_operations()[[operation]])),"This operation does not match the selected evidence stage.")
 kind<-web_b2_gates()[operation]
 if(!is.na(kind))web_assert(nrow(web_query(con,"SELECT id FROM gates WHERE project_id=?p AND version_id=?v AND gate_type=?k AND required_operation=?op AND status='APPROVED'",p=project_id,v=version_id,k=unname(kind),op=operation))==1L,"Explicit Human approval of this exact version is required.","USER_ACTION_REQUIRED")
 invisible(TRUE)
}
web_b2_accept <- function(con,settings,actor,project_id,version_id,job_id) {
 c<-web_b2_context(con,settings,actor,project_id,version_id)
 j<-web_scoped_job(con,actor,project_id,version_id,job_id,names(web_b2_operations()))
 web_assert(j$state=="COMPLETE","Wait for successful canonical processing before saving its outputs.")
 # Existing saved result is idempotent: return the original immutable version.
 old<-web_query(con,"SELECT version_id FROM evidence_versions WHERE project_id=?p AND source_job_id=?j",p=project_id,j=job_id)
 if(nrow(old))return(list(project_id=project_id,version_id=old$version_id))
 web_b2_guard(con,settings,actor,project_id,version_id,j$operation,
   accepting_completed_job=identical(j$operation,"discover_pdf_results"))
 root<-tempfile("evidence-result-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE))
 web_copy_version(con,settings,actor,project_id,version_id,root)
 web_overlay_job(con,settings,actor,project_id,version_id,job_id,root)
 x<-web_b2_packet(root);x$history<-c(x$history,list(list(source_version=version_id,job_id=job_id,operation=j$operation,engine=j$engine_id)))
 web_b2_write(root,x)
 web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,x$stage,job_id)
}
# Edit contracts are UI/identity permissions, not scientific decision algorithms.
web_b2_edit_spec <- function() list(
 Reviewer_1=list(stage="SCREENING",fields=character()), # AI-owned, strictly read-only
 Reviewer_2=list(stage="SCREENING",fields=character()), # RC28 Human decisions are Excel-only
 Author_Final_Review=list(stage="FULLTEXT",fields=c("author_final_decision","author_final_exclusion_reason","author_notes","study_id","retrieval_status")),
 Target_Study_Verification=list(stage="EXTRACTION",fields=NULL),
 Target_Outcome_Verification=list(stage="EXTRACTION",fields=NULL),
 Audit_Checks=list(stage="EXTRACTION",fields=c("audit_status","human_conclusion")),
 Domain_Review=list(stage="ROB",fields=c("author_domain_judgement","author_domain_rationale","author_override")),
 Overall_Review=list(stage="ROB",fields=c("author_final_overall_judgement","author_final_overall_rationale")),
 Signalling_Questions=list(stage="ROB",fields=c("human_response","human_evidence_location","human_rationale")))

# Final analysis study selection (Web-only; no retrospective screening rewrite).
# Study ID is the selection unit; all source reports belonging to a study move
# together. The original full-text Include list and AI extraction remain intact.
web_b2_final_study_catalog <- function(x) {
  ids <- as.character(unlist(x$included_reports %||b2% list(),use.names=FALSE))
  web_assert(!anyDuplicated(ids),"Duplicate full-text included report identifiers.","USER_ACTION_REQUIRED")
  web_assert(!anyDuplicated(ids),"Duplicate full-text included report identifiers.","USER_ACTION_REQUIRED")
  web_assert(length(ids)>0L && !anyNA(ids) && all(nzchar(ids)) &&
    !anyDuplicated(ids),"No valid full-text included report set is available.","USER_ACTION_REQUIRED")
  author <- x$tables$Author_Final_Review %||b2% list()
  result <- list()
  for(id in ids) {
    matches <- Filter(function(a)identical(as.character(a$screening_id %||b2% ""),id),author)
    web_assert(length(matches)==1L,"A full-text included report has no unique study crosswalk.","USER_ACTION_REQUIRED")
    a<-matches[[1L]]
    sid<-as.character(a$study_id %||b2% "")
    web_assert(length(sid)==1L && nzchar(trimws(sid)),
      "A full-text included report is missing its Study ID.","USER_ACTION_REQUIRED")
    title<-as.character(a$title %||b2% "")
    if(is.null(result[[sid]]))result[[sid]]<-list(study_id=sid,screening_ids=character(),titles=character())
    result[[sid]]$screening_ids<-c(result[[sid]]$screening_ids,id)
    if(length(title)==1L && nzchar(trimws(title)))result[[sid]]$titles<-unique(c(result[[sid]]$titles,title))
  }
  unname(result)
}
web_b2_final_active_study_ids <- function(x,require_confirmed=FALSE) {
  catalog<-web_b2_final_study_catalog(x)
  all_ids<-vapply(catalog,function(z)z$study_id,character(1))
  decision<-x$final_study_selection
  if(is.null(decision)) {
    web_assert(!isTRUE(require_confirmed),
      "Confirm Final Study Selection before proceeding to final extraction and analysis.",
      "USER_ACTION_REQUIRED")
    return(all_ids)
  }
  chosen<-as.character(unlist(decision$study_ids %||b2% list(),use.names=FALSE))
  web_assert(identical(decision$status,"HUMAN_CONFIRMED") &&
    length(chosen)>0L && !anyNA(chosen) && all(nzchar(chosen)) &&
    !anyDuplicated(chosen) && all(chosen %in% all_ids),
    "Final Study Selection is invalid for the saved full-text eligible reports.",
    "USER_ACTION_REQUIRED")
  chosen
}
web_b2_final_active_reports <- function(x,require_confirmed=FALSE) {
  chosen<-web_b2_final_active_study_ids(x,require_confirmed)
  catalog<-web_b2_final_study_catalog(x)
  unlist(lapply(Filter(function(a)a$study_id %in% chosen,catalog),`[[`,"screening_ids"),use.names=FALSE)
}
web_b2_final_filter_rows <- function(x,rows) {
  if(is.null(x$final_study_selection))return(rows)
  ids<-web_b2_final_active_reports(x,TRUE)
  if(!length(rows))return(rows)
  web_assert(all(vapply(rows,function(r)is.list(r)&&
    is.character(r$screening_id)&&length(r$screening_id)==1L&&
    !is.na(r$screening_id)&&nzchar(r$screening_id),logical(1))),
    "Cannot filter unscoped extraction records. A screening_id is required.",
    "USER_ACTION_REQUIRED")
  Filter(function(r)r$screening_id %in% ids,rows)
}
web_b2_selection_values <- function(values,key) {
  web_assert(is.list(values) && identical(names(values),key) &&
    is.character(values[[key]]) && length(values[[key]])==1L &&
    !is.na(values[[key]]) && nchar(values[[key]],type="bytes")<=50000L,
    "Invalid selection request.","VALIDATION_FAILED")
  ids<-tryCatch(jsonlite::fromJSON(values[[key]]),error=function(e)NULL)
  if(is.list(ids)&&length(ids)==0L)ids<-character()
  web_assert(is.character(ids) && !anyNA(ids) && !anyDuplicated(ids) &&
    all(nzchar(ids)),"Invalid selection identifiers.","VALIDATION_FAILED")
  ids
}
# RC28.19: source PDF result inventory is distinct from canonical R/25 outcomes.
# R/25 is protocol-limited and must never be represented as every reported PDF result.
# Every result is bound to one report and its verified PDF checksum. All AI numbers
# remain immutable; candidate R/25 rows are ONLY explicit Human source choices.
web_b2_discovery_ready <- function(x) {
  d<-x$pdf_result_discovery
  if(!is.list(d)||!identical(d$status,"COMPLETE"))return(FALSE)
  active<-web_b2_final_active_reports(x,TRUE)
  checks<-d$report_checksums %||b2% list()
  authors<-x$tables$Author_Final_Review %||b2% list()
  if(!length(active))return(FALSE)
  all(vapply(active,function(id){
    r<-Filter(function(a)identical(as.character(a$screening_id %||b2% ""),id),authors)
    length(r)==1L && !is.null(checks[[id]]) &&
      identical(as.character(checks[[id]]),as.character(r[[1L]]$.pdf_sha256 %||b2% "")) &&
      nzchar(as.character(checks[[id]]))
  },logical(1)))
}
web_b2_result_inventory <- function(x) {
  if(!web_b2_discovery_ready(x))return(list())
  active<-web_b2_final_active_reports(x,TRUE)
  reported<-x$pdf_result_discovery$results %||b2% list()
  ai<-x$tables$AI_outcome_data %||b2% list()
  ai<-Filter(function(r)as.character(r$screening_id %||b2% "") %in% active,ai)
  keys<-vapply(ai,function(r)as.character(r$.row_id %||b2% ""),character(1))
  web_assert(!length(ai)||(all(nzchar(keys))&&!anyNA(keys)&&!anyDuplicated(keys)),
    "Saved R/25 extraction source IDs are missing or duplicated.","USER_ACTION_REQUIRED")
  groups<-list()
  for(r in reported) {
    id<-as.character(r$screening_id %||b2% "")
    if(!id %in% active)next
    gkey<-as.character(r$result_id %||b2% "")
    web_assert(identical(as.character(r$pdf_sha256 %||b2% ""),
      as.character(x$pdf_result_discovery$report_checksums[[id]] %||b2% "")),
      "PDF result's source checksum no longer matches the selected report.","USER_ACTION_REQUIRED")
    web_assert(nzchar(gkey)&&is.null(groups[[gkey]]),
      "PDF result inventory contains a missing or duplicated result ID.","USER_ACTION_REQUIRED")
    src<-Filter(function(a)identical(as.character(a$screening_id %||b2% ""),id),ai)
    groups[[gkey]]<-list(id=gkey,screening_id=id,
      canonical_name=as.character(r$reported_name %||b2% ""),
      reported_name=as.character(r$reported_name %||b2% ""),
      reported_definition=as.character(r$reported_definition %||b2% ""),
      reported_timepoint=as.character(r$reported_timepoint %||b2% ""),
      source_location=as.character(r$source_location %||b2% ""),
      source_evidence=as.character(r$source_evidence %||b2% ""),
      result_kind=as.character(r$result_kind %||b2% "other"),
      reported_effect_measure=as.character(r$reported_effect_measure %||b2% ""),
      effect_measure_location=as.character(r$effect_measure_location %||b2% ""),
      pdf_sha256=as.character(r$pdf_sha256 %||b2% ""),
      # R25 numeric rows are not automatically mapped; Human chooses one.
      sources=src)
  }
  unname(groups)
}
# Effect measure is a source fact when explicitly reported, otherwise a
# labelled analysis candidate; never present RR/MD defaults as PDF findings.
# Historical v1 PDF discoveries lack this field and must remain usable without
# new AI calls. Human numerical verification remains a separate, mandatory gate.
web_b2_resolve_effect_measure <- function(g, source_row_id="") {
  measures<-c("RR","OR","HR","RD","MD","SMD")
  as_one<-function(v) {
    z<-as.character(v %||b2% "")
    if(length(z)!=1L||is.na(z))return("")
    trimws(z)
  }
  pdf<-toupper(as_one(g$reported_effect_measure))
  if(pdf %in% measures)
    return(list(measure=pdf,status="PDF_REPORTED",
      detail=paste0("Reported in PDF: ",as_one(g$effect_measure_location),
        ". Human verification pending.")))
  src_id<-as_one(source_row_id)
  sources<-g$sources %||b2% list()
  if(nzchar(src_id)) {
    matches<-Filter(function(r)identical(as_one(r$.row_id),src_id),sources)
    if(length(matches)==1L) {
      measure<-toupper(as_one(matches[[1L]]$effect_measure))
      if(measure %in% measures)
        return(list(measure=measure,status="R25_SOURCE",
          detail="R/25 source-reported effect measure; not independently verified."))
    }
  }
  # We do not automatically link any AI numerical source row by paper alone.
  # Exact matching is display-only and requires a nonempty explicit timepoint;
  # never copy numerical values, select source IDs, or infer 1y == 2y.
  label<-tolower(as_one(g$reported_name))
  window<-tolower(as_one(g$reported_timepoint))
  exact<-list()
  if(nzchar(label)&&nzchar(window)) {
    exact<-Filter(function(r)
      identical(tolower(as_one(r$outcome_name)),label)&&
      identical(tolower(as_one(r$timepoint)),window),sources)
    reported<-unique(vapply(exact,function(r){
      m<-toupper(as_one(r$effect_measure))
      if(m %in% measures &&
          all(vapply(c("effect_estimate","lower_ci","upper_ci"),function(f)
            nzchar(as_one(r[[f]])),logical(1))))m else ""
    },character(1)))
    reported<-reported[nzchar(reported)]
    if(length(reported)==1L)
      return(list(measure=reported,status="R25_EXACT_CANDIDATE",
        detail="Exact name/timepoint match in R/25; suggested only, no numerical mapping or verification."))
  }
  kind<-tolower(as_one(g$result_kind))
  # Only propose derived metrics when source-specific raw numerical inputs exist.
  # A binary/continuous label alone cannot prove RR/MD is computable.
  raw_rows<-if(nzchar(src_id)) Filter(function(r)identical(as_one(r$.row_id),src_id),sources) else exact
  has_fields<-function(r,fields)all(vapply(fields,function(f)nzchar(as_one(r[[f]])),logical(1)))
  if(identical(kind,"binary")&&any(vapply(raw_rows,has_fields,logical(1),
      fields=c("intervention_events","intervention_total","comparator_events","comparator_total"))))
    return(list(measure="RR",status="ANALYSIS_CANDIDATE",
      detail="Candidate RR from R/25 raw event counts, NOT PDF-reported. Verify both arms and denominator values before analysis."))
  if(identical(kind,"continuous")&&any(vapply(raw_rows,has_fields,logical(1),
      fields=c("intervention_mean","intervention_sd","intervention_n","comparator_mean","comparator_sd","comparator_n"))))
    return(list(measure="MD",status="ANALYSIS_CANDIDATE",
      detail="Candidate MD from R/25 mean/SD/N, NOT PDF-reported. Verify compatible scales and numeric inputs."))
  list(measure="",status="PENDING_NUMERIC_VERIFICATION",
    detail="PDF-reported effect measure not captured or unavailable for this result, and no exact raw data match. Remains pending without inventing an estimate.")
}

web_b2_result_group_lookup <- function(x) {
  z<-web_b2_result_inventory(x)
  stats::setNames(z,vapply(z,function(a)a$id,character(1)))
}
web_b2_result_selected_groups <- function(x) {
  chosen<-as.character(unlist(x$result_inventory_selection$group_ids %||b2% list(),use.names=FALSE))
  groups<-web_b2_result_group_lookup(x)
  web_assert(!anyNA(chosen)&&!anyDuplicated(chosen)&&all(chosen %in% names(groups)),
    "The saved result inventory is stale or contains unknown result groups.","USER_ACTION_REQUIRED")
  groups[chosen]
}
web_b2_save_result_inventory <- function(con,settings,actor,project_id,version_id,values) {
  c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
  web_assert(identical(x$stage,"EXTRACTION"),"Select results during extraction review.","USER_ACTION_REQUIRED")
  selected<-web_b2_selection_values(values,"result_group_ids_json")
  web_assert(web_b2_discovery_ready(x),
    "Run and save PDF Result Discovery before choosing outcomes.","USER_ACTION_REQUIRED")
  groups<-web_b2_result_group_lookup(x)
  web_assert(length(selected)>0L&&all(selected %in% names(groups)),
    "Select at least one actual R/25 extracted result for a selected study.","USER_ACTION_REQUIRED")
  selected<-names(groups)[names(groups)%in%selected]
  before<-as.character(unlist(x$result_inventory_selection$group_ids %||b2% list(),use.names=FALSE))
  web_assert(!identical(selected,before),"This result selection is already saved.","USER_ACTION_REQUIRED")
  old_target<-x$tables$Target_Outcome_Verification %||b2% list()
  # Re-selection cannot silently discard a prior independently verified row.
  locked<-Filter(function(r)isTRUE(r$.web_harmonized_result)&&
    as.character(r$verification_status %||b2% "") %in% c("Verified","Corrected"),old_target)
  web_assert(!length(locked),
    "A prior harmonized result is Human-verified. Resolve it before revising the inventory.",
    "USER_ACTION_REQUIRED")
  x$tables$Target_Outcome_Verification<-Filter(function(r)!isTRUE(r$.web_harmonized_result),old_target)
  x$result_inventory_selection<-list(status="HUMAN_SELECTED",group_ids=as.list(selected),
    actor=actor,at=web_now(),source_version=version_id)
  x$human_outcome_harmonization<-NULL
  x$synthesis_plan<-NULL
  x$synthesis_execution<-NULL
  x$history<-c(x$history,list(list(event="RESULT_INVENTORY_SELECTION",actor=actor,
    at=web_now(),source_version=version_id,group_ids=as.list(selected))))
  root<-tempfile("result-inventory-",settings$data_root);dir.create(root)
  on.exit(unlink(root,recursive=TRUE))
  web_copy_version(con,settings,actor,project_id,version_id,root)
  web_b2_invalidate(root,"EXTRACTION");web_b2_write(root,x)
  web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"EXTRACTION")
}
web_b2_harmonization_mapping_values <- function(values) {
  web_assert(is.list(values)&&identical(names(values),"mappings_json")&&
    is.character(values$mappings_json)&&length(values$mappings_json)==1L&&
    !is.na(values$mappings_json)&&nchar(values$mappings_json,type="bytes")<=1000000L,
    "Invalid Human harmonization submission.","VALIDATION_FAILED")
  z<-tryCatch(jsonlite::fromJSON(values$mappings_json,simplifyVector=FALSE),error=function(e)NULL)
  web_assert(is.list(z)&&length(z)>0L&&length(z)<=2000L,
    "No valid Human mappings submitted.","VALIDATION_FAILED")
  required<-c("group_id","source_row_id","final_name","final_definition",
    "analysis_window","effect_measure","meta_analysis_role","parent_analysis_group")
  for(a in z) {
    web_assert(is.list(a)&&identical(sort(names(a)),sort(required))&&
      all(vapply(a,function(v)is.character(v)&&length(v)==1L&&
        !is.na(v)&&nchar(v)<=2000L,logical(1))),
      "Each mapping requires eight single-value Human fields.","VALIDATION_FAILED")
  }
  z
}
web_b2_save_human_harmonization <- function(con,settings,actor,project_id,version_id,values) {
  c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
  web_assert(identical(x$stage,"EXTRACTION")&&
    identical(x$result_inventory_selection$status,"HUMAN_SELECTED"),
    "Save the study-specific result inventory first.","USER_ACTION_REQUIRED")
  groups<-web_b2_result_selected_groups(x)
  mappings<-web_b2_harmonization_mapping_values(values)
  mids<-vapply(mappings,`[[`,character(1),"group_id")
  web_assert(!anyDuplicated(mids)&&setequal(mids,names(groups)),
    "Harmonization must cover every Human-selected result exactly once.","VALIDATION_FAILED")
  mappings<-mappings[match(names(groups),mids)]
  allowed_roles<-c("Primary","Secondary","Sensitivity","Exploratory","Narrative only")
  measures<-c("RR","OR","HR","RD","MD","SMD")
  group_study_keys<-character()
  used_source_ids<-character()
  catalog<-web_b2_final_study_catalog(x)
  checked<-list()
  for(i in seq_along(mappings)) {
    a<-mappings[[i]];g<-groups[[a$group_id]]
    a<-lapply(a,trimws)
    web_assert(nzchar(a$final_name)&&nzchar(a$final_definition)&&
      nzchar(a$analysis_window)&&
      (a$effect_measure %in% measures || !nzchar(a$effect_measure))&&
      a$meta_analysis_role %in% allowed_roles,
      "Finalize each outcome name, operational definition, time window, measure and role.",
      "USER_ACTION_REQUIRED")
    source_ids<-vapply(g$sources,function(r)as.character(r$.row_id %||b2% ""),character(1))
    web_assert(!nzchar(a$source_row_id)||a$source_row_id %in% source_ids,
      "The chosen numerical source row does not belong to this report.","VALIDATION_FAILED")
    src<-if(nzchar(a$source_row_id))g$sources[[match(a$source_row_id,source_ids)]] else list()
    measure<-toupper(trimws(as.character(src$effect_measure %||b2% "")))
    recognized<-measure %in% measures
    if(nzchar(a$source_row_id)) {
      web_assert(!a$source_row_id %in% used_source_ids,
        "An R/25 numeric source row cannot be assigned to multiple PDF outcomes.",
        "USER_ACTION_REQUIRED")
      used_source_ids<-c(used_source_ids,a$source_row_id)
    }
    web_assert(!recognized||identical(a$meta_analysis_role,"Narrative only")||
      !nzchar(a$effect_measure)||identical(measure,a$effect_measure),
      "A source effect estimate cannot be relabelled as another effect measure.",
      "USER_ACTION_REQUIRED")
    dt<-tolower(as.character(src$data_type %||b2% ""))
    if(grepl("dichotom|binary|event",dt)&&nzchar(a$effect_measure))
      web_assert(a$effect_measure %in% c("RR","OR","RD"),"Binary raw inputs require RR/OR/RD.","USER_ACTION_REQUIRED")
    if(grepl("continuous|mean",dt)&&nzchar(a$effect_measure))
      web_assert(a$effect_measure %in% c("MD","SMD"),"Continuous raw inputs require MD/SMD.","USER_ACTION_REQUIRED")
    # Without a supported source-reported measure, a time-to-event result
    # remains unverified and excluded from pooling until Human numeric review.
    # Unknown timepoints are unverified and not eligible for numerical pooling.
    # Analysis identity includes time window, role and effect measure; a study
    # cannot be counted twice in one forest plot for the same analysis set.
    analysis<-paste(a$final_name,a$analysis_window,a$effect_measure,a$meta_analysis_role,sep=" | ")
    owning<-Filter(function(z)g$screening_id %in% z$screening_ids,catalog)
    web_assert(length(owning)==1L,"A result must map to exactly one final Study ID.","USER_ACTION_REQUIRED")
    match_key<-paste(owning[[1L]]$study_id,analysis,sep="\u241F")
    web_assert(!match_key %in% group_study_keys,
      "Two selected results from the same report map to the same forest plot. Choose one representation or distinguish analyses.",
      "USER_ACTION_REQUIRED")
    group_study_keys<-c(group_study_keys,match_key)
    a$analysis_group<-analysis
    a$screening_id<-g$screening_id
    checked[[i]]<-list(map=a,source=src,group=g)
  }
  base_rows<-x$tables$Target_Outcome_Verification %||b2% list()
  locked<-Filter(function(r)isTRUE(r$.web_harmonized_result)&&
    as.character(r$verification_status %||b2% "") %in% c("Verified","Corrected"),base_rows)
  web_assert(!length(locked),"Harmonization edits require explicit review of previously verified rows.","USER_ACTION_REQUIRED")
  base_rows<-Filter(function(r)!isTRUE(r$.web_harmonized_result),base_rows)
  # Existing independent verified included rows cannot be silently displaced.
  old_approved<-Filter(function(r)as.character(r$screening_id %||b2% "") %in%
    web_b2_final_active_reports(x,TRUE)&&
    identical(as.character(r$human_include_in_planned_analysis %||b2% ""),"Yes")&&
    as.character(r$verification_status %||b2% "") %in% c("Verified","Corrected"),base_rows)
  web_assert(!length(old_approved),
    "Pre-existing Human-verified analysis rows need reconciliation before new harmonization can replace them.",
    "USER_ACTION_REQUIRED")
  for(j in seq_along(base_rows)) {
    r<-base_rows[[j]]
    if(as.character(r$screening_id %||b2% "") %in% web_b2_final_active_reports(x,TRUE)&&
       identical(as.character(r$human_include_in_planned_analysis %||b2% ""),"Yes")) {
      r$human_include_in_planned_analysis<-"No"
      r$verification_status<-"Needs clarification"
      r$.verified_by<-NULL
      base_rows[[j]]<-r
    }
  }
  base_cols<-unique(unlist(lapply(base_rows,names),use.names=FALSE))
  base_cols<-base_cols[!startsWith(base_cols,".")]
  created<-list()
  authors<-x$tables$Author_Final_Review %||b2% list()
  for(item in checked) {
    a<-item$map;src<-item$source;g<-item$group
    row<-stats::setNames(as.list(rep("",length(base_cols))),base_cols)
    for(k in intersect(names(row),names(src)))row[[k]]<-as.character(src[[k]])
    for(k in c("screening_id","outcome_name","outcome_definition","timepoint",
      "data_type","source_location","source_evidence","effect_measure",
      "effect_estimate","lower_ci","upper_ci","intervention_events",
      "intervention_total","comparator_events","comparator_total",
      "intervention_mean","intervention_sd","intervention_n","comparator_mean",
      "comparator_sd","comparator_n"))
      if(is.null(row[[k]]))row[[k]]<-as.character(src[[k]] %||b2% "")
    row$screening_id<-g$screening_id
    # Never use the shared protocol label as the paper's original name.
    row$outcome_definition<-g$reported_definition
    row$timepoint<-g$reported_timepoint
    row$source_location<-g$source_location
    row$source_evidence<-g$source_evidence
    row$original_pdf_result_name<-g$reported_name
    row$original_pdf_result_definition<-g$reported_definition
    row$original_pdf_result_timepoint<-g$reported_timepoint
    row$original_pdf_reported_effect_measure<-g$reported_effect_measure
    row$original_pdf_measure_location<-g$effect_measure_location
    detected<-web_b2_resolve_effect_measure(g,a$source_row_id)
    row$effect_measure_origin<-if(nzchar(a$effect_measure)&&
      identical(a$effect_measure,detected$measure))detected$status else "PENDING_HUMAN_REVIEW"
    row$original_pdf_result_id<-g$id
    row$outcome_row_id<-paste0("WEB_HM_",substr(digest::digest(g$id,algo="sha256"),1L,20L))
    row$outcome_name<-a$final_name # human analysis label, never source field
    row$analysis_group<-a$analysis_group
    row$meta_analysis_role<-a$meta_analysis_role
    row$parent_analysis_group<-as.character(a$parent_analysis_group %||b2% "")
    # An unresolved measure cannot enter a numerical analysis, regardless
    # of selected role. Human verification may later resolve the measure.
    row$human_include_in_planned_analysis<-if(identical(a$meta_analysis_role,"Narrative only") ||
      !nzchar(a$effect_measure) ||
      tolower(a$analysis_window) %in% c("not reported","unreported","unknown","not stated"))"No" else "Yes"
    row$verification_status<-"Needs clarification"
    row$human_verified_effect_measure<-a$effect_measure
    numeric_keys<-c("intervention_events","comparator_events","intervention_mean","comparator_mean",
      "effect_estimate","lower_ci","upper_ci")
    has_source_value<-any(vapply(numeric_keys,function(k)nzchar(trimws(as.character(src[[k]] %||b2% ""))),logical(1)))
    row$value_status<-if(has_source_value)"REPORTED" else "MISSING"
    row$.row_id<-web_id()
    # Mark the Human mapping author; preserve independent subsequent numerical
    # verification (the existing Web gate requires a different verifier actor).
    row$.actor<-actor
    row$.timestamp<-web_now()
    row$.source<-if(nzchar(a$source_row_id))
      "PDF DISCOVERY / R25 SOURCE / HUMAN HARMONIZATION; UNVERIFIED" else
      "HUMAN ENTRY REQUIRED"
    row$.web_harmonized_result<-TRUE
    row$.source_ai_row_id<-a$source_row_id
    row$.source_ai_outcome_name<-as.character(src$outcome_name %||b2% "")
    row$.source_ai_timepoint<-as.character(src$timepoint %||b2% "")
    row$.analysis_window<-a$analysis_window
    row$.human_operational_definition<-a$final_definition
    # Public, non-destructive mapping columns travel in the R/26 workbook.
    # Original source data remain separately intact in AI_outcome_data.
    row$harmonized_outcome_name<-a$final_name
    row$harmonized_operational_definition<-a$final_definition
    row$harmonized_analysis_window<-a$analysis_window
    row$original_ai_outcome_name<-as.character(src$outcome_name %||b2% "")
    row$original_ai_timepoint<-as.character(src$timepoint %||b2% "")
    row$original_ai_data_type<-as.character(src$data_type %||b2% "")
    # R/26's existing contract accepts a reported HR, not the raw string
    # "time_to_event". Convert only the representation classification when
    # estimate and both CI bounds are explicitly present; never calculate them.
    if(tolower(as.character(src$data_type %||b2% "")) %in% c("time_to_event","time-to-event") &&
       a$effect_measure=="HR" &&
       all(vapply(c("effect_estimate","lower_ci","upper_ci"),function(k)
         nzchar(trimws(as.character(src[[k]] %||b2% ""))),logical(1))))
      row$data_type<-"reported_effect"
    row$original_ai_source_row_id<-a$source_row_id
    source_report<-Filter(function(r)identical(as.character(r$screening_id %||b2% ""),g$screening_id),authors)
    web_assert(length(source_report)==1L,"Result source report identity is not unique.","USER_ACTION_REQUIRED")
    row$study_id<-source_report[[1L]]$study_id
    row$.pdf_sha256<-source_report[[1L]]$.pdf_sha256
    created[[length(created)+1L]]<-row
  }
  all_rows<-c(base_rows,created)
  ids<-vapply(all_rows,function(r)as.character(r$outcome_row_id %||b2% ""),character(1))
  web_assert(!anyDuplicated(ids[nzchar(ids)]),
    "Harmonized Outcome IDs would conflict with existing rows.","USER_ACTION_REQUIRED")
  x$tables$Target_Outcome_Verification<-all_rows
  x$synthesis_plan<-NULL  # Prior synthesis plan invalid after remapping.
  x$synthesis_execution<-NULL
  x$human_outcome_harmonization<-list(status="HUMAN_CONFIRMED",
    mappings=lapply(checked,function(i)i$map),actor=actor,at=web_now(),
    source_version=version_id,original_source_untouched=TRUE,
    all_values_require_separate_human_verification=TRUE)
  x$history<-c(x$history,list(list(event="HUMAN_OUTCOME_HARMONIZATION",actor=actor,
    at=web_now(),source_version=version_id,
    pdf_result_ids=as.list(vapply(mappings,`[[`,character(1),"group_id")),
    source_rows=as.list(vapply(mappings,`[[`,character(1),"source_row_id")))))
  root<-tempfile("human-harmonization-",settings$data_root);dir.create(root)
  on.exit(unlink(root,recursive=TRUE))
  web_copy_version(con,settings,actor,project_id,version_id,root)
  web_b2_invalidate(root,"EXTRACTION");web_b2_write(root,x)
  web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"EXTRACTION")
}

web_b2_save_final_studies <- function(con,settings,actor,project_id,version_id,values) {
  c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
  web_assert(identical(x$stage,"EXTRACTION"),
    "Final Study Selection is available during extraction review. Earlier decisions remain immutable.",
    "USER_ACTION_REQUIRED")
  chosen<-web_b2_selection_values(values,"study_ids_json")
  catalog<-web_b2_final_study_catalog(x)
  all_ids<-vapply(catalog,`[[`,character(1),"study_id")
  web_assert(length(chosen)>0L && all(chosen %in% all_ids),
    "Select at least one full-text eligible Study ID.","USER_ACTION_REQUIRED")
  chosen<-all_ids[all_ids %in% chosen] # Canonical order, not browser order.
  previous<-if(is.null(x$final_study_selection))character() else
    web_b2_final_active_study_ids(x,TRUE)
  web_assert(!identical(chosen,previous),
    "This study selection is already saved in the selected Evidence Version.",
    "USER_ACTION_REQUIRED")
  # A revised final study set invalidates any later result inventory/mapping.
  # Verified harmonization rows cannot be silently dropped.
  old_harmonized<-x$tables$Target_Outcome_Verification %||b2% list()
  locked_harmonized<-Filter(function(r)isTRUE(r$.web_harmonized_result)&&
    as.character(r$verification_status %||b2% "") %in% c("Verified","Corrected"),old_harmonized)
  web_assert(!length(locked_harmonized),
    "A prior harmonized outcome has Human verification. Reconcile before changing Final Study Selection.",
    "USER_ACTION_REQUIRED")
  x$tables$Target_Outcome_Verification<-Filter(function(r)!isTRUE(r$.web_harmonized_result),old_harmonized)
  x$result_inventory_selection<-NULL
  x$human_outcome_harmonization<-NULL
  x$synthesis_plan<-NULL
  x$final_study_selection<-list(status="HUMAN_CONFIRMED",study_ids=as.list(chosen),
    actor=actor,at=web_now(),source_version=version_id)
  x$history<-c(x$history,list(list(event="FINAL_STUDY_SELECTION",actor=actor,
    at=web_now(),source_version=version_id,study_ids=as.list(chosen))))
  root<-tempfile("final-study-selection-",settings$data_root)
  dir.create(root);on.exit(unlink(root,recursive=TRUE))
  web_copy_version(con,settings,actor,project_id,version_id,root)
  web_b2_invalidate(root,"EXTRACTION")
  web_b2_write(root,x)
  web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"EXTRACTION")
}
# RC28.20 Web-only display normalization: group common variables for the UI.
# Keep EVERY source mapping / analysis_group unchanged. Only remove a suffix
# when it exactly agrees with the *saved Human* analysis timepoint.
web_b2_display_common_variable <- function(label,window) {
  label<-trimws(as.character(label %||b2% ""))
  window<-tolower(trimws(as.character(window %||b2% "")))
  if(length(label)!=1L||is.na(label)||!nzchar(label)||
     length(window)!=1L||is.na(window))return(label)
  suffixes<-list(
    list(s=c("_1yr","_1yrs","_1y"),p="(^|[^0-9])1[ -]year"),
    list(s=c("_2yr","_2yrs","_2y"),p="(^|[^0-9])2[ -]year"),
    list(s=c("_6m","_6mo","_6months"),p="(^|[^0-9])6[ -]month"),
    list(s=c("_30d","_30days"),p="(^|[^0-9])30[ -]day"))
  for(rule in suffixes)if(grepl(rule$p,window,perl=TRUE)) {
    for(suffix in rule$s)if(endsWith(tolower(label),suffix)) {
      cleaned<-trimws(substr(label,1L,nchar(label)-nchar(suffix)))
      if(nzchar(cleaned))return(cleaned)
    }
  }
  label
}

# RC28.19: Human synthesis planning is a separate, versioned Web decision.
# Group research variables by Human common name; retain time-window strata.
# No numerical value becomes Verified/Corrected through synthesis planning.
web_b2_synthesis_catalog <- function(x) {
  web_assert(identical(x$human_outcome_harmonization$status,"HUMAN_CONFIRMED"),
    "Confirm common outcomes before planning synthesis.","USER_ACTION_REQUIRED")
  mapped<-x$human_outcome_harmonization$mappings %||b2% list()
  selected<-web_b2_result_selected_groups(x)
  studies<-web_b2_final_study_catalog(x)
  active<-web_b2_final_active_study_ids(x,TRUE)
  out<-list()
  for(m in mapped) {
    g<-selected[[as.character(m$group_id)]]
    web_assert(!is.null(g),"Stale synthesis source mapping.","USER_ACTION_REQUIRED")
    owning<-Filter(function(s)g$screening_id %in% s$screening_ids,studies)
    web_assert(length(owning)==1L && owning[[1L]]$study_id %in% active,
      "A synthesis source does not belong to a selected study.","USER_ACTION_REQUIRED")
    name<-trimws(as.character(m$final_name))
    group<-as.character(m$analysis_group)
    web_assert(nzchar(name)&&nzchar(group),"Missing final outcome identity.","USER_ACTION_REQUIRED")
    display_name<-web_b2_display_common_variable(name,m$analysis_window)
    key<-tolower(display_name)
    if(is.null(out[[key]]))out[[key]]<-list(name=display_name,strata=list(),studies=character())
    if(is.null(out[[key]]$strata[[group]]))out[[key]]$strata[[group]]<-list(
      id=group,timepoint=as.character(m$analysis_window),
      effect_measure=as.character(m$effect_measure),
      role=as.character(m$meta_analysis_role),sources=list(),
      definitions=character(),study_ids=character())
    s<-owning[[1L]]
    entry<-list(result_id=as.character(g$id),study_id=s$study_id,screening_id=g$screening_id,
      title=if(length(s$titles))as.character(s$titles[[1L]]) else "",
      pdf_outcome=as.character(g$reported_name),
      pdf_definition=as.character(g$reported_definition),
      pdf_timepoint=as.character(g$reported_timepoint),
      common_definition=as.character(m$final_definition),
      source_location=as.character(g$source_location))
    out[[key]]$studies<-unique(c(out[[key]]$studies,s$study_id))
    z<-out[[key]]$strata[[group]]
    z$sources<-c(z$sources,list(entry))
    z$study_ids<-unique(c(z$study_ids,s$study_id))
    z$definitions<-unique(c(z$definitions,tolower(trimws(m$final_definition))))
    out[[key]]$strata[[group]]<-z
  }
  for(i in seq_along(out))for(j in seq_along(out[[i]]$strata)) {
    z<-out[[i]]$strata[[j]]
    z$quantitative_candidate<-length(z$study_ids)>=2L &&
      length(z$definitions)==1L && nzchar(z$effect_measure) &&
      z$effect_measure %in% c("RR","OR","HR","RD","MD","SMD") &&
      !identical(z$role,"Narrative only") &&
      !tolower(z$timepoint) %in% c("not reported","unknown","unreported","not stated")
    out[[i]]$strata[[j]]<-z
  }
  out
}
web_b2_synthesis_plan_values <- function(values) {
  web_assert(is.list(values)&&identical(names(values),"decisions_json")&&
    is.character(values$decisions_json)&&length(values$decisions_json)==1L&&
    !is.na(values$decisions_json)&&nchar(values$decisions_json,type="bytes")<=200000L,
    "Invalid synthesis planning request.","VALIDATION_FAILED")
  z<-tryCatch(jsonlite::fromJSON(values$decisions_json,simplifyVector=FALSE),error=function(e)NULL)
  web_assert(is.list(z)&&length(z)>0L&&length(z)<=2000L,
    "Select a synthesis route for each confirmed timepoint stratum.","VALIDATION_FAILED")
  for(a in z)web_assert(is.list(a)&&identical(sort(names(a)),c("analysis_group","mode"))&&
    is.character(a$analysis_group)&&length(a$analysis_group)==1L&&!is.na(a$analysis_group)&&
    nzchar(a$analysis_group)&&is.character(a$mode)&&length(a$mode)==1L&&
    a$mode %in% c("QUANTITATIVE_CANDIDATE","NARRATIVE","RECONCILE"),
    "Invalid synthesis planning decision.","VALIDATION_FAILED")
  z
}
web_b2_save_synthesis_plan <- function(con,settings,actor,project_id,version_id,values) {
  c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
  web_assert(identical(x$stage,"EXTRACTION"),
    "Synthesis planning is available during extraction review.","USER_ACTION_REQUIRED")
  catalog<-web_b2_synthesis_catalog(x)
  strata<-do.call(base::c,lapply(catalog,function(z)unname(z$strata)))
  names(strata)<-vapply(strata,function(s)as.character(s$id),character(1))
  web_assert(length(strata)>0L,"No harmonized outcomes available.","USER_ACTION_REQUIRED")
  decisions<-web_b2_synthesis_plan_values(values)
  ids<-vapply(decisions,`[[`,character(1),"analysis_group")
  web_assert(!anyDuplicated(ids)&&setequal(ids,names(strata)),
    "Synthesis decisions must cover every confirmed timepoint stratum exactly once.","VALIDATION_FAILED")
  decisions<-decisions[match(names(strata),ids)]
  for(i in seq_along(decisions)) {
    a<-decisions[[i]];s<-strata[[a$analysis_group]]
    if(identical(a$mode,"QUANTITATIVE_CANDIDATE"))
      web_assert(isTRUE(s$quantitative_candidate),
        paste0("Pooling is not eligible for this study/timepoint set: ",a$analysis_group),
        "USER_ACTION_REQUIRED")
  }
  prior<-x$synthesis_plan$decisions %||b2% list()
  old_ids<-vapply(prior,function(a)as.character(a$analysis_group),character(1))
  old_modes<-if(length(old_ids))stats::setNames(vapply(prior,function(a)as.character(a$mode),character(1)),old_ids) else character()
  web_assert(!length(old_modes)||!identical(unname(old_modes[names(strata)]),vapply(decisions,`[[`,character(1),"mode")),
    "This synthesis plan is already saved in the selected Evidence Version.","USER_ACTION_REQUIRED")
  # PLAN ONLY. Source numbers, verified status and R/26 input rows do not change.
  x$synthesis_plan<-list(status="HUMAN_CONFIRMED",decisions=decisions,
    actor=actor,at=web_now(),source_version=version_id,
    numeric_verification_required=TRUE)
  # Auto-first: prepare source-indexed outputs in this SAME immutable version.
  # No extra button, no AI call, no numerical approval and no scientific model.
  x$synthesis_execution<-web_b2_build_synthesis_execution(x)
  x$synthesis_execution$actor<-"TOOLKIT_AUTOMATED"
  x$synthesis_execution$at<-web_now()
  x$synthesis_execution$source_version<-version_id
  x$history<-c(x$history,list(list(event="HUMAN_SYNTHESIS_PLAN",actor=actor,
    at=web_now(),source_version=version_id,decisions=decisions)))
  root<-tempfile("synthesis-plan-",settings$data_root);dir.create(root)
  on.exit(unlink(root,recursive=TRUE))
  web_copy_version(con,settings,actor,project_id,version_id,root)
  web_b2_invalidate(root,"EXTRACTION");web_b2_write(root,x)
  web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"EXTRACTION")
}

# RC28.20 — Automatic synthesis execution, Web-only.
# No provider call, no estimated/missing numeric values, no canonical R mutation.
# The saved packet contains a source-bound, download-ready evidence matrix and
# a structured narrative draft. This is preliminary until numerical source QA.
web_b2_synthesis_source_digest <- function(x) {
  digest::digest(list(x$final_study_selection,x$result_inventory_selection,
    x$human_outcome_harmonization,x$synthesis_plan,
    x$pdf_result_discovery$report_checksums,
    x$tables$Target_Outcome_Verification),algo="sha256",serialize=TRUE)
}
web_b2_synthesis_text_value <- function(x,max_chars=500L) {
  z<-as.character(x %||b2% "")
  if(length(z)!=1L||is.na(z))return("")
  z<-gsub("[\r\n\t]+"," ",z)
  substr(trimws(z),1L,max_chars)
}
web_b2_verified_numerical_summary <- function(r) {
  if(is.null(r) || !as.character(r$verification_status %||b2% "") %in%
       c("Verified","Corrected") ||
       !nzchar(as.character(r$.verified_by %||b2% "")))return("")
  get<-function(k)web_b2_synthesis_text_value(r[[paste0("human_verified_",k)]] %||b2% r[[k]],90L)
  effect<-get("effect_estimate")
  lower<-get("lower_ci")
  upper<-get("upper_ci")
  measure<-get("effect_measure")
  if(nzchar(effect)&&nzchar(lower)&&nzchar(upper)&&
     measure %in% c("RR","OR","HR","RD","MD","SMD"))
    return(paste0(measure," ",effect," (95% CI ",lower,"–",upper,")"))
  a<-vapply(c("intervention_events","intervention_total",
    "comparator_events","comparator_total"),get,character(1))
  if(all(nzchar(a)))
    return(paste0("Intervention ",a[[1L]],"/",a[[2L]],
      "; comparator ",a[[3L]],"/",a[[4L]]))
  cont<-vapply(c("intervention_mean","intervention_sd","intervention_n",
    "comparator_mean","comparator_sd","comparator_n"),get,character(1))
  if(all(nzchar(cont)))
    return(paste0("Intervention mean ",cont[[1L]]," (SD ",cont[[2L]],
      ", n=",cont[[3L]],"); comparator mean ",cont[[4L]],
      " (SD ",cont[[5L]],", n=",cont[[6L]],")"))
  "" # Verified status alone does not establish a displayable effect.
}
web_b2_build_synthesis_execution <- function(x) {
  web_assert(identical(x$synthesis_plan$status,"HUMAN_CONFIRMED"),
    "Save Step 4 synthesis choices before executing the synthesis.","USER_ACTION_REQUIRED")
  catalog<-web_b2_synthesis_catalog(x)
  strata<-do.call(base::c,lapply(catalog,function(a)unname(a$strata)))
  web_assert(length(strata)>0L,"No saved analysis sets available.","USER_ACTION_REQUIRED")
  ids<-vapply(strata,function(s)as.character(s$id),character(1))
  decisions<-x$synthesis_plan$decisions %||b2% list()
  plan_ids<-vapply(decisions,function(d)as.character(d$analysis_group),character(1))
  web_assert(!anyDuplicated(ids)&&!anyDuplicated(plan_ids)&&setequal(ids,plan_ids),
    "The synthesis plan is stale relative to the confirmed common outcomes.","USER_ACTION_REQUIRED")
  modes<-stats::setNames(vapply(decisions,function(d)as.character(d$mode),character(1)),plan_ids)
  canonical<-x$tables$Target_Outcome_Verification %||b2% list()
  source_rows<-Filter(function(a)isTRUE(a$.web_harmonized_result),canonical)
  source_ids<-vapply(source_rows,function(a)
    as.character(a$original_pdf_result_id %||b2% ""),character(1))
  web_assert(!anyDuplicated(source_ids[nzchar(source_ids)]),
    "Duplicate source-specific numerical result IDs require correction.","USER_ACTION_REQUIRED")
  records<-list()
  set_summaries<-list()
  narratives<-list()
  for(v in catalog) for(s in v$strata) {
    mode<-as.character(modes[[s$id]])
    web_assert(mode %in% c("NARRATIVE","RECONCILE","QUANTITATIVE_CANDIDATE"),
      "Unrecognized synthesis route.","USER_ACTION_REQUIRED")
    web_assert(!identical(mode,"QUANTITATIVE_CANDIDATE") || isTRUE(s$quantitative_candidate),
      "An ineligible analysis set cannot enter quantitative synthesis.","USER_ACTION_REQUIRED")
    # Compare original PDF terminology, not only Human-harmonized labels.
    # Variation is a review flag rather than proof of non-comparability.
    pdf_names<-unique(tolower(trimws(vapply(s$sources,function(a)
      as.character(a$pdf_outcome),character(1)))))
    pdf_definitions<-unique(tolower(trimws(vapply(s$sources,function(a)
      as.character(a$pdf_definition %||b2% ""),character(1)))))
    pdf_definitions<-pdf_definitions[nzchar(pdf_definitions)]
    source_name_variation<-length(pdf_names)>1L
    source_definition_variation<-length(pdf_definitions)>1L
    rows<-list()
    for(a in s$sources) {
      rid<-as.character(a$result_id)
      hit<-which(source_ids==rid)
      web_assert(length(hit)<=1L,"Numerical evidence has ambiguous source identity.","USER_ACTION_REQUIRED")
      numerics<-if(length(hit)==1L)source_rows[[hit]] else NULL
      verified<-!is.null(numerics) &&
        as.character(numerics$verification_status %||b2% "") %in% c("Verified","Corrected") &&
        nzchar(as.character(numerics$.verified_by %||b2% ""))
      display_numbers<-if(verified)web_b2_verified_numerical_summary(numerics) else ""
      eligible<-verified && identical(mode,"QUANTITATIVE_CANDIDATE") &&
        identical(as.character(numerics$human_include_in_planned_analysis %||b2% ""),"Yes") &&
        nzchar(as.character(numerics$effect_measure %||b2% ""))
      issues<-character()
      if(source_name_variation)issues<-c(issues,"ORIGINAL_ENDPOINT_LABEL_VARIATION")
      if(source_definition_variation)issues<-c(issues,"ORIGINAL_ENDPOINT_DEFINITION_VARIATION")
      if(!nzchar(web_b2_synthesis_text_value(a$source_location)))issues<-c(issues,"SOURCE_LOCATION_MISSING")
      if(!verified)issues<-c(issues,"NUMERICAL_VALUES_NOT_HUMAN_VERIFIED")
      if(verified && !nzchar(display_numbers))issues<-c(issues,"VERIFIED_BUT_NO_DISPLAYABLE_NUMERIC_RESULT")
      if(identical(mode,"RECONCILE"))issues<-c(issues,"HARMONIZATION_REQUIRED")
      if(identical(mode,"QUANTITATIVE_CANDIDATE") && !eligible)
        issues<-c(issues,"R26_HUMAN_GATE_PENDING")
      row<-list(common_variable=as.character(v$name),
        analysis_group=as.character(s$id),analysis_timepoint=as.character(s$timepoint),
        role=as.character(s$role),route=mode,
        study_id=as.character(a$study_id),screening_id=as.character(a$screening_id),
        result_id=rid,original_outcome=as.character(a$pdf_outcome),
        reported_timepoint=as.character(a$pdf_timepoint),
        source_location=web_b2_synthesis_text_value(a$source_location,400L),
        original_pdf_definition=web_b2_synthesis_text_value(a$pdf_definition,700L),
        harmonized_operational_definition=web_b2_synthesis_text_value(a$common_definition,700L),
        numeric_status=if(verified)"HUMAN_VERIFIED" else "UNVERIFIED",
        verified_numeric_summary=display_numbers,
        r26_eligible=eligible,issues=paste(issues,collapse=";"))
      rows[[length(rows)+1L]]<-row
      records[[length(records)+1L]]<-row
    }
    all_verified<-length(rows)>=2L && all(vapply(rows,function(z)isTRUE(z$r26_eligible),logical(1)))
    set_summaries[[length(set_summaries)+1L]]<-list(
      common_variable=as.character(v$name),analysis_group=as.character(s$id),
      timepoint=as.character(s$timepoint),route=mode,
      contributing_studies=length(s$study_ids),
      r26_status=if(identical(mode,"QUANTITATIVE_CANDIDATE"))
        if(all_verified)"NUMERIC_VERIFIED; R26 HUMAN GATE STILL REQUIRED" else
          "PENDING_R26_NUMERIC_VERIFICATION" else "NOT_FOR_QUANTITATIVE_POOLING")
    lead<-paste0("At ",s$timepoint,", ",length(s$study_ids)," selected study/studies reported ",
      v$name," (",paste(s$study_ids,collapse=", "),").")
    trailing<-if(identical(mode,"QUANTITATIVE_CANDIDATE"))
       if(all_verified)"Quantitative candidate: subject to canonical R/26 and R/27 checks; no pooled effect calculated here." else
          "Quantitative candidate awaiting independent numerical verification; no pooled effect calculated." else
      if(identical(mode,"RECONCILE"))"Synthesis withheld pending definition/timepoint reconciliation." else
       "Planned narrative synthesis; no pooled effect calculated."
    sentences<-vapply(rows,function(z)paste0(z$study_id,": ",z$original_outcome,
      " (PDF timepoint: ",z$reported_timepoint,") at ",z$source_location,
      if(nzchar(z$verified_numeric_summary))paste0("; Human-verified data: ",z$verified_numeric_summary) else
        "; numerical findings not independently verified"),character(1))
    narratives[[length(narratives)+1L]]<-list(
      common_variable=v$name,analysis_group=s$id,paragraph=paste(c(lead,trailing,sentences),collapse=" "))
  }
  list(status="AUTO_PREPARED",source_digest=web_b2_synthesis_source_digest(x),
    sets=set_summaries,rows=records,narratives=narratives,
    counts=list(variables=length(catalog),analysis_sets=length(set_summaries),
      evidence_rows=length(records),quantitative_candidates=sum(vapply(set_summaries,
      function(s)identical(s$route,"QUANTITATIVE_CANDIDATE"),logical(1)))),
    disclosure="Automated source-indexed synthesis draft. Unverified PDF numbers are never presented as verified results. No statistical model was fitted. Quantitative pooling requires canonical R/26–R/27 Human gates.")
}
web_b2_synthesis_execution_table <- function(run) {
  cols<-c("common_variable","analysis_group","analysis_timepoint","role","route",
    "study_id","screening_id","result_id","original_outcome","reported_timepoint",
    "source_location","original_pdf_definition","harmonized_operational_definition",
    "numeric_status","verified_numeric_summary",
    "r26_eligible","issues")
  rows<-run$rows %||b2% list()
  if(!length(rows))return(as.data.frame(stats::setNames(rep(list(character()),length(cols)),cols)))
  as.data.frame(stats::setNames(lapply(cols,function(k)
    vapply(rows,function(a)as.character(a[[k]] %||b2% ""),character(1))),cols),
    stringsAsFactors=FALSE)
}
web_b2_synthesis_execution_markdown <- function(run) {
  web_assert(identical(run$status,"AUTO_PREPARED"),"Synthesis execution is unavailable.")
  lines<-c("# Automated synthesis — source-indexed draft", "",
    as.character(run$disclosure),"",
    paste0("Outcome variables: ",run$counts$variables,
      "; analysis sets: ",run$counts$analysis_sets,
      "; indexed study results: ",run$counts$evidence_rows),"",
    "## Results by analysis set","")
  for(a in run$narratives)lines<-c(lines,paste0("### ",a$common_variable,
    " — ",a$analysis_group),"",a$paragraph,"")
  c(lines,"## Methodological limitations","",
    "Reported outcomes and timepoints are presented without changing the Human-confirmed scientific analysis-group identities.",
    "Single-study or incompatible-timepoint sets are not pooled. Any pending numbers require independent source verification before quantitative reporting.")
}
web_b2_execute_synthesis <- function(con,settings,actor,project_id,version_id,values) {
  web_assert(is.list(values)&&length(values)==0L,
    "Unexpected synthesis execution input.","VALIDATION_FAILED")
  c<-web_b2_context(con,settings,actor,project_id,version_id)
  x<-c$packet
  web_assert(identical(x$stage,"EXTRACTION"),
    "Automatic synthesis planning is available during extraction.","USER_ACTION_REQUIRED")
  source<-web_b2_synthesis_source_digest(x)
  prior<-x$synthesis_execution
  if(is.list(prior)&&identical(prior$status,"AUTO_PREPARED")&&
     identical(prior$source_digest,source))
    return(list(project_id=project_id,version_id=version_id,already_executed=TRUE))
  report<-web_b2_build_synthesis_execution(x)
  report$actor<-actor
  report$at<-web_now()
  report$source_version<-version_id
  x$synthesis_execution<-report
  x$history<-c(x$history,list(list(event="AUTOMATED_SYNTHESIS_PREPARED",actor=actor,
    at=web_now(),source_version=version_id,source_digest=source,
    quantitative_candidates=report$counts$quantitative_candidates,
    numeric_values_auto_verified=FALSE)))
  root<-tempfile("synthesis-execution-",settings$data_root)
  dir.create(root)
  on.exit(unlink(root,recursive=TRUE))
  web_copy_version(con,settings,actor,project_id,version_id,root)
  web_b2_invalidate(root,"EXTRACTION")
  web_b2_write(root,x)
  web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"EXTRACTION")
}

web_b2_save_batch_outcomes <- function(con,settings,actor,project_id,version_id,values) {
  c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
  web_assert(is.null(x$result_inventory_selection),
    "Legacy template Outcome batch selection is disabled after study-specific inventory selection.",
    "USER_ACTION_REQUIRED")
  web_assert(identical(x$stage,"EXTRACTION"),"Review outcome choices during extraction.","USER_ACTION_REQUIRED")
  allowed_reports<-web_b2_final_active_reports(x,TRUE)
  chosen<-web_b2_selection_values(values,"outcome_row_ids_json")
  rows<-x$tables$Target_Outcome_Verification %||b2% list()
  active<-which(vapply(rows,function(r)
    as.character(r$screening_id %||b2% "") %in% allowed_reports,logical(1)))
  keys<-vapply(rows[active],function(r)as.character(r$.row_id %||b2% ""),character(1))
  web_assert(!anyNA(keys)&&all(nzchar(keys))&&!anyDuplicated(keys)&&
    all(chosen %in% keys),"Outcome selection contains an unknown or excluded study row.",
    "VALIDATION_FAILED")
  changed<-FALSE
  for(i in active) {
    row<-rows[[i]];desired<-if(row$.row_id %in% chosen)"Yes" else "No"
    if(!identical(as.character(row$human_include_in_planned_analysis %||b2% "No"),desired)) {
      row$human_include_in_planned_analysis<-desired
      row$.outcome_selected_by<-actor;row$.outcome_selected_at<-web_now()
      # Changing an analysis decision invalidates prior approval of that row.
      row$verification_status<-"Needs clarification";row$.verified_by<-NULL
      row$.verification_rationale<-NULL
      row$.actor<-actor
      rows[[i]]<-row;changed<-TRUE
    }
  }
  web_assert(changed,"The selected Outcome rows are already saved.","USER_ACTION_REQUIRED")
  x$tables$Target_Outcome_Verification<-rows
  x$history<-c(x$history,list(list(event="BATCH_OUTCOME_SELECTION",actor=actor,
    at=web_now(),source_version=version_id,selected_row_ids=as.list(chosen))))
  root<-tempfile("batch-outcome-selection-",settings$data_root)
  dir.create(root);on.exit(unlink(root,recursive=TRUE))
  web_copy_version(con,settings,actor,project_id,version_id,root)
  web_b2_invalidate(root,"EXTRACTION");web_b2_write(root,x)
  web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"EXTRACTION")
}
web_b2_edit <- function(con,settings,actor,project_id,version_id,table,row_id,values) {
  if(identical(table,"__FINAL_STUDY_SELECTION__") && identical(row_id,"__FINAL_STUDIES__"))
    return(web_b2_save_final_studies(con,settings,actor,project_id,version_id,values))
  if(identical(table,"__RESULT_INVENTORY_SELECTION__") && identical(row_id,"__RESULT_GROUPS__"))
    return(web_b2_save_result_inventory(con,settings,actor,project_id,version_id,values))
  if(identical(table,"__HUMAN_OUTCOME_HARMONIZATION__") && identical(row_id,"__HARMONIZATION__"))
    return(web_b2_save_human_harmonization(con,settings,actor,project_id,version_id,values))
  if(identical(table,"__SYNTHESIS_PLAN__") && identical(row_id,"__SYNTHESIS__"))
    return(web_b2_save_synthesis_plan(con,settings,actor,project_id,version_id,values))
  if(identical(table,"__SYNTHESIS_EXECUTION__") && identical(row_id,"__EXECUTE__"))
    return(web_b2_execute_synthesis(con,settings,actor,project_id,version_id,values))
  if(identical(table,"__NARRATIVE_DRAFT_ROUTE__") && identical(row_id,"__CONFIRM__"))
    return(web_b2_approve_narrative_draft(con,settings,actor,project_id,version_id,values))
  if(identical(table,"__NARRATIVE_SOURCE_REVIEW__"))
    return(web_b2_nr_save(con,settings,actor,project_id,version_id,row_id,values))
  if(identical(table,"__NARRATIVE_PDF_NUMERIC_AI__") && identical(row_id,"__EXTRACT_ALL__"))
    return(web_b2_nr_pdf_numbers_extract(con,settings,actor,project_id,version_id,values))
  if(identical(table,"__NARRATIVE_BATCH_REVIEW__") && identical(row_id,"__FINAL_VERIFY_ALL__"))
    return(web_b2_nr_batch_save(con,settings,actor,project_id,version_id,values))
  if(identical(table,"__NARRATIVE_BATCH_FREEZE__") && identical(row_id,"__FREEZE_AND_CONTINUE__"))
    return(web_b2_nr_batch_freeze(con,settings,actor,project_id,version_id,values))
  if(identical(table,"__OUTCOME_BATCH_SELECTION__") && identical(row_id,"__BATCH_OUTCOMES__"))
    return(web_b2_save_batch_outcomes(con,settings,actor,project_id,version_id,values))
  c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet;spec<-web_b2_edit_spec()[[table]]
  web_assert(!table %in% c("Reviewer_1","Reviewer_2"),
  "Title/abstract reviewers use AI Reviewer 1 and the selective Human Reviewer 2 Excel; on-page edits are disabled.","USER_ACTION_REQUIRED")
  if (identical(x$stage,"SCREENING") && length(x$tables$AI_Screening))
    x<-web_b2_bind_agent_reviewer1(x,c$version$root,c$version$manifest)
 web_assert(!is.null(spec)&&table %in% names(x$tables),"This review table is unavailable.")
 rows<-x$tables[[table]];hit<-which(vapply(rows,function(a)identical(a$.row_id,row_id),logical(1)))
 web_assert(length(hit)==1L,"This record does not belong to the selected review table/version.")
 i<-hit;row<-rows[[i]]
 if(table %in% c("Target_Study_Verification","Target_Outcome_Verification"))
   web_assert(as.character(row$screening_id %||b2% "") %in% web_b2_final_active_reports(x,TRUE),
     "This study is not in Final Study Selection.","USER_ACTION_REQUIRED")
 allowed<-spec$fields
 if(is.null(allowed))allowed<-unlist(x$editable[[table]],use.names=FALSE)
 # Golden R/25B Agent columns remain immutable. Only a NEW Human-created
 # outcome row can define its independent outcome_name for R/26 compatibility.
 if(identical(table,"Target_Outcome_Verification") &&
    identical(row$.source,"HUMAN ENTRY REQUIRED"))
   allowed<-unique(c(allowed,"outcome_name"))
 # RC28.19: web-created PDF outcomes may have no prior R/25 numerical source.
 # Human-editable overlay fields complete the canonical R/26 data contract,
 # including continuous variables; never edit immutable AI source fields.
 if(identical(table,"Target_Outcome_Verification") && isTRUE(row$.web_harmonized_result))
   allowed<-unique(c(allowed,"source_location","source_evidence","value_status",
     "human_verified_outcome_definition","human_verified_timepoint",
     "human_verified_data_type","human_verified_intervention_events",
     "human_verified_intervention_total","human_verified_comparator_events",
     "human_verified_comparator_total","human_verified_intervention_mean",
     "human_verified_intervention_sd","human_verified_intervention_n",
     "human_verified_comparator_mean","human_verified_comparator_sd",
     "human_verified_comparator_n","human_verified_unit",
     "human_verified_effect_measure","human_verified_effect_estimate",
     "human_verified_lower_ci","human_verified_upper_ci",
     "human_verified_adjusted","human_verified_adjustment_variables",
     "human_verified_source_location","human_outcome_notes"))
 web_assert(is.list(values)&&length(values)>0&&!is.null(names(values))&&!anyDuplicated(names(values))&&all(names(values)%in%allowed),"Only displayed editable review fields can be saved.")
 web_assert(all(vapply(values,function(v)is.character(v)&&length(v)==1L&&!is.na(v)&&nchar(v)<=20000L,logical(1))),"Use a single bounded text value per field.")
 if(table %in% c("Reviewer_1","Reviewer_2")) {
  other<-x$tables[[if(table=="Reviewer_1")"Reviewer_2" else "Reviewer_1"]][[i]]
  web_assert(is.null(other$.actor)||!identical(other$.actor,actor),"Independent reviewer decisions must come from different people.")
 }
 if(table=="Author_Final_Review"&&!is.null(values$retrieval_status)&&values$retrieval_status=="Available") {
  web_assert(!is.null(row$.pdf_sha256),"Upload and register the PDF before marking it available.")
  f<-web_inside(file.path(c$version$root,row$.pdf_relative),c$version$root)
  web_assert(identical(web_sha(f),row$.pdf_sha256),"The registered PDF failed checksum verification.")
 }
 before<-row
 for(n in names(values))row[[n]]<-values[[n]]
 if(table %in% c("Target_Study_Verification","Target_Outcome_Verification")) {
  if(is.null(row$.values))row$.values<-list()
  for(n in setdiff(names(values),c("source_location","source_evidence","raw_value","transformation_rule","value_status"))) {
   old<-before$.values[[n]]
   row$.values[[n]]<-list(source_report=row$screening_id,source_pdf_sha256=row$.pdf_sha256,
    reported_value=if(!is.null(old$reported_value))old$reported_value else if((row$value_status %||b2% "REPORTED") %in% c("DERIVED","CALCULATED","IMPUTED"))row$raw_value else values[[n]],
    prior_value=before[[n]],human_value=values[[n]],status=row$value_status %||b2% "REPORTED",
    location=row$source_location,evidence=row$source_evidence,raw_value=row$raw_value,transformation=row$transformation_rule,
    actor=actor,at=web_now(),source_version=version_id,verification="UNVERIFIED")
  }
 }
 row$.actor<-actor;row$.timestamp<-web_now();row$.source<-"HUMAN DECISION"
 for(n in intersect(names(row),c("reviewer","adjudicator","author_reviewer","verifier")))row[[n]]<-actor
 for(n in intersect(names(row),c("screened_at","resolved_at","author_reviewed_at","verified_at")))row[[n]]<-row$.timestamp
 # Edits always clear verification; separate explicit verification is required.
 if(table %in% c("Target_Study_Verification","Target_Outcome_Verification")){row$verification_status<-"Needs clarification";row$.verified_by<-NULL}
 if(identical(table,"Audit_Checks")){row$.verified_by<-NULL;row$verifier<-actor;row$verified_at<-web_now()}
 rows[[i]]<-row;x$tables[[table]]<-rows
 if(table %in% c("Signalling_Questions","Domain_Review")) {
  for(t in if(table=="Signalling_Questions")c("Domain_Review","Overall_Review") else "Overall_Review")
   for(j in seq_along(x$tables[[t]]))if(identical(x$tables[[t]][[j]]$screening_id,row$screening_id)) {
    x$tables[[t]][[j]]$.actor<-NULL
    for(k in intersect(names(x$tables[[t]][[j]]),c("author_domain_judgement","author_final_overall_judgement")))x$tables[[t]][[j]][[k]]<-""
   }
 }
 x$stage<-spec$stage;x<-web_b2_invalidate_packet(x)
 x$history<-c(x$history,list(list(event="HUMAN_EDIT",table=table,row_id=row_id,actor=actor,at=web_now(),source_version=version_id)))
 root<-tempfile("review-edit-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root)
 web_b2_invalidate(root,x$stage);web_b2_write(root,x);web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,x$stage)
}
web_b2_verify <- function(con,settings,actor,project_id,version_id,table,row_id,approved,rationale) {
 web_assert(isTRUE(approved)&&is.character(rationale)&&length(rationale)==1&&nzchar(trimws(rationale)),"Explicit confirmation and verification rationale are required.")
 c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
 web_assert(x$stage=="EXTRACTION"&&table %in% c("Target_Study_Verification","Target_Outcome_Verification"),"Only draft extraction values can be verified.")
 i<-which(vapply(x$tables[[table]],function(a)identical(a$.row_id,row_id),logical(1)));web_assert(length(i)==1,"Unknown extraction row.")
 row<-x$tables[[table]][[i]]
 web_assert(as.character(row$screening_id %||b2% "") %in% web_b2_final_active_reports(x,TRUE),
   "This study is not in Final Study Selection.","USER_ACTION_REQUIRED")
 web_assert(!is.null(row$.actor)&&row$.actor!=actor,"A second person must independently verify extracted values.")
 web_assert(nzchar(row$source_location %||b2% "")&&nzchar(row$source_evidence %||b2% ""),"Record source location and evidence before verification.")
 row$verification_status<-"Verified";row$verifier<-actor;row$verified_at<-web_now();row$.verified_by<-actor;row$.source<-"HUMAN VERIFIED";row$.verification_rationale<-rationale;
 for(k in names(row$.values)){row$.values[[k]]$verification<-"HUMAN VERIFIED";row$.values[[k]]$verifier<-actor;row$.values[[k]]$verified_at<-web_now()};x$tables[[table]][[i]]<-row
 root<-tempfile("verify-edit-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root);web_b2_write(root,x)
 web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"EXTRACTION")
}
`%||b2%`<-function(x,y)if(is.null(x)||!length(x)||is.na(x[1]))y else x
web_b2_upload <- function(con,settings,actor,project_id,version_id,row_id,upload,upload_root,
                           comparative_option="PASS",comparative_rationale="",comparative_approved=FALSE) {
 if(identical(row_id,"__SELECTIVE_SCREENING_XLSX_RC28__"))
  return(web_b2_import_selective_excel(con,settings,actor,project_id,version_id,upload,upload_root,
    comparative_option=comparative_option,comparative_rationale=comparative_rationale,
    comparative_approved=comparative_approved))
 c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
 web_assert(x$stage=="FULLTEXT","Upload full text while reviewing full-text eligibility.")
 i<-which(vapply(x$tables$Author_Final_Review,function(a)identical(a$.row_id,row_id),logical(1)));web_assert(length(i)==1,"Select a report belonging to this version.")
 web_assert(is.data.frame(upload)&&nrow(upload)==1&&all(c("name","datapath","size")%in%names(upload)),"Choose one PDF.")
 name<-upload$name[[1]];web_assert(grepl("^[A-Za-z0-9][A-Za-z0-9_. -]*[.]pdf$",name,ignore.case=TRUE)&&!grepl("..",name,fixed=TRUE),"Use a simple PDF filename without folders.")
 source<-web_inside(upload$datapath[[1]],upload_root);web_assert(!nzchar(Sys.readlink(upload$datapath[[1]])),"Symlink uploads are not accepted.")
 size<-file.info(source)$size;web_assert(is.finite(size)&&size>0&&size<=20*1024^2&&identical(as.numeric(upload$size[[1]]),as.numeric(size)),"The PDF must be nonempty and at most 20 MB.")
 f<-file(source,"rb");sig<-readBin(f,"raw",5);close(f);web_assert(identical(sig,charToRaw("%PDF-")),"The file does not have a valid PDF signature.")
 root<-tempfile("pdf-version-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root)
 row<-x$tables$Author_Final_Review[[i]];dest<-web_inside(file.path(root,row$.pdf_relative),root,exists=FALSE);dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE);web_assert(file.copy(source,dest,overwrite=TRUE),"PDF upload failed.")
 row$pdf_status<-"Available";row$retrieval_status<-"Available";row$pdf_path<-row$.pdf_relative;row$.pdf_sha256<-web_sha(dest);row$author_final_decision<-"";row$author_final_exclusion_reason<-"";row$.actor<-NULL;row$.source<-"SYSTEM OUTPUT";row$.uploaded_by<-actor;row$.uploaded_at<-web_now()
 x$tables$Author_Final_Review[[i]]<-row;x$tables$AI_Eligibility<-NULL;x<-web_b2_invalidate_packet(x);web_b2_invalidate(root,"FULLTEXT");web_b2_write(root,x)
 saved<-web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"FULLTEXT")
 web_audit(con,web_authorize(con,actor,project_id),"FULLTEXT_UPLOADED",saved$version_id,actor,list(report=row$screening_id,sha256=row$.pdf_sha256,source_version=version_id))
 saved
}
web_b2_view <- function(con,settings,actor,project_id,version_id) {
  c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
  # RC28.20.1: validate freshness against the FULL immutable source packet,
  # before the final-study UI projection removes excluded-study rows. The
  # projection is intentionally not a source hash input. No persisted state,
  # numeric verification or Human gate is changed by this view-only flag.
  x$.synthesis_execution_fresh <- is.list(x$synthesis_execution) &&
    identical(x$synthesis_execution$status,"AUTO_PREPARED") &&
    identical(x$synthesis_execution$source_digest,web_b2_synthesis_source_digest(x))
  # Source QA is computed BEFORE the view filters excluded-study rows.
  x$.narrative_analysis_assessment <- web_b2_narrative_assess(x)
  if(identical(x$stage,"SCREENING") && length(x$tables$AI_Screening)) {
    x<-web_b2_bind_agent_reviewer1(x,c$version$root,c$version$manifest)
    x$ai_screening_aggregate<-as.list(web_b2_ai_summary_counts(x$tables$AI_Screening))
  }
  # Final study selection is authoritative for all analysis-facing Web tables.
  # Eligibility history and original full-text review remain visible separately.
  if(!is.null(x$final_study_selection)) {
    analysis_tables<-c("Target_Study_Verification","Target_Outcome_Verification",
      "AI_outcome_data","AI_study_characteristics","AI_group_characteristics",
      "Verification_Plan","Audit_Checks","Included_Not_Targeted",
      "Advisory_Meta_Analysis_Sets","Advisory_Outcome_Harmonization","Advisory_Study_Roles")
    for(t in intersect(analysis_tables,names(x$tables))) {
      rows<-x$tables[[t]]
      if(!length(rows))next
      # Do not erase unscoped aggregate diagnostics: no report identity exists.
      # Raw evidence remains accessible in earlier immutable versions.
      if(all(vapply(rows,function(r)is.list(r)&&!is.null(r$screening_id),logical(1))))
        x$tables[[t]]<-web_b2_final_filter_rows(x,rows)
    }
  }
  # Reviewer 2 makes the final selective judgment; unlike double-blind review,
  # this workflow intentionally exposes AI recommendations and rationale.

 x
}

web_b2_invalidate <- function(root,stage) {
 cleanup<-list(SCREENING=c("data/processed/full_text","data/processed/extraction","data/processed/meta_analysis_ready","results/full_text","results/extraction","results/meta_analysis_ready","results/risk_of_bias","results/analysis_ready_evidence.json"),
 FULLTEXT=c("data/processed/extraction","data/processed/meta_analysis_ready","results/extraction","results/meta_analysis_ready","results/risk_of_bias","results/analysis_ready_evidence.json"),
 EXTRACTION=c("data/processed/meta_analysis_ready","results/meta_analysis_ready","results/risk_of_bias","results/analysis_ready_evidence.json"),
 ROB=c("results/risk_of_bias/rob_final_combined.csv","results/analysis_ready_evidence.json"))
 for(p in cleanup[[stage]])unlink(web_inside(file.path(root,p),root,exists=FALSE),recursive=TRUE)
}

web_b2_invalidate_packet <- function(x) {
 order<-c(SCREENING=1L,FULLTEXT=2L,INCLUDED=3L,EXTRACTION=4L,VERIFIED=5L,ROB=6L,READY=7L)
 specs<-web_b2_edit_spec();rank<-order[[x$stage]]
 for(t in names(x$tables)) {
  stage<-if(!is.null(specs[[t]]))specs[[t]]$stage else if(t %in% c("Study_Result_Manifest","AI_Risk_of_Bias"))"ROB" else if(t=="AI_Eligibility")"FULLTEXT" else if(t %in% c("AI_study_characteristics","AI_group_characteristics","AI_outcome_data"))"EXTRACTION" else NULL
  if(!is.null(stage)&&order[[stage]]>rank)x$tables[[t]]<-NULL
 }
 if(rank<=2L){x$included_reports<-NULL;x$editable<-NULL}
 x
}
# Create a genuinely BLANK, independent outcome/timepoint row.
# This works for both ordinary extraction and canonical R/25B targeted mode:
# the latter keeps Agent numeric columns READ-ONLY, so clearing only editable
# human_* fields would otherwise silently duplicate AI event counts/denominators.
# Carry report/PDF identity only; do not copy any source AI measurement, endpoint,
# effect, source citation or prior Human verdict into the new outcome row.
web_b2_blank_new_outcome_row <- function(source,editable_fields,new_id,new_outcome_id) {
 stopifnot(is.list(source),is.character(editable_fields),
   length(new_id)==1L,length(new_outcome_id)==1L)
 keep<-c("screening_id","study_id",".pdf_sha256",".pdf_relative","verification_tier")
 row<-source
 for(k in setdiff(names(row),keep))row[[k]]<-""
 row$.row_id<-new_id;row$outcome_row_id<-new_outcome_id
 row$outcome_name<-""
 row$.actor<-NULL;row$.verified_by<-NULL
 row$.values<-NULL;row$.human_review<-NULL
 row$verification_status<-"Needs clarification"
 row$human_include_in_planned_analysis<-"No"
 row$value_status<-"MISSING"
 row$verifier<-"";row$verified_at<-""
 row$.source<-"HUMAN ENTRY REQUIRED";row$.verification<-"UNVERIFIED"
 row$.timestamp<-NULL;row$.verification_rationale<-NULL
 row
}

web_b2_add_row <- function(con,settings,actor,project_id,version_id,row_id) {
 c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
 web_assert(x$stage=="EXTRACTION","Add outcome/timepoint rows during draft extraction.")
 rows<-x$tables$Target_Outcome_Verification;i<-which(vapply(rows,function(a)identical(a$.row_id,row_id),logical(1)));web_assert(length(i)==1L,"Select an included report's existing outcome row.")
 source_row<-rows[[i]]
 web_assert(as.character(source_row$screening_id %||b2% "") %in% web_b2_final_active_reports(x,TRUE),
   "This study is not in Final Study Selection.","USER_ACTION_REQUIRED")
 row<-web_b2_blank_new_outcome_row(source_row,
   as.character(unlist(x$editable$Target_Outcome_Verification,use.names=FALSE)),
   web_id(),paste0(source_row$screening_id,"_",web_id()))
 x$tables$Target_Outcome_Verification<-c(rows,list(row))
 root<-tempfile("outcome-revision-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root);web_b2_invalidate(root,"EXTRACTION");web_b2_write(root,x)
 web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"EXTRACTION")
}

# Reviewing a proposal never changes a scientific Human decision or verifies a value.
web_b2_review_proposal <- function(con,settings,actor,project_id,version_id,table,row_id,decision,rationale) {
 c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
 web_assert(startsWith(table,"AI_")&&table %in% names(x$tables),"Select an AI recommendation from this version.")
  stage<-if(table %in% c("AI_Screening","AI_Study_Design"))"SCREENING" else if(table=="AI_Eligibility")"FULLTEXT" else if(table=="AI_Risk_of_Bias")"ROB" else "EXTRACTION"
 web_assert(x$stage==stage,"Review proposals in their draft stage; create an evidence revision before revisiting finalized proposals.")
 web_assert(decision %in% c("ACCEPT_FOR_MANUAL_ENTRY","REJECT","UNCERTAIN")&&is.character(rationale)&&length(rationale)==1L&&!is.na(rationale)&&nzchar(trimws(rationale))&&nchar(rationale)<=20000,"Choose a proposal review and give a rationale.")
 i<-which(vapply(x$tables[[table]],function(a)identical(a$.row_id,row_id),logical(1)));web_assert(length(i)==1L,"This proposal is not in the selected version.")
 x$tables[[table]][[i]]$.human_review<-list(decision=decision,rationale=rationale,actor=actor,at=web_now(),version=version_id)
 root<-tempfile("proposal-review-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE))
 web_copy_version(con,settings,actor,project_id,version_id,root);web_b2_write(root,x)
 web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,x$stage)
}
web_b2_credential_name <- function(engine_root,operation) {
 if(operation=="recommend_risk_of_bias")return("OPENAI_API_KEY")
 yaml::read_yaml(file.path(engine_root,"config/agent_screening.yml"))$agent_screening$provider$api_key_env %||b2% "OPENAI_API_KEY"
}


# Safe, universal version revision for AI screening model/policy changes.
# Callable by the Web UI; it never edits an existing snapshot.
web_b2_revise_screening_model <- function(con,settings,actor,project_id,
                                           version_id,model,reasoning_effort="low") {
  context <- web_b2_context(con,settings,actor,project_id,version_id)
  web_assert(context$row$stage=="SCREENING",
    "Model revision is available only at the Screening stage.","USER_ACTION_REQUIRED")
  web_assert(is.character(model)&&length(model)==1L&&!is.na(model)&&
    grepl("^[A-Za-z0-9][A-Za-z0-9._-]{1,127}$",model),"Invalid model identifier.")
  web_assert(is.character(reasoning_effort) && length(reasoning_effort)==1L &&
    !is.na(reasoning_effort) &&
    reasoning_effort %in% c("none","minimal","low","medium","high","xhigh"),
    "Invalid reasoning effort.")
  dir <- tempfile("model-revision-",settings$data_root)
  dir.create(dir)
  on.exit(unlink(dir,recursive=TRUE),add=TRUE)
  web_copy_version(con,settings,actor,project_id,version_id,dir)
  path <- file.path(dir,"config/agent_screening.yml")
  legacy <- !file.exists(path)
  if (legacy) {
    # Pre-policy legacy versions: authenticate the policy against the release
    # that actually processed the paper; never infer a historic GPT model.
    prior <- web_query(con,
      "SELECT id,engine_id,diagnostic_locator FROM jobs WHERE project_id=?p AND version_id=?v AND operation='recommend_screening' AND state IN ('USER_ACTION_REQUIRED','FAILED','COMPLETE') ORDER BY created_at DESC",
      p=project_id,v=version_id)
    if (nrow(prior)) {
      # A research project may contain multiple historic screening attempts,
      # including attempts with different provider configurations. The model
      # policy must come from the authenticated job whose checkpoint covers
      # all previously valid IDs, not from an arbitrary job or a requirement
      # that every old job have exactly the same policy.
      prior_checkpoints <- list()
      for (i in seq_len(nrow(prior))) {
        loc <- as.character(prior$diagnostic_locator[[i]])
        if (is.na(loc) || !nzchar(loc)) next
        normalized_loc <- gsub("\\\\", "/", loc)
        web_assert(startsWith(normalized_loc,"jobs/") &&
          !grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))",normalized_loc),
          "Unsafe historical screening job locator.","USER_ACTION_REQUIRED")
        candidate_work <- file.path(settings$data_root,loc,"project")
        if (!dir.exists(candidate_work)) next
        old_work <- web_inside(candidate_work,settings$data_root)
        hits <- list.files(old_work,pattern="^agent_screening_checkpoint[.]csv$",
                           recursive=TRUE,full.names=TRUE)
        web_assert(length(hits)<=1L,"Ambiguous historical screening checkpoint.",
          "USER_ACTION_REQUIRED")
        if (length(hits)!=1L) next
        rows <- tryCatch(utils::read.csv(hits[[1L]],check.names=FALSE,
                            colClasses="character"),error=function(e) NULL)
        if (is.null(rows) || !all(c("screening_id","validation_status") %in% names(rows))) next
        good <- !is.na(rows$screening_id) & nzchar(trimws(rows$screening_id)) &
                !is.na(rows$validation_status) & rows$validation_status=="valid"
        good[is.na(good)] <- FALSE
        valid_rows <- rows[good,,drop=FALSE]
        if (!nrow(valid_rows)) next
        ids <- as.character(valid_rows$screening_id)
        web_assert(!anyDuplicated(ids),"Duplicate valid historical screening IDs.",
          "USER_ACTION_REQUIRED")
        # Even an unauthenticated checkpoint contributes to the coverage
        # fence: never silently drop historic valid IDs while picking policy.
        source_candidate <- file.path(old_work,"config/agent_screening.yml")
        authenticated_policy <- NULL
        if (file.exists(source_candidate)) {
          source <- web_inside(source_candidate,old_work)
          release <- web_query(con,
            "SELECT manifest_json FROM engine_releases WHERE id=?id",id=prior$engine_id[[i]])
          if (nrow(release)==1L) {
            hashes <- web_parse(as.character(release$manifest_json[[1L]]))
            expected <- hashes[["config/agent_screening.yml"]]
            if (!is.null(expected) &&
                identical(web_sha(source),as.character(expected)))
              authenticated_policy <- source
          }
        }
        prior_checkpoints[[length(prior_checkpoints)+1L]] <- list(
          ids=ids, checkpoint_sha256=web_sha(hits[[1L]]),
          source_job_id=as.character(prior$id[[i]]),
          policy=authenticated_policy)
      }
      certified <- Filter(function(x) !is.null(x$policy),prior_checkpoints)
      web_assert(length(certified)>0L,
        "No authenticated historical checkpoint policy is available.",
        "USER_ACTION_REQUIRED")
      sizes <- vapply(certified,function(x) length(x$ids),integer(1))
      winners <- which(sizes==max(sizes))
      # Equal-sized maximal candidates must be byte-identical; never pick an
      # arbitrary divergent set of historical AI recommendations.
      web_assert(length(unique(vapply(certified[winners],
        function(x) x$checkpoint_sha256,character(1))))==1L,
        "Competing maximal screening checkpoints disagree.","USER_ACTION_REQUIRED")
      chosen <- certified[[winners[[1L]]]]
      every_id <- unique(unlist(lapply(prior_checkpoints,
        function(x) x$ids),use.names=FALSE))
      web_assert(!length(setdiff(every_id,chosen$ids)),
        "An authenticated checkpoint does not cover every historic valid screening ID.",
        "USER_ACTION_REQUIRED")
      web_assert(isTRUE(file.copy(chosen$policy,path,overwrite=FALSE)),
        "Could not preserve the historical model policy.","SYSTEM_FAILED")
      policy_source_job_id <- chosen$source_job_id
      policy_source_checkpoint_sha256 <- chosen$checkpoint_sha256
      policy_source_valid_rows <- as.integer(length(chosen$ids))
    } else {
      # No historic screening decisions to protect: use the current approved
      # default, but still save it into a NEW immutable project version.
      default <- file.path(settings$engine_root,"config/agent_screening.yml")
      web_assert(file.exists(default) && isTRUE(file.copy(default,path,overwrite=FALSE)),
        "Could not prepare the default screening model policy.","SYSTEM_FAILED")
    }
  }
  cfg <- yaml::read_yaml(path)
  web_assert(is.list(cfg$agent_screening$provider),"Invalid screening model policy.")
  old_model <- as.character(cfg$agent_screening$provider$model)
  old_effort <- as.character(cfg$agent_screening$provider$reasoning_effort %||b2% "")
  web_assert(legacy || !identical(old_model,model) ||
    !identical(old_effort,reasoning_effort),
    "The requested model policy already matches the selected version.")
  web_assert(isTRUE(cfg$agent_screening$processing$resume_from_checkpoint) &&
    !isTRUE(cfg$agent_screening$processing$overwrite_existing_agent_decisions),
    "The source screening policy does not allow checkpoint reuse.","USER_ACTION_REQUIRED")
  cfg$agent_screening$provider$model <- model
  cfg$agent_screening$provider$reasoning_effort <- reasoning_effort
  # A model revision inherits only the official rate card for the selected
  # default model. This is cost telemetry, not a condition for API execution.
  default_path <- file.path(settings$engine_root,"config/agent_screening.yml")
  default_policy <- tryCatch(yaml::read_yaml(default_path),error=function(e) NULL)
  if (is.list(default_policy$agent_screening) &&
      identical(as.character(default_policy$agent_screening$provider$model),model))
    cfg$agent_screening$cost_optimization <- default_policy$agent_screening$cost_optimization
  yaml::write_yaml(cfg,path)
  saved <- web_b2_save_snapshot(con,settings,actor,project_id,version_id,dir,"SCREENING")
  web_audit(con,web_authorize(con,actor,project_id),"SCREENING_MODEL_POLICY_REVISED",
    saved$version_id,actor,list(parent_version=version_id,prior_model=old_model,
      target_model=model,target_reasoning=reasoning_effort,
      historic_policy_source_job=if(exists("policy_source_job_id",inherits=FALSE))
        policy_source_job_id else NULL,
      historic_checkpoint_sha256=if(exists("policy_source_checkpoint_sha256",inherits=FALSE))
        policy_source_checkpoint_sha256 else NULL,
      historic_checkpoint_valid_rows=if(exists("policy_source_valid_rows",inherits=FALSE))
        policy_source_valid_rows else NULL,
      historic_policy_selection=if(legacy) "MAX_VERIFIED_COVERING_CHECKPOINT" else "VERSION_PINNED"))
  saved
}


# Before queuing, recover a policy-less saved evidence version without any user
# file operations. The source snapshot remains immutable; only the new version
# carries the current approved default policy. This function never calls an API.
web_b2_auto_policy_version <- function(con,settings,actor,project_id,version_id) {
  context <- web_b2_context(con,settings,actor,project_id,version_id)
  if (context$row$stage!="SCREENING") return(version_id)
  original <- context$version$manifest
  policy <- "config/agent_screening.yml"
  if (policy %in% names(original)) return(version_id)
  running <- web_query(con,
    "SELECT id FROM jobs WHERE project_id=?p AND state IN ('RUNNING','QUEUED') LIMIT 1",
    p=project_id)
  web_assert(!nrow(running),"Wait for the active project operation before switching the model.","USER_ACTION_REQUIRED")
  default_path <- file.path(settings$engine_root,policy)
  web_assert(file.exists(default_path),"The approved screening policy is unavailable.","SYSTEM_FAILED")
  default_cfg <- yaml::read_yaml(default_path)
  model <- as.character(default_cfg$agent_screening$provider$model)
  effort <- as.character(default_cfg$agent_screening$provider$reasoning_effort %||b2% "low")
  web_assert(length(model)==1L && nzchar(model) && length(effort)==1L && nzchar(effort),
    "The approved screening model policy is invalid.","SYSTEM_FAILED")
  # Reuse a previous auto-generated child version when it has exactly the
  # source manifest plus one model policy; no proliferation on retry.
  children <- web_query(con,
    "SELECT version_id FROM evidence_versions WHERE project_id=?p AND parent_version_id=?v AND stage='SCREENING' ORDER BY created_at DESC",
    p=project_id,v=version_id)
  if (nrow(children)) for (vid in as.character(children$version_id)) {
    child <- tryCatch(web_version_root(con,settings,actor,project_id,vid),error=function(e) NULL)
    if (is.null(child)) next
    m <- child$manifest
    if (!setequal(names(m),c(names(original),policy)) ||
        any(vapply(names(original),function(n) !identical(as.character(original[[n]]),as.character(m[[n]])),logical(1)))) next
    candidate <- file.path(child$root,policy)
    if (!file.exists(candidate) || !identical(web_sha(candidate),as.character(m[[policy]]))) next
    config <- tryCatch(yaml::read_yaml(candidate),error=function(e) NULL)
    if (is.null(config) ||
        !identical(as.character(config$agent_screening$provider$model),model) ||
        !identical(as.character(config$agent_screening$provider$reasoning_effort %||b2% ""),effort)) next
    return(vid)
  }
  saved <- web_b2_revise_screening_model(con,settings,actor,project_id,
                                         version_id,model,effort)
  as.character(saved$version_id)
}
