# Canonical publication functions run in the existing isolated worker subprocess.
web_c2_canonical <- function(request,config,paths,root,input_manifest) {
 x<-web_c2_read(root);engine<-request$engine_root
 web_assert(web_c2_hash(x$source)==x$source_sha256&&web_c2_hash(x$source$references)==x$reference_sha256,"Manuscript source checksum mismatch.")
 for(a in x$source$artifacts)web_assert(web_sha(web_inside(file.path(root,a$path),root))==a$sha256,"Selected artifact checksum mismatch.")
 out<-file.path(root,"results/web_manuscript");unlink(out,recursive=TRUE);dir.create(out,recursive=TRUE)
 result<-list(version_id=request$version_id,manuscript_id=x$manuscript_id,source_sha256=x$source_sha256,reference_sha256=x$reference_sha256,evidence_version=x$evidence_version,route=x$source$route %||b2% "QUANTITATIVE",source_digest=x$source$narrative$source_digest %||b2% "",analyses=as.list(names(x$source$analyses)),artifacts=x$source$artifacts,content_sha256=web_c2_hash(x$sections),job_id=request$job_id,engine=request$engine_version,created_at=web_now(),generator_sha256=web_sha(file.path(engine,"web/adapter/manuscript.R")))
 if(request$action=="generate_manuscript_section") {
  web_assert(nzchar(Sys.getenv("OPENAI_API_KEY")),"AI unavailable; use Human entry.","PROVIDER_UNAVAILABLE")
  e<-web_c2_defs(engine,39);e$model_name<-x$generation$model;e$openai_api_key<-Sys.getenv("OPENAI_API_KEY")
  # Canonical API helper writes raw responses only to private temporary space, destroyed on exit.
  private<-tempfile("manuscript-provider-");dir.create(private,mode="0700");on.exit(unlink(private,recursive=TRUE),add=TRUE);e$raw_root<-private
  k<-x$generation$section
  bounded<-list(section=k,title=x$spec$title,language=x$spec$language)
  if(k %in% c("Introduction","Discussion")) {
    bounded$references<-lapply(x$source$references$records,function(r) {
      v<-r[intersect(names(r),c("pmid","title","abstract","year","publication_types"))]
      if(is.character(v$abstract) && length(v$abstract)==1L)
        v$abstract<-substr(v$abstract,1L,1800L)
      v
    })
    bounded$minimum_section_unique_references<-x$source$reference_policy[[tolower(k)]]
    bounded$minimum_manuscript_unique_references<-x$source$reference_policy$total
    # Included study PMIDs cannot be used as independent external comparators
    # to infer any additional study findings absent from verified source rows.
    bounded$current_review_reference_pmids<-x$source$reference_discovery$protected_pmids %||b2% list()
  }
  if(k %in% c("Abstract","Methods","Results","Discussion","Limitations","Conclusions"))bounded$facts<-x$source$facts
  if(k=="Discussion") {
    if(identical(x$source$route,"NARRATIVE")) {
      # These rows were already checked against registered PDFs and Human
      # outcome/timepoint approvals. Never transform into pooled statistics.
      bounded$current_review_results<-lapply(x$source$narrative$rows,function(r)
        r[intersect(names(r),c("study_label","original_outcome","common_variable",
          "reported_timepoint","analysis_timepoint","decision","fact_id","numeric_summary"))])
      bounded$synthesis_type<-"NARRATIVE_ONLY_NO_POOLED_EFFECTS"
      bounded$rob_pending<-isTRUE(x$source$narrative$unresolved_rob)
    } else {
      bounded$current_review_results<-lapply(x$source$analyses,function(a)
        list(role=a$plan$role,outcome=a$plan$outcome,
          timepoint=a$plan$timepoint,summary=a$result$summary))
    }
  }
  if(k=="Methods")bounded$methods<-if(identical(x$source$route,"NARRATIVE"))
    list(route="NARRATIVE",pooled_effects=FALSE,
      human_verified_pdf_results=length(x$source$narrative$rows)) else
    lapply(x$source$analyses,function(a)list(role=a$plan$role,methods=a$plan$methods))
  web_assert(nchar(as.character(web_json(bounded)))<=100000,
    "Source package exceeds the bounded drafting budget. Use Human drafting.","USER_ACTION_REQUIRED")
  general_prompt<-paste(
    "Draft only the requested section using the supplied verified data.",
    "Never invent scientific results, authors, references, sample sizes, methods, dates or calculations.",
    "All numeric findings of the current review must use supplied {{FACT:key}} tokens.",
    "Use exact [[PMID:id]] tokens ONLY for provided PubMed-verified citations.",
    "Repeat the same PMID in Introduction and Discussion whenever it supports separate claims;",
    "do not count repeated appearances as additional unique references.",
    "Do not use current included-study citations to import extra outcomes beyond the verified current-review results.",
    "Cite each literature-dependent paragraph with the appropriate supporting record, not a random paper.",
    "Do not use unverified publication abstract conclusions as proof of current-review outcomes.",
    "Retain design-specific limits and avoid causality or benefit claims without explicit evidence.",
    "Preserve Human RoB requirements, particularly any unresolved RoB status.",
    "Return scholarly prose without a reference list or fabricated citation tokens.",
    "The author will review/approve all text and every citation.")
  if(k=="Discussion")general_prompt<-paste(general_prompt,
    "Write a substantive, appropriately detailed scientific Discussion tailored to this review type.",
    "Address principal findings, agreement/disagreement with previous studies and reviews,",
    "plausible design-appropriate explanations, methodological heterogeneity where supported,",
    "risk of bias and uncertainty, limitations, practical implications and research gaps.",
    "Contrast contradictory studies explicitly with source-appropriate citations rather than adding filler.",
    "Do not force intervention or meta-analytic claims on a narrative-only review.",
    "Use enough distinct verified PMID records to support an intellectually deep Discussion;",
    "the authoritative minimum is supplied in minimum_section_unique_references.",
    "If the supplied reference library cannot support a proposed claim, omit it.")
  if(k=="Introduction")general_prompt<-paste(general_prompt,
    "Write a focused scholarly Introduction covering background, key prior evidence,",
    "unresolved gap and precise research objective, with adequate unique verified citations.",
    "Do not import the current review results into the Introduction.")
  budget<-if(k=="Discussion")8500L else if(k=="Introduction")6000L else 4000L
  text<-e$call_openai(general_prompt,as.character(web_json(bounded)),"section",max_output_tokens=budget)
  result$text<-web_c2_text(text,engine);result$section<-k;result$generation<-c(x$generation,list(at=web_now()))
 } else {
  narrative<-identical(x$source$route,"NARRATIVE")
  production<-identical(request$action,"generate_final_manuscript")
  publication_assets<-NULL
  if(production) {
    # 9C/9D: Verify PRISMA source completeness BEFORE any provider calls.
    web_c2_8g_progress_write(request$job_id,"CHECKING_PUBLICATION_SOURCES",
      "Checking same-review PRISMA and tables; no AI calls before source preflight")
    # 9BCD.1: source validation and asset creation now have truthful order.
    # Fail closed on registered CSV or checksum gaps before R/34/R/35 and AI.
    web_c2_9cd_source_preflight(root,x)
    web_c2_8g_progress_write(request$job_id,"BUILDING_PUBLICATION_ASSETS",
      "Source preflight passed; building canonical PRISMA, tables and figures")
    publication_assets<-web_c2_9cd_prepare(root,engine,out,x)
    on.exit(unlink(publication_assets$stage,recursive=TRUE,force=TRUE),add=TRUE)
    web_c2_9cd_require_prisma(publication_assets)
    if(!narrative)publication_assets<-web_c2_9cd_quant_assets(
      publication_assets,x,root,engine,out)
    rendered<-web_c2_auto_draft(x,engine,out,job_id=request$job_id,
      scientific_library=request$engine_library %||b2% "",
      publication_assets=publication_assets)
    result$production_qc<-rendered$qc
  } else rendered<-web_c2_render(x,engine,strict=!narrative)
  if(!production)web_c2_text(rendered$text,engine)
  web_no_secrets(rendered$text,engine)
  if(production)web_c2_8g_progress_write(request$job_id,"CREATING_DOCX",
    "Citation coverage and numeric checks passed. Creating editable Word document")
  e<-web_c2_defs(engine,41)
  needed<-c("officer","flextable");web_assert(all(vapply(needed,requireNamespace,logical(1),quietly=TRUE)),"Editable Word dependencies unavailable.","DEPENDENCY_FAILED")
  doc<-officer::read_docx();styles<-e$available_paragraph_styles(doc);e$style_map<-list(title=e$pick_paragraph_style(styles,c("Title","graphic title","heading 1")),heading1=e$pick_paragraph_style(styles,c("heading 1")),heading2=e$pick_paragraph_style(styles,c("heading 2")),heading3=e$pick_paragraph_style(styles,c("heading 3")),normal=e$pick_paragraph_style(styles,"Normal"))
  if(production)doc<-officer::body_add_par(doc,
    "AUTO-GENERATED AUTHOR-EDITABLE DRAFT — source/citation preflight passed, but standalone R/40 final release QC and author review are pending.",style="Normal")
  else if(narrative)doc<-officer::body_add_par(doc,
    "WORKING DRAFT — NOT FOR SUBMISSION; Human final review and RoB requirements remain.",style="Normal")
  # 9BCD.5: The body contains only cross-references to Tables/Figures.
  # Actual assets are appended at the very end, AFTER References.
  in_abstract<-FALSE
  for(block in e$parse_markdown_blocks(rendered$text)) {
    if(identical(block$type,"heading")) {
      heading<-trimws(as.character(block$text))
      if(identical(heading,"Abstract"))in_abstract<-TRUE
      if(identical(heading,"Keywords"))in_abstract<-FALSE
    }
    value<-trimws(as.character(block$text %||b2% ""))
    if(production && in_abstract && !identical(block$type,"heading") &&
       grepl("^(Background|Methods|Results|Conclusions|Conclusion|PROSPERO):[[:space:]]*",value)) {
      label<-sub("^([^:]+):.*$","\\1",value)
      detail<-sub("^[^:]+:[[:space:]]*","",value)
      doc<-officer::body_add_fpar(doc,officer::fpar(
        officer::ftext(paste0(label,": "),prop=officer::fp_text(bold=TRUE)),
        officer::ftext(detail)),style=e$style_map$normal)
    } else doc<-e$add_manuscript_block(doc,block)
  }
  if(production)
    doc<-web_c2_9e_append_endmatter(doc,publication_assets,e)
  # Preserve frozen scientific outputs; no recalculation or image manipulation.
  dest<-file.path(out,"manuscript.docx");print(doc,target=dest)
  if(production)web_c2_9e_format_docx(dest)
  web_no_file_secrets(dest,engine)
  if(production) {
    qc_doc<-web_c2_9cd_docx_integrity(dest,min_figures=1L,
      min_tables=2L)
    result$production_qc$publication_9cd<-c(qc_doc,list(
      prisma_status=publication_assets$prisma_status,
      r34_status=publication_assets$r34_status,
      source_manifest_sha256=publication_assets$source_manifest_sha256,
      included_studies=publication_assets$studies_included,
      limitations=as.list(publication_assets$messages),
      author_scientific_review="REQUIRED"))
  }
  result$docx_sha256<-web_sha(dest)
  result$canonical_generator_sha256<-web_sha(list.files(file.path(engine,"R"),pattern="^41_",full.names=TRUE))
  result$precheck<-if(production)rendered$qc else web_c2_check(x,engine)
  writeLines(rendered$text,file.path(out,"manuscript.md"))
  jsonlite::write_json(rendered$citations,file.path(out,"citation_map.json"),pretty=TRUE)
  if(production)jsonlite::write_json(result$production_qc,file.path(out,"production_qc.json"),
    auto_unbox=TRUE,pretty=TRUE,null="null")
  if(production) {
    # The frozen manuscript source is unchanged. Newly discovered PubMed
    # records and their query provenance accompany the versioned result.
    jsonlite::write_json(rendered$verification,
      file.path(out,"verified_reference_expansion.json"),
      auto_unbox=TRUE,pretty=TRUE,null="null",digits=NA)
    result$verified_literature_sha256<-web_sha(file.path(out,
      "verified_reference_expansion.json"))
    web_c2_8g_progress_write(request$job_id,"COMPLETE",
      "Editable Word generated; source and citation provenance saved")
  }
 }
 jsonlite::write_json(result,file.path(out,"result.json"),pretty=TRUE,auto_unbox=TRUE,null="null",digits=NA)
 outputs<-list.files(out,full.names=TRUE)
 web_result(request$action,request$project_id,request$job_id,"COMPLETE",
  "Author-editable manuscript generated. Automated evidence/citation checks passed; final author review and canonical R/40 release acceptance remain separate.",request$engine_version,artifacts=lapply(outputs,function(p)list(path=p,logical_name=basename(p),type="manuscript")),provenance=list(version_id=request$version_id,source_sha256=x$source_sha256,reference_sha256=x$reference_sha256,R=R.version.string))
}
