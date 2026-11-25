source(file.path(web_root,"web/ui/narrative_source_review.R"))
source(file.path(web_root,"web/ui/narrative_batch_review.R"))
# RC28.21 — Version-bound study-to-analysis routing, Web UI only.
# Pure helper: no DB writes, no AI calls, no mutation of canonical R.
srma_ui_analysis_status <- function(x) {
  `%||%` <- function(a,b) if(is.null(a)||!length(a)||is.na(a[[1L]])) b else a
  pending <- function(message)list(ready=FALSE,route="PENDING",message=message,
    studies=0L,variables=0L,sets=0L,quantitative_candidates=0L,reconcile=0L)
  if(!is.list(x))
    return(pending("Load the latest saved Evidence Version under Studies."))
  if(!identical(x$final_study_selection$status,"HUMAN_CONFIRMED"))
    return(pending("Complete Final Study Selection in Studies first."))
  if(!identical(x$result_inventory_selection$status,"HUMAN_SELECTED"))
    return(pending("Save the selected PDF-reported outcomes in Studies first."))
  if(!identical(x$human_outcome_harmonization$status,"HUMAN_CONFIRMED"))
    return(pending("Save the common outcome harmonization in Studies first."))
  if(!identical(x$synthesis_plan$status,"HUMAN_CONFIRMED"))
    return(pending("Save the synthesis plan in Studies first."))
  run <- x$synthesis_execution
  if(!is.list(run) || !identical(run$status,"AUTO_PREPARED") ||
     !isTRUE(x$.synthesis_execution_fresh))
    return(pending("Prepare or refresh the source-bound synthesis outputs in Studies before continuing."))
  decisions <- x$synthesis_plan$decisions
  if(!is.list(decisions)||!length(decisions))
    return(pending("No approved synthesis decisions were found."))
  modes <- vapply(decisions,function(d)as.character(d$mode %||% ""),character(1))
  if(any(!modes %in% c("NARRATIVE","QUANTITATIVE_CANDIDATE","RECONCILE")))
    return(pending("Unrecognized synthesis route. Review the saved synthesis plan."))
  rec <- sum(modes=="RECONCILE")
  nquant <- sum(modes=="QUANTITATIVE_CANDIDATE")
  studies <- length(unique(as.character(unlist(x$final_study_selection$study_ids,use.names=FALSE))))
  variables <- suppressWarnings(as.integer(run$counts$variables %||% 0L))
  sets <- length(modes)
  if(rec) return(list(ready=FALSE,route="RECONCILE",
    message=paste0(rec," analysis set(s) need harmonization/reconciliation before Analysis."),
    studies=studies,variables=variables,sets=sets,quantitative_candidates=nquant,reconcile=rec))
  if(nquant==0L) return(list(ready=TRUE,route="NARRATIVE_ONLY",
    message=paste0("No compatible multi-study quantitative sets in the saved plan (",
      sets," timepoint-specific sets). Structured Narrative Synthesis is the appropriate Analysis route. ",
      "No pooled estimate will be calculated. Source-based numerical claims still require verification."),
    studies=studies,variables=variables,sets=sets,quantitative_candidates=0L,reconcile=0L))
  list(ready=TRUE,route=if(nquant==sets)"QUANTITATIVE_CANDIDATES" else "MIXED",
    message=paste0(nquant," quantitative candidate set(s) identified. ",
      "Canonical R/26 human numeric gate and R/27 scientific analysis remain required; ",
      "unpooled sets continue as Structured Narrative Synthesis."),
    studies=studies,variables=variables,sets=sets,quantitative_candidates=nquant,reconcile=0L)
}

# Render only source-bound status. The primary manuscript integration is a
# later Web step; this view never claims that R/26 or R/27 was executed.
srma_ui_analysis_overview <- function(x) {
  z <- srma_ui_analysis_status(x)
  if(!isTRUE(z$ready)) return(shiny::tags$div(
    style="padding:18px;border:1px solid #e2e8f0;border-radius:9px;",
    shiny::h3("Synthesis Overview"),shiny::p(z$message),
    shiny::p("Return to Studies > Screening & Evidence to complete the outstanding step.")))
  narrative <- identical(z$route,"NARRATIVE_ONLY")
  shiny::tags$div(style="max-width:1120px;",
    shiny::h3("Analysis — automatic synthesis route"),
    shiny::tags$div(style="border-left:4px solid #2474a8;background:#f2f8fc;padding:15px;margin:12px 0;border-radius:6px;",
      shiny::tags$strong(if(narrative)"Structured Narrative Synthesis — selected automatically" else
        "Quantitative candidates identified — canonical verification required"),
      shiny::p(style="margin:8px 0 0;",z$message)),
    shiny::tags$p(paste(z$studies,"selected studies ·",z$variables,
      "harmonized outcome variables ·",z$sets,"timepoint-specific analysis sets ·",
      z$quantitative_candidates,"quantitative candidate sets")),
    shiny::tags$h4("Source-indexed outputs"),
    shiny::tags$p("The evidence matrix and narrative draft retain the original paper outcomes, timepoints and source locations. ",
      "Unverified numerical findings must not be presented as established results."),
    shiny::downloadButton("srma_analysis_matrix_csv","Download evidence matrix (CSV)"),
    shiny::downloadButton("srma_analysis_draft_md","Download narrative draft (MD)"),
    if(narrative) shiny::uiOutput("srma_narrative_analysis_panel"),
    shiny::tags$hr(),
    shiny::tags$h4("Next step"),
    shiny::tags$p(if(narrative)
      "Complete source-based result checks and risk-of-bias synthesis, then prepare a narrative-only systematic review manuscript. The existing R/26–R/27 pooled-analysis branch is skipped, not treated as failed."
      else "Verify quantitative source values and complete the existing Human Gate before submitting eligible analysis sets to the Meta-analysis screen. Narrative-only sets remain separate."),
    shiny::tags$p(style="color:#64748b;font-size:.92em;",
      "This overview routes the saved analysis plan; it does not automatically claim numerical verification or manuscript readiness."))
}


# RC28.22: source-indexed Narrative Synthesis. Never promote an unverified
# PDF number or missing ROB judgement into a verified manuscript claim.
srma_ui_narrative_analysis_panel <- function(x) {
  a <- if(is.list(x))x$.narrative_analysis_assessment else NULL
  if(!is.list(a) || identical(a$status,"PENDING"))
    return(shiny::div(shiny::h4("Narrative results not ready"),
      shiny::p(if(is.list(a))a$message else "Load the saved synthesis Evidence Version.")))
  frozen <- identical(web_b2_nr_one(x$narrative_source_finalization$status),"FINALIZED")
  ready <- as.integer(a$blocking)==0L && as.integer(a$numeric_unverified)==0L &&
    is.list(x$narrative_batch_verification) &&
    identical(x$narrative_batch_verification$source_digest,x$synthesis_execution$source_digest)
  shiny::div(style="max-width:1150px;",
    if(as.integer(a$blocking)>0L)shiny::p(style="color:#b91c1c;",
      "PDF source integrity needs attention. The toolkit cannot confirm results until the linked source is fixed."),
    srma_ui_nr_batch_editor(x),
    shiny::div(style="margin-top:16px;display:flex;align-items:center;gap:12px;flex-wrap:wrap;",
      if(ready && !frozen)shiny::actionButton("srma_batch_continue",
        "Finalize Analysis & Continue to Manuscript",class="btn-primary"),
      if(frozen)shiny::p(style="font-weight:700;color:#166534;",
        "Analysis finalized for this Evidence Version."),
      if(!ready)shiny::p(style="font-size:14px;color:#64748b;",
        "Complete Final Verification to unlock the next stage.")),
    if(!isTRUE(a$rob_complete))shiny::p(style="font-size:13px;color:#64748b;",
      "Risk-of-bias judgments remain separate and must be completed before the final publication manuscript."),
    shiny::tags$details(style="margin-top:12px;",
      shiny::tags$summary("Review draft / downloads"),
      shiny::downloadButton("srma_narrative_draft_md","Download structured narrative draft (MD)"),
      shiny::p(style="font-size:12px;color:#64748b;",
        "Draft output is not the final canonical manuscript_author_editable.docx."))
  )
}

srma_ui_narrative_manuscript <- function(x) {
  a <- if (is.list(x)) x$.narrative_analysis_assessment else NULL
  if(!is.list(a) || identical(a$status,"PENDING")) return(shiny::div(
    shiny::h3("Narrative manuscript working draft"),
    shiny::p("No current narrative source review is available. Return to Analysis > Synthesis Overview."),
    shiny::actionButton("srma_narrative_back_to_analysis","Back to Analysis")))
  shiny::div(style="max-width:1120px;",
    shiny::h3("Narrative systematic review — manuscript working draft"),
    shiny::p(style="font-weight:bold;color:#92400e;",
      "WORKING DRAFT — not a completed R/38–R/41 canonical manuscript or an approved final result."),
    shiny::p(paste0("Source integrity: ", if(as.integer(a$blocking)==0L) "PASS" else "BLOCKED",
      " · Numerical results awaiting source QA: ", a$numeric_unverified,
      " · Risk-of-bias assessments complete: ", if(isTRUE(a$rob_complete)) "Yes" else "No")),
    shiny::p(if(isTRUE(a$approval_fresh))
      "Draft handoff is saved to this Evidence Version."
      else "Draft is reproducible from saved source data; optionally save the handoff in Analysis."),
    shiny::div(style="display:flex;flex-wrap:wrap;gap:10px;margin:12px 0;",
      shiny::downloadButton("srma_narrative_draft_md_2","Download manuscript working draft (MD)"),
      shiny::actionButton("srma_narrative_back_to_analysis","Back to Analysis")),
    shiny::tags$details(
      shiny::tags$summary("Preview full source-indexed manuscript working draft"),
      shiny::verbatimTextOutput("srma_narrative_manuscript_preview"))
  )
}
