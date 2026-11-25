# RC28.22.3 — concise, all-at-once narrative source review.
# Only selected final studies and synthesis results appear. No exception queue.
srma_ui_nr_batch_editor <- function(x) {
  if(!is.list(x) || !is.list(x$synthesis_execution$rows))return(NULL)
  idx<-web_b2_nr_batch_index(x)
  if(!length(idx))return(shiny::p("No selected study results are ready."))
  get<-function(z)web_b2_nr_one(z)
  paper_ids<-unique(vapply(idx,function(z)if(is.list(z$detail))get(z$detail$row$screening_id) else "",character(1)))
  paper_ids<-paper_ids[nzchar(paper_ids)]
  paper_labels<-vapply(paper_ids,function(id) {
    d<-Filter(function(z)is.list(z$detail)&&identical(get(z$detail$row$screening_id),id),idx)[[1L]]$detail
    paste0(get(d$row$study_id)," · original PDF")
  },character(1))
  selected_pdf<-if(length(paper_ids))stats::setNames(paper_ids,paper_labels) else character()
  frozen<-identical(get(x$narrative_source_finalization$status),"FINALIZED")
  extracted<-exists("web_b2_nr_pdf_numbers_ready",mode="function") && web_b2_nr_pdf_numbers_ready(x)
  count_saved<-sum(vapply(idx,function(z)is.list(z$detail$review),logical(1)))
  shiny::div(class="srma-batch-review",
    shiny::tags$style(shiny::HTML("\n      .srma-batch-review {margin:16px 0; padding:18px; border:1px solid #d8e5ef; border-radius:12px;}\n      .srma-batch-review .srma-batch-table-wrap {overflow-x:auto;}\n      .srma-batch-review table {width:100%;border-collapse:collapse;table-layout:fixed;}\n      .srma-batch-review th {font-size:14px;background:#f5f8fb;padding:10px;border-bottom:1px solid #d7e1e9;}\n      .srma-batch-review td {vertical-align:top;padding:12px 10px;border-bottom:1px solid #e7edf2;overflow-wrap:anywhere;}\n      .srma-batch-review .srma-ai-number {font-size:14px;color:#18364b;white-space:pre-wrap;}\n      .srma-batch-review .srma-row-meta {font-size:12px;color:#667a8b;margin-top:7px;}\n      .srma-batch-review .form-group {margin:0;}\n      .srma-batch-review input[type=text] {min-height:37px;font-size:14px;}\n      .srma-batch-review details {margin-top:7px;font-size:13px;color:#4b6478;}\n      .srma-batch-review summary {cursor:pointer;}\n      .srma-batch-review .srma-source-excerpt {max-height:130px;overflow-y:auto;white-space:pre-wrap;}\n      .srma-batch-review .srma-batch-actions {display:flex;flex-wrap:wrap;gap:10px;align-items:center;margin-top:17px;}\n      @media(max-width:800px){.srma-batch-review table{min-width:880px;}}\n    ")),
    shiny::h4("AI-extracted study results — final verification"),
    if(!extracted && !frozen)shiny::div(style="margin:10px 0;padding:13px;border-radius:8px;background:#eef7ff;",
      shiny::p("First, AI reads all selected PDF outcomes and returns actual numbers with source citations."),
      shiny::actionButton("srma_batch_ai_extract","AI Extract All PDF Results",class="btn-primary")) else
      if(extracted)shiny::p(style="color:#166534;font-weight:600;",
        "PDF numerical extraction ready. Correct only mistakes; Final Verification may be repeated before continuing."),
    shiny::p("All selected outcomes are shown together. Compare the AI proposal with the PDF; leave the correction empty if correct. Edit only errors, then confirm everything once."),
    shiny::p(style="font-size:13px;color:#536778;",paste(length(idx),"result(s) ·",count_saved,
      "previously checked · Original AI extraction is preserved.")),
    shiny::div(style="display:flex;flex-wrap:wrap;gap:10px;align-items:center;margin:9px 0 16px;",
      shiny::selectInput("srma_batch_pdf_source","Original PDF",choices=selected_pdf,
        selected=if(length(paper_ids))paper_ids[[1L]] else character(),width="270px"),
      shiny::downloadButton("srma_batch_original_pdf","Download selected PDF")),
    shiny::div(class="srma-batch-table-wrap",
      shiny::tags$table(
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(style="width:21%;","Study · outcome"),
          shiny::tags$th(style="width:36%;","AI extraction (read only)"),
          shiny::tags$th(style="width:18%;","PDF evidence"),
          shiny::tags$th(style="width:25%;","Correction (only if needed)"))),
        shiny::tags$tbody(lapply(seq_along(idx),function(i){
          z<-idx[[i]];d<-z$detail
          if(is.null(d))return(shiny::tags$tr(shiny::tags$td(colspan="4",
            paste("Missing or non-unique PDF evidence:",z$result_id))))
          prior<-d$review
          source<-z$candidate
          suggested<-if(!is.null(source))web_b2_nr_summary(list(values=source)) else ""
          proposal<-if(exists("web_b2_nr_pdf_numbers_proposal",mode="function"))
            web_b2_nr_pdf_numbers_proposal(x,z$result_id) else NULL
          if(!nzchar(suggested) && is.list(proposal))
            suggested<-if(identical(get(proposal$status),"NOT_REPORTED"))"Not reported at this exact timepoint" else
              get(proposal$reported_result)
          if(!nzchar(suggested))suggested<-"AI result not yet available — run PDF extraction"
          old_correct<-if(is.list(prior)&&identical(get(prior$decision),"CORRECTED"))get(prior$reported_result) else ""
          old_not_reported<-if(is.list(prior))identical(get(prior$decision),"NOT_REPORTED") else
            is.list(proposal)&&identical(get(proposal$status),"NOT_REPORTED")
          location<-if(is.list(prior)&&nzchar(get(prior$source_location)))get(prior$source_location) else
            if(is.list(proposal) && nzchar(get(proposal$source_location)))get(proposal$source_location) else get(d$pdf$source_location)
          excerpt<-if(is.list(prior)&&nzchar(get(prior$source_evidence)))get(prior$source_evidence) else
            if(is.list(proposal) && nzchar(get(proposal$source_evidence)))get(proposal$source_evidence) else get(d$pdf$source_evidence)
          shiny::tags$tr(
            shiny::tags$td(
              shiny::tags$strong(get(d$row$study_id)),shiny::tags$div(get(d$row$common_variable)),
              shiny::tags$div(class="srma-row-meta",paste(get(d$row$analysis_timepoint),
                "· PDF:",get(d$pdf$reported_name))),
              shiny::tags$div(class="srma-row-meta",if(is.list(prior))
                paste0("Last checked: ",get(prior$decision)) else "Not yet verified")),
            shiny::tags$td(
              shiny::tags$div(class="srma-ai-number",suggested),
              if(!is.null(source))shiny::tags$div(class="srma-row-meta",
                paste("AI ID:",get(source$.row_id))),
              if(is.list(proposal)&&nzchar(get(proposal$uncertainty_note)))
                shiny::tags$div(class="srma-row-meta",
                  paste("AI check:",get(proposal$status),"—",get(proposal$uncertainty_note))),
              if(is.list(proposal) && nzchar(get(proposal$reported_result)))
                shiny::tags$div(class="srma-row-meta",get(proposal$reported_result)),
              if(is.null(source))shiny::tags$div(class="srma-row-meta",
                "No automatic numerical match; enter the PDF result or mark Not reported.")),
            shiny::tags$td(
              shiny::tags$div(location),
              shiny::tags$details(
                shiny::tags$summary("View evidence / edit citation"),
                shiny::tags$div(class="srma-source-excerpt",if(nzchar(excerpt))excerpt else "No excerpt captured"),
                shiny::textInput(paste0("srma_batch_location_",i),"PDF page/table",
                  value=location,width="100%"),
                shiny::textAreaInput(paste0("srma_batch_evidence_",i),"PDF evidence",
                  value=excerpt,rows=2,width="100%"))),
            shiny::tags$td(
              shiny::textInput(paste0("srma_batch_correction_",i),NULL,value=old_correct,
                placeholder=if(is.null(source))"Enter exact PDF value" else "Leave empty if AI is correct",
                width="100%"),
              shiny::checkboxInput(paste0("srma_batch_not_reported_",i),"Not reported in PDF",
                value=old_not_reported))
          )
        }))
      )),
    if(frozen)shiny::p(style="font-weight:700;color:#166534;",
      "Analysis verification is finalized and locked for this Evidence Version.") else
      shiny::div(class="srma-batch-actions",
        shiny::actionButton("srma_batch_verify_all","Final Verification — confirm all results",
          class="btn-primary"),
        shiny::span(style="font-size:13px;color:#64748b;",
          "You can edit and repeat verification until moving to Manuscript.")),
    shiny::textOutput("srma_batch_feedback")
  )
}
