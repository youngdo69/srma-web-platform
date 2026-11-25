# RC28.22.1 - per-result AI extraction / PDF evidence / Human confirmation.
srma_ui_nr_labels <- c(
  intervention_events="Intervention events",intervention_total="Intervention total",
  comparator_events="Comparator events",comparator_total="Comparator total",
  intervention_mean="Intervention mean",intervention_sd="Intervention SD",intervention_n="Intervention n",
  comparator_mean="Comparator mean",comparator_sd="Comparator SD",comparator_n="Comparator n",
  unit="Unit",data_type="Data type (binary, continuous, reported effect, etc.)",
  effect_measure="Effect measure (RR/OR/HR/RD/MD/SMD)",
  effect_estimate="Reported effect estimate",lower_ci="95% CI lower",upper_ci="95% CI upper",
  adjusted="Adjusted estimate? (as reported)",adjustment_variables="Adjustment variables (as reported)")
srma_ui_nr_editor <- function(x,result_id) {
  d <- web_b2_nr_detail(x,result_id)
  if(is.null(d))return(shiny::p("No unique, current PDF source was found for this result. Do not verify numbers."))
  r <- d$row;p <- d$pdf;review <- d$review
  get <- function(k) if(is.list(review))web_b2_nr_one(review[[k]]) else ""
  prior <- if(is.list(review)&&is.list(review$values))review$values else list()
  candidate_id <- if(nzchar(get("source_row_id")))get("source_row_id") else ""
  choices <- c("No AI row selected — enter exact PDF findings manually"="")
  if(length(d$ai)) {
    labels <- vapply(d$ai,function(s){
      exact <- identical(tolower(web_b2_nr_one(s$outcome_name)),tolower(web_b2_nr_one(p$reported_name))) &&
        identical(tolower(web_b2_nr_one(s$timepoint)),tolower(web_b2_nr_one(p$reported_timepoint)))
      paste0(if(exact)"[Exact label/time candidate] " else "[Other outcome/timepoint] ",
        web_b2_nr_one(s$outcome_name)," | ",web_b2_nr_one(s$timepoint),
        " | ID ",web_b2_nr_one(s$.row_id))
    },character(1))
    choices <- c(choices,stats::setNames(vapply(d$ai,function(s)web_b2_nr_one(s$.row_id),character(1)),labels))
  }
  shiny::div(style="margin:14px 0 18px;padding:18px;border:1px solid #b7d2e8;border-radius:12px;background:#f9fcff;",
    shiny::h4("Source Verification Workspace — PDF ↔ AI ↔ Human"),
    shiny::p(style="color:#475569;","Select one result in the queue, compare the AI proposal with the original PDF, then explicitly confirm or correct it. This does not authorize R/26 numerical pooling."),
    shiny::tags$div(style="padding:12px;border:1px solid #d9e5f1;border-radius:8px;background:white;",
      shiny::strong(paste0(web_b2_nr_one(r$study_id)," · ",web_b2_nr_one(r$common_variable)," · ",web_b2_nr_one(r$analysis_timepoint))),
      shiny::p(paste0("Original PDF endpoint: ",web_b2_nr_one(p$reported_name))),
      shiny::p(paste0("Original definition: ",web_b2_nr_one(p$reported_definition))),
      shiny::p(paste0("Reported timepoint: ",web_b2_nr_one(p$reported_timepoint))),
      shiny::p(paste0("PDF-reported effect measure (if captured): ",
        if(nzchar(web_b2_nr_one(p$reported_effect_measure)))web_b2_nr_one(p$reported_effect_measure) else "Not captured")),
      shiny::p(paste0("Effect measure location: ",web_b2_nr_one(p$effect_measure_location))),
      shiny::p(paste0("Source: ",web_b2_nr_one(p$source_location))),
      shiny::p(shiny::strong("PDF discovery excerpt (AI, not independently verified): "),
        if(nzchar(web_b2_nr_one(p$source_evidence)))web_b2_nr_one(p$source_evidence) else "No excerpt was captured"),
      shiny::p(style="font-size:.88em;color:#64748b;",paste0("PDF SHA-256: ",d$checksum)),
      shiny::downloadButton("srma_narrative_original_pdf","Download this study's original PDF")),
    shiny::tags$hr(),
    shiny::h5("Saved R/25 AI numerical proposals — read-only"),
    shiny::p(style="color:#9a3412;","These entries are candidates from the same report, NOT necessarily this outcome or timepoint. A compound endpoint must never inherit another endpoint's numbers."),
    if(!length(d$ai))shiny::p("No AI numerical extraction row was captured for this report. Compare the PDF and enter the result manually when available."),
    shiny::selectInput("srma_narrative_candidate","Choose exact AI source row for comparison",choices=choices,
      selected=if(candidate_id %in% unname(choices))candidate_id else "",width="100%"),
    shiny::tableOutput("srma_narrative_candidate_values"),
    shiny::actionButton("srma_narrative_load_ai","Copy selected AI values into UNSAVED review fields",class="btn-outline-primary"),
    shiny::tags$hr(),
    shiny::h5("Human source verification — editable"),
    if(!is.null(review))shiny::p(style="color:#166534;font-weight:600;",
      paste0("Previously saved: ",get("decision")," · ",get("verified_at")," · Reviewer ",get("verified_by"))),
    shiny::selectInput("srma_narrative_decision","PDF comparison outcome *",
      choices=c("Select a source-check decision"="","Confirmed — extracted AI values match PDF"="CONFIRMED",
        "Corrected / manually transcribed from PDF"="CORRECTED",
        "No numerical result reported in PDF"="NOT_REPORTED"),selected=get("decision"),width="100%"),
    shiny::div(style="display:grid;grid-template-columns:repeat(auto-fit,minmax(245px,1fr));gap:8px;",
      lapply(names(srma_ui_nr_labels),function(k)
        shiny::textInput(paste0("srma_nr_",k),unname(srma_ui_nr_labels[[k]]),
          value=web_b2_nr_one(prior[[k]]),width="100%"))),
    shiny::textAreaInput("srma_nr_reported_result","Additional exact PDF numerical result text (e.g., KM estimate or p-value)",
      value=get("reported_result"),rows=2,width="100%"),
    shiny::textInput("srma_nr_location","Verified PDF page/table/figure location *",
      value=if(nzchar(get("source_location")))get("source_location") else "",width="100%",
      placeholder=web_b2_nr_one(p$source_location)),
    shiny::textAreaInput("srma_nr_evidence","Supporting excerpt copied/checked from the original PDF *",
      value=get("source_evidence"),rows=3,width="100%",
      placeholder=web_b2_nr_one(p$source_evidence)),
    shiny::textAreaInput("srma_nr_rationale","Verification / correction rationale *",
      value=get("rationale"),rows=2,width="100%",
      placeholder="Checked original PDF Table X. Outcome, timepoint, groups and figures match; or explain corrections."),
    shiny::actionButton("srma_narrative_save_source","Save this result's source review as NEW Evidence Version",class="btn-primary"),
    shiny::textOutput("srma_narrative_source_feedback"),
    shiny::p(style="font-size:.9em;color:#475569;",
      "Confirmation requires exact agreement with the selected AI row. Corrections remain separate from the original AI extraction. This is a narrative-only source check; it does not mark the R/26 independent human gate as passed."))
}
