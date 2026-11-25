# RC28.22 — structured narrative-only analysis; Web Evidence sidecar.
# No statistical inference, AI calls, numeric auto-verification, or canonical R modifications.
web_b2_narrative_one <- function(x, default = "") {
  y <- if (is.null(x)) default else as.character(unlist(x, use.names = FALSE))
  if (!length(y) || is.na(y[[1L]])) default else trimws(y[[1L]])
}
# web/R/evidence.R supplies the approved engine root before sourcing this module.
# engine_entry.R changes getwd() to a version snapshot; never source Web code from it.
if (!exists("web_b2_narrative_root", inherits = TRUE))
  stop("Approved Web module source root is unavailable.")
source(file.path(web_b2_narrative_root, "web/R/narrative_source_review.R"), local = TRUE)
source(file.path(web_b2_narrative_root, "web/R/narrative_batch_review.R"), local = TRUE)
source(file.path(web_b2_narrative_root, "web/R/narrative_pdf_numbers.R"), local = TRUE)
web_b2_narrative_key <- function(x) {
  gsub("[^a-z0-9]", "", tolower(web_b2_narrative_one(x)))
}
web_b2_narrative_assess <- function(x) {
  pending <- function(message) list(status = "PENDING", message = message, digest = "",
    rows = list(), issues = list(), n_studies = 0L, n_sets = 0L,
    blocking = 0L, review = 0L, numeric_verified = 0L,
    numeric_unverified = 0L, rob_complete = FALSE, approval_fresh = FALSE)
  if (!is.list(x) || !identical(x$final_study_selection$status, "HUMAN_CONFIRMED") ||
      !identical(x$synthesis_plan$status, "HUMAN_CONFIRMED"))
    return(pending("Complete the saved Studies synthesis plan first."))
  run <- x$synthesis_execution
  if (!is.list(run) || !identical(run$status, "AUTO_PREPARED") ||
      !isTRUE(x$.synthesis_execution_fresh))
    return(pending("Source-indexed synthesis outputs are absent or stale."))
  modes <- vapply(x$synthesis_plan$decisions, function(a)
    web_b2_narrative_one(a$mode), character(1))
  if (!length(modes) || any(modes != "NARRATIVE"))
    return(pending("This review is not narrative-only. Use the mixed or quantitative synthesis route."))
  rows <- run$rows
  if (!is.list(rows) || !length(rows))
    return(pending("No source-bound outcome results were found."))
  ids <- vapply(rows, function(r) web_b2_narrative_one(r$result_id), character(1))
  if (any(!nzchar(ids)) || anyDuplicated(ids))
    return(pending("Source-specific result identities are missing or duplicated."))
  # The PDF discovery records, not harmonized labels or narrative prose, are
  # the immutable source anchors. No missing PDF value is guessed here.
  pdf <- x$pdf_result_discovery$results
  if (!is.list(pdf)) pdf <- list()
  pid <- vapply(pdf, function(p) web_b2_narrative_one(p$result_id), character(1))
  report_hashes <- x$pdf_result_discovery$report_checksums
  if (!is.list(report_hashes)) report_hashes <- list()
  selected <- as.character(unlist(x$final_study_selection$study_ids, use.names = FALSE))
  issues <- list()
  add <- function(code, severity, study_id, result_id, detail) {
    issues[[length(issues) + 1L]] <<- list(code = code, severity = severity,
      study_id = study_id, result_id = result_id, detail = detail)
  }
  checked <- list()
  for (row in rows) {
    sid <- web_b2_narrative_one(row$study_id)
    rid <- web_b2_narrative_one(row$result_id)
    screening <- web_b2_narrative_one(row$screening_id)
    matching <- which(pid == rid)
    source <- if (length(matching) == 1L) pdf[[matching]] else NULL
    if (!nzchar(sid) || !sid %in% selected)
      add("STUDY_ID_NOT_SELECTED", "BLOCK", sid, rid, "Result study is not in Final Study Selection")
    if (length(matching) != 1L || !identical(web_b2_narrative_one(source$screening_id), screening))
      add("PDF_RESULT_PROVENANCE_MISSING", "BLOCK", sid, rid, "No unique matching PDF result and screening ID")
    if (!is.null(source)) {
      if (!identical(web_b2_narrative_one(row$source_location),
                     web_b2_narrative_one(source$source_location)))
        add("PDF_SOURCE_LOCATION_MISMATCH", "BLOCK", sid, rid,
            "Selected result location differs from the saved PDF inventory")
      expected <- web_b2_narrative_one(report_hashes[[screening]])
      actual <- web_b2_narrative_one(source$pdf_sha256)
      if (!nzchar(expected) || !nzchar(actual) || !identical(expected, actual))
        add("PDF_SHA256_MISMATCH", "BLOCK", sid, rid, "PDF discovery checksum does not match saved report")
      if (!identical(web_b2_narrative_one(row$original_outcome), web_b2_narrative_one(source$reported_name)) ||
          !identical(web_b2_narrative_one(row$reported_timepoint), web_b2_narrative_one(source$reported_timepoint)))
        add("PDF_OUTCOME_OR_TIMEPOINT_MISMATCH", "BLOCK", sid, rid, "Draft does not match the original PDF discovery result")
    }
    loc <- web_b2_narrative_one(row$source_location)
    if (!nzchar(loc)) add("PDF_SOURCE_LOCATION_MISSING", "BLOCK", sid, rid,
      "A verifiable page, table or figure location is required")
    # Different human harmonized names are not silently treated as synonymous.
    if (!is.null(source)) {
      # A common variable can include a timepoint suffix (e.g. outcome_2yrs).
      # Do not make routine suffix normalization a human-review exception.
      norm <- web_b2_narrative_key(row$common_variable)
      pdf_name <- web_b2_narrative_key(source$reported_name)
      equivalent_text <- nzchar(norm) && nzchar(pdf_name) &&
        (identical(norm, pdf_name) || startsWith(norm, pdf_name) || startsWith(pdf_name, norm))
      if (!equivalent_text)
        add("ENDPOINT_SEMANTIC_DIFFERENCE", "REVIEW", sid, rid,
          "Check the PDF endpoint against the harmonized label and definition")
    }
    numeric <- web_b2_narrative_one(row$verified_numeric_summary)
    status <- if(identical(web_b2_narrative_one(row$numeric_status),"HUMAN_VERIFIED") && nzchar(numeric))
      "HUMAN_VERIFIED" else "NOT_VERIFIED"
    review_rec <- web_b2_nr_review_get(x,rid,web_b2_narrative_one(run$source_digest))
    if(!is.null(review_rec) && identical(web_b2_narrative_one(review_rec$pdf_sha256),
      web_b2_narrative_one(source$pdf_sha256))) {
      if(identical(web_b2_narrative_one(review_rec$decision),"NOT_REPORTED")) {
        numeric <- "";status <- "NOT_REPORTED"
      } else {
        numeric <- web_b2_nr_summary(review_rec)
        if(nzchar(numeric)) status <- if(identical(web_b2_narrative_one(review_rec$decision),"CORRECTED"))
          "NARRATIVE_SOURCE_CORRECTED" else "NARRATIVE_SOURCE_CONFIRMED"
      }
    }
    if(status=="NOT_VERIFIED") add("NUMERICAL_RESULT_UNVERIFIED", "REVIEW", sid, rid,
      "Review the saved AI values and registered PDF in the Source Verification Workspace")
    checked[[length(checked) + 1L]] <- list(
      study_id = sid, screening_id = screening, result_id = rid,
      outcome = web_b2_narrative_one(row$original_outcome),
      common_variable = web_b2_narrative_one(row$common_variable),
      timepoint = web_b2_narrative_one(row$analysis_timepoint),
      reported_timepoint = web_b2_narrative_one(row$reported_timepoint),
      source_location = loc,
      human_pdf_location = if(!is.null(review_rec))web_b2_nr_one(review_rec$source_location) else "",
      human_pdf_excerpt = if(!is.null(review_rec))web_b2_nr_one(review_rec$source_evidence) else "",
      ai_source_row_id = if(!is.null(review_rec))web_b2_nr_one(review_rec$source_row_id) else "",
      source_evidence = substr(gsub("[\r\n\t]+", " ",
        web_b2_narrative_one(source$source_evidence)), 1L, 500L),
      pdf_sha256 = web_b2_narrative_one(source$pdf_sha256),
      numerical_result = if (status!="NOT_VERIFIED") numeric else "",
      numeric_status = status)
  }
  # RoB is tracked separately. Absence is visible, never silently counted as low risk.
  rob <- x$tables$Overall_Review
  if (!is.list(rob)) rob <- list()
  rob_status <- lapply(selected, function(sid) {
    relevant <- Filter(function(r) identical(web_b2_narrative_one(r$study_id), sid), rob)
    # Many scientific ROB rows have screening_id rather than study_id.
    if (!length(relevant)) {
      sr <- unique(vapply(Filter(function(r) identical(r$study_id, sid), checked),
        function(r) r$screening_id, character(1)))
      relevant <- Filter(function(r) web_b2_narrative_one(r$screening_id) %in% sr, rob)
    }
    judgment <- unique(vapply(relevant, function(r)
      web_b2_narrative_one(r$author_final_overall_judgement), character(1)))
    judgment <- judgment[nzchar(judgment)]
    if (length(judgment) != 1L)
      add("ROB_ASSESSMENT_PENDING", "REVIEW", sid, "", "Final study-level risk-of-bias judgement unavailable or ambiguous")
    list(study_id = sid, judgment = if (length(judgment) == 1L) judgment else "PENDING")
  })
  flags <- vapply(issues, function(a) a$severity, character(1))
  block <- sum(flags == "BLOCK")
  review <- sum(flags == "REVIEW")
  nverified <- sum(vapply(checked, function(a) a$numeric_status %in%
    c("HUMAN_VERIFIED","NARRATIVE_SOURCE_CORRECTED","NARRATIVE_SOURCE_CONFIRMED"), logical(1)))
  not_reported <- sum(vapply(checked, function(a)identical(a$numeric_status,"NOT_REPORTED"),logical(1)))
  digest_value <- digest::digest(list(run$source_digest, checked, rob_status,
    x$final_study_selection$study_ids, x$synthesis_plan$decisions), algo = "sha256")
  approval <- x$narrative_analysis
  approval_fresh <- is.list(approval) && identical(approval$status, "DRAFT_ROUTE_CONFIRMED") &&
    identical(approval$analysis_digest, digest_value)
  list(status = if (block > 0L) "BLOCKED" else if (approval_fresh) "DRAFT_ROUTE_CONFIRMED" else "READY_FOR_DRAFT_ROUTE_CONFIRMATION",
    message = if (block > 0L) paste0(block, " source integrity issue(s) require correction in Studies.") else
      "Narrative-only analysis is source-indexed. Draft preparation is available; numerical and RoB exceptions remain explicit.",
    digest = digest_value, rows = checked, issues = issues,
    rob = rob_status, n_studies = length(selected),
    n_sets = length(x$synthesis_plan$decisions),
    blocking = block, review = review,
    numeric_verified = nverified, numeric_unverified = length(checked) - nverified - not_reported,
    numeric_not_reported = not_reported,
    rob_complete = all(vapply(rob_status, function(a) !identical(a$judgment, "PENDING"), logical(1))),
    approval_fresh = approval_fresh)
}
web_b2_narrative_issue_table <- function(a) {
  fields <- c("severity", "code", "study_id", "result_id", "detail")
  z <- a$issues
  if (!length(z)) return(as.data.frame(stats::setNames(rep(list(character()), length(fields)), fields)))
  as.data.frame(stats::setNames(lapply(fields, function(k)
    vapply(z, function(r) as.character(r[[k]]), character(1))), fields), stringsAsFactors = FALSE)
}
web_b2_narrative_markdown <- function(x, a = web_b2_narrative_assess(x)) {
  web_assert(!identical(a$status, "PENDING"), "Prepare a current source-indexed synthesis first.")
  lines <- c(
    "# Narrative-only systematic review — manuscript working draft", "",
    "**DRAFT — NOT FOR SUBMISSION.** This is a source-indexed handoff, not a completed R/38–R/41 canonical manuscript.", "",
    paste0("Evidence version: ", web_b2_narrative_one(x$source_version, "Not recorded")),
    paste0("Studies: ", a$n_studies, "; timepoint-specific analysis sets: ", a$n_sets,
      "; pooled effects: none."), "",
    "## Methods — synthesis", "",
    "The included reports were grouped using human-confirmed outcome definitions and timepoint-specific analysis sets. No compatible multi-study quantitative set was available; therefore, no pooled treatment effect was estimated. Results were organized by study and follow-up period using structured narrative synthesis. Statistical significance or equivalence was not inferred from the absence of pooling.", "",
    "## Results — source-indexed individual study findings", "",
    "The following entries record which source result was selected. Only independently human-verified numerical summaries may appear; unverified numbers are withheld.", "")
  for (r in a$rows) {
    lines <- c(lines, paste0("### ", r$common_variable, " — ", r$timepoint), "",
      paste0("- Study: ", r$study_id, "; original endpoint: ", r$outcome),
      paste0("- Original PDF timepoint: ", r$reported_timepoint, "; source: ", r$source_location),
      if(nzchar(r$human_pdf_location))paste0("- Human-checked PDF location: ",r$human_pdf_location) else NULL,
      if(nzchar(r$human_pdf_excerpt))paste0("- Human-checked source excerpt: ",r$human_pdf_excerpt) else NULL,
      if(nzchar(r$ai_source_row_id))paste0("- Compared AI R/25 row ID: ",r$ai_source_row_id) else NULL,
      if (r$numeric_status %in% c("HUMAN_VERIFIED","NARRATIVE_SOURCE_CONFIRMED","NARRATIVE_SOURCE_CORRECTED"))
        paste0("- Source-checked result (",r$numeric_status,"): ", r$numerical_result) else if(identical(r$numeric_status,"NOT_REPORTED"))
        "- Numerical result: Not reported (human PDF check recorded)" else
        "- Numerical result: [SOURCE VERIFICATION REQUIRED — no numerical claim supplied]", "")
  }
  lines <- c(lines, "## Risk-of-bias synthesis", "")
  for (r in a$rob) lines <- c(lines, paste0("- ", r$study_id, ": ", r$judgment))
  lines <- c(lines, "", "## Discussion — interpretation boundaries", "",
    "Studies with different endpoint definitions and/or follow-up periods were not pooled. Review individual study estimates, potential confounding, risk of bias, and generalizability before drawing conclusions. Do not claim that one intervention is superior without a supported, source-verified comparative finding.", "",
    "## Verification status", "",
    if(a$numeric_unverified>0L)"Some source results remain unverified; unsupported numbers are withheld."
      else "The reported individual results passed PDF-based source verification.",
    if(!isTRUE(a$rob_complete))"Final risk-of-bias assessments are pending."
      else "Final study-level risk-of-bias assessments are available.")
  c(lines, "", "## Editorial status", "",
    if (isTRUE(a$approval_fresh)) "Narrative synthesis *route* confirmed for this exact Evidence Version; individual numbers and RoB retain their own verification statuses." else
      "Narrative synthesis route not yet confirmed by the user.",
    "This handoff must be editorially reviewed and transferred into the canonical Manuscript Builder before journal submission.")
}
web_b2_approve_narrative_draft <- function(con, settings, actor, project_id, version_id, values) {
  web_assert(is.list(values) && setequal(names(values), c("analysis_digest", "decision")) &&
    length(values) == 2L && identical(values$decision, "CONFIRM_DRAFT_ROUTE") &&
    is.character(values$analysis_digest) && length(values$analysis_digest) == 1L &&
    grepl("^[a-f0-9]{64}$", values$analysis_digest),
    "Invalid narrative route decision.", "VALIDATION_FAILED")
  context <- web_b2_context(con, settings, actor, project_id, version_id)
  x <- context$packet
  x$.synthesis_execution_fresh <- is.list(x$synthesis_execution) &&
    identical(x$synthesis_execution$status, "AUTO_PREPARED") &&
    identical(x$synthesis_execution$source_digest, web_b2_synthesis_source_digest(x))
  a <- web_b2_narrative_assess(x)
  web_assert(a$status %in% c("READY_FOR_DRAFT_ROUTE_CONFIRMATION", "DRAFT_ROUTE_CONFIRMED") &&
    a$blocking == 0L && identical(a$digest, values$analysis_digest),
    "Narrative source validation or version binding has changed. Reload Analysis.",
    "USER_ACTION_REQUIRED")
  web_assert(!isTRUE(a$approval_fresh), "Narrative draft route already confirmed in this Evidence Version.",
    "USER_ACTION_REQUIRED")
  x$narrative_analysis <- list(status = "DRAFT_ROUTE_CONFIRMED", analysis_digest = a$digest,
    actor = actor, at = web_now(), source_version = version_id, numerical_auto_verified = FALSE,
    risk_of_bias_auto_approved = FALSE, unresolved_review_flags = a$review)
  x$history <- c(x$history, list(list(event = "NARRATIVE_DRAFT_ROUTE_CONFIRMED",
    actor = actor, at = web_now(), source_version = version_id,
    analysis_digest = a$digest, unresolved_review_flags = a$review,
    numerical_auto_verified = FALSE)))
  root <- tempfile("narrative-draft-route-", settings$data_root)
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  web_copy_version(con, settings, actor, project_id, version_id, root)
  # No R/26/R/27, no mutation of AI values or of human numeric gates.
  web_b2_write(root, x)
  web_b2_save_snapshot(con, settings, actor, project_id, version_id, root, x$stage)
}
