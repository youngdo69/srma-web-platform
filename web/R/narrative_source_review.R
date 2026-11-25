# RC28.22.1 - Web-only human PDF-to-number review sidecar.
# Do not overwrite AI evidence or set canonical R/26 numerical approval flags.
web_b2_nr_fields <- function() c(
  "intervention_events","intervention_total","comparator_events","comparator_total",
  "intervention_mean","intervention_sd","intervention_n",
  "comparator_mean","comparator_sd","comparator_n","unit","data_type",
  "effect_measure","effect_estimate","lower_ci","upper_ci",
  "adjusted","adjustment_variables")
web_b2_nr_one <- function(x) {
  y <- as.character(unlist(x, use.names=FALSE))
  if(!length(y) || is.na(y[[1L]])) "" else trimws(y[[1L]])
}
web_b2_nr_review_get <- function(x, result_id, digest="") {
  reviews <- x$narrative_numeric_reviews
  if(!is.list(reviews)) return(NULL)
  matches <- Filter(function(r) identical(web_b2_nr_one(r$result_id),result_id),reviews)
  if(length(matches)!=1L) return(NULL)
  r <- matches[[1L]]
  if(nzchar(digest) && !identical(web_b2_nr_one(r$source_digest),digest)) return(NULL)
  r
}
web_b2_nr_detail <- function(x,result_id) {
  if(!is.list(x) || !is.list(x$synthesis_execution$rows)) return(NULL)
  rows <- Filter(function(r)identical(web_b2_nr_one(r$result_id),result_id), x$synthesis_execution$rows)
  if(length(rows)!=1L) return(NULL)
  r <- rows[[1L]]
  pdf <- Filter(function(p)identical(web_b2_nr_one(p$result_id),result_id),
    x$pdf_result_discovery$results)
  if(length(pdf)!=1L) return(NULL)
  p <- pdf[[1L]]
  if(!identical(web_b2_nr_one(p$screening_id),web_b2_nr_one(r$screening_id))) return(NULL)
  checksum <- web_b2_nr_one(x$pdf_result_discovery$report_checksums[[web_b2_nr_one(r$screening_id)]])
  if(!nzchar(checksum) || !identical(web_b2_nr_one(p$pdf_sha256),checksum)) return(NULL)
  ai <- x$tables$AI_outcome_data
  if(!is.list(ai)) ai <- list()
  ai <- Filter(function(s) identical(web_b2_nr_one(s$screening_id),web_b2_nr_one(r$screening_id)),ai)
  # Identification uses exact source IDs, not name/timepoint similarity.
  ai <- Filter(function(s)nzchar(web_b2_nr_one(s$.row_id)),ai)
  ids <- vapply(ai,function(s)web_b2_nr_one(s$.row_id),character(1))
  if(anyDuplicated(ids)) ai <- list() # fail closed on inconsistent candidates
  mapped <- Filter(function(s)
    identical(web_b2_nr_one(s$original_pdf_result_id),result_id) && isTRUE(s$.web_harmonized_result),
    x$tables$Target_Outcome_Verification)
  mapped_ai_id <- if(length(mapped)==1L) web_b2_nr_one(mapped[[1L]]$.source_ai_row_id) else ""
  # Verification bound to exact selected PDF, synthesis inputs and result ID.
  review <- web_b2_nr_review_get(x,result_id,web_b2_nr_one(x$synthesis_execution$source_digest))
  if(!is.null(review) && !identical(web_b2_nr_one(review$pdf_sha256),checksum)) review <- NULL
  list(row=r,pdf=p,checksum=checksum,ai=ai,mapped_ai_id=mapped_ai_id,review=review)
}
web_b2_nr_summary <- function(review) {
  if(is.null(review))return("")
  if(identical(web_b2_nr_one(review$decision),"NOT_REPORTED"))return("")
  v <- review$values
  if(!is.list(v)) v <- list()
  get <- function(k) web_b2_nr_one(v[[k]])
  measure <- get("effect_measure")
  est <- get("effect_estimate");lo <- get("lower_ci"); hi <- get("upper_ci")
  out <- character()
  if(nzchar(est)) out <- c(out,paste0(if(nzchar(measure)) paste0(measure," ") else "Effect ",est,
    if(nzchar(lo)&&nzchar(hi))paste0(" (95% CI ",lo,"–",hi,")") else ""))
  bin <- vapply(c("intervention_events","intervention_total","comparator_events","comparator_total"),get,character(1))
  if(all(nzchar(bin))) out <- c(out,paste0("Intervention ",bin[[1L]],"/",bin[[2L]],
    "; comparator ",bin[[3L]],"/",bin[[4L]]))
  cont <- vapply(c("intervention_mean","intervention_sd","intervention_n",
    "comparator_mean","comparator_sd","comparator_n"),get,character(1))
  if(all(nzchar(cont))) out <- c(out,paste0("Intervention ",cont[[1L]]," (SD ",cont[[2L]],
    ", n=",cont[[3L]],"); comparator ",cont[[4L]]," (SD ",cont[[5L]],", n=",cont[[6L]],")"))
  if(nzchar(get("adjusted"))) out <- c(out,paste0("Adjustment: ",get("adjusted"),
    if(nzchar(get("adjustment_variables")))paste0(" (",get("adjustment_variables"),")") else ""))
  if(nzchar(web_b2_nr_one(review$reported_result))) out <- c(out,web_b2_nr_one(review$reported_result))
  paste(unique(out),collapse="; ")
}
web_b2_nr_save <- function(con,settings,actor,project_id,version_id,result_id,values) {
  fields <- web_b2_nr_fields()
  allowed <- c("source_digest","decision","source_row_id","source_location","source_evidence",
    "reported_result","rationale",fields)
  web_assert(is.list(values) && !is.null(names(values)) &&
    setequal(names(values),allowed) && length(values)==length(allowed) &&
    all(vapply(values,function(v)is.character(v)&&length(v)==1L&&
      !is.na(v)&&nchar(v,type="bytes")<=6000L,logical(1))),
    "Invalid source verification fields.","VALIDATION_FAILED")
  context <- web_b2_context(con,settings,actor,project_id,version_id)
  x <- context$packet
  web_assert(identical(x$synthesis_plan$status,"HUMAN_CONFIRMED") &&
    identical(x$synthesis_execution$status,"AUTO_PREPARED") &&
    identical(web_b2_nr_one(x$synthesis_execution$source_digest),web_b2_synthesis_source_digest(x)) &&
    identical(values$source_digest,web_b2_nr_one(x$synthesis_execution$source_digest)),
    "The synthesis source changed. Reload Analysis and verify the current Evidence Version.","USER_ACTION_REQUIRED")
  d <- web_b2_nr_detail(x,result_id)
  web_assert(!is.null(d),"This result is not uniquely linked to a current PDF report.","USER_ACTION_REQUIRED")
  canonical <- Filter(function(r)
    identical(web_b2_nr_one(r$original_pdf_result_id),result_id) &&
    web_b2_nr_one(r$verification_status) %in% c("Verified","Corrected") &&
    nzchar(web_b2_nr_one(r$.verified_by)),x$tables$Target_Outcome_Verification)
  web_assert(!length(canonical),
    "This outcome already has independently human-verified canonical extraction. Correct it through the canonical verification workflow, not through a separate narrative override.",
    "USER_ACTION_REQUIRED")
  web_assert(web_b2_nr_one(d$row$study_id) %in% unlist(x$final_study_selection$study_ids,use.names=FALSE),
    "This result is not in Final Study Selection.","USER_ACTION_REQUIRED")
  # Server-side PDF integrity check, not merely a saved SHA string comparison.
  authors <- Filter(function(r)identical(web_b2_nr_one(r$screening_id),web_b2_nr_one(d$row$screening_id)),
    x$tables$Author_Final_Review)
  web_assert(length(authors)==1L && identical(web_b2_nr_one(authors[[1L]]$.pdf_sha256),d$checksum),
    "Original PDF registration is not current.","USER_ACTION_REQUIRED")
  pdfpath <- web_inside(file.path(context$version$root,web_b2_nr_one(authors[[1L]]$.pdf_relative)),context$version$root)
  web_assert(identical(web_sha(pdfpath),d$checksum),"Registered PDF checksum failed.","USER_ACTION_REQUIRED")
  decision <- values$decision
  web_assert(decision %in% c("CONFIRMED","CORRECTED","NOT_REPORTED"),
    "Choose Confirmed, Corrected, or Not reported.","VALIDATION_FAILED")
  web_assert(nzchar(trimws(values$rationale)) && nzchar(trimws(values$source_location)) &&
    nzchar(trimws(values$source_evidence)),
    "PDF location, supporting excerpt and a review rationale are required.","USER_ACTION_REQUIRED")
  sourceid <- values$source_row_id
  source <- Filter(function(r)identical(web_b2_nr_one(r$.row_id),sourceid),d$ai)
  web_assert(!nzchar(sourceid) || length(source)==1L,
    "The AI extraction row is not a unique result of this PDF.","VALIDATION_FAILED")
  if(identical(decision,"CONFIRMED")) web_assert(length(source)==1L,
    "To confirm an AI extraction, select the matching AI source row; use Corrected / manual entry otherwise.","USER_ACTION_REQUIRED")
  v <- values[fields]
  v <- lapply(v,trimws)
  provided <- names(v)[vapply(v,nzchar,logical(1))]
  if(identical(decision,"NOT_REPORTED")) {
    web_assert(!length(provided) && !nzchar(values$reported_result),
      "Not reported cannot contain source numerical data.","VALIDATION_FAILED")
  } else {
    web_assert(length(setdiff(provided,c("unit","data_type","effect_measure","adjusted","adjustment_variables")))>0L ||
      nzchar(trimws(values$reported_result)),
      "Enter a PDF-based result or at least one structured numerical value.","USER_ACTION_REQUIRED")
  }
  if(identical(decision,"CONFIRMED")) {
    web_assert(identical(tolower(web_b2_nr_one(source[[1L]]$outcome_name)),
       tolower(web_b2_nr_one(d$pdf$reported_name))) &&
       identical(tolower(web_b2_nr_one(source[[1L]]$timepoint)),
       tolower(web_b2_nr_one(d$pdf$reported_timepoint))),
       "The AI candidate outcome/timepoint is not an exact PDF inventory match. Use Corrected / manual entry with documented correspondence.",
       "USER_ACTION_REQUIRED")
    web_assert(!nzchar(values$reported_result),
      "Additional or differently reported values require Corrected / manual entry and a rationale.","USER_ACTION_REQUIRED")
    web_assert(all(vapply(fields,function(k)
      identical(web_b2_nr_one(v[[k]]),web_b2_nr_one(source[[1L]][[k]])),logical(1))),
      "Confirmed values differ from the selected AI source row. Use Corrected / manual entry.","USER_ACTION_REQUIRED")
  }
  integer_fields <- c("intervention_events","intervention_total","comparator_events","comparator_total",
    "intervention_n","comparator_n")
  for(k in integer_fields) if(nzchar(v[[k]]))
    web_assert(grepl("^[0-9]+$",v[[k]]),paste("Invalid nonnegative integer for",k),"VALIDATION_FAILED")
  decimal_fields <- c("intervention_mean","intervention_sd","comparator_mean","comparator_sd",
    "effect_estimate","lower_ci","upper_ci")
  for(k in decimal_fields) if(nzchar(v[[k]]))
    web_assert(grepl("^-?([0-9]+(\\.[0-9]+)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$",v[[k]]),
      paste("Invalid numeric value for",k),"VALIDATION_FAILED")
  for(a in c("intervention","comparator")) {
    ev <- v[[paste0(a,"_events")]];n <- v[[paste0(a,"_total")]]
    if(nzchar(ev)&&nzchar(n))web_assert(as.numeric(ev)<=as.numeric(n),
      paste(a,"event count exceeds total"),"VALIDATION_FAILED")
  }
  web_assert(identical(nzchar(v$lower_ci),nzchar(v$upper_ci)),
    "Provide both confidence interval limits together, or leave both empty if the paper did not report them.",
    "VALIDATION_FAILED")
  if(nzchar(v$lower_ci)&&nzchar(v$upper_ci))
    web_assert(as.numeric(v$lower_ci)<=as.numeric(v$upper_ci),
      "Confidence interval lower limit is larger than upper limit.","VALIDATION_FAILED")
  if(nzchar(v$effect_measure)) web_assert(v$effect_measure %in% c("RR","OR","HR","RD","MD","SMD"),
    "Unknown effect measure; do not relabel the reported result.","VALIDATION_FAILED")
  if(v$effect_measure %in% c("RR","OR","HR")) {
    for(k in c("effect_estimate","lower_ci","upper_ci")) if(nzchar(v[[k]]))
      web_assert(as.numeric(v[[k]])>0,paste("A ratio measure must be positive:",k),"VALIDATION_FAILED")
  }
  record <- list(result_id=result_id,screening_id=web_b2_nr_one(d$row$screening_id),
    study_id=web_b2_nr_one(d$row$study_id),pdf_sha256=d$checksum,
    source_digest=values$source_digest,source_row_id=sourceid,decision=decision,
    source_location=trimws(values$source_location),source_evidence=trimws(values$source_evidence),
    reported_result=trimws(values$reported_result),rationale=trimws(values$rationale),
    values=v,verified_by=actor,verified_at=web_now(),
    scope="NARRATIVE_ONLY_SOURCE_CHECK; NOT_CANONICAL_R26")
  if(!is.list(x$narrative_numeric_reviews))x$narrative_numeric_reviews<-list()
  old <- x$narrative_numeric_reviews
  x$narrative_numeric_reviews <- c(Filter(function(r)!identical(web_b2_nr_one(r$result_id),result_id),old),list(record))
  x$narrative_analysis <- NULL # a changed individual result invalidates any earlier draft route approval
  x$history <- c(x$history,list(list(event="NARRATIVE_PDF_SOURCE_REVIEW",actor=actor,
    result_id=result_id,decision=decision,source_ai_row_id=sourceid,
    source_version=version_id,pdf_sha256=d$checksum,at=web_now())))
  root<-tempfile("narrative-source-review-",settings$data_root)
  dir.create(root)
  on.exit(unlink(root,recursive=TRUE))
  web_copy_version(con,settings,actor,project_id,version_id,root)
  web_b2_write(root,x)
  web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,x$stage)
}
