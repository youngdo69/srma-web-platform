# RC28.22.3 — atomic narrative-only verification of all source results.
# The existing AI extraction remains immutable. This overlay is not R/26 approval.
web_b2_nr_batch_candidate <- function(d, x=NULL, result_id="") {
  if(!is.null(x) && nzchar(result_id) && exists("web_b2_nr_pdf_numbers_proposal",mode="function")) {
    proposed<-web_b2_nr_pdf_numbers_proposal(x,result_id)
    if(is.list(proposed) && !identical(web_b2_nr_one(proposed$status),"NOT_REPORTED") &&
       !grepl("INVALID AI NUMERIC PROPOSAL",web_b2_nr_one(proposed$uncertainty_note),fixed=TRUE)) {
      fields<-web_b2_nr_fields()
      keys<-setdiff(fields,c("unit","data_type","effect_measure","adjusted","adjustment_variables"))
      if(nzchar(web_b2_nr_one(proposed$reported_result)) ||
          any(vapply(keys,function(k)nzchar(web_b2_nr_one(proposed[[k]])),logical(1))))
        return(proposed)
    }
    # Once a matched PDF result exists, do NOT fall back to an unrelated R/25 number.
    if(is.list(proposed))return(NULL)
  }
  if (is.null(d)) return(NULL)
  one <- function(z) web_b2_nr_one(z)
  pdf_name <- gsub("[^a-z0-9]", "", tolower(one(d$pdf$reported_name)))
  pdf_time <- gsub("[^a-z0-9]", "", tolower(one(d$pdf$reported_timepoint)))
  mapped <- one(d$mapped_ai_id)
  match <- Filter(function(r) {
    nm <- gsub("[^a-z0-9]", "", tolower(one(r$outcome_name)))
    tm <- gsub("[^a-z0-9]", "", tolower(one(r$timepoint)))
    (nzchar(pdf_name) && nzchar(pdf_time) && identical(nm,pdf_name) && identical(tm,pdf_time)) ||
      (nzchar(mapped) && identical(one(r$.row_id),mapped))
  },d$ai)
  if(length(match)!=1L) return(NULL)
  r <- match[[1L]]
  actual_fields <- setdiff(web_b2_nr_fields(), c("unit","data_type","effect_measure","adjusted","adjustment_variables"))
  if(!any(vapply(actual_fields,function(k)nzchar(one(r[[k]])),logical(1)))) return(NULL)
  r
}
web_b2_nr_batch_index <- function(x) {
  if(!is.list(x$synthesis_execution$rows)) return(list())
  lapply(x$synthesis_execution$rows,function(r) {
    id<-web_b2_nr_one(r$result_id)
    d<-web_b2_nr_detail(x,id)
    list(result_id=id,detail=d,candidate=web_b2_nr_batch_candidate(d,x,id))
  })
}
web_b2_nr_batch_save <- function(con,settings,actor,project_id,version_id,values) {
  web_assert(is.list(values) && setequal(names(values),c("source_digest","decision","rows")) &&
    identical(values$decision,"FINAL_VERIFY_ALL") &&
    is.character(values$source_digest) && length(values$source_digest)==1L &&
    is.list(values$rows),"Invalid batch verification request.","VALIDATION_FAILED")
  context<-web_b2_context(con,settings,actor,project_id,version_id)
  x<-context$packet
  source_digest<-web_b2_nr_one(x$synthesis_execution$source_digest)
  web_assert(identical(x$synthesis_plan$status,"HUMAN_CONFIRMED") &&
    identical(x$synthesis_execution$status,"AUTO_PREPARED") &&
    identical(source_digest,web_b2_synthesis_source_digest(x)) &&
    identical(source_digest,values$source_digest),
    "Synthesis sources changed. Reload Analysis before final verification.","USER_ACTION_REQUIRED")
  web_assert(!identical(web_b2_nr_one(x$narrative_source_finalization$status),"FINALIZED"),
    "The selected Analysis version was locked when you moved to Manuscript.","USER_ACTION_REQUIRED")
  idx<-web_b2_nr_batch_index(x)
  ids<-vapply(idx,function(z)z$result_id,character(1))
  web_assert(length(ids)>0L && all(nzchar(ids)) && !anyDuplicated(ids),
    "Result inventory must contain unique nonempty IDs.","USER_ACTION_REQUIRED")
  supplied<-vapply(values$rows,function(z)if(is.list(z))web_b2_nr_one(z$result_id) else "",character(1))
  web_assert(length(supplied)==length(ids) && !anyDuplicated(supplied) && setequal(supplied,ids),
    "The full, current result set must be reviewed together. Reload Analysis.","USER_ACTION_REQUIRED")
  new_reviews<-vector("list",length(idx))
  fields<-web_b2_nr_fields()
  pdf_checked<-new.env(parent=emptyenv())
  for(i in seq_along(idx)) {
    d<-idx[[i]]$detail;rid<-idx[[i]]$result_id
    web_assert(!is.null(d),paste("No unique PDF evidence for",rid),"USER_ACTION_REQUIRED")
    item<-values$rows[[match(rid,supplied)]]
    allowed<-c("result_id","mode","source_row_id","corrected_result","source_location","source_evidence")
    web_assert(is.list(item) && setequal(names(item),allowed) && length(item)==length(allowed) &&
      all(vapply(item,function(v)is.character(v)&&length(v)==1L&&!is.na(v)&&
        nchar(v,type="bytes")<=6000L,logical(1))),
      paste("Invalid verification fields for",rid),"VALIDATION_FAILED")
    web_assert(web_b2_nr_one(d$row$study_id) %in%
      unlist(x$final_study_selection$study_ids,use.names=FALSE),
      paste("Excluded study reached Analysis:",rid),"USER_ACTION_REQUIRED")
    screening<-web_b2_nr_one(d$row$screening_id)
    authors<-Filter(function(z)identical(web_b2_nr_one(z$screening_id),screening),
      x$tables$Author_Final_Review)
    web_assert(length(authors)==1L &&
      identical(web_b2_nr_one(authors[[1L]]$.pdf_sha256),d$checksum),
      paste("Original PDF registration does not match",rid),"USER_ACTION_REQUIRED")
    if(!exists(screening,envir=pdf_checked,inherits=FALSE)) {
      path<-web_inside(file.path(context$version$root,
        web_b2_nr_one(authors[[1L]]$.pdf_relative)),context$version$root)
      web_assert(identical(web_sha(path),d$checksum),
        paste("PDF checksum changed:",screening),"USER_ACTION_REQUIRED")
      assign(screening,TRUE,envir=pdf_checked)
    }
    canonical<-Filter(function(z)
      identical(web_b2_nr_one(z$original_pdf_result_id),rid) &&
        web_b2_nr_one(z$verification_status) %in% c("Verified","Corrected") &&
        nzchar(web_b2_nr_one(z$.verified_by)),x$tables$Target_Outcome_Verification)
    web_assert(!length(canonical),
      paste("Independently verified canonical numerical result cannot be overwritten in this UI:",rid),
      "USER_ACTION_REQUIRED")
    mode<-item$mode
    web_assert(mode %in% c("AI","CORRECTED","NOT_REPORTED"),
      paste("Select a valid source decision:",rid),"VALIDATION_FAILED")
    source_location<-trimws(item$source_location)
    source_evidence<-trimws(item$source_evidence)
    web_assert(nzchar(source_location) && nzchar(source_evidence),
      paste("A PDF source location and source excerpt are needed:",rid),"USER_ACTION_REQUIRED")
    rvalues<-as.list(stats::setNames(rep("",length(fields)),fields))
    reported<-"";sourceid<-"";decision<-"";rationale<-""
    if(identical(mode,"AI")) {
      src<-idx[[i]]$candidate
      sourceid<-item$source_row_id
      web_assert(!is.null(src) && identical(sourceid,web_b2_nr_one(src$.row_id)),
        paste("No unique source-matched AI result for",rid,
          "— enter the exact PDF result in Correction instead."),"USER_ACTION_REQUIRED")
      web_assert(!nzchar(trimws(item$corrected_result)),
        paste("AI confirmation and a correction cannot be combined:",rid),"VALIDATION_FAILED")
      rvalues<-lapply(fields,function(k)web_b2_nr_one(src[[k]]));names(rvalues)<-fields
      for (k in c("intervention_events","intervention_total","comparator_events","comparator_total",
                  "intervention_n","comparator_n")) if(nzchar(rvalues[[k]]))
        web_assert(grepl("^[0-9]+$",rvalues[[k]]),
          paste("AI numerical source needs correction:",rid,k),"USER_ACTION_REQUIRED")
      for (k in c("intervention_mean","intervention_sd","comparator_mean","comparator_sd",
                  "effect_estimate","lower_ci","upper_ci")) if(nzchar(rvalues[[k]]))
        web_assert(grepl("^-?([0-9]+(\\.[0-9]+)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$",rvalues[[k]]),
          paste("AI numerical source needs correction:",rid,k),"USER_ACTION_REQUIRED")
      web_assert(identical(nzchar(rvalues$lower_ci),nzchar(rvalues$upper_ci)),
        paste("AI confidence interval needs checking:",rid),"USER_ACTION_REQUIRED")
      if(nzchar(rvalues$lower_ci))web_assert(as.numeric(rvalues$lower_ci)<=as.numeric(rvalues$upper_ci),
        paste("AI confidence interval is reversed:",rid),"USER_ACTION_REQUIRED")
      for(a in c("intervention","comparator")){
        ev<-rvalues[[paste0(a,"_events")]];tot<-rvalues[[paste0(a,"_total")]]
        if(nzchar(ev) && nzchar(tot))web_assert(as.numeric(ev)<=as.numeric(tot),
          paste("AI event count exceeds denominator:",rid),"USER_ACTION_REQUIRED")
      }
      web_assert(!nzchar(rvalues$effect_measure) ||
        rvalues$effect_measure %in% c("RR","OR","HR","RD","MD","SMD"),
        paste("AI effect measure needs checking:",rid),"USER_ACTION_REQUIRED")
      if(rvalues$effect_measure %in% c("RR","OR","HR"))for(k in c("effect_estimate","lower_ci","upper_ci"))
        if(nzchar(rvalues[[k]]))web_assert(as.numeric(rvalues[[k]])>0,
          paste("AI ratio estimate must be positive:",rid,k),"USER_ACTION_REQUIRED")
      reported<-web_b2_nr_one(src$reported_result)
      decision<-"CONFIRMED";rationale<-"Batch human confirmation of the source-bound AI PDF proposal; unchanged original AI evidence retained."
    } else if(identical(mode,"CORRECTED")) {
      reported<-trimws(item$corrected_result)
      web_assert(nzchar(reported),paste("Enter the corrected PDF result for",rid),"USER_ACTION_REQUIRED")
      decision<-"CORRECTED";rationale<-"Human-transcribed result corrected against the original PDF; previous AI values not reused."
    } else {
      web_assert(!nzchar(trimws(item$corrected_result)),
        paste("'Not reported' cannot include a numerical correction:",rid),"VALIDATION_FAILED")
      decision<-"NOT_REPORTED";rationale<-"Human checked the PDF and confirmed no numerical value for this exact endpoint/timepoint."
    }
    new_reviews[[i]]<-list(result_id=rid,screening_id=screening,
      study_id=web_b2_nr_one(d$row$study_id),pdf_sha256=d$checksum,source_digest=source_digest,
      source_row_id=sourceid,decision=decision,source_location=source_location,
      source_evidence=source_evidence,reported_result=reported,rationale=rationale,
      values=rvalues,verified_by=actor,verified_at=web_now(),
      scope="NARRATIVE_ONLY_SOURCE_CHECK; NOT_CANONICAL_R26",batch_verification=TRUE)
  }
  old<-if(is.list(x$narrative_numeric_reviews)) x$narrative_numeric_reviews else list()
  x$narrative_numeric_reviews<-c(Filter(function(r) !web_b2_nr_one(r$result_id) %in% ids,old),new_reviews)
  x$narrative_analysis<-NULL
  x$narrative_batch_verification<-list(status="HUMAN_VERIFIED_EDITABLE",source_digest=source_digest,
    result_ids=as.list(ids),actor=actor,at=web_now())
  x$history<-c(x$history,list(list(event="NARRATIVE_BATCH_FINAL_VERIFICATION",at=web_now(),
    actor=actor,source_version=version_id,result_ids=as.list(ids),source_digest=source_digest)))
  root<-tempfile("narrative-batch-verification-",settings$data_root)
  dir.create(root)
  on.exit(unlink(root,recursive=TRUE))
  web_copy_version(con,settings,actor,project_id,version_id,root)
  web_b2_write(root,x)
  web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,x$stage)
}
web_b2_nr_batch_freeze <- function(con,settings,actor,project_id,version_id,values) {
  web_assert(is.list(values)&&setequal(names(values),c("source_digest","decision"))&&
    identical(values$decision,"FREEZE_AND_CONTINUE")&&
    is.character(values$source_digest)&&length(values$source_digest)==1L,
    "Invalid narrative finalization request.","VALIDATION_FAILED")
  context<-web_b2_context(con,settings,actor,project_id,version_id)
  x<-context$packet
  digest<-web_b2_nr_one(x$synthesis_execution$source_digest)
  web_assert(identical(digest,values$source_digest) &&
    identical(digest,web_b2_synthesis_source_digest(x)),
    "Synthesis sources are stale. Reload Analysis.","USER_ACTION_REQUIRED")
  web_assert(!identical(web_b2_nr_one(x$narrative_source_finalization$status),"FINALIZED"),
    "This Analysis version was already finalized.","USER_ACTION_REQUIRED")
  z<-x$narrative_batch_verification
  web_assert(is.list(z) && identical(z$status,"HUMAN_VERIFIED_EDITABLE") &&
    identical(z$source_digest,digest),"Final Verification must be saved first.","USER_ACTION_REQUIRED")
  x$.synthesis_execution_fresh<-TRUE
  a<-web_b2_narrative_assess(x)
  web_assert(!identical(a$status,"PENDING") && a$blocking==0L && a$numeric_unverified==0L,
    "Some results lack current PDF-based human verification.","USER_ACTION_REQUIRED")
  # Risk of bias remains visible as an independent scientific requirement.
  # Do not convert missing RoB judgments into verified data.
  x$narrative_source_finalization<-list(status="FINALIZED",source_digest=digest,
    actor=actor,at=web_now(),unresolved_rob=!isTRUE(a$rob_complete),
    scope="NARRATIVE_ANALYSIS_LOCKED; NOT_CANONICAL_R26")
  x$history<-c(x$history,list(list(event="NARRATIVE_ANALYSIS_FINALIZED",actor=actor,
    at=web_now(),source_version=version_id,unresolved_rob=!isTRUE(a$rob_complete))))
  root<-tempfile("narrative-analysis-finalization-",settings$data_root)
  dir.create(root)
  on.exit(unlink(root,recursive=TRUE))
  web_copy_version(con,settings,actor,project_id,version_id,root)
  web_b2_write(root,x)
  web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,x$stage)
}
