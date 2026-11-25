# RC28.25.9BCD.6.3.6 -- policy-locked, fail-closed AI QC child.
# Only bounded error codes and approved scientific QC results leave this process.
# Never emit request text, provider response bodies, tokens, or local file paths.

srma_qc_child_add_user_lib <- function() {
  appdata <- Sys.getenv("LOCALAPPDATA", unset="")
  if (!nzchar(appdata)) return(invisible(FALSE))
  minor <- strsplit(as.character(R.version$minor), ".", fixed=TRUE)[[1L]][[1L]]
  lib <- file.path(appdata, "R", "win-library", paste0(R.version$major, ".", minor))
  if (dir.exists(lib) && !lib %in% .libPaths()) {
    .libPaths(c(.libPaths(), lib))
  }
  invisible(TRUE)
}

srma_qc_child_missing <- function() {
  srma_qc_child_add_user_lib()
  required <- c("jsonlite", "digest", "httr2")
  required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
}

srma_qc_child_error <- function(e, phase) {
  msg <- conditionMessage(e)
  fixed <- c("QC_POLICY_INTEGRITY_FAILED", "QC_CONTEXT_INVALID",
    "QC_PROVIDER_UNAVAILABLE", "QC_MODEL_CONFIG_INVALID",
    "QC_AI_RESPONSE_INCOMPLETE", "QC_AI_RESPONSE_FAILED",
    "QC_EMPTY_AI_OUTPUT", "QC_INVALID_JSON")
  code <- if (msg %in% fixed) msg else "QC_CHILD_UNEXPECTED_ERROR"
  missing <- character()
  if (startsWith(msg,"QC_CHILD_DEPENDENCY_MISSING:")) {
    code <- "QC_CHILD_DEPENDENCY_MISSING"
    entries <- strsplit(sub("^QC_CHILD_DEPENDENCY_MISSING:","",msg),",",fixed=TRUE)[[1L]]
    missing <- entries[grepl("^[A-Za-z][A-Za-z0-9.]*$",entries)]
    missing <- unique(head(missing, 10L))
  }
  if (identical(phase,"PROVIDER_REQUEST") && identical(code,"QC_CHILD_UNEXPECTED_ERROR")) {
    # Extract HTTP status only. Do not reveal provider error message or response body.
    hit <- regmatches(msg, regexpr("(^|[^0-9])(400|401|402|403|404|408|413|422|429|500|502|503|504)([^0-9]|$)",msg,perl=TRUE))
    status <- gsub("[^0-9]","",hit)
    if (length(status)==1L && nzchar(status)) code <- paste0("QC_PROVIDER_HTTP_", status)
    else if (grepl("timeout|timed out",msg,ignore.case=TRUE)) code <- "QC_PROVIDER_TIMEOUT"
    else code <- "QC_PROVIDER_REQUEST_FAILED"
  }
  if (identical(phase,"PROVIDER_RESPONSE") && identical(code,"QC_CHILD_UNEXPECTED_ERROR"))
    code <- "QC_PROVIDER_BODY_INVALID"
  if (identical(phase,"PARSE_AI_JSON") && identical(code,"QC_CHILD_UNEXPECTED_ERROR"))
    code <- "QC_AI_OUTPUT_PROCESSING_FAILED"
  list(status="NOT_ASSESSED",error_code=code,phase=phase,
    missing_packages=as.list(missing))
}

srma_qc_child_parse_response <- function(obj) {
  if (!is.list(obj)) stop("QC_PROVIDER_BODY_INVALID",call.=FALSE)
  if (identical(obj$status,"incomplete")) stop("QC_AI_RESPONSE_INCOMPLETE",call.=FALSE)
  if (identical(obj$status,"failed")) stop("QC_AI_RESPONSE_FAILED",call.=FALSE)
  if (identical(obj$status,"cancelled")) stop("QC_AI_RESPONSE_FAILED",call.=FALSE)
  chunks <- character()
  if (is.list(obj$output)) {
    for (z in obj$output) {
      if (is.list(z) && is.list(z$content)) {
        for (entry in z$content) {
          if (is.list(entry) && identical(entry$type,"output_text") && is.character(entry$text))
            chunks <- c(chunks,entry$text)
        }
      }
    }
  }
  if (!length(chunks) && is.character(obj$output_text)) chunks <- obj$output_text
  raw <- trimws(paste(chunks,collapse="\n"))
  if (!nzchar(raw)) stop("QC_EMPTY_AI_OUTPUT",call.=FALSE)
  raw <- sub("^```[[:space:]]*(json)?[[:space:]]*", "", raw, perl=TRUE,ignore.case=TRUE)
  raw <- sub("[[:space:]]*```[[:space:]]*$", "", raw, perl=TRUE)
  qc <- tryCatch(jsonlite::fromJSON(raw,simplifyVector=FALSE),error=function(e)NULL)
  if (!is.list(qc) || is.null(names(qc))) stop("QC_INVALID_JSON",call.=FALSE)
  qc
}

srma_qc_child_self_test <- function() {
  stopifnot(identical(srma_qc_child_error(simpleError("QC_PROVIDER_UNAVAILABLE"),"VALIDATE_PROVIDER")$error_code,"QC_PROVIDER_UNAVAILABLE"))
  stopifnot(identical(srma_qc_child_error(simpleError("HTTP 401 unauthorized"),"PROVIDER_REQUEST")$error_code,"QC_PROVIDER_HTTP_401"))
  stopifnot(identical(srma_qc_child_error(simpleError("HTTP 429 rate limited"),"PROVIDER_REQUEST")$error_code,"QC_PROVIDER_HTTP_429"))
  stopifnot(identical(srma_qc_child_error(simpleError("QC_CHILD_DEPENDENCY_MISSING:httr2,digest"),"DEPENDENCIES")$error_code,"QC_CHILD_DEPENDENCY_MISSING"))
  if (requireNamespace("jsonlite",quietly=TRUE)) {
    v <- list(output=list(list(content=list(list(type="output_text",text='{"checks":[],"policy_id":"test"}')))),status="completed")
    parsed <- srma_qc_child_parse_response(v)
    stopifnot(is.list(parsed),identical(parsed$policy_id,"test"))
    bad <- tryCatch({srma_qc_child_parse_response(list(status="completed",output=list()));"BAD"},error=function(e)conditionMessage(e))
    stopifnot(identical(bad,"QC_EMPTY_AI_OUTPUT"))
    bad <- tryCatch({srma_qc_child_parse_response(list(status="incomplete"));"BAD"},error=function(e)conditionMessage(e))
    stopifnot(identical(bad,"QC_AI_RESPONSE_INCOMPLETE"))
    bad <- tryCatch({srma_qc_child_parse_response(list(status="completed",output=list(list(content=list(list(type="output_text",text="not-json"))))));"BAD"},error=function(e)conditionMessage(e))
    stopifnot(identical(bad,"QC_INVALID_JSON"))
  }
  cat("RC28.25.9BCD.6.3.6 R CHILD OFFLINE FIXTURES: PASS\n")
}

args <- commandArgs(trailingOnly=TRUE)
if (length(args)==1L && identical(args[[1L]],"--self-test")) {
  srma_qc_child_self_test()
  quit(save="no",status=0L)
}
if (length(args)!=3L) stop("QC_CHILD_INPUT_INVALID",call.=FALSE)
infile <- args[[1L]]
outfile <- args[[2L]]
root <- args[[3L]]
phase <- "INITIALIZE"
run <- tryCatch({
  phase <- "DEPENDENCIES"
  missing <- srma_qc_child_missing()
  if (length(missing))
    stop(paste0("QC_CHILD_DEPENDENCY_MISSING:",paste(missing,collapse=",")),call.=FALSE)
  phase <- "LOAD_RUNTIME"
  root <- normalizePath(root,winslash="/",mustWork=TRUE)
  source(file.path(root,"web/qc/runtime.R"),local=TRUE)
  phase <- "READ_INPUT"
  inp <- jsonlite::read_json(infile,simplifyVector=FALSE)
  phase <- "LOAD_POLICY"
  pol <- srma_qc_policy_load(root)
  if (!identical(srma_qc_one(inp$policy_sha256),pol$sha256) ||
      !identical(srma_qc_one(inp$policy_version),pol$version) ||
      !identical(srma_qc_one(inp$policy_id),pol$id))
    stop("QC_POLICY_INTEGRITY_FAILED",call.=FALSE)
  if (!is.list(inp$context) ||
      !identical(srma_qc_one(inp$context$policy_sha256),pol$sha256))
    stop("QC_CONTEXT_INVALID",call.=FALSE)
  phase <- "VALIDATE_PROVIDER"
  key <- Sys.getenv("OPENAI_API_KEY",unset="")
  if (!nzchar(key)) stop("QC_PROVIDER_UNAVAILABLE",call.=FALSE)
  model <- srma_qc_one(inp$model,"gpt-5.6")
  if (!grepl("^[a-zA-Z0-9._-]+$",model))
    stop("QC_MODEL_CONFIG_INVALID",call.=FALSE)
  prompt <- paste(
    "Examine the complete manuscript against the supplied verified source facts.",
    "Apply each applicable rule in the SYSTEM QC Master Policy. Do not infer performed analyses.",
    "Report missing source data as NOT_ASSESSED and never invent results or author approvals.",
    "Use exact frozen study IDs and artifact references. Do not silently convert final included k into outcome-specific k.",
    "Return JSON only in the exact schema of Policy Section 8. No markdown fences.",
    as.character(jsonlite::toJSON(inp$context,auto_unbox=TRUE,null="null",na="null",digits=NA)),
    sep="\n\n")
  phase <- "PROVIDER_REQUEST"
  response <- httr2::request("https://api.openai.com/v1/responses") |>
    httr2::req_headers(Authorization=paste("Bearer",key),`Content-Type`="application/json") |>
    httr2::req_body_json(list(model=model,max_output_tokens=12000L,
      input=list(list(role="system",content=list(list(type="input_text",text=pol$text))),
        list(role="user",content=list(list(type="input_text",text=prompt))))),auto_unbox=TRUE) |>
    httr2::req_timeout(seconds=240) |>
    httr2::req_perform()
  phase <- "PROVIDER_RESPONSE"
  obj <- httr2::resp_body_json(response,simplifyVector=FALSE)
  phase <- "PARSE_AI_JSON"
  qc <- srma_qc_child_parse_response(obj)
  list(status="COMPLETE",qc=qc)
}, error=function(e) srma_qc_child_error(e,phase))

# Parent reads this file even when the child exits nonzero.
# If jsonlite is absent entirely, the parent reports QC_CHILD_NO_OUTPUT.
if (requireNamespace("jsonlite",quietly=TRUE)) {
  jsonlite::write_json(run,outfile,auto_unbox=TRUE,null="null",na="null",pretty=TRUE)
}
if (!identical(run$status,"COMPLETE")) quit(save="no",status=2L)
