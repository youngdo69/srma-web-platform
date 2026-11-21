# RC28.25.9BCD.6: Web-only, policy-bound Scientific Publication QC.
# RC28.25.9BCD.6.3.6: safe child diagnostic protocol (no scientific source changes).
# Never edits canonical R/34-R/41, Human-finalized Evidence or manuscript artifacts.
srma_qc_policy_path <- "web/qc/prompts/SCIENTIFIC_PUBLICATION_QC_MASTER_v1.md"
srma_qc_policy_load <- function(root) {
  root <- normalizePath(root, winslash="/", mustWork=TRUE)
  manifest_path <- file.path(root,"web/qc/approved-qc-policy.json")
  policy_path <- file.path(root,srma_qc_policy_path)
  if(!file.exists(manifest_path) || !file.exists(policy_path))
    stop("QC_POLICY_INTEGRITY_FAILED: Approved QC policy or manifest missing.")
  if(nzchar(Sys.readlink(manifest_path)) || nzchar(Sys.readlink(policy_path)))
    stop("QC_POLICY_INTEGRITY_FAILED: Symbolic link not accepted.")
  expected <- jsonlite::read_json(manifest_path,simplifyVector=TRUE)
  stopifnot(is.list(expected))
  actual <- digest::digest(file=policy_path,algo="sha256")
  if(!identical(as.character(expected$policy_id),"SRMA_UNIVERSAL_SCIENTIFIC_PUBLICATION_QC") ||
     !identical(as.character(expected$policy_version),"1.0.0") ||
     !identical(as.character(expected$policy_sha256),actual) ||
     !identical(as.character(expected$path),srma_qc_policy_path))
    stop("QC_POLICY_INTEGRITY_FAILED: Approved master policy SHA-256 mismatch.")
  bytes <- file.info(policy_path)$size
  text <- paste(readLines(policy_path,encoding="UTF-8",warn=FALSE),collapse="\n")
  required <- c("## 0. Mandatory execution contract","## 2. Universal scientific QC",
                "## 3. Narrative synthesis","## 4. Quantitative pairwise meta-analysis",
                "## 5. IPD / reconstructed time-to-event","## 7. QC severity")
  if(!is.finite(bytes) || bytes<18000L || nchar(text,type="bytes")<18000L ||
     !all(vapply(required,grepl,logical(1),x=text,fixed=TRUE)))
    stop("QC_POLICY_INTEGRITY_FAILED: Truncated or unsupported policy.")
  list(text=text,sha256=actual,id=expected$policy_id,version=expected$policy_version)
}
srma_qc_one <- function(x,default="") {
  if(is.null(x)||!length(x))return(default)
  v<-as.character(unlist(x,recursive=TRUE,use.names=FALSE))
  if(!length(v)||is.na(v[[1L]])) default else trimws(v[[1L]])
}
srma_qc_profiles <- function(x) {
  route<-toupper(srma_qc_one(x$source$route))
  p<-"CORE"
  if(route=="NARRATIVE")p<-c(p,"NARRATIVE_SWIM")
  if(route %in% c("QUANTITATIVE","META_ANALYSIS","PAIRWISE") ||
     length(x$source$analyses)>0L)p<-c(p,"PAIRWISE_META")
  # IPD / NMA activation requires affirmative executed analysis metadata,
  # not mentions in a title or Discussion.
  meta<-x$source$analysis_profiles
  if(!is.null(meta)){
    flags<-toupper(as.character(unlist(meta,use.names=FALSE)))
    if(any(flags %in% c("IPD_SURVIVAL","RECONSTRUCTED_IPD","IPD")))p<-c(p,"IPD_SURVIVAL")
    if(any(flags %in% c("NMA","NETWORK_META")))p<-c(p,"NMA")
  }
  unique(p)
}
srma_qc_issue <- function(rule,status,detail,action="",ref="")
  list(rule_id=rule,status=status,detail=detail,action=action,source_ref=ref)
srma_qc_deterministic <- function(x,policy) {
  checks<-list()
  add<-function(...)checks[[length(checks)+1L]] <<- srma_qc_issue(...)
  if(is.null(x$docx$artifact_id) || !nzchar(srma_qc_one(x$docx$artifact_id)))
    add("C07","NOT_ASSESSED","No registered editable DOCX for selected manuscript version.","Select a completed manuscript version")
  else add("C07","PASS","Registered editable DOCX artifact is linked to this manuscript.",ref="manuscript.docx")
  if(!is.list(x$source) || !nzchar(srma_qc_one(x$source_sha256))) {
    add("C01","BLOCKED","Frozen manuscript source or source digest missing.")
  } else {
    # Deterministic integrity of selected packet's frozen source.
    actual<-web_c2_hash(x$source)
    if(!identical(actual,srma_qc_one(x$source_sha256)))
      add("C01","BLOCKED","Manuscript source digest does not match selected immutable version.")
    else add("C01","PASS","Frozen source digest matches packet.",ref="manuscript.source_sha256")
  }
  if("NARRATIVE_SWIM" %in% srma_qc_profiles(x)) {
    ids<-tryCatch(web_c2_9cd_human_ids(x),error=function(e)e)
    if(inherits(ids,"error"))
      add("C01","BLOCKED","Human Finalized Study Selection binding or included-study IDs are inconsistent.",ref="narrative.human_final_selection")
    else add("C01","PASS",paste(length(ids),"Human Finalized included study IDs source-bound; outcome contributors are separate."),ref="narrative.human_final_selection")
  } else {
    add("C01","NOT_ASSESSED","Quantitative final selection/analysis lineage requires verified canonical R/40 and matching source manifest.")
  }
  # Full-review inclusion count and outcome-specific k must never be conflated.
  analyses<-x$source$analyses
  if("PAIRWISE_META" %in% srma_qc_profiles(x)) {
    if(!is.list(analyses)||!length(analyses))
      add("Q01","BLOCKED","Quantitative analysis was requested without immutable approved analyses.")
    else add("Q01","NOT_ASSESSED","Approved analyses exist, but effect direction, estimates, CIs, weights and plots require canonical R/40 comparison.",
      "Import exact same-version R/40 QC and inspect outcome-specific study counts")
  }
  refs<-x$source$references$records
  if(!is.list(refs)||!length(refs))
    add("C05","BLOCKED","Verified reference library is absent.")
  else if(!all(vapply(refs,function(r)is.list(r)&&isTRUE(r$metadata_verified),logical(1))))
    add("C05","BLOCKED","At least one cited reference lacks verified metadata.")
  else add("C05","PASS","Verified reference metadata present; body-citation coverage must be checked by R/40.",ref="source.references")
  if("NARRATIVE_SWIM" %in% srma_qc_profiles(x) && isTRUE(x$source$narrative$unresolved_rob))
    add("C04","NOT_ASSESSED","Risk-of-bias judgments are not finalized.","Complete study-specific RoB before final publication")
  else add("C04","NOT_ASSESSED","No independently verified RoB/CERTAINTY audit was supplied to this QC invocation.")
  q<-x$production$qc$publication_9cd
  if(is.list(q) && length(q)) {
    add("C05","PASS","Publication source/asset preflight record is present; R/40 release acceptance remains separate.",ref="production.qc.publication_9cd")
  } else add("C05","NOT_ASSESSED","Production publication QC record not available.")
  # Never infer R/40 PASS from earlier Web source preflight.
  add("C05","NOT_ASSESSED","Standalone canonical R/40 scientific release result has not been imported and verified.","Run or import exact R/40 acceptance evidence")
  checks
}
# Retrieve the *same* authorized DOCX bytes as the working Manuscript Download UI.
# Never read raw storage locators or assume web_store$get returns bytes/path.
# Fail closed on binding/digest/ZIP errors, before any AI call.
# QC-only dependency preflight. Respect existing library order (including renv)
# and use the current user's installed R library only as a fallback. This never
# writes package files or changes the scientific engine's R configuration.
srma_qc_xml2_ready <- function() {
  if(requireNamespace("xml2",quietly=TRUE))return(invisible(TRUE))
  appdata<-Sys.getenv("LOCALAPPDATA",unset="")
  r_minor<-sub("\\..*$","",R.version$minor)
  r_series<-paste(R.version$major,r_minor,sep=".")
  user_candidates<-character()
  if(nzchar(appdata))
    user_candidates<-c(user_candidates,file.path(appdata,"R","win-library",r_series))
  configured<-Sys.getenv("R_LIBS_USER",unset="")
  if(nzchar(configured))
    user_candidates<-c(user_candidates,strsplit(configured,.Platform$path.sep,fixed=TRUE)[[1L]])
  user_candidates<-unique(path.expand(user_candidates[nzchar(user_candidates)]))
  user_candidates<-user_candidates[vapply(user_candidates,function(p)
    dir.exists(p)&&file.exists(file.path(p,"xml2","DESCRIPTION")),logical(1))]
  if(length(user_candidates))
    .libPaths(c(.libPaths(),user_candidates))
  if(!requireNamespace("xml2",quietly=TRUE))
    stop("QC_DOCX_XML2_MISSING_IN_APP",call.=FALSE)
  invisible(TRUE)
}
srma_qc_docx_bytes <- function(request,project_id,x) {
  trace_on <- isTRUE(getOption("srma.qc.docx.trace",FALSE))
  artifact_id <- srma_qc_one(x$docx$artifact_id)
  expected_sha <- tolower(srma_qc_one(x$docx$docx_sha256))
  if(!nzchar(artifact_id) || !grepl("^[a-f0-9]{64}$",expected_sha))
    stop("QC_DOCX_BINDING_MISSING",call.=FALSE)
  work <- tempfile("srma-qc-docx-")
  dir.create(work,recursive=TRUE,mode="0700")
  on.exit(unlink(work,recursive=TRUE,force=TRUE),add=TRUE)
  target <- file.path(work,"source.docx")
  # This Web action is already used by the working Editable DOCX Download button.
  # Some implementations return NULL despite writing the destination successfully.
  tryCatch(request("download",list(project_id=project_id,
    artifact_id=artifact_id,destination=target)),
    error=function(e)stop("QC_DOCX_AUTHORIZED_DOWNLOAD_FAILED",call.=FALSE))
  info <- if(file.exists(target))file.info(target)$size else NA_real_
  if(length(info)!=1L || is.na(info) || info<=0L)
    stop("QC_DOCX_AUTHORIZED_DOWNLOAD_EMPTY",call.=FALSE)
  actual_sha <- digest::digest(file=target,algo="sha256")
  if(!identical(tolower(actual_sha),expected_sha))
    stop("QC_DOCX_HASH_MISMATCH",call.=FALSE)
  if(trace_on)message("[SRMA QC TRACE] DOCX_SHA256=",tolower(actual_sha),
    " DOCX_BYTES=",info)
  # Read Word XML directly from its ZIP member; no temporary unzip path.
  # With some ZIP writers, the archive contains word/document.xml but no
  # separate word/ directory entry. Extraction may silently fail on Windows.
  listing <- tryCatch(utils::unzip(target,list=TRUE),
    error=function(e)stop("QC_DOCX_ZIP_INVALID",call.=FALSE))
  member <- which(listing$Name=="word/document.xml")
  if(length(member)!=1L)
    stop("QC_DOCX_XML_MISSING_OR_DUPLICATE",call.=FALSE)
  xml_length <- as.numeric(listing$Length[[member]])
  if(length(xml_length)!=1L || !is.finite(xml_length) || xml_length<10 ||
     xml_length>50*1024*1024)
    stop("QC_DOCX_XML_SIZE_INVALID",call.=FALSE)
  reader <- tryCatch(unz(target,"word/document.xml",open="rb"),
    error=function(e)stop("QC_DOCX_XML_READ_FAILED",call.=FALSE))
  xml_raw <- tryCatch(readBin(reader,what="raw",n=as.integer(xml_length)),
    error=function(e)structure(list(),class="srma_qc_read_error"))
  try(close(reader),silent=TRUE)
  if(inherits(xml_raw,"srma_qc_read_error"))
    stop("QC_DOCX_XML_READ_FAILED",call.=FALSE)
  if(length(xml_raw)!=xml_length)
    stop("QC_DOCX_XML_INCOMPLETE",call.=FALSE)
  if(trace_on)message("[SRMA QC TRACE] XML_BYTES=",length(xml_raw),
    " XML_SHA256=",digest::digest(xml_raw,algo="sha256",serialize=FALSE))
  xml_raw
}

srma_qc_docx_parse <- function(xml_raw) {
  old_libs <- .libPaths()
  on.exit(.libPaths(old_libs),add=TRUE)
  srma_qc_xml2_ready()
  trace_on <- isTRUE(getOption("srma.qc.docx.trace",FALSE))
  # Preserve strict XML validation; reveal the original failure only for opt-in
  # diagnostics. Never log XML payload, manuscript paragraphs, or credentials.
  doc <- tryCatch(xml2::read_xml(xml_raw,options=c("NOBLANKS","NONET")),
    warning=function(w) {
      if(trace_on)message("[SRMA QC TRACE] XML_WARNING_CLASS=",paste(class(w),collapse=","),
        " XML_WARNING=",substr(gsub("[\r\n]"," ",conditionMessage(w)),1L,250L))
      stop("QC_DOCX_XML_PARSE_FAILED",call.=FALSE)
    },
    error=function(e) {
      if(trace_on)message("[SRMA QC TRACE] XML_ERROR_CLASS=",paste(class(e),collapse=","),
        " XML_ERROR=",substr(gsub("[\r\n]"," ",conditionMessage(e)),1L,250L))
      stop("QC_DOCX_XML_PARSE_FAILED",call.=FALSE)
    })
  if(trace_on)message("[SRMA QC TRACE] XML_PARSE=PASS")
  if(length(xml2::xml_find_all(doc,"/*[local-name()='document']"))!=1L)
    stop("QC_DOCX_XML_ROOT_INVALID",call.=FALSE)
  paragraphs <- xml2::xml_find_all(doc,"//*[local-name()='p']")
  lines <- vapply(paragraphs,function(p)
    paste(xml2::xml_text(xml2::xml_find_all(p,".//*[local-name()='t']")),collapse=""),character(1))
  text <- paste(lines[nzchar(trimws(lines))],collapse="\n")
  if(!nzchar(trimws(text)))stop("QC_DOCX_TEXT_EMPTY",call.=FALSE)
  text
}
srma_qc_docx_text <- function(request,project_id,x) {
  srma_qc_docx_parse(srma_qc_docx_bytes(request,project_id,x))
}

# RC28.25.9BCD.6.3.5: redact non-scientific private metadata in QC request ONLY.
# Source evidence, saved versions, manuscript, and canonical files are unchanged.
# Never disclose sensitive values or paths in logs or the QC report.
srma_qc_private_pattern <- "sk-[A-Za-z0-9_-]{15,}|[A-Za-z]:[/\\\\]"
srma_qc_private_key <- function(key) {
  if(length(key)!=1L || is.na(key) || !nzchar(key))return(FALSE)
  grepl(paste0("(^|[._-])(",
    "api_key|access_token|refresh_token|auth_token|session_token|provider_token|",
    "password|credential|secret|private_key|authorization|",
    "storage_locator|diagnostic_locator|local_path|file_path|raw_path|temp_path|",
    "filesystem_path|repo_root|project_root|engine_root|upload_path|",
    "input_path|output_path|input_dir|output_dir|temp_dir",
    ")($|[._-])"),key,ignore.case=TRUE,perl=TRUE)
}
srma_qc_sanitize_source_facts <- function(facts) {
  audit<-new.env(parent=emptyenv())
  audit$masked_fields<-0L
  audit$masked_strings<-0L
  scrub<-function(value,key="",depth=0L) {
    if(depth>60L)stop("QC_CONTEXT_NOT_ASSESSED: Source nesting exceeds safety budget; no AI call.",call.=FALSE)
    if(srma_qc_private_key(key)) {
      audit$masked_fields<-audit$masked_fields+1L
      return("[QC_REDACTED_PRIVATE_METADATA]")
    }
    if(is.factor(value)) value<-as.character(value)
    if(is.character(value)) {
      return(vapply(value,function(v){
        if(is.na(v))return(NA_character_)
        if(grepl(srma_qc_private_pattern,v,perl=TRUE)) {
          audit$masked_strings<-audit$masked_strings+1L
          return("[QC_REDACTED_PRIVATE_SOURCE_STRING]")
        }
        v
      },character(1),USE.NAMES=FALSE))
    }
    if(is.data.frame(value)) {
      for(j in seq_along(value))
        value[[j]]<-scrub(value[[j]],names(value)[[j]],depth+1L)
      return(value)
    }
    if(is.list(value)) {
      nms<-names(value)
      for(j in seq_along(value)) {
        child_key<-if(is.null(nms) || is.na(nms[[j]])) "" else nms[[j]]
        value[j]<-list(scrub(value[[j]],child_key,depth+1L))
      }
      return(value)
    }
    value
  }
  safe<-scrub(facts)
  list(facts=safe,masked_fields=audit$masked_fields,
       masked_strings=audit$masked_strings)
}

# Safe, testable fail-closed transport contract for child failures.
# Never echo arbitrary child values, text, file paths, or provider error details.
srma_qc_child_failure_message <- function(raw) {
  allowed_codes<-c("QC_CHILD_DEPENDENCY_MISSING","QC_POLICY_INTEGRITY_FAILED",
    "QC_CONTEXT_INVALID","QC_PROVIDER_UNAVAILABLE","QC_MODEL_CONFIG_INVALID",
    "QC_AI_RESPONSE_INCOMPLETE","QC_AI_RESPONSE_FAILED","QC_EMPTY_AI_OUTPUT",
    "QC_INVALID_JSON","QC_CHILD_UNEXPECTED_ERROR","QC_PROVIDER_REQUEST_FAILED",
    "QC_PROVIDER_TIMEOUT","QC_PROVIDER_BODY_INVALID","QC_AI_OUTPUT_PROCESSING_FAILED",
    paste0("QC_PROVIDER_HTTP_",c(400,401,402,403,404,408,413,422,429,500,502,503,504)))
  allowed_phases<-c("INITIALIZE","DEPENDENCIES","LOAD_RUNTIME","READ_INPUT",
    "LOAD_POLICY","VALIDATE_PROVIDER","PROVIDER_REQUEST","PROVIDER_RESPONSE",
    "PARSE_AI_JSON")
  code<-srma_qc_one(raw$error_code,"QC_CHILD_INVALID_STATUS")
  phase<-srma_qc_one(raw$phase,"UNKNOWN")
  if(!code %in% allowed_codes)code<-"QC_CHILD_INVALID_STATUS"
  if(!phase %in% allowed_phases)phase<-"UNKNOWN"
  pkg_text<-""
  if(identical(code,"QC_CHILD_DEPENDENCY_MISSING")) {
    pkg<-unique(as.character(unlist(raw$missing_packages,use.names=FALSE)))
    pkg<-head(pkg[grepl("^[A-Za-z][A-Za-z0-9.]*$",pkg)],10L)
    if(length(pkg))pkg_text<-paste0(" MISSING_PACKAGES=",paste(pkg,collapse=","))
  }
  paste0("QC_AI_NOT_ASSESSED: ",code," [",phase,"]",pkg_text,
    "; no QC acceptance.")
}

# Pure child-library contract. Unlike implicit .libPaths inheritance, this is
# verifiable before AI calls and never mixes R 4.3 and R 4.6 package trees.
srma_qc_app_rscript <- function() {
  stem<-file.path(R.home("bin"),"Rscript")
  if(.Platform$OS.type=="windows" && file.exists(paste0(stem,".exe")))
    return(paste0(stem,".exe"))
  stem
}
srma_qc_child_library_env <- function(script,app_script,app_libs,
                                      configured="",inherited="") {
  canon<-function(s) {
    if(!nzchar(s) || !file.exists(s))return(NA_character_)
    tryCatch(tolower(normalizePath(s,winslash="/",mustWork=TRUE)),
      error=function(e)NA_character_)
  }
  same<-isTRUE(identical(canon(script),canon(app_script))) &&
    !is.na(canon(script))
  if(same){
    # Current App .libPaths() has already loaded the correct Web library.
    available<-as.character(app_libs)
    available<-available[!is.na(available)&nzchar(available)]
    available<-available[dir.exists(available)]
    preferred<-if(nzchar(configured))strsplit(configured,.Platform$path.sep,fixed=TRUE)[[1L]] else character()
    paths<-unique(c(preferred,available))
    return(paste(paths[nzchar(paths)],collapse=.Platform$path.sep))
  }
  # Dedicated R executable: only explicit, version-specific library inputs.
  # Never inject App 4.3 binary packages into a Scientific R 4.6 process.
  if(nzchar(configured))return(configured)
  inherited
}

srma_qc_ai <- function(x,doc_text,policy,project_id,version_number,engine) {
  # The scientific child receives full, verified policy as SYSTEM instruction.
  # No DB / Evidence writes and no credential serialized in a request.
  if(!nzchar(trimws(doc_text)))stop("QC_TEXT_NOT_ASSESSED: DOCX text unavailable; no AI call.")
  if(nchar(doc_text,type="bytes")>200000L)stop("QC_TEXT_NOT_ASSESSED: DOCX exceeds complete-context safety limit.")
  if(!nzchar(Sys.getenv("OPENAI_API_KEY",unset="")))
    stop("QC_PROVIDER_UNAVAILABLE: Provider credential unavailable; no AI call.")
  source_facts<-list(route=x$source$route,
    research_question=x$source$research_question,
    approved_protocol=x$source$protocol,
    registration=x$source$registration,
    search_provenance=x$source$search,
    screening=x$source$screening,
    evidence_manifest=x$source$evidence_manifest,
    analysis_profiles=x$source$analysis_profiles,
    reference_records=x$source$references$records,
    outcome_analyses=x$source$analyses,
    narrative=x$source$narrative,
    expected_study_inclusion=x$source$final_study_selection,
    human_selection=x$source$narrative$human_final_selection,
    study_artifacts=x$source$artifacts,
    approved_production_qc=x$production$qc)
  # Preserve the immutable source; only an ephemeral AI input copy is masked.
  # Manuscript content with a private pattern requires author review, not silent edits.
  if(grepl(srma_qc_private_pattern,doc_text,perl=TRUE))
    stop("QC_CONTEXT_NOT_ASSESSED: Manuscript contains sensitive credential or local path; no AI call.",call.=FALSE)
  privacy<-srma_qc_sanitize_source_facts(source_facts)
  context<-list(policy_id=policy$id,policy_version=policy$version,
    policy_sha256=policy$sha256,project_id=project_id,
    manuscript_version_number=as.integer(version_number),
    frozen_evidence_version_id=x$evidence_version,
    analysis_profiles=srma_qc_profiles(x),
    source_sha256=x$source_sha256,reference_sha256=x$reference_sha256,
    qc_context_privacy=list(private_source_fields_masked=privacy$masked_fields,
      private_source_strings_masked=privacy$masked_strings,
      instruction="Masked source fields must never support a PASS; if essential, mark NOT_ASSESSED."),
    frozen_evidence=privacy$facts,manuscript_full_text=doc_text)
  payload<-jsonlite::toJSON(context,auto_unbox=TRUE,null="null",na="null",digits=NA)
  if(grepl(srma_qc_private_pattern,as.character(payload),perl=TRUE))
    stop("QC_CONTEXT_NOT_ASSESSED: Private pattern remains after redaction; no AI call.",call.=FALSE)
  if(nchar(payload,type="bytes")>380000L)
    stop("QC_CONTEXT_NOT_ASSESSED: Complete factual context exceeds safe budget. Manual author QC required.")
  child<-file.path(engine,"web/qc/ai_child.R")
  if(!file.exists(child))stop("QC_ENGINE_NOT_ASSESSED: Approved AI child missing.")
  script<-Sys.getenv("SRMA_SCIENTIFIC_RSCRIPT",unset="")
  if(!nzchar(script)) script<-srma_qc_app_rscript()
  if(!file.exists(script))stop("QC_ENGINE_NOT_ASSESSED: Scientific Rscript not available.")
  private<-tempfile("srma-qc-run-");dir.create(private,recursive=TRUE,mode="0700")
  on.exit(unlink(private,recursive=TRUE,force=TRUE),add=TRUE)
  inp<-file.path(private,"input.json");out<-file.path(private,"output.json")
  req<-list(policy_sha256=policy$sha256,policy_version=policy$version,
    policy_id=policy$id,context=context,
    model=Sys.getenv("SRMA_QC_MODEL",unset=Sys.getenv("SRMA_MANUSCRIPT_MODEL",unset="gpt-5.6")))
  jsonlite::write_json(req,inp,auto_unbox=TRUE,null="null",na="null",digits=NA)
  # Child Rscript --vanilla does not inherit the parent process .libPaths().
  # Forward the active, already-approved App R library ONLY when the child uses
  # the exact same Rscript executable. An explicit separate Scientific Rscript
  # must receive only its own configured R_LIBS/SRMA_SCIENTIFIC_LIBRARY.
  lib<-Sys.getenv("SRMA_SCIENTIFIC_LIBRARY",unset="")
  previous<-Sys.getenv("R_LIBS",unset=NA_character_)
  child_libs<-srma_qc_child_library_env(
    script=script, app_script=srma_qc_app_rscript(),
    app_libs=.libPaths(), configured=lib,
    inherited=if(is.na(previous))"" else previous)
  if(nzchar(child_libs))Sys.setenv(R_LIBS=child_libs) else Sys.unsetenv("R_LIBS")
  on.exit(if(is.na(previous))Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS=previous),add=TRUE)
  run<-tryCatch(suppressWarnings(system2(script,shQuote(c("--vanilla",child,inp,out,engine)),
    stdout=TRUE,stderr=TRUE,timeout=300)),error=function(e)e)
  # A child may exit nonzero after writing a well-formed fail-closed diagnostic.
  # Never print child stdout/stderr: provider errors may include sensitive metadata.
  if(inherits(run,"error"))
    stop("QC_AI_NOT_ASSESSED: QC_CHILD_LAUNCH_FAILED; no QC acceptance.",call.=FALSE)
  if(!file.exists(out))
    stop("QC_AI_NOT_ASSESSED: QC_CHILD_NO_OUTPUT; no QC acceptance.",call.=FALSE)
  raw<-tryCatch(jsonlite::read_json(out,simplifyVector=FALSE),error=function(e)NULL)
  if(!is.list(raw))
    stop("QC_AI_NOT_ASSESSED: QC_CHILD_OUTPUT_UNREADABLE; no QC acceptance.",call.=FALSE)
  if(!identical(srma_qc_one(raw$status),"COMPLETE"))
    stop(srma_qc_child_failure_message(raw),call.=FALSE)
  if(!is.list(raw$qc))
    stop("QC_AI_NOT_ASSESSED: QC_CHILD_OUTPUT_SHAPE_INVALID; no QC acceptance.",call.=FALSE)
  result<-raw$qc
  if(!identical(srma_qc_one(result$policy_id),policy$id) ||
     !identical(srma_qc_one(result$policy_version),policy$version) ||
     !identical(srma_qc_one(result$policy_sha256),policy$sha256) ||
     !identical(srma_qc_one(result$project_id),project_id) ||
     !identical(srma_qc_one(result$frozen_evidence_version_id),srma_qc_one(x$evidence_version)) ||
     !identical(as.integer(result$manuscript_version_number),as.integer(version_number)) ||
     !is.list(result$checks))stop("QC_AI_OUTPUT_REJECTED: Policy/version mismatch or invalid JSON.")
  allowed<-c("PASS","WARNING","BLOCKED","NOT_ASSESSED","NOT_APPLICABLE")
  for(z in result$checks){
    if(!is.list(z)||!srma_qc_one(z$status) %in% allowed ||
       !nzchar(srma_qc_one(z$rule_id)))stop("QC_AI_OUTPUT_REJECTED: Invalid check.")
    if(identical(srma_qc_one(z$status),"PASS") && !length(z$evidence_refs))
      stop("QC_AI_OUTPUT_REJECTED: AI PASS without evidence references.")
  }
  if(privacy$masked_fields+privacy$masked_strings>0L) {
    result$checks<-c(result$checks,list(srma_qc_issue(
      "QC_INPUT_PRIVACY","WARNING",
      "Private storage/credential-like source metadata was masked in the AI QC copy; source evidence was not changed.",
      "Review any NOT_ASSESSED claim requiring masked metadata; manuscript DOCX remains unchanged.",
      "qc_context_privacy")))
  }
  result$qc_input_privacy<-list(private_source_fields_masked=privacy$masked_fields,
    private_source_strings_masked=privacy$masked_strings,
    original_evidence_unchanged=TRUE)
  result
}
srma_qc_status <- function(checks,ai=NULL) {
  states<-vapply(checks,function(v)srma_qc_one(v$status),character(1))
  if(is.list(ai) && length(ai$checks))states<-c(states,vapply(ai$checks,function(v)srma_qc_one(v$status),character(1)))
  if("BLOCKED" %in% states)return("BLOCKED")
  if("NOT_ASSESSED" %in% states || is.null(ai))return("NOT_ASSESSED")
  if("WARNING" %in% states)return("WARNING")
  "PASS"
}
# RC28.25.9BCD.6.3.7: restart-safe, version-bound QC reports.
# Scientific Evidence/Manuscript/DB remain immutable; these are derivative reports
# in an append-only, dedicated local data-root directory (no provider call on load).
srma_qc_report_key <- function(policy,project_id,version_id,x) {
  # Preserve the existing successful AI cache key to avoid a duplicate AI call.
  digest::digest(paste("qc-source-privacy-v1",policy$sha256,project_id,version_id,
    srma_qc_one(x$source_sha256),srma_qc_one(x$docx$docx_sha256),sep="|"),
    algo="sha256",serialize=FALSE)
}
srma_qc_cached_ai_valid <- function(saved,key,policy,project_id,version_number,x) {
  if(!is.list(saved) || !identical(srma_qc_one(saved$cache_key),key) ||
     !identical(srma_qc_one(saved$policy_sha256),policy$sha256) ||
     !identical(srma_qc_one(saved$evidence_version_id),srma_qc_one(x$evidence_version)) ||
     !identical(suppressWarnings(as.integer(srma_qc_one(saved$manuscript_version_number))),
                as.integer(version_number)))return(FALSE)
  z<-saved$ai
  if(!is.list(z) || !is.list(z$checks) ||
     !identical(srma_qc_one(z$policy_id),policy$id) ||
     !identical(srma_qc_one(z$policy_version),policy$version) ||
     !identical(srma_qc_one(z$policy_sha256),policy$sha256) ||
     !identical(srma_qc_one(z$project_id),as.character(project_id)) ||
     !identical(srma_qc_one(z$frozen_evidence_version_id),srma_qc_one(x$evidence_version)) ||
     !identical(suppressWarnings(as.integer(srma_qc_one(z$manuscript_version_number))),
                as.integer(version_number)))return(FALSE)
  allowed<-c("PASS","WARNING","BLOCKED","NOT_ASSESSED","NOT_APPLICABLE")
  all(vapply(z$checks,function(c) {
    is.list(c) && nzchar(srma_qc_one(c$rule_id)) &&
      srma_qc_one(c$status) %in% allowed &&
      (!identical(srma_qc_one(c$status),"PASS") || length(c$evidence_refs)>0L)
  },logical(1)))
}
srma_qc_report_dir <- function(settings,key) {
  if(!grepl("^[a-f0-9]{64}$",key))stop("QC_REPORT_KEY_INVALID",call.=FALSE)
  file.path(settings$data_root,"scientific-publication-qc",paste0(key,"-reports"))
}
srma_qc_report_record <- function(key,policy,project_id,version_id,x,result) {
  list(schema="SRMA_QC_REPORT_V1",cache_key=key,policy_sha256=policy$sha256,
    project_id=as.character(project_id),manuscript_version_id=as.character(version_id),
    evidence_version_id=srma_qc_one(x$evidence_version),
    source_sha256=srma_qc_one(x$source_sha256),
    reference_sha256=srma_qc_one(x$reference_sha256),
    docx_sha256=srma_qc_one(x$docx$docx_sha256),
    recorded_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
    report=result)
}
srma_qc_report_save <- function(settings,record) {
  dir<-srma_qc_report_dir(settings,srma_qc_one(record$cache_key))
  if(!dir.exists(dir) && !dir.create(dir,recursive=TRUE,showWarnings=FALSE,mode="0700"))
    return(FALSE)
  # Previous runs are never overwritten; latest valid snapshot is restored.
  stamp<-format(Sys.time(),"%Y%m%dT%H%M%OS3",tz="UTC")
  name<-paste0("qc-",stamp,"-",substr(digest::digest(paste(Sys.getpid(),
    runif(1L),sep="|"),algo="sha256"),1L,12L),".json")
  target<-file.path(dir,name)
  tmp<-tempfile(".srma-qc-",tmpdir=dir,fileext=".json")
  on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
  ok<-tryCatch({
    jsonlite::write_json(record,tmp,auto_unbox=TRUE,null="null",na="null",pretty=TRUE)
    Sys.chmod(tmp,mode="0600")
    file.rename(tmp,target)
  },error=function(e)FALSE)
  isTRUE(ok)
}
srma_qc_report_valid <- function(rec,key,policy,project_id,version_id,x) {
  is.list(rec) && identical(srma_qc_one(rec$schema),"SRMA_QC_REPORT_V1") &&
    identical(srma_qc_one(rec$cache_key),key) &&
    identical(srma_qc_one(rec$policy_sha256),policy$sha256) &&
    identical(srma_qc_one(rec$project_id),as.character(project_id)) &&
    identical(srma_qc_one(rec$manuscript_version_id),as.character(version_id)) &&
    identical(srma_qc_one(rec$evidence_version_id),srma_qc_one(x$evidence_version)) &&
    identical(srma_qc_one(rec$source_sha256),srma_qc_one(x$source_sha256)) &&
    identical(srma_qc_one(rec$reference_sha256),srma_qc_one(x$reference_sha256)) &&
    identical(srma_qc_one(rec$docx_sha256),srma_qc_one(x$docx$docx_sha256)) &&
    is.list(rec$report) &&
    srma_qc_one(rec$report$status) %in% c("PASS","WARNING","BLOCKED","NOT_ASSESSED") &&
    identical(srma_qc_one(rec$report$policy),policy$sha256) &&
    is.list(rec$report$checks)
}
srma_qc_report_restore <- function(settings,key,policy,project_id,version_id,x) {
  dir<-srma_qc_report_dir(settings,key)
  if(!dir.exists(dir))return(NULL)
  paths<-list.files(dir,pattern="^qc-[0-9]{8}T[0-9]{6}\\.[0-9]{3}-[a-f0-9]{12}\\.json$",
    full.names=TRUE)
  if(!length(paths))return(NULL)
  # Read the newest first and tolerate a corrupt or interrupted older snapshot.
  for(f in rev(sort(paths))) {
    if(is.na(file.info(f)$size) || file.info(f)$size>3000000L)next
    rec<-tryCatch(jsonlite::read_json(f,simplifyVector=FALSE),error=function(e)NULL)
    if(srma_qc_report_valid(rec,key,policy,project_id,version_id,x))return(rec$report)
  }
  NULL
}
srma_qc_safe_error <- function(e) {
  # No arbitrary messages, paths, secrets, or raw provider responses in saved UI.
  msg<-conditionMessage(e)
  if(grepl("^QC_[A-Z0-9_]+(: [A-Za-z0-9_ ,;.-]+)?$",msg,perl=TRUE))
    return(msg)
  "QC_RUN_UNEXPECTED_ERROR: Inspect safe child diagnostics; no acceptance."
}
srma_qc_request_error_hint <- function(e) {
  if(!inherits(e,"srma_web_error"))return("UNCLASSIFIED")
  if(identical(e$code,"VALIDATION_FAILED") &&
     identical(conditionMessage(e),"A project path is outside its permitted location."))
    return("DATA_ROOT_PATH_OUTSIDE")
  code<-srma_qc_one(e$code)
  if(grepl("^[A-Z_]+$",code))paste0("WEB_REQUEST_",code) else "WEB_REQUEST_ERROR"
}
srma_qc_restore_selected <- function(request,settings,con,project_id,version_id,on_error=function(e)NULL) {
  if(!nzchar(srma_qc_one(project_id)) || !nzchar(srma_qc_one(version_id)))return(NULL)
  tryCatch({
    policy<-srma_qc_policy_load(settings$engine_root)
    x<-request("manuscript_view",list(project_id=project_id,version_id=version_id))
    if(!is.list(x) || is.null(x$stage))return(NULL)
    key<-srma_qc_report_key(policy,project_id,version_id,x)
    restored<-srma_qc_report_restore(settings,key,policy,project_id,version_id,x)
    if(!is.null(restored))return(restored)
    # Legacy AI-only cache: restore an already-verified result without
    # running AI again. Never accept a cache without full policy/version binding.
    v<-web_query(con,"SELECT version FROM project_versions WHERE id=?v AND project_id=?p",
      v=version_id,p=project_id)
    if(nrow(v)!=1L)return(NULL)
    number<-as.integer(v$version[[1L]])
    legacy<-file.path(settings$data_root,"scientific-publication-qc",paste0(key,".json"))
    if(!file.exists(legacy))return(NULL)
    cached<-tryCatch(jsonlite::read_json(legacy,simplifyVector=FALSE),error=function(e)NULL)
    if(!srma_qc_cached_ai_valid(cached,key,policy,project_id,number,x))return(NULL)
    checks<-srma_qc_deterministic(x,policy)
    list(status=srma_qc_status(checks,cached$ai),number=number,policy=policy$sha256,
      evidence_version=x$evidence_version,checks=checks,ai=cached$ai,
      notice="Previously verified AI QC result restored; no new AI call.",
      publication_ready_release_allowed=FALSE)
  },error=on_error)
}


# One active AI call per frozen policy + Version + DOCX across App processes.
# Never delete a pre-existing lock automatically: stale locks require an
# operator audit, avoiding accidental duplicate paid API calls.
srma_qc_lock_acquire <- function(settings,key) {
  if(!grepl("^[a-f0-9]{64}$",key))stop("QC_REPORT_KEY_INVALID",call.=FALSE)
  parent <- file.path(settings$data_root,"scientific-publication-qc","active")
  if(!dir.exists(parent) && !dir.create(parent,recursive=TRUE,showWarnings=FALSE,mode="0700"))
    stop("QC_LOCK_STORAGE_UNAVAILABLE",call.=FALSE)
  path <- file.path(parent,paste0(key,".lock"))
  if(!dir.create(path,showWarnings=FALSE,mode="0700"))
    stop("QC_ALREADY_RUNNING_OR_LOCKED",call.=FALSE)
  path
}
srma_qc_lock_release <- function(path) {
  if(is.character(path) && length(path)==1L && nzchar(path) && dir.exists(path))
    unlink(path,recursive=TRUE,force=TRUE)
  invisible(TRUE)
}

# RC28.25.9BCD.6.3.9: Universal, deterministic QC evidence reconciliation.
# Operates on the exact saved QC result and immutable manuscript_view packet.
# Never calls an AI model, edits scientific evidence, modifies canonical selection,
# revises any AI finding, or authorizes publication release.
srma_qc_recon_schema <- "SRMA_QC_RECONCILIATION_V1"
srma_qc_recon_nonempty <- function(v) {
  if(is.null(v) || length(v)==0L)return(FALSE)
  if(is.character(v))return(any(!is.na(v) & nzchar(trimws(v))))
  if(is.list(v) || is.data.frame(v))return(length(v)>0L)
  if(is.logical(v))return(any(!is.na(v)))
  TRUE
}
srma_qc_recon_binding <- function(entry,x) {
  # Mere presence is a candidate, NOT a verified link. Explicit approval,
  # source verification, matching evidence version and stable reference required.
  is.list(entry) && isTRUE(entry$source_verified) &&
    identical(toupper(srma_qc_one(entry$approval_state)),"APPROVED") &&
    identical(srma_qc_one(entry$evidence_version_id),srma_qc_one(x$evidence_version)) &&
    nzchar(srma_qc_one(entry$source_ref))
}
srma_qc_recon_catalog <- function(x) {
  s<-x$source
  if(!is.list(s))s<-list()
  n<-s$narrative
  if(!is.list(n))n<-list()
  p<-x$production$qc
  if(!is.list(p))p<-list()
  ref_records<-if(is.list(s$references))s$references$records else NULL
  # Every candidate carries its exact internal source field. A missing
  # candidate is never mislabeled as an existing evidence record.
  choices<-list(
    protocol=list("source.approved_protocol"=s$approved_protocol,
      "source.protocol"=s$protocol,"source.registration"=s$registration),
    search=list("source.search"=s$search,
      "source.search_provenance"=s$search_provenance),
    prisma=list("source.prisma"=s$prisma,
      "source.screening"=s$screening,"production.qc.prisma"=p$prisma),
    selection=list("source.narrative.human_final_selection"=n$human_final_selection,
      "source.final_study_selection"=s$final_study_selection),
    outcomes=list("source.narrative.reportable_endpoints"=n$reportable_endpoints,
      "source.narrative.outcome_inventory"=n$outcome_inventory,
      "source.narrative.verified_outcomes"=n$verified_outcomes,
      "source.verified_outcomes"=s$verified_outcomes,
      "source.outcome_inventory"=s$outcome_inventory),
    rob=list("source.rob"=s$rob,"source.risk_of_bias"=s$risk_of_bias,
      "source.narrative.risk_of_bias"=n$risk_of_bias),
    certainty=list("source.certainty"=s$certainty,"source.grade"=s$grade,
      "source.narrative.certainty"=n$certainty,
      "source.narrative.grade"=n$grade),
    r40=list("source.r40_release"=s$r40_release,
      "production.qc.r40_release"=p$r40_release,
      "production.qc.r40_final_release"=p$r40_final_release,
      "source.r40_acceptance"=s$r40_acceptance),
    analysis=list("source.analyses"=s$analyses,
      "source.analysis_results"=s$analysis_results),
    references=list("source.references.records"=ref_records,
      "source.reference_records"=s$reference_records),
    artifacts=list("source.artifacts"=s$artifacts,
      "production.artifacts"=x$production$artifacts))
  names_out<-names(choices)
  result<-lapply(names_out,function(k) {
    options<-choices[[k]]
    present<-which(vapply(options,srma_qc_recon_nonempty,logical(1)))
    if(!length(present))return(list(name=k,source_ref="not_in_frozen_packet",
      present=FALSE,approved_binding=FALSE))
    index<-present[[1L]]
    value<-options[[index]]
    list(name=k,source_ref=names(options)[[index]],present=TRUE,
      approved_binding=srma_qc_recon_binding(value,x))
  })
  names(result)<-names_out
  result
}
srma_qc_recon_family <- function(rule_id,detail="") {
  id<-toupper(rule_id)
  hint<-toupper(substr(srma_qc_one(detail),1L,600L))
  if(id=="C02")return("pico")
  if(id=="C03")return("outcomes")
  if(id %in% c("C01","C04","C05")) {
    # Legacy deterministic checks reuse generic C01/C05 IDs. Disambiguate
    # by the specific check's own category words, not a review-specific topic.
    if(grepl("R/40|R40|SCIENTIFIC RELEASE",hint))return("r40")
    if(grepl("RISK.OF.BIAS|ROB|BIAS JUDGMENT",hint))return("rob")
    if(grepl("REFERENC|BIBLIO",hint))return("references")
    if(grepl("PROTOCOL|REGISTR",hint))return("protocol")
    if(grepl("PRISMA|SELECTION FLOW|SCREENING",hint))return("prisma")
    if(id=="C04")return("rob")
  }
  # Rule-family classification only; no topic, intervention or outcome hardcoding.
  if(grepl("PROTOCOL|REGISTR|PROSPERO",id))return("protocol")
  if(grepl("R/40|R40|RELEASE_QC|SCIENTIFIC_RELEASE",id))return("r40")
  if(grepl("GRADE|CERTAINTY|SWIM8|SWIM-8",id))return("certainty")
  if(grepl("ROB|RISK.OF.BIAS|BIAS_ASSESS",id))return("rob")
  if(grepl("PICO|COMPARATOR|ELIGIB|MIXED",id))return("pico")
  if(grepl("PRISMA|SCREEN|DEDU[PC]|SELECTION_FLOW",id))return("prisma")
  if(grepl("SEARCH|DATABASE",id))return("search")
  if(grepl("PRIMARY|SECONDARY|OUTCOME|ENDPOINT",id) || grepl("^C03($|[-_.])",id))return("outcomes")
  if(grepl("FOREST|STATISTIC|HETEROG|EFFECT_SIZE|ANALYSIS|META_REG|NMA|IPD",id))return("analysis")
  if(grepl("CITATION|REFERENCE|BIBLIO",id))return("references")
  if(grepl("FIGURE|TABLE|ARTIFACT",id))return("artifacts")
  if(grepl("SWIM",id))return("analysis")
  "other"
}
srma_qc_recon_action <- function(family,classification) {
  if(identical(classification,"AUTO_RESOLVED_BINDING"))return(
    "Approved version-bound source binding found; retain original QC finding until independent revalidation.")
  switch(family,
    protocol="Retrieve approved protocol/registration and verify eligibility against the frozen version.",
    prisma="Link executed selection flow and reconcile counts and exclusion reasons against Human Final Selection.",
    search="Import executed search dates/queries from the same review's canonical search record.",
    selection="Verify included-study IDs against Human Final Selection; do not change the approved list.",
    outcomes="Check prespecified outcomes against PDF-bound Human Finalized extraction; flag genuinely unreported items.",
    rob="Link study-specific approved RoB assessments, or request the minimum necessary reviewer judgments.",
    certainty="Link outcome-specific certainty assessments; request author judgment only where still required.",
    pico="Inspect study-specific eligibility and comparator differences; author determines applicability.",
    r40="Import exact-version canonical R/40 acceptance evidence; do not infer PASS from Web preflight.",
    analysis="Compare executed effect models, outcomes, plots and estimates with approved analytic outputs/R/40.",
    references="Cross-check included studies and cited references against source-verified bibliography.",
    artifacts="Bind and inspect the actual saved tables/figures; titles alone do not establish validity.",
    "Review frozen source and original AI QC finding; supply explicit source evidence or author judgment.")
}
srma_qc_recon_class <- function(family,record,source_digest_ok) {
  if(!isTRUE(source_digest_ok))return("SOURCE_VERIFICATION_REQUIRED")
  if(family=="pico")return("HUMAN_DECISION_REQUIRED")
  # Link previously finalized, version-bound assessments without re-asking
  # users to finalize unchanged RoB/certainty results. The original AI
  # finding still remains BLOCKED until independent source-bound QC.
  if(family %in% c("rob","certainty") && isTRUE(record$approved_binding))
    return("AUTO_RESOLVED_BINDING")
  if(family %in% c("rob","certainty"))return("HUMAN_DECISION_REQUIRED")
  # Only an explicitly approved, same-evidence-version binding can fix
  # a missing INPUT linkage. This never converts the original AI QC to PASS.
  if(family %in% c("protocol","r40") && isTRUE(record$approved_binding))
    return("AUTO_RESOLVED_BINDING")
  if(!isTRUE(record$present))return("SOURCE_DATA_MISSING_IN_PACKET")
  "SOURCE_VERIFICATION_REQUIRED"
}
srma_qc_recon_finding_digest <- function(qc) {
  checks<-c(if(is.list(qc$checks))qc$checks else list(),
    if(is.list(qc$ai) && is.list(qc$ai$checks))qc$ai$checks else list())
  val<-as.character(jsonlite::toJSON(checks,auto_unbox=TRUE,null="null",na="null",digits=NA))
  digest::digest(val,algo="sha256",serialize=FALSE)
}
srma_qc_reconcile <- function(qc,x,policy,project_id,version_id,version_number) {
  if(!is.list(qc) || !is.list(qc$checks) || !is.list(x$source))
    stop("QC_RECON_INPUT_UNAVAILABLE",call.=FALSE)
  if(!identical(srma_qc_one(qc$policy),srma_qc_one(policy$sha256)) ||
     !identical(srma_qc_one(qc$evidence_version),srma_qc_one(x$evidence_version)) ||
     !identical(suppressWarnings(as.integer(srma_qc_one(qc$number))),as.integer(version_number)))
    stop("QC_RECON_VERSION_MISMATCH",call.=FALSE)
  source_ok<-tryCatch(identical(web_c2_hash(x$source),srma_qc_one(x$source_sha256)),
    error=function(e)FALSE)
  catalog<-srma_qc_recon_catalog(x)
  if(!isTRUE(source_ok))catalog<-lapply(catalog,function(entry){
    entry$approved_binding<-FALSE
    entry
  })
  checks<-c(lapply(qc$checks,function(z)list(z=z,origin="DETERMINISTIC")),
    if(is.list(qc$ai) && is.list(qc$ai$checks))
      lapply(qc$ai$checks,function(z)list(z=z,origin="AI_ADVISORY")) else list())
  findings<-list()
  for(item in checks) {
    z<-item$z
    if(!is.list(z))next
    severity<-srma_qc_one(z$status)
    if(!severity %in% c("BLOCKED","NOT_ASSESSED","WARNING"))next
    code<-srma_qc_one(z$rule_id,"UNIDENTIFIED")
    if(!grepl("^[A-Za-z0-9_./-]{1,100}$",code))code<-"UNIDENTIFIED"
    family<-srma_qc_recon_family(code,srma_qc_one(z$detail,srma_qc_one(z$observed)))
    # Only source-bound, explicitly named proof may clear a missing link.
    source_family<-if(family=="pico")"selection" else if(family=="other")"artifacts" else family
    record<-catalog[[source_family]]
    if(is.null(record))record<-list(present=FALSE,approved_binding=FALSE,source_ref="not_bound")
    classification<-srma_qc_recon_class(family,record,source_ok)
    findings[[length(findings)+1L]]<-list(rule_id=code,original_status=severity,
      origin=item$origin,family=family,classification=classification,
      source_ref=record$source_ref,source_candidate_present=isTRUE(record$present),
      source_binding_approved=isTRUE(record$approved_binding),
      next_action=srma_qc_recon_action(family,classification))
  }
  classes<-c("AUTO_RESOLVED_BINDING","SOURCE_DATA_MISSING_IN_PACKET",
    "HUMAN_DECISION_REQUIRED","SOURCE_VERIFICATION_REQUIRED")
  counts<-as.list(setNames(vapply(classes,function(c)
    as.integer(sum(vapply(findings,function(f)identical(f$classification,c),logical(1)))),
    integer(1)),classes))
  # One actionable row per issue family/class, while preserving all originals.
  group_ids<-unique(vapply(findings,function(f)
    paste(f$classification,f$family,sep="|"),character(1)))
  tasks<-lapply(group_ids,function(g) {
    items<-Filter(function(z)identical(paste(z$classification,z$family,sep="|"),g),findings)
    one<-items[[1L]]
    list(classification=one$classification,family=one$family,source_ref=one$source_ref,
      finding_count=length(items),rule_ids=unique(vapply(items,`[[`,character(1),"rule_id")),
      next_action=one$next_action)
  })
  classification_order<-c("HUMAN_DECISION_REQUIRED","SOURCE_DATA_MISSING_IN_PACKET",
    "SOURCE_VERIFICATION_REQUIRED","AUTO_RESOLVED_BINDING")
  if(length(tasks))tasks<-tasks[order(vapply(tasks,function(t)
    match(t$classification,classification_order),integer(1)))]
  list(schema=srma_qc_recon_schema,recon_engine="RC28.25.9BCD.6.3.9",
    policy_sha256=policy$sha256,project_id=as.character(project_id),
    manuscript_version_id=as.character(version_id),manuscript_version_number=as.integer(version_number),
    frozen_evidence_version_id=srma_qc_one(x$evidence_version),
    source_sha256=srma_qc_one(x$source_sha256),docx_sha256=srma_qc_one(x$docx$docx_sha256),
    original_qc_status=srma_qc_one(qc$status),original_qc_finding_sha256=srma_qc_recon_finding_digest(qc),
    source_digest_verified=isTRUE(source_ok),
    generated_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
    # Present ≠ verified; no private fields, manuscript text or local locators exported.
    source_catalog=lapply(catalog,function(z)z[c("source_ref","present","approved_binding")]),
    counts=counts,original_finding_count=length(findings),findings=findings,tasks=tasks,
    scientific_qc_original_unchanged=TRUE,publication_ready_release_allowed=FALSE)
}
srma_qc_recon_path <- function(settings,key,finding_digest) {
  if(!grepl("^[a-f0-9]{64}$",key) || !grepl("^[a-f0-9]{64}$",finding_digest))
    stop("QC_RECON_KEY_INVALID",call.=FALSE)
  file.path(settings$data_root,"scientific-publication-qc",
    paste0(key,"-reconciliations"),paste0("recon-v1-",finding_digest,".json"))
}
srma_qc_recon_valid <- function(rec,qc,x,policy,pid,vid,number) {
  is.list(rec) && identical(srma_qc_one(rec$schema),srma_qc_recon_schema) &&
    identical(srma_qc_one(rec$policy_sha256),srma_qc_one(policy$sha256)) &&
    identical(srma_qc_one(rec$project_id),as.character(pid)) &&
    identical(srma_qc_one(rec$manuscript_version_id),as.character(vid)) &&
    identical(suppressWarnings(as.integer(srma_qc_one(rec$manuscript_version_number))),as.integer(number)) &&
    identical(srma_qc_one(rec$frozen_evidence_version_id),srma_qc_one(x$evidence_version)) &&
    identical(srma_qc_one(rec$source_sha256),srma_qc_one(x$source_sha256)) &&
    identical(srma_qc_one(rec$docx_sha256),srma_qc_one(x$docx$docx_sha256)) &&
    identical(srma_qc_one(rec$original_qc_finding_sha256),srma_qc_recon_finding_digest(qc)) &&
    identical(rec$publication_ready_release_allowed,FALSE) && is.list(rec$findings)
}
srma_qc_recon_safe_error <- function(e) {
  msg<-conditionMessage(e)
  # Never reveal local file paths, credentials, raw source facts or manuscript.
  if(grepl("^QC_(RECON|POLICY|REPORT)_[A-Z0-9_]+$",msg))return(msg)
  "QC_RECON_RUNTIME_ERROR"
}
srma_qc_recon_cached <- function(settings,key,qc,x,policy,pid,vid,number) {
  path<-srma_qc_recon_path(settings,key,srma_qc_recon_finding_digest(qc))
  if(file.exists(path) && !is.na(file.info(path)$size) && file.info(path)$size<3000000L) {
    rec<-tryCatch(jsonlite::read_json(path,simplifyVector=FALSE),error=function(e)NULL)
    if(srma_qc_recon_valid(rec,qc,x,policy,pid,vid,number))return(rec)
  }
  rec<-srma_qc_reconcile(qc,x,policy,pid,vid,number)
  parent<-dirname(path)
  if(!dir.exists(parent))dir.create(parent,recursive=TRUE,showWarnings=FALSE,mode="0700")
  if(dir.exists(parent) && !file.exists(path)) {
    tmp<-tempfile(".srma-recon-",tmpdir=parent,fileext=".json")
    on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
    tryCatch({
      jsonlite::write_json(rec,tmp,auto_unbox=TRUE,null="null",na="null",pretty=TRUE)
      Sys.chmod(tmp,mode="0600")
      # Never overwrite a previous immutable derivative report.
      if(!file.exists(path))file.rename(tmp,path)
    },error=function(e)NULL)
  }
  rec
}

# Canonical evidence reuse for the selected immutable manuscript.
# Bounded, authenticated, read-only retrieval from the SAME review. No AI,
# scientific reruns, manuscript edits, SQLite writes, or automatic QC approval.
srma_qc_link_schema <- "SRMA_QC_CANONICAL_AUTOLINK_V3"
srma_qc_link_one <- function(x, default="") srma_qc_one(x, default)
srma_qc_link_status <- function(x) toupper(srma_qc_link_one(x$status))
srma_qc_link_list <- function(x) if(is.list(x)) x else list()
srma_qc_link_ids <- function(x) {
  vals <- if(is.null(x)) character() else as.character(unlist(x,use.names=FALSE))
  unique(vals[!is.na(vals) & nzchar(trimws(vals))])
}
srma_qc_link_sha <- function(x) grepl("^[a-fA-F0-9]{64}$",srma_qc_link_one(x))
srma_qc_link_artifact_metadata <- function(rows,pid,versions) {
  if(!is.data.frame(rows) || !nrow(rows))return(list())
  needed <- c("id","project_id","version_id","logical_name","status","checksum","approval_state","job_id")
  if(!all(needed %in% names(rows)))return(list())
  keep <- !is.na(rows$project_id) & rows$project_id==pid &
    !is.na(rows$version_id) & rows$version_id %in% versions &
    !is.na(rows$status) & rows$status=="AVAILABLE" &
    !is.na(rows$id) & nzchar(rows$id)
  rows <- head(rows[keep,,drop=FALSE],1000L)
  if(!nrow(rows))return(list())
  lapply(seq_len(nrow(rows)),function(i) {
    # No absolute paths, storage locators, PHI or raw evidence in snapshots.
    name <- basename(gsub("\\\\","/",as.character(rows$logical_name[[i]])))
    name <- substr(name,1L,150L)
    list(id=as.character(rows$id[[i]]),version=as.character(rows$version_id[[i]]),
      kind=tolower(name),checksum=tolower(srma_qc_link_one(rows$checksum[[i]])),
      job=srma_qc_link_one(rows$job_id[[i]]),
      approval_state=srma_qc_link_one(rows$approval_state[[i]],"UNREVIEWED"),
      approved=identical(toupper(as.character(rows$approval_state[[i]])),"APPROVED"))
  })
}
srma_qc_link_artifact_family <- function(z) {
  k <- srma_qc_link_one(z$kind)
  if(grepl("prisma|(^|[/_.-])r35([/_.-]|$)",k))return("prisma")
  if(grepl("(^|[/_.-])r40([/_.-]|$)|scientific[_ -]?qc|numeric[_ -]?consistency",k))return("r40")
  if(grepl("risk[_ -]?of[_ -]?bias|(^|[/_.-])rob([/_.-]|$)",k))return("rob")
  if(grepl("grade|certainty",k))return("certainty")
  if(grepl("search|pubmed|embase|central",k))return("search")
  if(grepl("forest|meta[_ -]?analysis|heterogen|analysis",k))return("analysis")
  if(grepl("reference|bibliog",k))return("references")
  if(grepl("outcome|extract|evidence",k))return("outcomes")
  if(grepl("figure|table|plot",k))return("artifacts")
  if(grepl("protocol|prospero|registration",k))return("protocol")
  "other"
}
srma_qc_link_json_candidates <- function(artifacts) {
  Filter(function(a)isTRUE(a$approved) && srma_qc_link_sha(a$checksum) &&
    grepl("[.]json$",a$kind) && srma_qc_link_artifact_family(a)!="other",artifacts)
}
srma_qc_link_reference_set <- function(x) {
  references<-x$source$references
  records<-references$records
  if(!length(records) || !all(vapply(records,web_c2_ref_valid,logical(1))) ||
     !identical(web_c2_hash(references),x$reference_sha256))return(NULL)
  ids<-vapply(records,function(r)srma_qc_one(r$pmid),character(1))
  if(anyDuplicated(ids))return(NULL)
  web_c2_hash(unname(records[order(ids)]))
}
srma_qc_link_reference_approval <- function(request,con,pid,vid,x) {
  reference_set<-srma_qc_link_reference_set(x)
  if(is.null(reference_set))return(NULL)
  approvals<-web_query(con,paste(
    "SELECT m.version_id,m.parent_version_id,m.reference_sha256,g.id AS gate_id,g.gate_type,g.reviewer_id,g.decided_at",
    "FROM manuscript_versions m JOIN project_versions v ON v.id=m.version_id AND v.project_id=m.project_id",
    "JOIN project_versions target ON target.id=?v AND target.project_id=m.project_id",
    "JOIN gates g ON g.project_id=m.project_id AND g.status='APPROVED' AND g.decision='APPROVED'",
    "AND ((m.stage='APPROVED' AND g.gate_type='manuscript_review' AND g.version_id=m.parent_version_id)",
    "OR (m.stage='PLAN' AND g.gate_type='manuscript_plan' AND g.version_id=m.version_id))",
    "WHERE m.project_id=?p AND m.evidence_version_id=?e AND v.version<=target.version",
    "ORDER BY CASE WHEN m.reference_sha256=?h THEN 0 ELSE 1 END,v.version DESC,g.id LIMIT 100"),
    p=pid,v=vid,e=x$evidence_version,h=x$reference_sha256)
  for(i in seq_len(nrow(approvals))) {
    a<-as.list(approvals[i,])
    prior<-if(identical(a$version_id,vid))x else tryCatch(
      request("manuscript_view",list(project_id=pid,version_id=a$version_id)),error=function(e)NULL)
    if(!is.list(prior) || !identical(prior$evidence_version,x$evidence_version) ||
       !identical(prior$reference_sha256,a$reference_sha256) ||
       !identical(srma_qc_link_reference_set(prior),reference_set) ||
       !nzchar(srma_qc_one(a$reviewer_id)) || !nzchar(srma_qc_one(a$decided_at)))next
    if(a$gate_type=="manuscript_review" &&
       (!identical(prior$review$gate,a$gate_id) || !identical(prior$review$actor,a$reviewer_id)))next
    return(list(version=a$version_id,gate_id=a$gate_id,reference_set_sha256=reference_set))
  }
  NULL
}
srma_qc_link_inspect_approved_json <- function(read_artifact,pid,vid,x,qualified,selected) {
  out<-list()
  for(a in head(qualified,12L)) {
    fam<-srma_qc_link_artifact_family(a)
    z<-tryCatch({
      path<-read_artifact(a)
      size<-file.info(path)$size
      if(length(size)==1L && is.finite(size) && size<=1000000L &&
         identical(digest::digest(file=path,algo="sha256"),a$checksum))
        jsonlite::read_json(path,simplifyVector=FALSE)
    },error=function(e)NULL)
    if(!is.list(z) || !identical(srma_qc_link_one(z$project_id),pid) ||
       !identical(srma_qc_link_one(z$version_id),a$version) ||
       !identical(srma_qc_link_one(z$evidence_version_id),srma_qc_link_one(x$evidence_version)) ||
       !srma_qc_link_sha(z$source_sha256) ||
       !identical(srma_qc_link_one(z$source_sha256),srma_qc_link_one(x$source_sha256)) ||
       !isTRUE(z$source_verified) || !identical(z$approval_state,"APPROVED") ||
       !nzchar(srma_qc_link_one(z$source_ref)))next
    if(fam=="r40" && (!identical(a$version,vid) || !identical(z$module,"R40") ||
       !srma_qc_link_sha(x$docx$docx_sha256) ||
       !identical(z$docx_sha256,x$docx$docx_sha256)))next
    if(fam %in% c("rob","prisma") && (!length(selected) ||
       !setequal(srma_qc_link_ids(z$study_ids),selected)))next
    if(fam=="prisma" && !identical(srma_qc_link_one(z$included_studies),as.character(length(selected))))next
    if(fam=="certainty" && !length(srma_qc_link_ids(z$outcome_ids)))next
    out[[length(out)+1L]]<-list(family=fam,id=a$id,version=a$version,
      sha256=a$checksum,source_ref=z$source_ref,status=srma_qc_link_one(z$status),
      docx_sha256=srma_qc_link_one(z$docx_sha256),
      outcome_ids=srma_qc_link_ids(z$outcome_ids))
  }
  out
}

srma_qc_link_collect <- function(request,con,settings,pid,vid,x) {
  evidence_version<-srma_qc_link_one(x$evidence_version)
  source_version<-srma_qc_link_one(x$source$project_version)
  versions<-unique(c(vid,evidence_version,source_version,srma_qc_link_one(x$docx$version_id)))
  versions<-versions[nzchar(versions)]
  read_source<-function(expr)tryCatch(expr,error=function(e)NULL)
  # Query only the exact immutable lineage already authenticated by manuscript_view.
  slots<-paste0("?v",seq_along(versions))
  rows<-read_source(do.call(web_query,c(list(con,paste0(
    "SELECT id,project_id,version_id,logical_name,status,checksum,approval_state,job_id,storage_locator ",
    "FROM artifacts WHERE project_id=?p AND version_id IN (",paste(slots,collapse=","),
    ") ORDER BY CASE WHEN approval_state='APPROVED' THEN 0 ELSE 1 END,id LIMIT 1001"),p=pid),
    setNames(as.list(versions),substring(slots,2L)))))
  artifacts<-srma_qc_link_artifact_metadata(rows,pid,versions)
  # evidence_view verifies the immutable manifest and computes freshness before
  # projecting away excluded studies. Read only the matching registry metadata.
  e<-read_source(request("evidence_view",list(project_id=pid,version_id=evidence_version)))
  ev<-if(is.list(e))read_source({
    row<-web_query(con,"SELECT manifest_json FROM project_versions WHERE project_id=?p AND id=?v",
      p=pid,v=evidence_version)
    if(nrow(row)==1L)list(packet=e,version=list(manifest=web_parse(row$manifest_json[[1L]])))
  })
  ready<-if(identical(e$stage,"READY"))read_source({
    proof<-web_query(con,paste(
      "SELECT j.id AS job_id,g.id AS gate_id FROM evidence_versions e",
      "JOIN jobs j ON j.id=e.source_job_id AND j.project_id=e.project_id AND j.version_id=e.parent_version_id",
      "JOIN gates g ON g.project_id=e.project_id AND g.version_id=e.parent_version_id",
      "WHERE e.project_id=?p AND e.version_id=?v AND e.stage='READY'",
      "AND j.operation='finalize_evidence' AND j.state='COMPLETE'",
      "AND g.gate_type='b2_rob' AND g.status='APPROVED'"),p=pid,v=evidence_version)
    if(nrow(proof)==1L)as.list(proof[1L,])
  })
  protocol_available<-NULL
  protocol<-read_source({
    cfg<-request("configuration",list(project_id=pid,version_id=source_version))
    protocol_available<-length(cfg$protocol)>0L || length(cfg$registration)>0L
    config<-web_query(con,"SELECT manifest_json,config_hash,version FROM project_versions WHERE project_id=?p AND id=?v",
      p=pid,v=source_version)
    sha<-web_parse(config$manifest_json[[1L]])[["config/project_config.yml"]]
    # A canonical activation can prove approval of identical config bytes.
    # This links the historical approval, leaving the selected version's gates alone.
    gates<-web_query(con,paste(
      "SELECT g.id,g.version_id,g.status,g.reviewer_id,g.decided_at",
      "FROM gates g JOIN project_versions v ON v.id=g.version_id AND v.project_id=g.project_id",
      "LEFT JOIN version_origins o ON o.version_id=v.id AND o.project_id=v.project_id",
      "WHERE g.project_id=?p AND v.config_hash=?h AND v.version<=?n",
      "AND g.gate_type='protocol_registration'",
      "AND (v.id=?v OR o.kind='ACTIVATED' OR g.status='REJECTED')",
      "ORDER BY v.version DESC,g.id LIMIT 100"),p=pid,h=sha,n=config$version[[1L]],v=source_version)
    origin<-if(nrow(gates))gates$version_id[[1L]] else ""
    approval<-gates[gates$version_id==origin,,drop=FALSE]
    if(nrow(gates)<100L && nrow(approval) && !any(gates$status=="REJECTED") &&
       all(approval$status=="APPROVED" & !is.na(approval$reviewer_id) &
       nzchar(approval$reviewer_id) & !is.na(approval$decided_at) & nzchar(approval$decided_at)) &&
       srma_qc_link_sha(sha) && identical(sha,config$config_hash[[1L]]) &&
       protocol_available &&
       identical(web_c2_hash(cfg$protocol),web_c2_hash(x$source$protocol)) &&
       identical(web_c2_hash(cfg$registration),web_c2_hash(x$source$registration))) {
      historical<-if(identical(origin,source_version))cfg else
        request("configuration",list(project_id=pid,version_id=origin))
      origin_row<-web_query(con,"SELECT manifest_json FROM project_versions WHERE project_id=?p AND id=?v",
        p=pid,v=origin)
      origin_sha<-web_parse(origin_row$manifest_json[[1L]])[["config/project_config.yml"]]
      if(identical(origin_sha,sha) && identical(web_c2_hash(historical),web_c2_hash(cfg)))
        list(version=source_version,approval_version=origin,sha256=sha,
          approval_ids=as.list(approval$id))
    }
  })
  read_artifact<-function(a) {
    if(!dir.exists(file.path(settings$data_root,"objects")))return(NULL)
    row<-rows[match(a$id,rows$id),,drop=FALSE]
    web_store(settings)$get(row$storage_locator[[1L]],a$checksum)
  }
  list(evidence=ev,ready=ready,protocol=protocol,artifacts=artifacts,
    reference_approval=read_source(srma_qc_link_reference_approval(request,con,pid,vid,x)),
    protocol_material_available=protocol_available,
    read_artifact=read_artifact,registry_available=is.data.frame(rows),
    registry_limited=is.data.frame(rows) && nrow(rows)>1000L)
}

srma_qc_link_r35 <- function(payload,x,manifest,selected) {
  docx<-x$docx
  expected<-c("figure1_prisma.png","prisma_flow_audit.csv","human_final_prisma_reconciliation.json")
  same_job<-Filter(function(a)identical(a$job,docx$job_id) &&
    identical(a$version,docx$version_id),payload$artifacts)
  kinds<-vapply(same_job,function(a)a$kind,character(1))
  if(any(vapply(expected,function(k)sum(kinds==k)!=1L,logical(1))))return(NULL)
  records<-same_job[match(expected,kinds)]
  paths<-lapply(records,function(a)tryCatch({
    if(!srma_qc_link_sha(a$checksum))return(NULL)
    path<-payload$read_artifact(a)
    if(length(path)==1L && identical(web_sha(path),a$checksum))path
  },error=function(e)NULL))
  if(any(lengths(paths)!=1L))return(NULL)
  audit<-tryCatch({
    if(file.info(paths[[3L]])$size<=1000000L)
      jsonlite::read_json(paths[[3L]],simplifyVector=FALSE)
  },error=function(e)NULL)
  hashes<-audit$original_source_sha256
  count<-as.character(length(selected))
  if(!identical(audit$evidence_version,x$evidence_version) ||
     !setequal(srma_qc_link_ids(audit$human_final_study_ids),selected) ||
     !identical(srma_qc_one(audit$human_final_included_count),count) ||
     !identical(srma_qc_one(audit$staged_R24_included_count),count) ||
     !setequal(names(hashes),c("included_studies.csv","final_full_text_eligibility.csv","excluded_after_full_text.csv")) ||
     !all(vapply(names(hashes),function(k)
       srma_qc_link_sha(hashes[[k]]) &&
         sum(basename(names(manifest))==k & unlist(manifest)==hashes[[k]])==1L,logical(1))))return(NULL)
  list(version=docx$version_id,job=docx$job_id,manifest_sha256=web_c2_hash(manifest),
    docx_sha256=docx$docx_sha256,included_studies=length(selected),
    artifacts=lapply(records,function(a)a[c("id","kind","checksum","approval_state")]))
}

srma_qc_link_snapshot <- function(payload,x,pid,vid) {
  e<-payload$evidence$packet
  s<-x$source
  n<-s$narrative
  ev_version<-srma_qc_link_one(x$evidence_version)
  packet_sha<-payload$evidence$version$manifest[["config/generated/web_evidence.json"]]
  live_selection<-e$final_study_selection
  selected<-srma_qc_link_ids(live_selection$study_ids)
  frozen_ids<-if(identical(s$route,"NARRATIVE"))
    tryCatch(web_c2_9cd_human_ids(x),error=function(e)character()) else
      srma_qc_link_ids(s$evidence$final_study_selection$study_ids)
  manifest<-web_c2_9cd_manifest(x)
  same_manifest<-length(manifest)>0L && srma_qc_link_sha(packet_sha) &&
    identical(web_c2_hash(manifest),web_c2_hash(payload$evidence$version$manifest))
  final_selected<-same_manifest && length(selected)>0L &&
    length(unlist(live_selection$study_ids))==length(selected) &&
    identical(srma_qc_link_status(live_selection),"HUMAN_CONFIRMED") &&
    nzchar(srma_qc_link_one(live_selection$actor)) && nzchar(srma_qc_link_one(live_selection$at)) &&
    length(frozen_ids)>0L && setequal(selected,frozen_ids)
  source_digest<-srma_qc_link_one(n$source_digest)
  narrative_final<-final_selected && srma_qc_link_sha(source_digest) &&
    isTRUE(e$.synthesis_execution_fresh) &&
    identical(source_digest,e$synthesis_execution$source_digest) &&
    identical(source_digest,e$narrative_batch_verification$source_digest) &&
    identical(source_digest,e$narrative_source_finalization$source_digest) &&
    identical(srma_qc_link_status(e$human_outcome_harmonization),"HUMAN_CONFIRMED") &&
    identical(srma_qc_link_status(e$narrative_source_finalization),"FINALIZED") &&
    identical(srma_qc_link_status(e$narrative_batch_verification),"HUMAN_VERIFIED_EDITABLE")
  links<-list()
  add<-function(family,source,stage="UNVERIFIED",provenance=NULL,approval="UNCONFIRMED") {
    old<-links[[family]]
    if(is.null(old) || old$stage!="VERIFIED_SOURCE" ||
       (stage=="VERIFIED_SOURCE" && approval=="APPROVED" && old$approval!="APPROVED"))
      links[[family]] <<- list(present=TRUE,stage=stage,approval=approval,source=source,provenance=provenance)
  }
  verified<-function(family,source,provenance,approval="APPROVED")
    add(family,source,"VERIFIED_SOURCE",provenance,approval)
  catalog<-srma_qc_recon_catalog(x)
  for(fam in names(catalog)) {
    entry<-catalog[[fam]]
    if(isTRUE(entry$present))add(fam,entry$source_ref)
  }
  if(isTRUE(payload$protocol_material_available))add("protocol","canonical_configuration")
  for(a in payload$artifacts) {
    fam<-srma_qc_link_artifact_family(a)
    if(fam!="other")add(fam,"registered_artifact")
  }
  ev_proof<-list(version=ev_version,sha256=packet_sha)
  if(final_selected)verified("selection","human_final_selection",ev_proof)
  if(narrative_final)verified("outcomes","finalized_outcome_inventory",
    c(ev_proof,list(source_digest=source_digest)))
  rob_assessment<-e$.narrative_analysis_assessment
  if(narrative_final && identical(e$narrative_source_finalization$unresolved_rob,FALSE) &&
     isTRUE(rob_assessment$rob_complete) &&
     setequal(vapply(srma_qc_link_list(rob_assessment$rob),function(r)
       srma_qc_link_one(r$study_id),character(1)),selected))
    verified("rob","finalized_narrative_rob",c(ev_proof,list(source_digest=source_digest)))
  if(!is.null(payload$protocol))verified("protocol","protocol_registration_gate",payload$protocol)
  if(!is.null(payload$ready) && same_manifest) {
    rob_path<-"results/risk_of_bias/rob_final_combined.csv"
    rob_sha<-manifest[[rob_path]]
    legacy_ready<-!identical(s$route,"NARRATIVE") &&
      length(e$included_reports)>0L &&
      setequal(srma_qc_link_ids(e$included_reports),srma_qc_link_ids(s$evidence$included_reports))
    if(srma_qc_link_sha(rob_sha) && (final_selected || legacy_ready))
      verified("rob","approved_evidence_finalization",list(version=ev_version,
        sha256=rob_sha,approval=payload$ready))
    if(!identical(s$route,"NARRATIVE"))verified("outcomes","approved_analysis_ready_evidence",ev_proof)
  }
  q<-x$production$qc$publication_9cd
  docx<-x$docx
  prisma_conflict<-final_selected && !is.null(q$included_studies) &&
    !identical(srma_qc_link_one(q$included_studies),as.character(length(selected)))
  docx_registered<-any(vapply(payload$artifacts,function(a)
    identical(a$id,docx$artifact_id) && identical(a$job,docx$job_id) &&
      identical(a$version,docx$version_id) && identical(a$checksum,docx$docx_sha256),logical(1)))
  if(final_selected && !prisma_conflict &&
     identical(q$prisma_status,"CANONICAL_R35_PASS") && !is.null(q$included_studies) &&
     identical(q$source_manifest_sha256,web_c2_hash(manifest)) &&
     identical(docx$source_sha256,x$source_sha256) && srma_qc_link_sha(docx$docx_sha256) &&
     docx_registered) {
    prisma<-srma_qc_link_r35(payload,x,manifest,selected)
    if(!is.null(prisma)) {
      states<-vapply(prisma$artifacts,function(a)a$approval_state,character(1))
      approval<-if(any(states=="REJECTED"))"REJECTED" else if(all(states=="APPROVED"))"APPROVED" else "UNREVIEWED"
      verified("prisma","bound_r35_production_record",prisma,approval)
    }
  }
  records<-s$references$records
  reference_status<-srma_qc_one(s$reference_discovery$review_status)
  reference_status<-if(reference_status=="HUMAN_SELECTION_REQUIRED")reference_status else "UNREVIEWED"
  if(!is.null(srma_qc_link_reference_set(x)))
    verified("references","frozen_verified_reference_library",
      list(version=vid,sha256=x$reference_sha256,count=length(records),
        prior_approval=payload$reference_approval),
      if(!is.null(payload$reference_approval))"APPROVED" else reference_status)
  analysis_verified<-length(s$analyses)>0L && all(vapply(s$analyses,function(a)
    identical(a$stage,"APPROVED") && identical(a$evidence_version,ev_version) &&
      identical(a$result$status,"PASS") && nzchar(srma_qc_link_one(a$result_review$gate_id)) &&
      length(a$result$output_hashes)>0L &&
      all(vapply(a$result$output_hashes,srma_qc_link_sha,logical(1))),logical(1)))
  if(analysis_verified)
    verified("analysis","frozen_approved_analyses",
      list(versions=names(s$analyses),sha256=x$source_sha256))
  assets<-s$artifacts
  if(analysis_verified && length(assets) && all(vapply(assets,function(a)
    nzchar(srma_qc_link_one(a$id)) && srma_qc_link_one(a$analysis_version) %in% names(s$analyses) &&
      srma_qc_link_sha(a$sha256) && a$sha256 %in% unlist(
        s$analyses[[a$analysis_version]]$result$output_hashes,use.names=FALSE),logical(1))))
    verified("artifacts","frozen_approved_analysis_assets",
      lapply(assets,function(a)a[c("id","analysis_version","sha256")]))
  qualified<-srma_qc_link_json_candidates(payload$artifacts)
  inspected<-srma_qc_link_inspect_approved_json(payload$read_artifact,pid,vid,x,
    qualified,if(final_selected)selected else character())
  certainty_plan<-if(is.list(s$protocol))s$protocol$certainty else NULL
  certainty_outcomes<-if(is.list(certainty_plan))srma_qc_link_ids(certainty_plan$outcome_ids) else character()
  if(!length(certainty_outcomes))certainty_outcomes<-srma_qc_link_ids(n$analysis_groups)
  for(z in inspected) {
    # The production branch already knows the exact R/35 outputs. Approval
    # of a separate report cannot supersede those outputs' registry states.
    if(z$family=="prisma" && (length(q)>0L || prisma_conflict ||
       !identical(z$docx_sha256,x$docx$docx_sha256) ||
       !identical(z$version,vid) || z$status!="PASS"))next
    if(z$family=="references")next # A metadata artifact cannot replace a Human reference decision.
    if(z$family=="rob" && !z$status %in% c("PASS","FINALIZED"))next
    if(z$family=="certainty" && (z$status!="PASS" || !length(certainty_outcomes) ||
       !all(certainty_outcomes %in% z$outcome_ids)))next
    verified(z$family,"approved_artifact_json",z)
  }
  state<-list(families=links,approved_artifacts=inspected,
    registry_candidates=Filter(function(a)srma_qc_link_artifact_family(a)!="other",payload$artifacts),
    selection_final=final_selected,
    selection_conflict=length(selected)>0L && length(frozen_ids)>0L && !setequal(selected,frozen_ids),
    narrative_final=narrative_final,prisma_conflict=prisma_conflict,
    rob_pending=narrative_final && isTRUE(e$narrative_source_finalization$unresolved_rob),
    evidence_available=!is.null(e),registry_available=payload$registry_available,
    protocol_material_available=payload$protocol_material_available,
    registry_limited=isTRUE(payload$registry_limited) || length(qualified)>12L)
  list(links=links,meta=state,snapshot_sha256=web_c2_hash(state))
}

# Applicability is derived from frozen methods and the pinned policy's CORE and
# analysis-specific rules. A source link and a saved finding remain separate.
srma_qc_submission_evaluate <- function(x,qc,snapshot) {
  profiles<-srma_qc_profiles(x)
  quantitative<-any(profiles %in% c("PAIRWISE_META","IPD_SURVIVAL","NMA"))
  production<-length(x$production$qc$publication_9cd)>0L
  protocol<-x$source$protocol
  links<-snapshot$links
  approved<-function(family)identical(links[[family]]$stage,"VERIFIED_SOURCE") &&
    identical(links[[family]]$approval,"APPROVED")
  r40<-Filter(function(a)identical(a$family,"r40"),snapshot$meta$approved_artifacts)
  r40_pass<-length(r40)>0L && all(vapply(r40,function(a)identical(a$status,"PASS"),logical(1)))
  valid_rob<-approved("rob")
  assessment<-if(is.list(protocol))protocol$certainty else NULL
  certainty_required<-isTRUE(assessment) || (is.list(assessment) &&
    (isTRUE(assessment$required) || nzchar(srma_qc_one(assessment$method))))
  registration_required<-length(x$source$registration)>0L ||
    (is.list(protocol) && is.list(protocol$registration) && isTRUE(protocol$registration$required))
  r40_required<-quantitative || production ||
    (is.list(protocol) && isTRUE(protocol$r40_required))
  rob_config<-if(is.list(protocol))protocol$risk_of_bias else NULL
  rob_required<-!(approved("protocol") && is.list(rob_config) &&
    identical(rob_config$applicable,FALSE) && nzchar(srma_qc_one(rob_config$rationale)))
  blockers<-character()
  human<-list()
  add<-function(code)blockers<<-unique(c(blockers,code))
  if(!length(qc$ai$checks))add("SCIENTIFIC_QC_REPORT_MISSING")
  if(!approved("references"))add("REFERENCES_APPROVAL_PENDING")
  if(production || "NARRATIVE_SWIM" %in% profiles) {
    if(!approved("prisma"))add("PRISMA_APPROVAL_PENDING")
  }
  if("NARRATIVE_SWIM" %in% profiles && !isTRUE(snapshot$meta$narrative_final))
    add("NARRATIVE_EVIDENCE_FINALIZATION_PENDING")
  if("NARRATIVE_SWIM" %in% profiles && !production)
    add("NARRATIVE_RELEASE_ARCHIVE_PENDING")
  if(isTRUE(snapshot$meta$prisma_conflict) || isTRUE(snapshot$meta$selection_conflict))
    add("HUMAN_FINAL_SOURCE_CONFLICT")
  if(rob_required && !valid_rob) add("ROB_APPROVAL_PENDING")
  if(registration_required && !approved("protocol"))add("PROTOCOL_REGISTRATION_PENDING")
  if(certainty_required && !approved("certainty"))add("CERTAINTY_ASSESSMENT_PENDING")
  if(r40_required && !r40_pass) add("R40_REQUIRED_RESULT_PENDING")
  for(a in c(srma_qc_link_list(qc$checks),srma_qc_link_list(qc$ai$checks))) {
    id<-srma_qc_one(a$rule_id,"UNIDENTIFIED_SCIENTIFIC_QC")
    status<-srma_qc_one(a$status)
    detail<-srma_qc_one(a$detail,srma_qc_one(a$observed))
    family<-srma_qc_recon_family(id,detail)
    if(status=="BLOCKED") {
      add(id)
      next
    }
    if(!status %in% c("WARNING","NOT_ASSESSED"))next
    if(!quantitative && grepl("^[QM][0-9]",id) && status=="NOT_ASSESSED")next
    if(family=="pico" || (family=="references" && approved("references")) ||
       (family=="prisma" && approved("prisma")) || (family=="search" && approved("search")) ||
       (family=="certainty" && "NARRATIVE_SWIM" %in% profiles && grepl("^SWIM-?8",id) &&
          (!certainty_required || approved("certainty")))) {
      key<-paste0("publication-",substr(web_c2_hash(list(id,status,detail)),1L,16L))
      human[[key]]<-list(id=key,check="scientific_publication_qc",severity="MAJOR",
        affected=id,source=x$source_sha256,machine_result="REVIEW_REQUIRED",
        message=detail,mode="HUMAN",action="Record the evidence and scientific disposition.")
    } else if(status=="NOT_ASSESSED" && (
       (family=="outcomes" && grepl("PRIMARY|CRITICAL|^C03$",id,ignore.case=TRUE)) ||
       (family=="search" && !approved("search")) ||
       (family=="rob" && rob_required && !valid_rob) ||
       (family=="certainty" && certainty_required && !approved("certainty")) ||
       (family=="r40" && r40_required && !r40_pass) ||
       (family=="protocol" && !approved("protocol") &&
          (registration_required || !grepl("REGISTR",id,ignore.case=TRUE)))))add(id)
  }
  if(identical(qc$status,"BLOCKED") && !length(blockers))add("SCIENTIFIC_QC_BLOCKED")
  list(blockers=as.list(blockers),human=unname(human),r40=if(r40_pass)r40 else list(),
    profiles=as.list(profiles),
    finding_sha256=if(is.list(qc))srma_qc_recon_finding_digest(qc) else NULL,
    evidence_sha256=snapshot$snapshot_sha256)
}

srma_qc_submission <- function(request,con,settings,pid,vid,x) {
  policy<-srma_qc_policy_load(settings$engine_root)
  qc<-srma_qc_restore_selected(request,settings,con,pid,vid)
  version<-web_query(con,"SELECT version FROM project_versions WHERE project_id=?p AND id=?v",p=pid,v=vid)
  if(!is.list(qc) || !identical(qc$policy,policy$sha256) ||
     !identical(qc$evidence_version,x$evidence_version) || nrow(version)!=1L ||
     !identical(as.integer(qc$number),as.integer(version$version[[1L]])))
    return(list(blockers=list("SCIENTIFIC_QC_REPORT_MISSING"),human=list()))
  key<-srma_qc_report_key(policy,pid,vid,x)
  wrapped<-list(cache_key=key,policy_sha256=qc$policy,evidence_version_id=qc$evidence_version,
    manuscript_version_number=qc$number,ai=qc$ai)
  if(!srma_qc_cached_ai_valid(wrapped,key,policy,pid,qc$number,x))
    return(list(blockers=list("SCIENTIFIC_QC_REPORT_UNVERIFIED"),human=list()))
  rec<-srma_qc_reconcile(qc,x,policy,pid,vid,as.integer(qc$number))
  if(!isTRUE(rec$source_digest_verified))
    return(list(blockers=list("SCIENTIFIC_QC_SOURCE_MISMATCH"),human=list()))
  sources<-srma_qc_link_collect(request,con,settings,pid,vid,x)
  srma_qc_submission_evaluate(x,qc,srma_qc_link_snapshot(sources,x,pid,vid))
}
srma_qc_link_question <- function(family,qc,snapshot) {
  if(!isTRUE(snapshot$meta$selection_final))return(NULL)
  if(family=="rob" && isTRUE(snapshot$meta$rob_pending) &&
     !identical(snapshot$links$rob$stage,"VERIFIED_SOURCE"))
    return("선정된 연구의 미완료 비뚤림 위험 평가를 어떻게 판정하시겠습니까?")
  if(!family %in% c("pico","outcomes"))return(NULL)
  for(z in srma_qc_link_list(qc$ai$checks)) {
    detail<-srma_qc_one(z$detail,srma_qc_one(z$observed))
    if(!srma_qc_one(z$status) %in% c("BLOCKED","WARNING") ||
       srma_qc_recon_family(srma_qc_one(z$rule_id),detail)!=family)next
    if(family=="pico" && grepl("mixed ([[:alnum:]-]+ ){0,3}comparators?|partial(ly)? eligib|indirect evidence|혼합 비교|간접 근거",
       detail,ignore.case=TRUE))
      return("혼합 비교군이나 선정 기준을 일부만 충족한 연구의 결과를 어떻게 해석하시겠습니까?")
    if(family=="outcomes" && identical(snapshot$links$outcomes$stage,"VERIFIED_SOURCE") &&
       grepl("not poolable|incompatible (timepoints?|definitions?)|통합 불가|시점 불일치",detail,ignore.case=TRUE))
      return("정의나 측정 시점이 다른 결과를 별도로 보고하시겠습니까?")
  }
  NULL
}
srma_qc_link_family_label <- function(fam) {
  switch(fam,pico="연구 선정기준과 실제 비교군",rob="연구의 비뚤림 위험 평가",
    certainty="근거의 확실성 평가",protocol="연구계획서·등록정보",
    r40="최종 통계 검증",prisma="논문 선정 과정",search="문헌 검색 기록",
    outcomes="주요 결과지표",analysis="통계 분석 결과",references="참고문헌",
    artifacts="표와 그림",selection="최종 연구 선정",other="기타 검증 항목",fam)
}
srma_qc_link_build <- function(rec,qc,x,policy,pid,vid,num,snapshot) {
  if(!srma_qc_recon_valid(rec,qc,x,policy,pid,vid,num) ||
      !isTRUE(rec$source_digest_verified))stop("QC_AUTOLINK_SOURCE_VERSION_MISMATCH",call.=FALSE)
  families<-unique(vapply(srma_qc_link_list(rec$tasks),function(t)
    srma_qc_link_one(t$family),character(1)))
  families<-unique(c(families[nzchar(families)],names(snapshot$links)))
  narrative_only<-identical(srma_qc_profiles(x),c("CORE","NARRATIVE_SWIM"))
  result<-lapply(families,function(fam) {
    link<-snapshot$links[[fam]]
    task_rows<-Filter(function(t)identical(srma_qc_link_one(t$family),fam),rec$tasks)
    count<-sum(vapply(task_rows,function(t)
      as.integer(srma_qc_link_one(t$finding_count,"0")),integer(1)))
    ids<-unlist(lapply(task_rows,function(t)t$rule_ids),use.names=FALSE)
    not_applicable<-narrative_only && length(ids)>0L && all(grepl("^Q[0-9]",ids))
    stage<-if(not_applicable)"NOT_APPLICABLE" else if(!is.null(link))link$stage else
      if(isTRUE(snapshot$meta$registry_available) && isTRUE(snapshot$meta$evidence_available) &&
         !isTRUE(snapshot$meta$registry_limited))"NO_BOUND_SOURCE" else "SOURCE_ACCESS_INCOMPLETE"
    list(family=fam,status=stage,label=srma_qc_link_family_label(fam),
      present=!is.null(link),approval=if(is.null(link))"UNCONFIRMED" else link$approval,
      question=if(count>0L && !not_applicable)srma_qc_link_question(fam,qc,snapshot),
      finding_count=count,source_kind=if(is.null(link))"none" else srma_qc_link_one(link$source))
  })
  list(schema=srma_qc_link_schema,linker_version="RC28.25.9BCD.6.3.11",
    project_id=as.character(pid),manuscript_version_id=as.character(vid),
    manuscript_version_number=as.integer(num),policy_sha256=policy$sha256,
    evidence_version_id=srma_qc_link_one(x$evidence_version),
    source_sha256=srma_qc_link_one(x$source_sha256),
    docx_sha256=srma_qc_link_one(x$docx$docx_sha256),
    qc_finding_sha256=srma_qc_recon_finding_digest(qc),
    canonical_snapshot_sha256=snapshot$snapshot_sha256,
    selection_finalized=isTRUE(snapshot$meta$selection_final),
    narrative_source_verified=isTRUE(snapshot$meta$narrative_final),
    findings=result,provenance=snapshot$meta,
    original_qc_status=srma_qc_link_one(qc$status),
    scientific_qc_original_unchanged=TRUE,publication_ready_release_allowed=FALSE)
}
srma_qc_link_path <- function(settings,key,snapshot_sha,finding_sha) {
  if(!all(grepl("^[a-f0-9]{64}$",c(key,snapshot_sha,finding_sha))))
    stop("QC_AUTOLINK_KEY_INVALID",call.=FALSE)
  # Keep Windows paths short. The key includes project/version/policy, and
  # the derivative still validates the full snapshot and finding digests.
  name<-web_c2_hash(list(key,snapshot_sha,finding_sha))
  file.path(settings$data_root,"scientific-publication-qc","links-v3",paste0(name,".json"))
}
srma_qc_link_valid <- function(z,rec,qc,x,policy,pid,vid,num,snapshot) {
  is.list(z) && identical(srma_qc_link_one(z$schema),srma_qc_link_schema) &&
    identical(srma_qc_link_one(z$project_id),as.character(pid)) &&
    identical(srma_qc_link_one(z$manuscript_version_id),as.character(vid)) &&
    identical(as.integer(srma_qc_link_one(z$manuscript_version_number)),as.integer(num)) &&
    identical(srma_qc_link_one(z$policy_sha256),policy$sha256) &&
    identical(srma_qc_link_one(z$evidence_version_id),srma_qc_link_one(x$evidence_version)) &&
    identical(srma_qc_link_one(z$source_sha256),srma_qc_link_one(x$source_sha256)) &&
    identical(srma_qc_link_one(z$docx_sha256),srma_qc_link_one(x$docx$docx_sha256)) &&
    identical(srma_qc_link_one(z$qc_finding_sha256),srma_qc_recon_finding_digest(qc)) &&
    identical(srma_qc_link_one(z$canonical_snapshot_sha256),snapshot$snapshot_sha256) &&
    identical(z$publication_ready_release_allowed,FALSE) && is.list(z$findings)
}
srma_qc_link_cached <- function(settings,key,rec,qc,x,policy,pid,vid,num,snapshot) {
  f<-srma_qc_link_path(settings,key,snapshot$snapshot_sha256,
    srma_qc_recon_finding_digest(qc))
  if(file.exists(f) && !is.na(file.info(f)$size) && file.info(f)$size<2000000L) {
    z<-tryCatch(jsonlite::read_json(f,simplifyVector=FALSE),error=function(e)NULL)
    if(srma_qc_link_valid(z,rec,qc,x,policy,pid,vid,num,snapshot))return(z)
  }
  z<-srma_qc_link_build(rec,qc,x,policy,pid,vid,num,snapshot)
  parent<-dirname(f)
  if(!dir.exists(parent))dir.create(parent,recursive=TRUE,showWarnings=FALSE,mode="0700")
  if(dir.exists(parent) && !file.exists(f)) {
    tmp<-tempfile(".srma-autolink-",tmpdir=parent,fileext=".json")
    on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
    tryCatch({
      jsonlite::write_json(z,tmp,auto_unbox=TRUE,null="null",na="null",pretty=TRUE)
      Sys.chmod(tmp,"0600")
      if(!file.exists(f))file.rename(tmp,f)
    },error=function(e)NULL)
  }
  z
}

srma_qc_simple_ui <- function() {
  old<-web_c3_ui()
  if(!inherits(old,"shiny.tag"))stop("QC_UI_INTEGRITY_FAILED: Legacy Final QC tab is not a Shiny tabPanel")
  previous<-old$children
  old$children<-list(
    shiny::tags$div(class="srma-qc-simple",
      shiny::tags$style(shiny::HTML(".srma-qc-simple{max-width:920px;padding:14px 0 20px}.srma-qc-simple h3{font-weight:650;font-size:23px;margin:4px 0 10px}.srma-qc-simple .btn{margin-right:8px;margin-bottom:8px}.srma-qc-legacy{max-width:970px;margin-top:22px;border-top:1px solid #e2e8f0;padding-top:14px}.srma-qc-legacy summary,.srma-qc-detail summary{cursor:pointer;color:#475569}.srma-qc-findings{margin:12px 0;padding:14px;border:1px solid #dce2e8;border-radius:6px;background:#f8fafc}.srma-qc-live{padding:9px 12px;margin:10px 0;border:1px solid #e2e8f0;border-radius:6px;color:#475569}.srma-qc-live[data-state=running]{border-color:#bfdbfe;background:#eff6ff;color:#1d4ed8}.srma-qc-live[data-state=disconnected]{border-color:#fed7aa;background:#fff7ed}.srma-qc-summary-stats{font-size:13px;color:#475569;margin:8px 0}.srma-qc-findings li{margin:7px 0}.srma-qc-detail{margin:10px 0 0}.srma-qc-detail p{margin:6px 0 10px}")),
      shiny::tags$script(shiny::HTML("(function(){\n if(window.srmaQcFeedbackBound)return;\n window.srmaQcFeedbackBound=true;\n var pending=false;\n function status(message,mode){\n   var el=document.getElementById(\"srma-qc-live\");\n   if(!el)return;\n   el.textContent=message;\n   el.setAttribute(\"data-state\",mode||\"idle\");\n }\n function buttons(enabled){\n   [\"srma_qc_run\",\"srma_qc_retry\"].forEach(function(id){\n     var el=document.getElementById(id);\n     if(el)el.disabled=!enabled;\n   });\n }\n document.addEventListener(\"click\",function(ev){\n   var button=ev.target.closest?ev.target.closest(\"#srma_qc_run, #srma_qc_retry\"):null;\n   if(!button)return;\n   if(pending){ev.preventDefault();ev.stopImmediatePropagation();return;}\n   if(button.id===\"srma_qc_retry\" && !window.confirm(\"Retry failed AI QC for this Version? This may use API tokens.\")){\n     ev.preventDefault();ev.stopImmediatePropagation();return;\n   }\n   pending=true;\n   status(\"QC request submitted. Checking saved results or running scientific QC; please do not click again.\",\"running\");\n   window.setTimeout(function(){if(pending)buttons(false);},0);\n },true);\n function initializeHandler(){\n   if(window.srmaQcHandlerBound)return true;\n   if(!window.Shiny || !window.Shiny.addCustomMessageHandler)return false;\n   window.srmaQcHandlerBound=true;\n   window.Shiny.addCustomMessageHandler(\"srma_qc_done\",function(info){\n     pending=false;\n     buttons(true);\n     var state=info && info.state?String(info.state):\"NOT_ASSESSED\";\n     var msg=info && info.cached?\"Saved QC result loaded; no new AI call.\":\n       (state===\"PASS\"||state===\"WARNING\"||state===\"BLOCKED\"?\n        \"Scientific QC finished. Review the findings below.\":\n        \"QC finished without acceptance. See the diagnostic below.\");\n     status(msg,\"done\");\n   });\n   return true;\n }\n if(!initializeHandler()){\n   var attempts=0;\n   var registration=window.setInterval(function(){\n     attempts+=1;\n     if(initializeHandler()||attempts>100)window.clearInterval(registration);\n   },100);\n }\n document.addEventListener(\"shiny:disconnected\",function(){\n   pending=false;buttons(true);\n   status(\"Connection interrupted. Reopen the selected Version; completed QC is saved when available.\",\"disconnected\");\n });\n})();")),
      shiny::tags$h3("Scientific Publication QC"),
      shiny::textOutput("srma_qc_target"),
      shiny::actionButton("srma_qc_downloads","Open Downloads"),
      shiny::uiOutput("srma_qc_autolink_ui")),
    shiny::tags$details(class="srma-qc-legacy",
      shiny::tags$summary("고급 · 운영자 진단"),
      shiny::uiOutput("srma_qc_summary"),
      shiny::uiOutput("srma_qc_link_diagnostics"),
      shiny::uiOutput("srma_qc_reconciliation_ui"),
      shiny::downloadButton("srma_qc_report_download","검증 기록 다운로드"),
      shiny::actionButton("srma_qc_diagnose_docx","DOCX 상태 확인"),
      shiny::verbatimTextOutput("srma_qc_docx_diagnostic"),
      shiny::actionButton("srma_qc_run","Run / Load Scientific QC",class="btn-primary"),
      shiny::uiOutput("srma_qc_retry_ui"),
      shiny::tags$div(id="srma-qc-live",class="srma-qc-live",role="status",`aria-live`="polite"),
      shiny::tags$details(class="srma-qc-detail",
        shiny::tags$summary("제출 승인 및 이력 관리"),
        do.call(shiny::tagList,previous))))
  old
}
srma_qc_researcher_review <- function(q,z) {
  if(is.null(q))return(htmltools::tags$p("이 원고에 저장된 검토 결과가 없습니다."))
  if(!is.null(q$error))return(htmltools::tags$p("저장된 검토 결과에 접근할 수 없습니다. 운영자 진단을 확인해 주세요."))
  issues<-c(srma_qc_link_list(q$checks),srma_qc_link_list(q$ai$checks))
  remaining<-sum(vapply(issues,function(t)
    srma_qc_one(t$status) %in% c("BLOCKED","WARNING","NOT_ASSESSED"),logical(1)))
  saved<-if(identical(q$status,"BLOCKED"))
    htmltools::tags$p(sprintf("저장된 검토 사항 %d건 · 최종 제출 차단",remaining))
  if(is.null(z))return(htmltools::tagList(saved,htmltools::tags$p("기존 자료를 자동으로 확인하고 있습니다.")))
  if(!is.null(z$error))return(htmltools::tagList(saved,
    htmltools::tags$p("추가 자료 확인에 문제가 있습니다. 운영자 진단을 확인해 주세요.")))
  questions<-unique(Filter(nzchar,vapply(z$findings,function(t)
    srma_qc_one(t$question),character(1))))
  message<-if(length(questions))"연구자 판단 필요. 기존 검토 결과에 제출 전 확인 사항이 남아 있습니다." else
    if(remaining>0L || !identical(q$status,"PASS"))"제출 전 확인할 사항이 남아 있습니다. 운영자 진단에서 확인할 수 있습니다." else
      "기존 자료 자동 확인 완료. 최종 제출에는 별도 승인이 필요합니다."
  htmltools::tagList(saved,htmltools::tags$p(message),
    if(length(questions))htmltools::tags$ul(lapply(questions,htmltools::tags$li)))
}
srma_qc_attach <- function(input,output,session,request,settings,con) {
  report<-shiny::reactiveVal(NULL)
  reconciliation<-shiny::reactiveVal(NULL)
  autolink<-shiny::reactiveVal(NULL)
  diagnostic<-shiny::reactiveVal(NULL)
  busy<-shiny::reactiveVal(FALSE)
  # Isolate diagnostics from manuscript/AI generation. Report the failed phase
  # IN THE WEB UI, since stderr may be redirected and need not reach Terminal 2.
  shiny::observeEvent(input$srma_qc_diagnose_docx,{
    phase <- new.env(parent=emptyenv())
    phase$current <- "SELECTION"
    result <- local({
      before <- getOption("srma.qc.docx.trace",FALSE)
      on.exit(options(srma.qc.docx.trace=before),add=TRUE)
      options(srma.qc.docx.trace=TRUE)
      tryCatch({
        if(!nzchar(srma_qc_one(input$project)) || !nzchar(srma_qc_one(input$version)))
          stop("SELECT_SAVED_VERSION_FIRST",call.=FALSE)
        phase$current <- "MANUSCRIPT_VIEW_REQUEST"
        x <- request("manuscript_view",list(project_id=input$project,version_id=input$version))
        phase$current <- "MANUSCRIPT_VIEW_SHAPE"
        if(!is.list(x)||is.null(x$stage))stop("MANUSCRIPT_VERSION_NOT_AVAILABLE",call.=FALSE)
        phase$current <- "DOCX_READ"
        xml <- srma_qc_docx_bytes(request,input$project,x)
        phase$current <- "DOCX_XML_PARSE"
        txt <- tryCatch(srma_qc_docx_parse(xml),error=function(e) {
          if(identical(conditionMessage(e),"QC_DOCX_XML2_MISSING_IN_APP"))return(NULL)
          stop(e)
        })
        phase$current <- "COMPLETE"
        list(status=if(is.null(txt))"NOT_ASSESSED" else "PASS",bytes_status="PASS",
          code=if(is.null(txt))"QC_DOCX_BYTES_VERIFIED" else "QC_DOCX_XML_PARSE_PASS",
          characters=if(is.null(txt))0L else nchar(txt,type="chars"),
          phase=phase$current,error_class="NONE",
          error_hint=if(is.null(txt))"XML_PARSE_NOT_ASSESSED_XML2_MISSING" else "NONE")
      },error=function(e){
        code <- conditionMessage(e)
        # Never show raw exception text, paths, manuscript text or credentials.
        if(!grepl("^(QC_DOCX_[A-Z0-9_]+|SELECT_SAVED_VERSION_FIRST|MANUSCRIPT_VERSION_NOT_AVAILABLE)$",code))
          code <- "QC_DOCX_DIAGNOSTIC_ERROR"
        hint <- if(identical(code,"QC_DOCX_XML2_MISSING_IN_APP") ||
          inherits(e,"packageNotFoundError")) "R_PACKAGE_MISSING" else if(
          inherits(e,"srma_web_error")) srma_qc_request_error_hint(e) else if(
          grepl("unused argument",conditionMessage(e),ignore.case=TRUE)) "CALL_SIGNATURE_MISMATCH" else if(
          grepl("not found|could not find",conditionMessage(e),ignore.case=TRUE)) "NOT_FOUND_OR_MISSING_SYMBOL" else if(
          grepl("subscript out of bounds",conditionMessage(e),ignore.case=TRUE)) "UNEXPECTED_RESULT_SHAPE" else if(
          grepl("permission|unauthorized|forbidden",conditionMessage(e),ignore.case=TRUE)) "ACCESS_DENIED" else "UNCLASSIFIED"
        list(status="FAIL",code=code,characters=0L,phase=phase$current,
          error_class=paste(class(e),collapse="/"),error_hint=hint)
      })
    })
    diagnostic(result)
  })
  output$srma_qc_docx_diagnostic<-shiny::renderText({
    d<-diagnostic()
    if(is.null(d))return("Run this only if DOCX QC fails. No AI call will be made.")
    paste0("DOCX_DIAGNOSTIC: ",d$status,
      "\nPHASE: ",d$phase,
      "\nCODE: ",d$code,
      if(!is.null(d$bytes_status))paste0("\nDOWNLOAD_INTEGRITY: ",d$bytes_status),
      "\nERROR_CLASS: ",d$error_class,
      "\nERROR_HINT: ",d$error_hint,
      "\nEXTRACTED_CHARACTERS: ",d$characters,
      "\nNo raw error details, manuscript text, storage paths, or credentials are shown.")
  })
  output$srma_qc_target<-shiny::renderText({
    pid<-srma_qc_one(input$project);vid<-srma_qc_one(input$version)
    if(!nzchar(pid)||!nzchar(vid))return("Dashboard에서 Manuscript Version을 선택하세요.")
    tryCatch({
      v<-web_query(con,"SELECT version FROM project_versions WHERE id=?v AND project_id=?p",
        v=vid,p=pid)
      if(nrow(v)!=1L)return("Selected Version could not be verified.")
      paste0("Dashboard selection: Version ",as.integer(v$version[[1L]]))
    },error=function(e)"Dashboard Version could not be loaded.")
  })
  shiny::observeEvent(list(input$project,input$version),{
    # Read-only restore. Never launches AI and never changes source artifacts.
    restored<-srma_qc_restore_selected(request,settings,con,input$project,input$version,
      on_error=function(e)list(status="NOT_ASSESSED",error=srma_qc_request_error_hint(e)))
    report(restored)
  },ignoreInit=FALSE)
  # Reconcile an existing, version-bound QC report automatically. No AI call,
  # SQLite write, scientific rerun, or change to the original AI assessment.
  shiny::observeEvent(list(report(),input$project,input$version),{
    q<-report()
    pid<-srma_qc_one(input$project);vid<-srma_qc_one(input$version)
    if(!is.list(q) || !is.list(q$checks) || !nzchar(pid) || !nzchar(vid)) {
      reconciliation(NULL)
      autolink(NULL)
      return(invisible(NULL))
    }
    result<-tryCatch({
      pol<-srma_qc_policy_load(settings$engine_root)
      x<-request("manuscript_view",list(project_id=pid,version_id=vid))
      if(!is.list(x) || !is.list(x$source))stop("QC_RECON_SOURCE_UNAVAILABLE")
      v<-web_query(con,"SELECT version FROM project_versions WHERE id=?v AND project_id=?p",
        v=vid,p=pid)
      if(nrow(v)!=1L)stop("QC_RECON_VERSION_UNVERIFIED")
      num<-as.integer(v$version[[1L]])
      key<-srma_qc_report_key(pol,pid,vid,x)
      srma_qc_recon_cached(settings,key,q,x,pol,pid,vid,num)
    },error=function(e)list(error=paste0(srma_qc_recon_safe_error(e),
      ": Could not reconcile the selected QC report; original QC retained.")))
    reconciliation(result)
  },ignoreInit=FALSE)
  # RC28.25.9BCD.6.3.10: authorized, read-only Canonical Source lookup.
  # Reuses saved QC; no AI call, canonical scientific job, or SQLite mutation.
  shiny::observeEvent(list(reconciliation(),input$project,input$version),{
    rec<-reconciliation()
    q<-report()
    pid<-srma_qc_one(input$project);vid<-srma_qc_one(input$version)
    if(!is.list(rec) || !is.null(rec$error) || !is.list(q) ||
       !nzchar(pid) || !nzchar(vid)) {
      autolink(if(is.list(q))list(error="QC_SOURCE_UNAVAILABLE") else NULL)
      return(invisible(NULL))
    }
    result<-tryCatch({
      pol<-srma_qc_policy_load(settings$engine_root)
      x<-request("manuscript_view",list(project_id=pid,version_id=vid))
      v<-web_query(con,"SELECT version FROM project_versions WHERE id=?v AND project_id=?p",
        v=vid,p=pid)
      if(!is.list(x) || !is.list(x$source) || nrow(v)!=1L)
        stop("QC_AUTOLINK_VERSION_UNAVAILABLE",call.=FALSE)
      num<-as.integer(v$version[[1L]])
      if(!srma_qc_recon_valid(rec,q,x,pol,pid,vid,num))
        stop("QC_AUTOLINK_SOURCE_VERSION_MISMATCH",call.=FALSE)
      if(!isTRUE(rec$source_digest_verified))stop("QC_AUTOLINK_SOURCE_VERSION_MISMATCH")
      packet<-srma_qc_link_collect(request,con,settings,pid,vid,x)
      snap<-srma_qc_link_snapshot(packet,x,pid,vid)
      key<-srma_qc_report_key(pol,pid,vid,x)
      srma_qc_link_cached(settings,key,rec,q,x,pol,pid,vid,num,snap)
    },error=function(e)list(error="현재 자료를 자동으로 확인할 수 없습니다. 기존 QC 결과는 보존됩니다."))
    autolink(result)
  },ignoreInit=FALSE)
  shiny::observeEvent(input$srma_qc_downloads,{
    shiny::updateTabsetPanel(session,"page",selected="Download Center")
  })
  output$srma_qc_retry_ui<-shiny::renderUI({
    x<-report()
    if(!is.list(x)||is.list(x$ai)||!nzchar(srma_qc_one(x$notice)))return(NULL)
    if(!grepl("^QC_(AI|CHILD|PROVIDER|INVALID_JSON|EMPTY_AI_OUTPUT)",srma_qc_one(x$notice)))return(NULL)
    shiny::actionButton("srma_qc_retry","Retry failed AI QC (uses API)")
  })
  srma_qc_execute <- function(force=FALSE) {
    # A synchronous provider call blocks Shiny: the browser immediately shows
    # a truthful pending indicator instead of an invented percentage.
    if(isTRUE(busy()))return(invisible(NULL))
    busy(TRUE)
    completion<-list(state="NOT_ASSESSED",cached=FALSE)
    on.exit({
      busy(FALSE)
      try(session$sendCustomMessage("srma_qc_done",completion),silent=TRUE)
    },add=TRUE)
    compute <- function() {
      pid<-srma_qc_one(input$project);vid<-srma_qc_one(input$version)
      if(!nzchar(pid)||!nzchar(vid))stop("QC_VERSION_NOT_SELECTED",call.=FALSE)
      # For all routine requests, restore the exact completed report first.
      # Even an earlier NOT_ASSESSED report does not silently trigger paid AI.
      if(!isTRUE(force)) {
        saved_report<-srma_qc_restore_selected(request,settings,con,pid,vid)
        if(!is.null(saved_report)) {
          completion$cached <<- TRUE
          return(saved_report)
        }
      }
      policy<-srma_qc_policy_load(settings$engine_root)
      x<-request("manuscript_view",list(project_id=pid,version_id=vid))
      if(!is.list(x)||is.null(x$stage))stop("QC_MANUSCRIPT_NOT_AVAILABLE",call.=FALSE)
      v<-web_query(con,"SELECT version FROM project_versions WHERE id=?v AND project_id=?p",
        v=vid,p=pid)
      if(nrow(v)!=1L)stop("QC_VERSION_NOT_VERIFIED",call.=FALSE)
      number<-as.integer(v$version[[1L]])
      if(is.na(number))stop("QC_VERSION_NOT_VERIFIED",call.=FALSE)
      report_key<-srma_qc_report_key(policy,pid,vid,x)
      checks<-srma_qc_deterministic(x,policy)
      qc_docx_error<-NULL
      text<-tryCatch(srma_qc_docx_text(request,pid,x),error=function(e){
        qc_docx_error <<- conditionMessage(e)
        if(!grepl("^QC_DOCX_[A-Z0-9_]+$",qc_docx_error))
          qc_docx_error <<- "QC_DOCX_ACCESS_FAILED"
        message("[SRMA QC DOCX] ",qc_docx_error)
        ""
      })
      blocked<-any(vapply(checks,function(v)identical(v$status,"BLOCKED"),logical(1)))
      ai<-NULL;notice<-NULL
      if(blocked)notice<-"Source integrity is blocked; semantic AI QC was not called."
      else if(!nzchar(text))notice<-paste0(
        "DOCX access needs repair (",if(is.null(qc_docx_error))"QC_DOCX_TEXT_EMPTY" else qc_docx_error,
        "). AI QC NOT_ASSESSED; existing manuscript remains downloadable.")
      else {
        key<-report_key
        base<-file.path(settings$data_root,"scientific-publication-qc",key)
        cache<-paste0(base,".json")
        if(!isTRUE(force) && file.exists(cache)) {
          saved<-tryCatch(jsonlite::read_json(cache,simplifyVector=FALSE),error=function(e)NULL)
          if(srma_qc_cached_ai_valid(saved,key,policy,pid,number,x)) {
            ai<-saved$ai;notice<-"Version-bound QC result reused; no new AI call."
            completion$cached <<- TRUE
          }
        }
        if(is.null(ai)) {
          lock<-srma_qc_lock_acquire(settings,key)
          on.exit(srma_qc_lock_release(lock),add=TRUE)
          # Recheck the cache under the lock: another App may have completed.
          if(!isTRUE(force) && file.exists(cache)) {
            saved<-tryCatch(jsonlite::read_json(cache,simplifyVector=FALSE),error=function(e)NULL)
            if(srma_qc_cached_ai_valid(saved,key,policy,pid,number,x)) {
              ai<-saved$ai;notice<-"Version-bound QC result reused; no new AI call."
              completion$cached <<- TRUE
            }
          }
          if(is.null(ai)) {
            try(web_provider_ready(settings),silent=TRUE)
            ai<-tryCatch(srma_qc_ai(x,text,policy,pid,number,settings$engine_root),error=function(e)e)
            if(inherits(ai,"error")) {
              notice<-srma_qc_safe_error(ai);ai<-NULL
            } else {
              dir.create(dirname(cache),recursive=TRUE,showWarnings=FALSE,mode="0700")
              record<-list(cache_key=key,policy_sha256=policy$sha256,
                manuscript_version_number=number,manuscript_version_id=vid,
                evidence_version_id=x$evidence_version,ai=ai)
              tmp<-tempfile(".qc-",tmpdir=dirname(cache),fileext=".json")
              jsonlite::write_json(record,tmp,auto_unbox=TRUE,null="null",na="null",pretty=TRUE)
              if(file.exists(cache))unlink(tmp) else if(!file.rename(tmp,cache)) {
                unlink(tmp);notice<-"AI QC completed but its immutable cache could not be saved."
              }
            }
          }
        }
      }
      state<-srma_qc_status(checks,ai)
      result<-list(status=state,number=number,policy=policy$sha256,
        evidence_version=x$evidence_version,checks=checks,ai=ai,
        notice=notice,publication_ready_release_allowed=FALSE)
      rec<-srma_qc_report_record(report_key,policy,pid,vid,x,result)
      if(!srma_qc_report_save(settings,rec))
        result$notice<-paste(srma_qc_one(result$notice),
          "QC_REPORT_NOT_PERSISTED: No source data was changed.")
      result
    }
    result<-tryCatch(compute(),error=function(e)list(status="NOT_ASSESSED",error=srma_qc_safe_error(e)))
    report(result)
    completion$state<-srma_qc_one(result$status,"NOT_ASSESSED")
    invisible(result)
  }
  shiny::observeEvent(input$srma_qc_run,{srma_qc_execute(force=FALSE)})
  shiny::observeEvent(input$srma_qc_retry,{srma_qc_execute(force=TRUE)})
  output$srma_qc_summary<-shiny::renderUI({
    x<-report()
    if(is.null(x))return(shiny::p("선택한 원고의 검토 결과가 없습니다."))
    if(!is.null(x$error))return(shiny::p("검토 결과를 불러올 수 없습니다. ",x$error))
    items<-c(if(is.list(x$checks))x$checks else list(),
      if(is.list(x$ai)&&is.list(x$ai$checks))x$ai$checks else list())
    issues<-Filter(is.list,items)
    blocked<-sum(vapply(issues,function(v)identical(srma_qc_one(v$status),"BLOCKED"),logical(1)))
    remaining<-sum(vapply(issues,function(v)
      srma_qc_one(v$status) %in% c("BLOCKED","NOT_ASSESSED","WARNING"),logical(1)))
    status<-srma_qc_one(x$status)
    detail<-function(v) {
      phrase<-srma_qc_one(v$detail,srma_qc_one(v$observed))
      action<-srma_qc_one(v$suggested_action,srma_qc_one(v$action))
      shiny::tags$li(shiny::tags$strong(paste0("[",srma_qc_one(v$status),"] ",
        srma_qc_one(v$rule_id)))," — ",phrase,
        if(nzchar(action))shiny::tags$p(action) else NULL)
    }
    shiny::tagList(
      shiny::tags$p(shiny::tags$strong(paste0("Version ",x$number)),
        " — ",if(status=="PASS")"자동 검토 완료" else
          "논문 제출 전에 확인할 사항이 있습니다."),
      shiny::tags$p(paste0("필수 확인 ",blocked,"건 · 전체 검토사항 ",remaining,"건"),
        style="font-size:13px;color:#475569;"),
      if(length(issues))shiny::tags$details(class="srma-qc-detail",
        shiny::tags$summary("전체 과학적 검토 내역 보기"),
        shiny::tags$ul(lapply(issues,detail)),
        shiny::tags$p("Policy SHA-256: ",srma_qc_one(x$policy),
          style="font-size:11px;color:#64748b;")) else NULL)
  })
  output$srma_qc_autolink_ui<-shiny::renderUI({
    srma_qc_researcher_review(report(),autolink())
  })
  output$srma_qc_link_diagnostics<-shiny::renderUI({
    z<-autolink()
    if(is.null(z))return(NULL)
    if(!is.null(z$error))return(shiny::p(z$error))
    shiny::tagList(
      shiny::tags$ul(lapply(z$findings,function(t)
        shiny::tags$li(t$label," / ",t$status," / ",t$approval," / ",t$source_kind))),
      shiny::tags$pre(as.character(jsonlite::toJSON(z$provenance,
        auto_unbox=TRUE,null="null",pretty=TRUE))))
  })
  output$srma_qc_reconciliation_ui<-shiny::renderUI({
    rec<-reconciliation()
    if(is.null(rec))return(shiny::tags$p(
      "Existing QC findings will be reconciled automatically when a saved report is loaded.",
      style="color:#64748b;font-size:12px;"))
    if(!is.null(rec$error))return(shiny::tags$p(rec$error))
    if(!isTRUE(rec$source_digest_verified))return(shiny::tags$p(
      "Frozen source digest could not be verified. No findings were automatically resolved."))
    captions<-c(AUTO_RESOLVED_BINDING="Approved links found",
      SOURCE_DATA_MISSING_IN_PACKET="Data not in QC packet",
      HUMAN_DECISION_REQUIRED="Author judgment",
      SOURCE_VERIFICATION_REQUIRED="Source verification")
    counts<-vapply(names(captions),function(k){
      n<-suppressWarnings(as.integer(srma_qc_one(rec$counts[[k]],"0")))
      if(is.na(n))0L else n
    },integer(1))
    report_task<-function(t) {
      ids<-paste(head(as.character(unlist(t$rule_ids,use.names=FALSE)),6L),collapse=", ")
      shiny::tags$li(shiny::tags$strong(paste0(t$family," — ",t$finding_count," finding(s)")),
        shiny::tags$p(t$next_action,style="margin:2px 0 3px;"),
        shiny::tags$small(paste0("Rules: ",ids," | Source: ",t$source_ref)))
    }
    tasks<-if(is.list(rec$tasks))rec$tasks else list()
    urgent<-Filter(function(z)is.list(z) && z$classification %in%
      c("HUMAN_DECISION_REQUIRED","SOURCE_DATA_MISSING_IN_PACKET"),tasks)
    others<-Filter(function(z)is.list(z) && !z$classification %in%
      c("HUMAN_DECISION_REQUIRED","SOURCE_DATA_MISSING_IN_PACKET"),tasks)
    shiny::tagList(
      shiny::tags$p(paste(captions,counts,collapse="  |  "),
        style="font-size:12px;color:#475569;"),
      shiny::tags$p(paste0(rec$original_finding_count,
        " original findings classified; no new AI call, no scientific approval changed."),
        style="font-size:12px;color:#475569;"),
      if(length(urgent))shiny::tags$ul(lapply(head(urgent,5L),report_task)) else
        shiny::tags$p("No unresolved author/data tasks were identified in this QC snapshot."),
      if(length(urgent)>5L || length(others))shiny::tags$details(
        shiny::tags$summary(paste0("Show remaining source checks and actions (",
          length(urgent)-min(length(urgent),5L)+length(others),")")),
        shiny::tags$ul(lapply(c(if(length(urgent)>5L)urgent[-seq_len(5L)] else list(),others),report_task))),
      shiny::tags$p(
        "Auto-resolved means a verified source binding was found, NOT that an AI BLOCKED finding or release gate was cleared. 'Data not in QC packet' does not prove absence from PDFs or the repository.",
        style="font-size:11px;color:#64748b;")
    )
  })
  output$srma_qc_report_download<-shiny::downloadHandler(
    filename=function(){x<-report();paste0("SRMA_QC_Version_",if(is.null(x$number))"unassessed" else x$number,".json")},
    content=function(file){x<-report();if(is.null(x))stop("No QC report yet.")
      rec<-reconciliation()
      links<-autolink()
      # The original saved AI/deterministic QC is unmodified. Attach only a
      # separately version-bound derivative reconciliation for export.
      if(is.list(rec) && is.null(rec$error) &&
         identical(srma_qc_one(rec$manuscript_version_number),srma_qc_one(x$number)) &&
         identical(srma_qc_one(rec$policy_sha256),srma_qc_one(x$policy)) &&
         identical(srma_qc_one(rec$frozen_evidence_version_id),srma_qc_one(x$evidence_version)))
        x$reconciliation<-rec
      if(is.list(links) && is.null(links$error) &&
         identical(srma_qc_link_one(links$manuscript_version_number),srma_qc_one(x$number)) &&
         identical(srma_qc_link_one(links$policy_sha256),srma_qc_one(x$policy)) &&
         identical(srma_qc_link_one(links$evidence_version_id),srma_qc_one(x$evidence_version)))
        x$canonical_autolink<-links
      jsonlite::write_json(x,file,auto_unbox=TRUE,null="null",na="null",pretty=TRUE)})
  invisible(TRUE)
}

# Results Library enhancement is presentation-only: reuse the authorized download handler.
srma_qc_artifact_click_ui <- function() {
  root<-if(exists("web_root",inherits=TRUE))get("web_root",inherits=TRUE) else stop("QC UI root unavailable")
  path<-file.path(root,"web/qc/artifact_click.js")
  m<-jsonlite::read_json(file.path(root,"web/qc/approved-qc-policy.json"),simplifyVector=TRUE)
  if(!file.exists(path) || !identical(digest::digest(file=path,algo="sha256"),
      as.character(m$artifact_click_sha256)))
    stop("QC_ARTIFACT_UI_INTEGRITY_FAILED")
  shiny::tags$script(shiny::HTML(paste(readLines(path,encoding="UTF-8",warn=FALSE),collapse="\n")))
}
