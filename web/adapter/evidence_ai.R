# DEC-008 bridge for an explicitly queued AI extraction operation.
# The saved Evidence Version is immutable. This function adjusts ONLY the
# private Worker attempt's configuration. Missing policy flags are not Human
# approvals: extraction proposals remain UNVERIFIED and require Human review.
web_b2_dec008_enabled <- function(value) {
 if(is.logical(value) && length(value)==1L && !is.na(value))return(isTRUE(value))
 if(is.numeric(value) && length(value)==1L && is.finite(value))return(value==1)
 if(is.character(value) && length(value)==1L && !is.na(value))
  return(tolower(trimws(value)) %in% c("yes","true","1","enabled"))
 FALSE
}
web_b2_dec008_extraction_bridge <- function(root,x) {
 path<-file.path(root,"config","project_config.yml")
 web_assert(file.exists(path),"Saved project configuration is unavailable.","USER_ACTION_REQUIRED")
 cfg<-yaml::read_yaml(path)
 web_assert(is.list(cfg) && (is.null(cfg$data_extraction) || is.list(cfg$data_extraction)),
  "The project data_extraction policy must be an object.","USER_ACTION_REQUIRED")
 policy<-cfg$data_extraction
 if(is.null(policy))policy<-list()
 required<-c("agent_extraction","human_verification")
 # An explicit project-level disable must never be turned into an enable.
 present<-required[required %in% names(policy)]
 for(key in present)web_assert(web_b2_dec008_enabled(policy[[key]]),
   paste0("DEC-008 setting '",key,"' is disabled or invalid. Review the project configuration before AI extraction."),
   "USER_ACTION_REQUIRED")
 added<-setdiff(required,present)
 if(!length(added))return(x)
 before<-web_sha(path)
 for(key in added)policy[[key]]<-TRUE
 cfg$data_extraction<-policy
 yaml::write_yaml(cfg,path)
 verified<-yaml::read_yaml(path)$data_extraction
 web_assert(all(vapply(required,function(key)web_b2_dec008_enabled(verified[[key]]),logical(1))),
   "Private DEC-008 extraction policy did not validate.","SYSTEM_FAILED")
 after<-web_sha(path)
 x$history<-c(x$history,list(list(
   event="DEC008_EXPLICIT_EXTRACTION_REQUEST_BRIDGE",at=web_now(),
   scope="PRIVATE_WORKER_ATTEMPT_ONLY",added_settings=as.list(added),
   source_config_sha256=before,effective_config_sha256=after,
   human_decisions_changed=FALSE,
   note="AI extraction was explicitly queued. Human verification is required, not completed.")))
 x
}

# RC28.6 generic supplementary title/abstract study-design classification.
# This is deliberately independent of scientific R/19 screening judgments.
web_b2_design_prompt <- function() paste(
  "Classify study design based ONLY on the record title and abstract.",
  "You do not decide eligibility, replace Reviewer 1, or sign a Human decision.",
  "COMPARATIVE: explicit analyzable comparator/control group or comparative design,",
  "including reliably stated comparative cohorts or randomized arms.",
  "NON_COMPARATIVE: clearly uncontrolled single-arm intervention, uncontrolled",
  "cohort, case series or similar study without a genuine comparator.",
  "UNCLEAR: insufficient methods detail, ambiguous comparator, protocols,",
  "reviews, or merely mentioned comparison treatments without study groups.",
  "A single group with pre/post measurements is not automatically a controlled",
  "comparative study; classify based on actual design and explicit methods.",
  "A lone intervention arm in a multiarm study is not single-arm; classify study-level.",
  "If uncertain, choose UNCLEAR, never infer solely from keywords.",
  "Provide a brief textual rationale and direct source excerpt when available.",sep="\n")

web_b2_design_response <- function(engine,group) {
  web_assert(requireNamespace("curl",quietly=TRUE),
    "The scientific R environment needs the curl package for supplemental AI classification.",
    "DEPENDENCY_FAILED")
  cfg <- yaml::read_yaml("config/agent_screening.yml")$agent_screening$provider
  model <- as.character(cfg$model %||b2% "")
  provider <- as.character(cfg$name %||b2% "")
  envname <- as.character(cfg$api_key_env %||b2% "OPENAI_API_KEY")
  web_assert(identical(tolower(provider),"openai"),
    "This release supports only the configured OpenAI provider for supplementary classification.",
    "USER_ACTION_REQUIRED")
  web_assert(grepl("^[A-Za-z_][A-Za-z0-9_]*$",envname),"Invalid API credential variable.","USER_ACTION_REQUIRED")
  web_assert(nzchar(model),"No screening model configured.","USER_ACTION_REQUIRED")
  key <- Sys.getenv(envname,unset="")
  web_assert(nzchar(key),"Operator AI credential is unavailable in the worker.","USER_ACTION_REQUIRED")
  schema <- list(type="object",additionalProperties=FALSE,
    properties=list(results=list(type="array",items=list(type="object",
      additionalProperties=FALSE,
      properties=list(record_id=list(type="string"),
        classification=list(type="string",enum=c("COMPARATIVE","NON_COMPARATIVE","UNCLEAR")),
        rationale=list(type="string"),evidence_excerpt=list(type="string")),
      required=c("record_id","classification","rationale","evidence_excerpt")))),
    required=list("results"))
  records <- lapply(group,function(r) list(
    record_id=as.character(r$screening_id),
    title=substr(as.character(r$title %||b2% ""),1L,2000L),
    abstract=substr(as.character(r$abstract %||b2% ""),1L,16000L)))
  payload <- list(model=model,
    input=list(list(role="system",content=web_b2_design_prompt()),
               list(role="user",content=as.character(jsonlite::toJSON(records,
                 auto_unbox=TRUE,null="null")))),
    text=list(format=list(type="json_schema",name="srma_design_triage",
      strict=TRUE,schema=schema)),max_output_tokens=5000L)
  json <- as.character(jsonlite::toJSON(payload,auto_unbox=TRUE,null="null"))
  h<-curl::new_handle()
  curl::handle_setopt(h,postfields=json,timeout=180L,connecttimeout=20L)
  curl::handle_setheaders(h,Authorization=paste("Bearer",key),
    `Content-Type`="application/json")
  result<-tryCatch(curl::curl_fetch_memory("https://api.openai.com/v1/responses",handle=h),
    error=function(e)web_error("PROVIDER_UNAVAILABLE", "Supplementary AI provider request failed; no source decisions were changed."))
  web_assert(result$status_code>=200L && result$status_code<300L,
    paste("Supplementary AI provider returned HTTP status",result$status_code,
      "; no AI screening decision was changed."),"PROVIDER_UNAVAILABLE")
  response<-tryCatch(jsonlite::fromJSON(rawToChar(result$content),simplifyVector=FALSE),
    error=function(e)NULL)
  web_assert(is.list(response) && identical(response$status,"completed"),
    "Supplementary AI response did not complete; no classifications were accepted.","PROVIDER_UNAVAILABLE")
  msg<-Filter(function(o)identical(o$type,"message"),response$output %||b2% list())
  parts<-unlist(lapply(msg,function(m)lapply(Filter(function(c)identical(c$type,"output_text"),
    m$content %||b2% list()),function(c)c$text)),use.names=FALSE)
  web_assert(length(parts)==1L,"Malformed supplementary AI response.","PROVIDER_UNAVAILABLE")
  parsed<-tryCatch(jsonlite::fromJSON(parts[[1L]],simplifyVector=FALSE),error=function(e)NULL)
  rows<-parsed$results
  web_assert(is.list(rows) && length(rows)==length(group),
    "Supplementary AI response returned the wrong number of rows.","PROVIDER_UNAVAILABLE")
  got<-vapply(rows,function(r)as.character(r$record_id %||b2% ""),character(1))
  expected<-vapply(group,function(r)as.character(r$screening_id),character(1))
  web_assert(all(nzchar(got)) && !anyDuplicated(got) && setequal(got,expected),
    "AI response record IDs were inconsistent; refusing to match by row position.","PROVIDER_UNAVAILABLE")
  list(rows=rows,model=as.character(response$model %||b2% model),provider=provider,
       response_id=as.character(response$id %||b2% ""))
}

# RC28.6 checkpoint: one file in the PRIVATE job attempt, never in an Evidence Version.
# Only completed, validated API batches are committed. The source/policy hashes
# prevent replay of results against changed titles, reviewer decisions or model.
web_b2_design_checkpoint_paths <- function(root) {
  directory<-file.path(root,"results","logs")
  if(!dir.exists(directory))return(character())
  files<-list.files(directory,pattern="^rc286_design_checkpoint_[0-9]{5}[.]rds$",
    full.names=TRUE)
  sort(files,decreasing=TRUE)
}
web_b2_design_checkpoint_path <- function(root,completed) {
  file.path(root,"results","logs",sprintf("rc286_design_checkpoint_%05d.rds",completed))
}
web_b2_design_checkpoint_verify <- function(cp,include,digest,policy_sha) {
  ids<-vapply(include,function(r)as.character(r$screening_id),character(1))
  web_assert(is.list(cp) && identical(cp$schema_version,1L) &&
    identical(cp$source_digest,digest) && identical(cp$policy_sha256,policy_sha) &&
    identical(cp$include_ids,ids) && is.list(cp$rows),
    "Stored design checkpoint is incompatible with this exact version/model; reconcile before retry.",
    "USER_ACTION_REQUIRED")
  rows<-cp$rows;n<-length(rows)
  web_assert(n<=length(ids) && (n %% 8L==0L || n==length(ids)),
    "Stored design checkpoint does not end at a complete AI batch.","USER_ACTION_REQUIRED")
  got<-vapply(rows,function(r)as.character(r$screening_id %||b2% ""),character(1))
  web_assert(identical(got,head(ids,n)) && all(vapply(rows,function(r) {
    cl<-as.character(r$classification %||b2% "")
    identical(as.character(r$original_ai_decision %||b2% ""),"Include") &&
      cl %in% c("COMPARATIVE","NON_COMPARATIVE","UNCLEAR") &&
      identical(as.character(r$.source_ai_sha256 %||b2% ""),digest) &&
      identical(as.character(r$.source %||b2% ""),"AI RECOMMENDATION") &&
      identical(as.character(r$.verification %||b2% ""),"UNVERIFIED") &&
      nzchar(as.character(r$rationale %||b2% "")) &&
      nzchar(as.character(r$.model %||b2% "")) &&
      nzchar(as.character(r$.provider %||b2% "")) &&
      nzchar(as.character(r$.response_id %||b2% "")) &&
      (cl!="NON_COMPARATIVE" || {
        excerpt<-as.character(r$evidence_excerpt %||b2% "")
        source<-include[[match(r$screening_id,ids)]]
        nzchar(excerpt) && grepl(excerpt,
          paste(as.character(source$title %||b2% ""),
                as.character(source$abstract %||b2% "")),fixed=TRUE)
      })
  },logical(1))),"Stored design checkpoint has invalid provenance or row IDs.",
    "USER_ACTION_REQUIRED")
  invisible(rows)
}
web_b2_design_checkpoint_commit <- function(root,cp) {
  path<-web_b2_design_checkpoint_path(root,length(cp$rows))
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE,mode="0700")
  web_assert(!file.exists(path),"This AI design checkpoint batch already exists.",
    "USER_ACTION_REQUIRED")
  tmp<-tempfile("rc286-design-",tmpdir=dirname(path))
  on.exit(unlink(tmp),add=TRUE)
  saveRDS(cp,tmp,version=3)
  try(Sys.chmod(tmp,mode="0600"),silent=TRUE)
  web_assert(file.rename(tmp,path),"Could not preserve the completed AI design batch.",
    "SYSTEM_FAILED")
  invisible(TRUE)
}

web_b2_design_classify <- function(engine,root,x) {
  # Verify the R/19 decision log before calculating the Include-only set.
  verified<-web_b2_bind_agent_reviewer1(x,root)
  z<-web_b2_selective_review_targets(verified)
  web_assert(is.null(x$screening_excel_review),
    "Study design must be classified before Human screening import.","USER_ACTION_REQUIRED")
  include<-z$ai[z$groups=="INCLUDE"]
  # Prevent unintentional overwriting or cross-version mixing of prior results.
  digest<-web_b2_design_source_digest(verified)
  previous<-x$tables$AI_Study_Design %||b2% list()
  if(length(previous)) {
    # Verified prior results are already versioned; do not spend API credits twice.
    valid_previous <- web_b2_study_design_validate(verified,required=TRUE)
    web_assert(identical(valid_previous$source_digest,digest),
      "An earlier design classification belongs to different AI screening input.","USER_ACTION_REQUIRED")
    return(x)
  }
  prior_files<-web_b2_design_checkpoint_paths(root)
  policy_file<-file.path(root,"config","agent_screening.yml")
  web_assert(file.exists(policy_file),"Study-design provider policy was not saved with the job.","USER_ACTION_REQUIRED")
  policy_sha<-web_sha(policy_file)
  include_ids<-vapply(include,function(r)as.character(r$screening_id),character(1))
  result<-list()
  if(length(prior_files)) {
    cp<-tryCatch(readRDS(prior_files[[1L]]),error=function(e)NULL)
    web_b2_design_checkpoint_verify(cp,include,digest,policy_sha)
    result<-cp$rows
  }
  already<-length(result)
  # Only a COMPLETE batch is checkpointed. A retry skips its original API call.
  if(length(include)>already)for(start in seq.int(already+1L,length(include),by=8L)) {
    idx<-seq.int(start,min(start+7L,length(include)))
    batch<-include[idx]
    response<-web_b2_design_response(engine,batch)
    web_assert(nzchar(response$response_id),"AI provider response ID is missing.","PROVIDER_UNAVAILABLE")
    ids<-vapply(response$rows,function(r)as.character(r$record_id),character(1))
    mapped<-setNames(response$rows,ids)
    for(j in idx) {
      r<-include[[j]];id<-as.character(r$screening_id);p<-mapped[[id]]
      class<-as.character(p$classification %||b2% "")
      rationale<-trimws(as.character(p$rationale %||b2% ""))
      web_assert(class %in% c("COMPARATIVE","NON_COMPARATIVE","UNCLEAR") &&
        nzchar(rationale),"Missing AI design classification or rationale.","PROVIDER_UNAVAILABLE")
      excerpt <- trimws(as.character(p$evidence_excerpt %||b2% ""))
      text_source <- paste(as.character(r$title %||b2% ""),as.character(r$abstract %||b2% ""))
      # No source-verifiable quote -> never mark a record for exclusion.
      if(class=="NON_COMPARATIVE" && (!nzchar(excerpt) ||
         !grepl(excerpt,text_source,fixed=TRUE))) {
        class <- "UNCLEAR"
        rationale <- paste(rationale,"[No verifiable source excerpt; Human review required]")
      }
      result[[j]]<-list(screening_id=id,original_ai_decision="Include",classification=class,
        rationale=rationale,evidence_excerpt=as.character(p$evidence_excerpt %||b2% ""),
        .row_id=web_id(),.source="AI RECOMMENDATION",.verification="UNVERIFIED",
        .provider=response$provider,.model=response$model,.response_id=response$response_id,
        .timestamp=web_now(),.source_ai_sha256=digest)
    }
    cp<-list(schema_version=1L,source_digest=digest,policy_sha256=policy_sha,
      include_ids=include_ids,rows=result)
    web_b2_design_checkpoint_verify(cp,include,digest,policy_sha)
    web_b2_design_checkpoint_commit(root,cp)
  }
  x$tables$AI_Study_Design<-result
  x$history<-c(x$history,list(list(event="AI_INCLUDE_DESIGN_CLASSIFIED",at=web_now(),
    source_ai_sha256=digest,classified=length(result),
    non_comparative=sum(vapply(result,function(r)identical(r$classification,"NON_COMPARATIVE"),logical(1))))))
  x
}

# Real provider boundary; all returned content remains an AI RECOMMENDATION.
# Called only in a worker with operator credential and validated scientific runtime.
web_b2_ai <- function(op,engine,root,paths,x,run) {
 web_assert(nzchar(Sys.getenv(web_b2_credential_name(engine,op),"")),"AI assistance needs an operator credential. Continue with manual Human review.","USER_ACTION_REQUIRED")
 for(p in list.files(file.path(engine,"prompts"),full.names=TRUE)){dir.create("prompts",showWarnings=FALSE);file.copy(p,file.path("prompts",basename(p)),overwrite=TRUE)}
 before_tables<-x$tables
 if(op=="recommend_extraction") {
  p<-"data/processed/full_text/included_studies.csv";d<-readr::read_csv(p,show_col_types=FALSE,col_types=readr::cols(.default=readr::col_character()))
  for(i in seq_len(nrow(d))) {
   r<-Filter(function(a)identical(a$screening_id,d$screening_id[i]),x$tables$Author_Final_Review)[[1]]
   if(!is.null(r$.pdf_sha256)){f<-web_inside(file.path(root,r$.pdf_relative),root);web_assert(identical(web_sha(f),r$.pdf_sha256),"Source PDF checksum failed.");d$pdf_path[i]<-f}
  };readr::write_csv(d,p)
 }
 if(op=="recommend_study_design") {
  # One supplementary call per AI Include batch; never rerun R/19.
  x<-web_b2_design_classify(engine,root,x)
 } else if(op=="recommend_screening") {
  run("19_run_agent_title_abstract_screening.R")
  cfg<-yaml::read_yaml("config/agent_screening.yml")$agent_screening
  file<-file.path(paths$screening_results_directory,paste0(cfg$output$workbook_basename,".xlsx"))
  x$tables$AI_Screening<-web_b2_rows(openxlsx::read.xlsx(file,sheet="Reviewer_1",startRow=4))
 } else if(op=="recommend_eligibility") {
  run("22_prepare_full_text_registry.R");run("23_run_multimodal_full_text_eligibility_2stage.R")
  x$tables$AI_Eligibility<-web_b2_rows(openxlsx::read.xlsx("results/full_text/full_text_eligibility_agent.xlsx",sheet="Agent_Review",startRow=3))
 } else if(op=="recommend_extraction") {
  # Complete missing DEC-008 policy fields only in this isolated attempt.
  # Never alter an explicit disabled setting or a saved project version.
  x<-web_b2_dec008_extraction_bridge(root,x)
  run("25_run_multimodal_data_extraction.R")
  for(n in c("study_characteristics","group_characteristics","outcome_data"))x$tables[[paste0("AI_",n)]]<-web_b2_rows(readr::read_csv(file.path("data/processed/extraction",paste0(n,"_agent.csv")),show_col_types=FALSE,col_types=readr::cols(.default=readr::col_character())))
 } else if(op=="generate_scope_advisory") {
  # R/25A is optional and costs an AI call. Run ONLY on explicit user request.
  web_assert(file.exists("data/processed/extraction/study_characteristics_agent.csv") &&
    file.exists("data/processed/extraction/outcome_data_agent.csv"),
    "The saved R/25 extraction CSV files are required for the optional advisory.","USER_ACTION_REQUIRED")
  x<-web_b2_dec008_extraction_bridge(root,x)
  prior<-Sys.getenv("SRMA_SCOPE_ADVISORY",unset=NA_character_)
  Sys.setenv(SRMA_SCOPE_ADVISORY="yes")
  on.exit(if(is.na(prior))Sys.unsetenv("SRMA_SCOPE_ADVISORY") else
    Sys.setenv(SRMA_SCOPE_ADVISORY=prior),add=TRUE)
  run("25A_generate_research_scope_advisory.R")
  advisory_dir<-"results/extraction/research_scope_advisory"
  web_assert(file.exists(file.path(advisory_dir,"research_scope_advisory.json")),
    "R/25A completed without a saved advisory; targeted verification is blocked.","SYSTEM_FAILED")
  for(pair in list(c("Advisory_Meta_Analysis_Sets","candidate_meta_analysis_sets.csv"),
                   c("Advisory_Outcome_Harmonization","outcome_harmonization.csv"),
                   c("Advisory_Study_Roles","study_roles.csv"))) {
    data<-readr::read_csv(file.path(advisory_dir,pair[[2L]]),show_col_types=FALSE,
      col_types=readr::cols(.default=readr::col_character()))
    x$tables[[pair[[1L]]]]<-web_b2_rows(data)
  }
  x$advisory_source="CANONICAL_R25A"
  x$history<-c(x$history,list(list(event="R25A_OPTIONAL_ADVISORY",
    at=web_now(),source="Golden Baseline R/25A",human_approval=FALSE)))
 } else if(op=="recommend_risk_of_bias") {
  Sys.setenv(SRMA_ROB_AGENT_RUN="true");run("36_generate_risk_of_bias.R")
  x$tables$AI_Risk_of_Bias<-web_b2_rows(readr::read_csv("results/risk_of_bias/rob_agent_assessments.csv",show_col_types=FALSE,col_types=readr::cols(.default=readr::col_character())))
 }
 cfg<-yaml::read_yaml("config/agent_screening.yml")$agent_screening$provider
 for(t in names(x$tables)[startsWith(names(x$tables),"AI_") & !vapply(names(x$tables),function(n)identical(x$tables[[n]],before_tables[[n]]),logical(1))])for(i in seq_along(x$tables[[t]])) {
  x$tables[[t]][[i]]$.source<-"AI RECOMMENDATION";x$tables[[t]][[i]]$.verification<-"UNVERIFIED"
   if(op!="recommend_study_design") {
    x$tables[[t]][[i]]$.model<-if(op=="recommend_risk_of_bias")Sys.getenv("SRMA_ROB_AGENT_MODEL","gpt-5.6") else cfg$model
    x$tables[[t]][[i]]$.provider<-cfg$name;x$tables[[t]][[i]]$.timestamp<-web_now()
   }
 }
 # Human tables are intentionally retained, including prior override rationale.
 x
}
