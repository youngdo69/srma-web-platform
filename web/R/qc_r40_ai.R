# Operator-controlled approval and append-only call journal. An unresolved
# reservation can have reached the provider and must never be sent again.
web_r40_ai_steps <- function() c("01_numeric_claim_mapping","02_cross_section_consistency")

web_r40_ai_write <- function(value,path) {
  tmp<-tempfile(".r40-",tmpdir=dirname(path))
  on.exit(unlink(tmp),add=TRUE)
  jsonlite::write_json(list(value=value,sha256=web_c2_hash(value)),tmp,auto_unbox=TRUE,null="null",digits=NA)
  Sys.chmod(tmp,"0600")
  web_assert(!file.exists(path) && file.rename(tmp,path),"Unable to preserve the R/40 journal.","SYSTEM_FAILED")
}

web_r40_ai_read <- function(path,limit=2e6) {
  web_assert(isTRUE(file.info(path)$size<=limit),"R/40 journal record is missing or oversized.","SYSTEM_FAILED")
  x<-jsonlite::read_json(path,simplifyVector=FALSE)
  web_assert(identical(x$sha256,web_c2_hash(x$value)),"R/40 journal integrity failed.","ARTIFACT_INTEGRITY_FAILED")
  x$value
}

web_r40_ai_dir <- function(settings,fingerprint) {
  web_assert(srma_qc_link_sha(fingerprint),"Invalid R/40 approval scope.")
  web_inside(file.path(settings$data_root,"r40-ai",fingerprint),settings$data_root,FALSE)
}

web_r40_ai_uint <- function(x,maximum=1e9) is.numeric(x) && length(x)==1L && is.finite(x) &&
  x>=0 && x<=maximum && x==floor(x)

web_r40_ai_validate_grant <- function(g,scope,mode) {
  numbers<-g[c("budget_microusd","max_input_tokens","max_output_tokens","input_microusd_per_million",
    "cached_input_microusd_per_million","output_microusd_per_million")]
  web_assert(identical(g$scope,scope) && identical(g$mode,mode) && nzchar(srma_qc_one(g$operator)) &&
    length(numbers)==6L && all(vapply(numbers,web_r40_ai_uint,logical(1))) &&
    g$max_input_tokens>0 && g$max_output_tokens>0 && g$max_output_tokens<=12000 &&
    g$input_microusd_per_million>=g$cached_input_microusd_per_million &&
    isTRUE(g$expires_at>as.numeric(Sys.time())) && web_r40_ai_uint(g$expires_at,1e11) &&
    grepl("^[a-zA-Z0-9._-]+$",srma_qc_one(g$model)),"R/40 call approval is invalid or expired.","USER_ACTION_REQUIRED")
  if(mode=="LIVE")web_assert(g$budget_microusd>0 && g$input_microusd_per_million>0 &&
    g$output_microusd_per_million>0 && grepl("-[0-9]{4}-[0-9]{2}-[0-9]{2}$",g$model) &&
    nzchar(srma_qc_one(g$rate_source)) && web_r40_ai_uint(g$model_context_tokens) &&
    g$model_context_tokens>0 && g$max_input_tokens>=g$model_context_tokens,
    "Live R/40 needs a priced, fixed model snapshot and its full context ceiling.","USER_ACTION_REQUIRED")
  reserve<-ceiling((as.double(g$max_input_tokens)*g$input_microusd_per_million+
    as.double(g$max_output_tokens)*g$output_microusd_per_million)/1e6)
  web_assert(reserve*length(web_r40_ai_steps())<=g$budget_microusd,
    "R/40 approval cannot cover the maximum charge of the authorized steps.","USER_ACTION_REQUIRED")
  reserve
}

web_r40_ai_grant <- function(settings,scope,grant,mode) {
  grant$scope<-scope
  grant$mode<-mode
  web_r40_ai_validate_grant(grant,scope,mode)
  dir<-web_r40_ai_dir(settings,scope$fingerprint)
  dir.create(dir,recursive=TRUE,showWarnings=FALSE,mode="0700")
  web_assert(dir.create(file.path(dir,"approval"),mode="0700",showWarnings=FALSE),
    "An R/40 approval already exists for this scope.","USER_ACTION_REQUIRED")
  web_r40_ai_write(grant,file.path(dir,"approval","grant.json"))
  invisible(TRUE)
}

web_r40_ai_revoke <- function(settings,scope,operator) {
  dir<-web_r40_ai_dir(settings,scope$fingerprint)
  approval<-web_inside(file.path(dir,"approval"),settings$data_root)
  grant<-web_r40_ai_read(web_inside(file.path(approval,"grant.json"),settings$data_root))
  web_assert(identical(grant$scope,scope) && identical(grant$operator,operator),"Only the recorded operator can revoke this approval.")
  if(dir.create(file.path(approval,"revoked"),mode="0700",showWarnings=FALSE))
    web_r40_ai_write(list(scope=scope,operator=operator,at=web_now()),file.path(approval,"revoked","record.json"))
  invisible(TRUE)
}

web_r40_ai_cost <- function(usage,g) {
  input<-usage$input_tokens
  cached<-usage$cached_input_tokens
  output<-usage$output_tokens
  web_assert(web_r40_ai_uint(input,g$max_input_tokens) && web_r40_ai_uint(cached,input) &&
    web_r40_ai_uint(output,g$max_output_tokens),"R/40 provider usage is missing or outside its approved bounds.","SYSTEM_FAILED")
  ceiling(((as.double(input)-cached)*g$input_microusd_per_million+as.double(cached)*g$cached_input_microusd_per_million+
    as.double(output)*g$output_microusd_per_million)/1e6)
}

web_r40_ai_call <- function(settings,scope,request,policy,transport,mode,check_lease) {
  web_assert(request$step %in% web_r40_ai_steps() && identical(request$policy_sha256,policy$sha256) &&
    all(vapply(request[c("system","user")],function(s)is.character(s) && length(s)==1L && nzchar(s),logical(1))),
    "Invalid R/40 semantic request.","VALIDATION_FAILED")
  dir<-web_r40_ai_dir(settings,scope$fingerprint)
  approval<-web_inside(file.path(dir,"approval","grant.json"),settings$data_root,FALSE)
  if(!file.exists(approval))return(list(state="AI_APPROVAL_REQUIRED"))
  g<-web_r40_ai_read(approval)
  # Live classes are selected only in the Worker. Tests inject their own transport.
  if(!identical(g$mode,mode))return(list(state="AI_APPROVAL_MODE_MISMATCH"))
  system<-paste(policy$text,request$system,sep="\n\n")
  body<-list(model=g$model,system=system,user=request$user,max_output_tokens=g$max_output_tokens)
  identity<-web_c2_hash(list(scope=scope,step=request$step,body=body))
  slot<-web_inside(file.path(dir,request$step),settings$data_root,FALSE)
  result_file<-web_inside(file.path(slot,"result.json"),settings$data_root,FALSE)
  if(file.exists(result_file)) {
    response<-web_r40_ai_read(result_file)
    web_assert(identical(response$id,identity) && identical(response$mode,mode),
      "R/40 checkpoint belongs to a different request.","STALE_UPSTREAM_VERSION")
    response$reused<-TRUE
    return(response)
  }
  if(dir.exists(slot))return(list(state="AI_REQUEST_OUTCOME_UNKNOWN"))
  if(dir.exists(file.path(dirname(approval),"revoked")))return(list(state="AI_APPROVAL_REVOKED"))
  if(mode=="LIVE" && !identical(Sys.getenv("SRMA_R40_ALLOW_PAID"),"1"))return(list(state="AI_LIVE_DISABLED"))
  reserve<-tryCatch(web_r40_ai_validate_grant(g,scope,mode),error=function(e)NULL)
  if(is.null(reserve))return(list(state="AI_APPROVAL_INVALID_OR_EXPIRED"))
  if(mode=="LIVE" && !nzchar(Sys.getenv("OPENAI_API_KEY","")))return(list(state="AI_PROVIDER_CREDENTIAL_REQUIRED"))
  # Use full approved input ceiling for cost reservation, including framing.
  if(nchar(enc2utf8(system),type="bytes")+nchar(enc2utf8(request$user),type="bytes")+1024>g$max_input_tokens)
    return(list(state="AI_INPUT_LIMIT_EXCEEDED"))
  if(mode=="LIVE")web_no_secrets(body,settings$engine_root)
  if(!isTRUE(check_lease()))return(list(state="WORKER_LEASE_LOST"))
  if(!dir.create(slot,mode="0700",showWarnings=FALSE))return(list(state="AI_REQUEST_OUTCOME_UNKNOWN"))
  web_r40_ai_write(list(id=identity,scope=scope,step=request$step,model=g$model,
    reserve_microusd=reserve,at=web_now()),file.path(slot,"reservation.json"))
  if(!isTRUE(check_lease()) || dir.exists(file.path(dirname(approval),"revoked")))
    return(list(state="AI_REQUEST_STOPPED_BEFORE_DISPATCH",id=identity))
  outcome<-tryCatch({
    response<-transport(body)
    web_assert(identical(response$model,g$model) && nzchar(srma_qc_one(response$provider_id)),
      "Unverified R/40 provider response.","SYSTEM_FAILED")
    usage<-response$usage
    cost<-web_r40_ai_cost(usage,g)
    status<-if(!identical(response$completed,FALSE) && nzchar(srma_qc_one(response$text)) &&
      nchar(response$text,type="bytes")<=512000L)"COMPLETE" else "AI_RESPONSE_INCOMPLETE"
    list(state=status,id=identity,mode=mode,step=request$step,model=g$model,
      text=if(status=="COMPLETE")response$text,
      provider_id=response$provider_id,usage=usage,cost_microusd=cost,at=web_now())
  },error=function(e)NULL)
  if(is.null(outcome))return(list(state="AI_REQUEST_OUTCOME_UNKNOWN",id=identity,reserve_microusd=reserve,dispatched=TRUE))
  web_r40_ai_write(outcome,result_file)
  outcome$reused<-FALSE
  outcome$dispatched<-TRUE
  outcome
}

web_r40_ai_openai <- function(body) {
  key<-Sys.getenv("OPENAI_API_KEY","")
  web_assert(nzchar(key),"R/40 provider credential is unavailable.","DEPENDENCY_FAILED")
  payload<-list(model=body$model,max_output_tokens=body$max_output_tokens,store=FALSE,service_tier="default",
    input=list(list(role="system",content=body$system),list(role="user",content=body$user)))
  ans<-httr2::request("https://api.openai.com/v1/responses") |>
    httr2::req_headers(Authorization=paste("Bearer",key)) |>
    httr2::req_options(followlocation=FALSE) |>
    httr2::req_body_json(payload,auto_unbox=TRUE) |>
    httr2::req_timeout(seconds=150) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector=FALSE)
  content<-unlist(lapply(Filter(function(o)identical(o$type,"message"),ans$output),function(o)
    lapply(Filter(function(a)identical(a$type,"output_text"),o$content),`[[`,"text")),use.names=FALSE)
  list(model=ans$model,provider_id=ans$id,completed=identical(ans$status,"completed"),text=paste(content,collapse="\n"),usage=list(
    input_tokens=ans$usage$input_tokens,output_tokens=ans$usage$output_tokens,
    cached_input_tokens=ans$usage$input_tokens_details$cached_tokens %||b2% 0L))
}
