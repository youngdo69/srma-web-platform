# Operator-only. Creating a grant performs no provider requests or DB writes.
source("web/load.R")
source("web/qc/runtime.R",encoding="UTF-8")
args<-commandArgs(trailingOnly=TRUE)
stopifnot(length(args) %in% c(3L,4L))
settings<-web_settings()
con<-if(settings$backend=="sqlite")DBI::dbConnect(RSQLite::SQLite(),settings$sqlite_file,flags=RSQLite::SQLITE_RO) else web_db(settings)
tryCatch({
  pre<-web_r40_full_preflight(con,settings,args[[1L]],args[[2L]],args[[3L]])
  plan<-pre$publication$r40_validation_plan
  if(length(args)==3L)cat(jsonlite::toJSON(plan$execution_scope,auto_unbox=TRUE,pretty=TRUE),"\n") else if(args[[4L]]=="--revoke") {
    web_r40_ai_revoke(settings,plan$execution_scope,args[[1L]])
    cat("R/40 scoped approval revoked. Existing call records are retained.\n")
  } else {
    stopifnot(isTRUE(plan$native_ready),isTRUE(file.info(args[[4L]])$size<=16384L))
    grant<-jsonlite::read_json(args[[4L]],simplifyVector=FALSE)
    fields<-c("fingerprint","operator","explicit_paid_approval","model","rate_source","model_context_tokens",
      "expires_at","budget_microusd","max_input_tokens","max_output_tokens","input_microusd_per_million",
      "cached_input_microusd_per_million","output_microusd_per_million")
    stopifnot(identical(grant$fingerprint,plan$execution_scope$fingerprint),
      identical(grant$operator,args[[1L]]),isTRUE(grant$explicit_paid_approval),
      setequal(names(grant),fields),!anyDuplicated(names(grant)))
    web_r40_ai_grant(settings,plan$execution_scope,grant,"LIVE")
    cat("R/40 scoped approval recorded. No provider call was made.\n")
  }
},finally=DBI::dbDisconnect(con))
