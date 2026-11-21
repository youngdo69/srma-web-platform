# Rscript --vanilla tests/test_rc286_ai_mock.R
# No API calls. Tests Include-only population, label/source validation,
# and AI recommendation isolation. Run in the repository root.
source("web/R/core.R")
source("web/R/evidence.R")
source("web/adapter/evidence_ai.R")
ids <- c("A","B","C","D")
dec <- c("Include","Unclear","Exclude","Include")
make <- function(i) list(screening_id=ids[[i]],title=paste("Title",ids[[i]]),
    abstract=paste("Abstract",ids[[i]]),decision=dec[[i]],
    .actor="AI_AGENT_REVIEWER_1",.response_id=paste0("response",ids[[i]]))
x <- list(stage="SCREENING",tables=list(Reviewer_1=lapply(1:4,make),
    Reviewer_2=lapply(1:4,function(i)list(screening_id=ids[[i]])),
    AI_Screening=lapply(1:4,make)),choices=list(reasons=list("Not eligible")))
saved_r1<-unserialize(serialize(x$tables$Reviewer_1,NULL))
saved_ai<-unserialize(serialize(x$tables$AI_Screening,NULL))
# Inject a trusted R/19 binder and provider response to test boundaries without network.
web_b2_bind_agent_reviewer1 <- function(x,root,manifest=NULL)x
seen <- character()
web_b2_design_response <- function(engine,group) {
    ii<-vapply(group,function(r)as.character(r$screening_id),character(1))
    seen <<- c(seen,ii)
    list(rows=lapply(ii,function(id)list(record_id=id,
      classification=if(id=="A")"NON_COMPARATIVE" else "COMPARATIVE",
      rationale="Design justified from abstract",evidence_excerpt=paste("Title",id))),
      model="mock-model",provider="openai",response_id="mock-response")
}
y<-web_b2_design_classify("unused","unused",x)
stopifnot(identical(seen,c("A","D")),
    identical(saved_r1,y$tables$Reviewer_1),
    identical(saved_ai,y$tables$AI_Screening),
    length(y$tables$AI_Study_Design)==2L,
    identical(web_b2_study_design_validate(y,TRUE)$classes,c("NON_COMPARATIVE","COMPARATIVE")))
# A repeat on the already-classified version must not call the provider.
web_b2_design_response <- function(engine,group) stop("double-charged provider call")
y2<-web_b2_design_classify("unused","unused",y)
stopifnot(identical(y2$tables$AI_Study_Design,y$tables$AI_Study_Design))
cat("PASS: RC28.6 mock provider Include-only, version-provenance, no repeated calls\n")
