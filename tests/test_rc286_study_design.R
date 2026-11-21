# Run under the repository root with: Rscript --vanilla tests/test_rc286_study_design.R
# No external provider calls: deterministic contract tests for classification policy.
source('web/R/core.R')
source('web/R/evidence.R')

make_rows <- function(ids, decisions, actor=TRUE) {
  lapply(seq_along(ids),function(i) {
    r<-list(screening_id=ids[[i]],decision=decisions[[i]],
      title=paste('Title',ids[[i]]),abstract=paste('Abstract',ids[[i]]),
      .response_id=paste0('response-',ids[[i]]))
    if(actor) r$.actor<-'AI_AGENT_REVIEWER_1'
    r
  })
}
ids<-c('A','B','C','D','E','F')
decisions<-c('Include','Unclear','Exclude','Include','Include','Unclear')
x<-list(stage='SCREENING',tables=list(
  Reviewer_1=make_rows(ids,decisions),
  Reviewer_2=make_rows(ids,rep('',length(ids)),FALSE),
  AI_Screening=make_rows(ids,decisions)),
  choices=list(reasons=list('Not eligible','Other')))
original<-unserialize(serialize(x,NULL))
hash<-web_b2_design_source_digest(x)
make_design<-function(id,cl)list(screening_id=id,original_ai_decision='Include',
  classification=cl,rationale='AI classification based on title/abstract',
  .source='AI RECOMMENDATION',.verification='UNVERIFIED',
  .source_ai_sha256=hash,.provider='OpenAI',.model='test-model',
  .response_id='resp-1',evidence_excerpt='Test source excerpt')
x$tables$AI_Study_Design<-list(make_design('A','NON_COMPARATIVE'),
  make_design('D','COMPARATIVE'),make_design('E','UNCLEAR'))
stopifnot(identical(web_b2_study_design_validate(x,TRUE)$ids,c('A','D','E')))
for(policy in c('PASS','SINGLE_ARM','AI_UNCLEAR','BOTH')) {
  p<-web_b2_comparative_policy_targets(x,policy,if(policy=='PASS')'' else 'Protocol-dependent exclusion is approved.')
  stopifnot(p$counts[['total']]==6L,p$counts[['ai_include']]==3L,
    p$counts[['ai_unclear']]==2L,p$counts[['ai_exclude']]==1L)
  if(policy=='PASS')stopifnot(length(p$auto_ids)==0L)
  if(policy=='SINGLE_ARM')stopifnot(identical(p$auto_ids,'A'))
  if(policy=='AI_UNCLEAR')stopifnot(setequal(p$auto_ids,c('B','F')))
  if(policy=='BOTH')stopifnot(setequal(p$auto_ids,c('A','B','F')),
    p$counts[['policy_auto_exclude']]==3L,p$counts[['human_review']]==2L)
}
# Prior Human override is never replaced, even when it is noncomparative.
x$tables$Reviewer_2[[1]]$.actor<-'human-reviewer'
p<-web_b2_comparative_policy_targets(x,'BOTH','Human researcher approved these protocol criteria.')
stopifnot(!('A' %in% p$auto_ids),p$protected_human==1L)
x$tables$Reviewer_2[[1]]$.actor<-NULL
# AI Unclear and AI Exclude must never enter the supplemental design population.
invalid<-x
invalid$tables$AI_Study_Design[[1]]$screening_id<-'B'
stopifnot(inherits(try(web_b2_study_design_validate(invalid,TRUE),silent=TRUE),'try-error'))
invalid<-x
invalid$tables$AI_Study_Design[[1]]$.source_ai_sha256<-'stale'
stopifnot(inherits(try(web_b2_study_design_validate(invalid,TRUE),silent=TRUE),'try-error'))
invalid<-x
invalid$tables$AI_Study_Design<-NULL
stopifnot(inherits(try(web_b2_comparative_policy_targets(invalid,'SINGLE_ARM',
  'Researcher-approved protocol criteria.'),silent=TRUE),'try-error'))
stopifnot(identical(original$tables$Reviewer_1,x$tables$Reviewer_1),
  identical(original$tables$AI_Screening,x$tables$AI_Screening))
cat('PASS: RC28.6 include-only, no re-review, human override, unique counts, version hashes\n')

# New Worker-stage registration and finalizer must use the authenticated
# Include-only design table; a missing legacy keyword helper is forbidden.
stopifnot(identical(unname(web_b2_operations()[["recommend_study_design"]]),"SCREENING"),
  "recommend_study_design" %in% names(web_b2_ai_operations()))
worker_lines <- readLines("web/adapter/evidence.R",warn=FALSE)
stopifnot(any(grepl("x<-web_b2_ai(op,engine,root,paths,x,run)",worker_lines,fixed=TRUE)),
  any(grepl('authenticated_noncomparative',worker_lines,fixed=TRUE)),
  !any(grepl('web_b2_explicit_single_arm',worker_lines,fixed=TRUE)))
cat("PASS: RC28.6 Worker dispatch and finalization checks\n")
