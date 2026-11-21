# Web-only, no DB, AI calls, Golden R execution, or source modifications.
# Run from repo root: Rscript --vanilla tests/test_final_study_selection.R
stopifnot(file.exists("web/R/evidence.R"),file.exists("web/ui/evidence.R"),
          file.exists("web/adapter/evidence.R"))
for(f in c("web/R/evidence.R","web/ui/evidence.R","web/adapter/evidence.R"))parse(file=f)
mod<-new.env(parent=globalenv());source("web/R/evidence.R",local=mod)
mod$web_assert<-function(condition,message,code="USER_ACTION_REQUIRED")
 if(!isTRUE(condition))stop(message,call.=FALSE)
row<-function(id,study,title)list(screening_id=id,study_id=study,title=title)
x<-list(stage="EXTRACTION",included_reports=as.list(c("SCR_001","SCR_002","SCR_003")),
 tables=list(Author_Final_Review=list(row("SCR_001","S1","Trial A"),
    row("SCR_002","S1","Trial A follow-up"),row("SCR_003","S2","Trial B")),
    AI_outcome_data=list(list(screening_id="SCR_001",outcome_name="Death"),
      list(screening_id="SCR_002",outcome_name="Reintervention"),
      list(screening_id="SCR_003",outcome_name="Stroke")),
    Target_Outcome_Verification=list(list(screening_id="SCR_001",.row_id="a"),
      list(screening_id="SCR_002",.row_id="b"),
      list(screening_id="SCR_003",.row_id="c"))))
cat1<-mod$web_b2_final_study_catalog(x)
stopifnot(length(cat1)==2L,identical(cat1[[1L]]$study_id,"S1"),
          identical(cat1[[1L]]$screening_ids,c("SCR_001","SCR_002")))
stopifnot(identical(mod$web_b2_final_active_reports(x),c("SCR_001","SCR_002","SCR_003")))
no_decision<-try(mod$web_b2_final_active_reports(x,TRUE),silent=TRUE)
stopifnot(inherits(no_decision,"try-error"))
x$final_study_selection<-list(status="HUMAN_CONFIRMED",study_ids=list("S2"))
stopifnot(identical(mod$web_b2_final_active_reports(x,TRUE),"SCR_003"))
filter<-mod$web_b2_final_filter_rows(x,x$tables$Target_Outcome_Verification)
stopifnot(length(filter)==1L,identical(filter[[1L]]$screening_id,"SCR_003"),
  length(x$tables$Target_Outcome_Verification)==3L,
  length(x$tables$AI_outcome_data)==3L)
x$final_study_selection<-list(status="HUMAN_CONFIRMED",study_ids=list("S2","INVALID"))
stopifnot(inherits(try(mod$web_b2_final_active_reports(x,TRUE),silent=TRUE),"try-error"))
stopifnot(identical(mod$web_b2_selection_values(list(study_ids_json='["S1","S2"]'),"study_ids_json"),c("S1","S2")))
stopifnot(identical(mod$web_b2_selection_values(list(outcome_row_ids_json='[]'),"outcome_row_ids_json"),character()))
stopifnot(inherits(try(mod$web_b2_selection_values(list(study_ids_json='["S1","S1"]'),"study_ids_json"),silent=TRUE),"try-error"))
cat("PASS: final selection by Study ID, linked reports, no silent default, single source filtering, provenance preserved, invalid IDs rejected\n")
