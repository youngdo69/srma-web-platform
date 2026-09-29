# Separate-process read of completed, invalidated QC; no state fabrication.
source("web/load.R")
x<-readRDS(Sys.getenv("C3_SAVED_STATE"));s<-x$settings;s$engine_root<-getwd();con<-web_db(s)
stopifnot(DBI::dbGetQuery(con,"PRAGMA integrity_check")[[1]]=="ok")
q<-web_c3_view(con,s,"local-researcher",x$qc$project_id,x$qc$qc_id)
stopifnot(q$status=="PASSED",q$result$machine_pass,q$effective_status=="INVALIDATED",
 !q$eligibility$RELEASE_ELIGIBLE,!q$eligibility$HUMAN_QC_APPROVED)
stopifnot(nrow(web_c3_list(con,"local-researcher",x$qc$project_id))>=3)
DBI::dbDisconnect(con)
cat("PASS: separate-process SQLite integrity, immutable approved QC history and persisted upstream invalidation\n")
