# RC28.19 offline: no API, DB, PDFs or Scientific R execution.
stopifnot(requireNamespace("jsonlite",quietly=TRUE))
for(f in c("web/R/evidence.R","web/ui/evidence.R","web/adapter/evidence.R","web/adapter/pdf_results.R"))parse(file=f)
mod<-new.env(parent=globalenv())
source("web/R/evidence.R",local=mod)
mod$web_assert<-function(condition,message,code="USER_ACTION_REQUIRED")
 if(!isTRUE(condition))stop(message,call.=FALSE)
x<-list(stage="EXTRACTION",included_reports=list("SCR_01","SCR_02","SCR_03"),
 final_study_selection=list(status="HUMAN_CONFIRMED",study_ids=list("S1","S2")),
 tables=list(
  Author_Final_Review=list(
    list(screening_id="SCR_01",study_id="S1",.pdf_sha256="pdf1"),
    list(screening_id="SCR_02",study_id="S2",.pdf_sha256="pdf2"),
    list(screening_id="SCR_03",study_id="S3",.pdf_sha256="pdf3")),
  AI_outcome_data=list(
    list(screening_id="SCR_01",.row_id="source1",outcome_name="Protocol only",outcome_definition="X",timepoint="30 days",intervention_events="5"),
    list(screening_id="SCR_03",.row_id="source3",outcome_name="Excluded",intervention_events="99"))),
 pdf_result_discovery=list(status="COMPLETE",report_checksums=list(SCR_01="pdf1",SCR_02="pdf2"),
   results=list(
     list(result_id="RI_A",screening_id="SCR_01",reported_name="Original paper label",reported_definition="Definition A",reported_timepoint="At discharge",source_location="Table 2",pdf_sha256="pdf1"),
     list(result_id="RI_B",screening_id="SCR_02",reported_name="Another result without R/25 numbers",reported_definition="Study-defined",reported_timepoint="",source_location="Results",pdf_sha256="pdf2"),
     list(result_id="RI_EXCLUDED",screening_id="SCR_03",reported_name="Should not appear",reported_timepoint="30 days",source_location="Table 3",pdf_sha256="pdf3"))))
stopifnot(isTRUE(mod$web_b2_discovery_ready(x)))
g<-mod$web_b2_result_inventory(x)
stopifnot(length(g)==2L,
  identical(vapply(g,function(z)z$reported_name,character(1)),
    c("Original paper label","Another result without R/25 numbers")),
  identical(g[[1]]$reported_timepoint,"At discharge"),
  identical(g[[2]]$reported_timepoint,""),
  length(g[[1]]$sources)==1L,length(g[[2]]$sources)==0L,
  !any(vapply(g,function(z)z$screening_id=="SCR_03",logical(1))))
# Source PDF change makes discovery stale; do not silently reuse.
x$tables$Author_Final_Review[[1]]$.pdf_sha256<-"changed"
stopifnot(!mod$web_b2_discovery_ready(x),length(mod$web_b2_result_inventory(x))==0L)
# No research domain/outcome labels hard-coded in the PDF discovery schema.
source_code<-paste(readLines("web/adapter/pdf_results.R",warn=FALSE),collapse="\n")
stopifnot(grepl("source_location",source_code,fixed=TRUE),
          grepl("report_checksums",source_code,fixed=TRUE),
          grepl("reported_name",source_code,fixed=TRUE),
          !grepl("outcome_name = list(type",source_code,fixed=TRUE))
cat("PASS: PDF results inventory is study-specific, source-hash bound, no protocol-template cloning, no excluded studies\n")
