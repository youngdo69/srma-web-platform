source("web/load.R")
source("web/qc/runtime.R",encoding="UTF-8")

pdf_sha<-strrep("a",64L)
pdf_path<-"data/raw/reports/study.pdf"
manifest<-setNames(list(pdf_sha),pdf_path)
x<-list(source=list(research_question=list(outcomes=list(
  primary=list("Reported outcome","Pending outcome","Missing outcome"))),
  narrative=list(rows=list(list(study_id="selected",common_variable=" Reported   Outcome ",
    decision="NOT_REPORTED",verified_by="reviewer",verified_at="recorded",
    pdf_relative=pdf_path,pdf_sha256=pdf_sha)))))
e<-list(final_study_selection=list(study_ids=list("selected")),
  tables=list(Target_Outcome_Verification=list(
    list(study_id="selected",outcome_name="pending outcome"),
    list(study_id="excluded",outcome_name="missing outcome"))))
inventory<-srma_qc_link_primary_inventory(x,e,manifest,TRUE)
stopifnot(identical(vapply(inventory,`[[`,character(1),"state"),
  c("VERIFIED_ROWS","REVIEW_PENDING","NO_MATCH_IN_BOUND_INVENTORY")),
  identical(inventory[[1L]]$decisions,list("NOT_REPORTED")),
  is.null(srma_qc_link_primary_inventory(x,e,manifest,FALSE)))
manifest[[pdf_path]]<-strrep("b",64L)
stopifnot(identical(srma_qc_link_primary_inventory(x,e,manifest,TRUE)[[1L]]$state,"REVIEW_PENDING"))
cat("Primary inventory exact matching, explicit NOT_REPORTED and source hash rejection PASS\n")
