# RC28.19 Web-only offline scientific-boundary regression.
# No database, PDF AI request, Golden R call or project data mutation.
stopifnot(requireNamespace("digest",quietly=TRUE),requireNamespace("jsonlite",quietly=TRUE))
for(f in c("web/R/evidence.R","web/ui/evidence.R","web/adapter/evidence.R")) parse(file=f)
mod<-new.env(parent=globalenv())
source("web/R/evidence.R",local=mod)
mod$web_assert<-function(condition,message,code="USER_ACTION_REQUIRED")
 if(!isTRUE(condition))stop(message,call.=FALSE)
mod$web_id<-local({i<-0L;function(){i<<-i+1L;paste0("new_",i)}})
mod$web_now<-function()"2026-11-08T00:00:00"
mk<-function(id,src,name,def,t,ev)list(screening_id=id,.row_id=src,
  outcome_row_id=paste0(id,"_",src),outcome_name=name,outcome_definition=def,
  timepoint=t,data_type="dichotomous",intervention_events=as.character(ev),
  intervention_total="100",comparator_events="2",comparator_total="100",
  effect_measure="",source_location="Table 2",source_evidence="PDF report")
old<-list(screening_id="SCR_003",outcome_name="Old protocol template",.row_id="historical",
  outcome_row_id="old",human_include_in_planned_analysis="No",verification_status="Needs clarification")
x<-list(stage="EXTRACTION",included_reports=as.list(c("SCR_001","SCR_002","SCR_003")),
  final_study_selection=list(status="HUMAN_CONFIRMED",study_ids=list("S1","S2")),
  result_inventory_selection=NULL,
  tables=list(Author_Final_Review=list(
    list(screening_id="SCR_001",study_id="S1",title="First",.pdf_sha256="sha1"),
    list(screening_id="SCR_002",study_id="S2",title="Second",.pdf_sha256="sha2"),
    list(screening_id="SCR_003",study_id="S3",title="Excluded",.pdf_sha256="sha3")),
    AI_outcome_data=list(mk("SCR_001","ai1","AKI", "Renal complication, PDF-specific", "In-hospital",5L),
      mk("SCR_001","ai2","AKI", "Renal complication, PDF-specific", "In-hospital",5L),
      mk("SCR_002","ai3","Acute kidney injury", "Acute renal injury, study-defined", "At discharge",7L),
      mk("SCR_003","ai4","Death", "Mortality", "30 days",99L)),
    Target_Outcome_Verification=list(old)),
  pdf_result_discovery=list(status="COMPLETE",
    report_checksums=list(SCR_001="sha1",SCR_002="sha2"),
    results=list(
      list(result_id="RI_001",screening_id="SCR_001",reported_name="AKI",reported_definition="Renal complication, PDF-specific",reported_timepoint="In-hospital",source_location="Table 2",pdf_sha256="sha1"),
      list(result_id="RI_002",screening_id="SCR_002",reported_name="Acute renal injury",reported_definition="Acute renal injury, study-defined",reported_timepoint="At discharge",source_location="Table 2",pdf_sha256="sha2"),
      list(result_id="RI_003",screening_id="SCR_003",reported_name="Death",reported_definition="Mortality",reported_timepoint="30 days",source_location="Table 3",pdf_sha256="sha3"))))
stopifnot(identical(mod$web_b2_final_active_reports(x,TRUE),c("SCR_001","SCR_002")))
groups<-mod$web_b2_result_inventory(x)
stopifnot(length(groups)==2L,
  length(groups[[1]]$sources)==2L,length(groups[[2]]$sources)==1L,
  !any(vapply(groups,function(g)identical(g$screening_id,"SCR_003"),logical(1))))
selected<-vapply(groups,`[[`,character(1),"id")
x$result_inventory_selection<-list(status="HUMAN_SELECTED",group_ids=as.list(selected))
mods<-list(
  list(group_id=selected[[1]],source_row_id="ai1",final_name="AKI",final_definition="Acute kidney injury",
    analysis_window="In-hospital",effect_measure="RR",meta_analysis_role="Primary",parent_analysis_group=""),
  list(group_id=selected[[2]],source_row_id="ai3",final_name="AKI",final_definition="Acute kidney injury",
    analysis_window="In-hospital",effect_measure="RR",meta_analysis_role="Primary",parent_analysis_group=""))
mod$web_b2_context<-function(...)list(packet=x)
mod$web_copy_version<-function(...)invisible(TRUE)
mod$web_b2_invalidate<-function(...)invisible(TRUE)
mod$web_b2_write<-function(root,value)assign("captured",value,envir=.GlobalEnv)
mod$web_b2_save_snapshot<-function(...)list(version_id="MOCK_NEW_VERSION")
settings<-list(data_root=tempdir())
mj<-as.character(jsonlite::toJSON(mods,auto_unbox=TRUE))
mod$web_b2_save_human_harmonization(NULL,settings,"reviewer","p","previous",list(mappings_json=mj))
y<-get("captured",envir=.GlobalEnv)
new<-Filter(function(z)isTRUE(z$.web_harmonized_result),y$tables$Target_Outcome_Verification)
stopifnot(length(new)==2L,identical(new[[1]]$original_ai_source_row_id,"ai1"),
  identical(new[[2]]$original_ai_source_row_id,"ai3"),
  identical(new[[1]]$intervention_events,"5"),
  identical(new[[2]]$intervention_events,"7"),
  identical(new[[2]]$timepoint,"At discharge"),
  identical(new[[2]]$harmonized_analysis_window,"In-hospital"),
  identical(new[[2]]$original_ai_timepoint,"At discharge"),
  identical(new[[2]]$original_pdf_result_name,"Acute renal injury"),
  all(vapply(new,function(z)identical(z$.actor,"reviewer"),logical(1))),
  all(vapply(new,function(z)identical(z$verification_status,"Needs clarification"),logical(1))),
  !any(vapply(new,function(z)isTRUE(z$.verified_by),logical(1))),
  identical(y$tables$AI_outcome_data,x$tables$AI_outcome_data),
  identical(y$human_outcome_harmonization$status,"HUMAN_CONFIRMED"),
  identical(y$final_study_selection,x$final_study_selection))
# A published AI source with multiple data representations cannot be silently
# selected twice into the same study & analysis group.
bad<-mods;bad[[2]]$source_row_id<-"ai2";bad[[2]]$group_id<-selected[[1]]
stopifnot(inherits(try(mod$web_b2_harmonization_mapping_values(
  list(mappings_json=as.character(jsonlite::toJSON(bad,auto_unbox=TRUE)))),silent=TRUE),"try-error")==FALSE)
# Guard against stale/duplicate group assignment in validation at save time.
stopifnot(inherits(try(mod$web_b2_save_human_harmonization(NULL,settings,"reviewer","p","previous",
  list(mappings_json=as.character(jsonlite::toJSON(bad,auto_unbox=TRUE)))),silent=TRUE),"try-error"))
rm(captured,envir=.GlobalEnv)
cat("PASS: PDF-specific result inventory, selected-study scope, human mapping, original AI provenance, separate unverified numerics\n")
