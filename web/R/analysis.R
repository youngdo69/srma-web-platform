# C1 orchestrates immutable canonical results; it contains no statistical formulas.
web_c1_operations <- function() c(run_meta_analysis="PLAN")
web_c1_methods <- function() list(model="Random-effects",estimator="REML",inference="Knapp-Hartung",confidence="95%",binary_correction="0.5 only0; drop00 FALSE",transformation="Canonical measure-specific transformation; ratio measures log scale")
web_c1_hash <- function(x) digest::digest(as.character(web_json(x)),algo="sha256",serialize=FALSE)
web_c1_read <- function(root) jsonlite::read_json(file.path(root,"config/generated/web_analysis.json"),simplifyVector=FALSE)
web_c1_write <- function(root,x) jsonlite::write_json(x,file.path(root,"config/generated/web_analysis.json"),auto_unbox=TRUE,null="null",pretty=TRUE,digits=NA)
web_c1_csv <- function(path) readr::read_csv(path,col_types=readr::cols(.default=readr::col_character()),show_col_types=FALSE,na=character())
web_c1_current <- function(con,project_id,evidence_version) {
 # A newer evidence revision invalidates active use, but never changes old approvals.
 n<-web_query(con,"SELECT v.id FROM project_versions v LEFT JOIN analysis_versions a ON a.version_id=v.id WHERE v.project_id=?p AND a.version_id IS NULL AND v.version>(SELECT version FROM project_versions WHERE id=?v AND project_id=?p)",p=project_id,v=evidence_version)
 web_assert(nrow(n)==0L,"Evidence has changed. Finish and approve the new evidence, then create a new analysis plan.","USER_ACTION_REQUIRED")
}
web_c1_evidence <- function(con,settings,actor,project_id,version_id,current=TRUE) {
 c<-web_b2_context(con,settings,actor,project_id,version_id)
 web_assert(c$row$stage=="READY","Use the exact Human-approved analysis-ready evidence version.","USER_ACTION_REQUIRED")
 j<-web_scoped_job(con,actor,project_id,c$row$parent_version_id,c$row$source_job_id,"finalize_evidence")
 g<-web_query(con,"SELECT id FROM gates WHERE project_id=?p AND version_id=?v AND gate_type='b2_rob' AND status='APPROVED'",p=project_id,v=c$row$parent_version_id)
 web_assert(j$state=="COMPLETE"&&nrow(g)==1L,"The evidence lacks a completed Human-approved finalization.")
 if(current)web_c1_current(con,project_id,version_id)
 c
}
web_c1_catalog <- function(con,settings,actor,project_id,version_id) {
 c<-web_c1_evidence(con,settings,actor,project_id,version_id);root<-c$version$root
 m<-web_c1_csv(file.path(root,"data/processed/meta_analysis_ready/meta_analysis_manifest.csv"))
 group_rows<-setNames(lapply(m$dataset_file,function(p){rel<-file.path("data/processed/meta_analysis_ready",basename(p));web_assert(rel %in% names(c$version$manifest),"Unknown evidence dataset.");as.list(web_c1_csv(web_inside(file.path(root,rel),root))$outcome_row_id)}),m$analysis_group)
 parents<-web_query(con,"SELECT a.version_id,v.version FROM analysis_versions a JOIN project_versions v ON v.id=a.version_id WHERE a.project_id=?p AND a.evidence_version_id=?e AND a.stage='APPROVED' ORDER BY v.version DESC",p=project_id,e=version_id)
 parents$label<-character(nrow(parents));keep<-logical(nrow(parents))
 for(i in seq_len(nrow(parents))){px<-web_c1_context(con,settings,actor,project_id,parents$version_id[i])$packet;keep[i]<-px$plan$role %in% c("PRIMARY","SECONDARY");parents$label[i]<-paste("Version",parents$version[i],px$plan$role,px$plan$outcome,px$plan$timepoint)}
 parents<-parents[keep,,drop=FALSE]
 list(groups=m,group_rows=group_rows,parents=parents,methods=web_c1_methods(),rows=web_b2_table(c$packet$tables$Target_Outcome_Verification),evidence_version=version_id)
}
web_c1_context <- function(con,settings,actor,project_id,version_id,current=FALSE) {
 v<-web_version_root(con,settings,actor,project_id,version_id)
 a<-web_query(con,"SELECT * FROM analysis_versions WHERE project_id=?p AND version_id=?v",p=project_id,v=version_id)
 web_assert(nrow(a)==1L,"Select an analysis version belonging to this project.")
 x<-web_c1_read(v$root)
 web_assert(identical(x$analysis_id,a$analysis_id)&&identical(x$stage,a$stage)&&identical(web_c1_hash(x$plan),a$plan_sha256)&&identical(x$evidence_version,a$evidence_version_id),"Analysis lineage failed integrity verification.")
 if(current)web_c1_current(con,project_id,a$evidence_version_id)
 list(version=v,row=a,packet=x)
}
web_c1_save <- function(con,settings,actor,project_id,parent_version,root,x,job_id=NULL) {
 p<-web_authorize(con,actor,project_id);web_c1_write(root,x)
 web_tx(con,function(){
  web_exec(con,"UPDATE projects SET title=title WHERE id=?p",p=project_id)
  web_c1_current(con,project_id,x$evidence_version)
  v<-web_import_project(con,settings,actor,p$workspace_id,root,configuration_only=TRUE,transaction=function(con,f)f())
  web_assert(v$project_id==project_id,"Analysis project changed.")
  web_exec(con,"INSERT INTO analysis_versions VALUES(?v,?p,?a,?parent,?e,?related,?stage,?hash,?job,?actor,?now)",v=v$version_id,p=project_id,a=x$analysis_id,parent=parent_version,e=x$evidence_version,related=x$plan$parent_version %||b2% NA_character_,stage=x$stage,hash=web_c1_hash(x$plan),job=job_id %||b2% NA_character_,actor=actor,now=web_now())
  if(x$stage %in% c("PLAN","RESULT")) {
   kind<-if(x$stage=="PLAN")"analysis_plan" else "analysis_results"
   op<-if(x$stage=="PLAN")"run_meta_analysis" else "finalize_analysis_results"
   web_exec(con,"INSERT INTO gates(id,project_id,workspace_id,version_id,gate_type,required_operation,status,created_at) VALUES(?g,?p,?w,?v,?k,?op,'PENDING',?now)",g=web_id(),p=project_id,w=p$workspace_id,v=v$version_id,k=kind,op=op,now=web_now())
  }
  web_audit(con,p,paste0("ANALYSIS_",x$stage,"_SAVED"),v$version_id,actor,list(analysis_id=x$analysis_id,evidence=x$evidence_version,plan_sha256=web_c1_hash(x$plan),job=job_id))
  v
 })
}
web_c1_plan <- function(con,settings,actor,project_id,version_id,spec) {
 c<-web_c1_evidence(con,settings,actor,project_id,version_id);root0<-c$version$root
 fields<-c("group","row_ids","role","outcome","definition","timepoint","design","population","estimate_basis","methods","exclusions","rationale","parent_version","diagnostics","exploratory","moderators")
 web_assert(is.list(spec)&&setequal(names(spec),fields)&&!anyDuplicated(names(spec)),"Complete the displayed analysis specification; extra fields are forbidden.")
 for(k in c("group","role","outcome","definition","timepoint","design","population","estimate_basis","rationale"))web_assert(is.character(spec[[k]])&&length(spec[[k]])==1&&!is.na(spec[[k]])&&nzchar(trimws(spec[[k]]))&&nchar(spec[[k]])<=4000,paste("Specify",k))
 web_assert(identical(spec$methods,web_c1_methods()),"Explicitly accept the displayed canonical model/methods. Other methods require protocol and engine review.")
 web_assert(is.logical(spec$diagnostics)&&length(spec$diagnostics)==1&&!is.na(spec$diagnostics)&&is.logical(spec$exploratory)&&length(spec$exploratory)==1&&!is.na(spec$exploratory),"Choose diagnostics and exploratory testing explicitly.")
 ids<-unlist(spec$row_ids,use.names=FALSE);web_assert(is.character(ids)&&length(ids)>0&&!anyNA(ids)&&!anyDuplicated(ids),"Select unique extraction row IDs.")
 m<-web_c1_csv(file.path(root0,"data/processed/meta_analysis_ready/meta_analysis_manifest.csv"));m<-m[m$analysis_group==spec$group,,drop=FALSE];web_assert(nrow(m)==1L,"Select a canonical analysis-ready group.")
 data_path<-file.path("data/processed/meta_analysis_ready",basename(m$dataset_file));web_assert(data_path %in% names(c$version$manifest),"Analysis dataset is not in the evidence manifest.")
 d<-web_c1_csv(web_inside(file.path(root0,data_path),root0));web_assert(all(ids %in% d$outcome_row_id),"Rows do not belong to this exact evidence group.")
 d<-d[match(ids,d$outcome_row_id),,drop=FALSE]
 verified<-c$packet$tables$Target_Outcome_Verification
 rows<-lapply(ids,function(id){hit<-Filter(function(a)identical(a$outcome_row_id,id),verified);web_assert(length(hit)==1L,"Unknown extraction row.");hit[[1]]})
 web_assert(all(vapply(rows,function(a)!is.null(a$.verified_by)&&a$verification_status %in% c("Verified","Corrected")&&identical(a$human_include_in_planned_analysis,"Yes")&&a$screening_id %in% unlist(c$packet$included_reports),logical(1))),"Use only included, independently verified extraction rows.")
 studies<-vapply(rows,function(a)a$study_id %||b2% "",character(1));web_assert(all(nzchar(studies))&&!anyDuplicated(studies),"Resolve dependent or overlapping reports before pooling.","USER_ACTION_REQUIRED")
 rob<-web_c1_csv(file.path(root0,"results/risk_of_bias/rob_final_combined.csv"));web_assert(all(d$screening_id %in% rob$screening_id),"Some selected reports lack Human-reviewed risk of bias.")
 for(k in c("outcome","timepoint","design")) {
  values<-switch(k,outcome=d$outcome_name,timepoint=d$final_timepoint,design=d$design_class)
  web_assert(length(values)>0&&all(!is.na(values)&nzchar(values))&&length(unique(values))==1&&identical(unique(values),spec[[k]]),paste("Resolve inconsistent",k,"in the evidence before analysis."),"USER_ACTION_REQUIRED")
 }
 measures<-unique(d$final_effect_measure);web_assert(length(measures)==1&&measures %in% c("RR","OR","HR","RD","MD","SMD")&&identical(measures,m$effect_measure),"Effect measures need Human reconciliation.")
 web_assert(spec$estimate_basis %in% c("RAW","ADJUSTED","UNADJUSTED"),"Choose raw counts/measurements or the reported estimate adjustment status explicitly.")
 if(spec$estimate_basis=="RAW")web_assert(all(d$meta_input_type %in% c("raw_dichotomous","raw_continuous")),"The estimate policy conflicts with reported-effect rows.")
 else {
  web_assert(all(d$meta_input_type=="reported_effect"),"The estimate policy conflicts with raw-data rows.")
  # B2 per-row source evidence must explicitly identify adjustment, rather than inferred numeric policy.
  web_assert(all(vapply(rows,function(a)toupper(a$adjustment_status %||b2% a$adjusted %||b2% "") %in% if(spec$estimate_basis=="ADJUSTED")c("ADJUSTED","YES","Y","TRUE") else c("UNADJUSTED","NO","N","FALSE"),logical(1))),"Record a consistent explicit adjusted/unadjusted status in B2 evidence before analysis.","USER_ACTION_REQUIRED")
 }
 web_assert(spec$role %in% c("PRIMARY","SECONDARY","SENSITIVITY","SUBGROUP","EXPLORATORY"),"Choose an analysis role.")
 parent<-spec$parent_version
 if(spec$role %in% c("SENSITIVITY","SUBGROUP","EXPLORATORY")) {
  web_assert(is.character(parent)&&length(parent)==1&&nzchar(parent),"Link this analysis to its approved primary/secondary result.")
  pc<-web_c1_context(con,settings,actor,project_id,parent,TRUE)$packet
  web_assert(pc$stage=="APPROVED"&&pc$plan$role %in% c("PRIMARY","SECONDARY")&&pc$evidence_version==version_id,"Use an approved primary/secondary result from the same evidence version.")
  web_assert(all(ids %in% unlist(pc$plan$row_ids)),"A linked subset must use its parent's verified rows.")
 } else {
  web_assert(is.null(parent)||identical(parent,""),"Primary/secondary plans do not take a parent result.");spec$parent_version<-NULL
  web_assert(identical(toupper(m$meta_analysis_role),spec$role),"The role must agree with the approved evidence manifest.")
 }
 omitted<-setdiff(web_c1_csv(file.path(root0,data_path))$outcome_row_id,ids)
 web_assert(is.list(spec$exclusions)&&setequal(names(spec$exclusions) %||b2% character(),omitted)&&all(vapply(spec$exclusions,function(a)is.character(a)&&length(a)==1&&!is.na(a)&&nzchar(trimws(a)),logical(1))),"Record a reason for every excluded analysis-ready row.")
 mods<-unlist(spec$moderators,use.names=FALSE)
 web_assert(!length(mods)||(is.character(mods)&&!anyDuplicated(mods)&&all(mods %in% names(d))),"Select only existing evidence columns as moderators.")
 web_assert(spec$exploratory||!length(mods),"Explicitly enable exploratory diagnostics before selecting moderators.")
 cfg<-yaml::read_yaml(file.path(root0,"config/project_config.yml"))$analysis
 expected<-list(confidence=cfg$statistical_significance$confidence_interval,tau=cfg$heterogeneity$tau_squared_estimator,hk=cfg$heterogeneity$hartung_knapp_adjustment,correction=cfg$zero_event_handling$continuity_correction)
 for(k in names(expected))if(!is.null(expected[[k]]))web_assert(isTRUE(all.equal(expected[[k]],switch(k,confidence=.95,tau="REML",hk=TRUE,correction=.5))),"Project methodology conflicts with the supported canonical model. Resolve the protocol/configuration first.","USER_ACTION_REQUIRED")
 spec$row_ids<-as.list(ids);spec$moderators<-as.list(mods);spec$effect_measure<-measures;spec$study_ids<-as.list(studies);spec$report_ids<-as.list(d$screening_id);spec$dataset_path<-data_path;spec$dataset_sha256<-web_sha(file.path(root0,data_path))
 thresholds<-list(publication_bias=cfg$exploratory_diagnostics$publication_bias$standard_min_k %||b2% cfg$publication_bias$minimum_number_of_studies %||b2% 10L,meta_regression=cfg$exploratory_diagnostics$meta_regression$standard_min_k %||b2% cfg$meta_regression$minimum_number_of_studies %||b2% 10L)
 for(k in names(thresholds))web_assert(is.numeric(thresholds[[k]])&&length(thresholds[[k]])==1&&is.finite(thresholds[[k]])&&thresholds[[k]]>=3&&thresholds[[k]]==as.integer(thresholds[[k]]),"Resolve invalid exploratory study-count thresholds in project configuration.")
 spec$exploratory_policy<-c(thresholds,list(auto_moderators=FALSE,allow_partial_effects=FALSE,underpowered_testing_explicitly_requested=spec$exploratory))
 selected_group_ids<-web_c1_csv(file.path(root0,data_path))$outcome_row_id
 spec$other_evidence_records<-lapply(Filter(function(a)!a$outcome_row_id %in% selected_group_ids,verified),function(a)list(outcome_row_id=a$outcome_row_id,reason=if(identical(a$human_include_in_planned_analysis,"Yes"))"Outside the explicitly selected canonical evidence group" else "Not selected in Human-approved B2 evidence"))
 x<-list(analysis_id=web_id(),evidence_version=version_id,evidence_manifest_sha256=web_c1_hash(c$version$manifest),stage="PLAN",plan=spec,created_at=web_now(),actor=actor)
 root<-tempfile("analysis-plan-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root)
 web_c1_save(con,settings,actor,project_id,version_id,root,x)
}
web_c1_guard <- function(con,settings,actor,project_id,version_id,operation) {
 if(!operation %in% names(web_c1_operations()))return(invisible(TRUE))
 c<-web_c1_context(con,settings,actor,project_id,version_id,TRUE)
 web_assert(c$packet$stage=="PLAN","Run an explicitly approved analysis plan, not a result version.")
 g<-web_query(con,"SELECT id FROM gates WHERE project_id=?p AND version_id=?v AND gate_type='analysis_plan' AND status='APPROVED'",p=project_id,v=version_id)
 web_assert(nrow(g)==1L,"Explicit Human approval of this exact analysis plan is required.","USER_ACTION_REQUIRED")
 web_c1_evidence(con,settings,actor,project_id,c$packet$evidence_version)
 web_science_settings(settings);invisible(TRUE)
}
web_c1_accept <- function(con,settings,actor,project_id,version_id,job_id) {
 c<-web_c1_context(con,settings,actor,project_id,version_id,TRUE)
 j<-web_scoped_job(con,actor,project_id,version_id,job_id,names(web_c1_operations()))
 web_assert(c$packet$stage=="PLAN"&&j$state=="COMPLETE","Save only a successfully completed result for this exact plan.")
 old<-web_query(con,"SELECT version_id FROM analysis_versions WHERE project_id=?p AND source_job_id=?j AND stage='RESULT'",p=project_id,j=job_id);if(nrow(old))return(list(project_id=project_id,version_id=old$version_id))
 root<-tempfile("analysis-result-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root);web_overlay_job(con,settings,actor,project_id,version_id,job_id,root)
 reg<-jsonlite::read_json(file.path(root,"results/web_analysis/result.json"),simplifyVector=FALSE)
 web_assert(identical(reg$plan_sha256,web_c1_hash(c$packet$plan))&&reg$evidence_version==c$packet$evidence_version&&reg$analysis_id==c$packet$analysis_id&&reg$job_id==job_id&&reg$project_version==version_id,"The result does not belong to this plan/evidence/job.")
 for(p in names(reg$output_hashes))web_assert(identical(web_sha(web_inside(file.path(root,p),root)),reg$output_hashes[[p]]),"Result artifact checksum mismatch.")
 x<-c$packet;x$stage<-"RESULT";x$result<-reg;x$source_job<-job_id;x$source_plan_version<-version_id;x$job_provenance<-web_parse(j$result_json)$provenance
 web_c1_save(con,settings,actor,project_id,version_id,root,x,job_id)
}
web_c1_approve <- function(con,settings,actor,project_id,version_id,confirmed,rationale) {
 web_assert(isTRUE(confirmed)&&is.character(rationale)&&length(rationale)==1&&!is.na(rationale)&&nzchar(trimws(rationale)),"Explicitly confirm studies, outcome/timepoint, warnings and analysis role, with a rationale.")
 c<-web_c1_context(con,settings,actor,project_id,version_id,TRUE);web_assert(c$packet$stage=="RESULT","Review a completed result version.")
 web_assert(identical(c$packet$result$status,"PASS"),"Non-estimable or failed results cannot become an approved statistical source.")
 g<-web_query(con,"SELECT id,status FROM gates WHERE project_id=?p AND version_id=?v AND gate_type='analysis_results'",p=project_id,v=version_id)
 web_assert(nrow(g)==1L&&g$status %in% c("PENDING","APPROVED"),"The result review was rejected or is unavailable.")
 root<-tempfile("analysis-approved-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root)
 if(g$status=="PENDING")web_decide_gate(con,actor,project_id,g$id,"APPROVED",rationale)
 x<-c$packet;x$stage<-"APPROVED";x$result_review<-list(gate_id=g$id,actor=actor,rationale=rationale,at=web_now(),warnings_reviewed=TRUE)
 web_c1_save(con,settings,actor,project_id,version_id,root,x,x$source_job)
}
web_c1_view <- function(con,settings,actor,project_id,version_id) {
 c<-web_c1_context(con,settings,actor,project_id,version_id);x<-c$packet
 x$current<-tryCatch({web_c1_current(con,project_id,x$evidence_version);TRUE},error=function(e)FALSE)
 x$artifacts<-web_query(con,"SELECT id,logical_name,checksum FROM artifacts WHERE project_id=?p AND job_id=?j AND status='AVAILABLE'",p=project_id,j=x$source_job %||b2% "")
 x$linked<-web_query(con,"SELECT version_id,analysis_id,stage FROM analysis_versions WHERE project_id=?p AND related_version_id=?v",p=project_id,v=version_id)
 x
}

web_c1_invalidate_audit <- function(con,project,actor,new_version) {
 a<-web_query(con,"SELECT version_id,evidence_version_id FROM analysis_versions WHERE project_id=?p AND stage='APPROVED'",p=project$id)
 if(nrow(a))web_audit(con,project,"DOWNSTREAM_ANALYSIS_INVALIDATED",new_version,actor,list(historical_approved_versions=as.list(a$version_id),reason="New upstream evidence revision; historical records preserved"))
}
