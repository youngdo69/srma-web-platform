# B2 extends ProjectVersion/Job/Gate/Artifact; every edit is a new frozen snapshot.
web_b2_ai_operations <- function() c(recommend_screening="SCREENING",recommend_eligibility="FULLTEXT",recommend_extraction="EXTRACTION",recommend_risk_of_bias="ROB")
web_b2_operations <- function() c(web_b2_ai_operations(),register_fulltext="FULLTEXT",prepare_screening="INPUT",finalize_screening="SCREENING",finalize_included_studies="FULLTEXT",prepare_extraction="INCLUDED",finalize_extraction="EXTRACTION",prepare_risk_of_bias="VERIFIED",finalize_evidence="ROB")
web_b2_gates <- function() c(prepare_screening="b2_protocol",finalize_screening="b2_screening",finalize_included_studies="b2_inclusion",finalize_extraction="b2_extraction",finalize_evidence="b2_rob")
web_b2_packet <- function(root) jsonlite::read_json(file.path(root,"config/generated/web_evidence.json"),simplifyVector=FALSE)
web_b2_write <- function(root,x) {dir.create(file.path(root,"config/generated"),recursive=TRUE,showWarnings=FALSE);jsonlite::write_json(x,file.path(root,"config/generated/web_evidence.json"),auto_unbox=TRUE,null="null",pretty=TRUE)}
web_b2_table <- function(x) {
 if(!length(x))return(data.frame())
 jsonlite::fromJSON(as.character(web_json(x)),simplifyDataFrame=TRUE)
}
web_b2_context <- function(con,settings,actor,project_id,version_id) {
 v<-web_version_root(con,settings,actor,project_id,version_id)
 row<-web_query(con,"SELECT * FROM evidence_versions WHERE project_id=?p AND version_id=?v",p=project_id,v=version_id)
 web_assert(nrow(row)==1L,"Start an evidence review from this saved input version first.","USER_ACTION_REQUIRED")
 x<-web_b2_packet(v$root);web_assert(identical(x$stage,row$stage),"Evidence state failed integrity verification.")
 list(version=v,row=row,packet=x)
}
web_b2_save_snapshot <- function(con,settings,actor,project_id,parent_version,root,stage,job_id=NULL) {
 project<-web_authorize(con,actor,project_id)
 web_tx(con,function(){
  saved<-web_import_project(con,settings,actor,project$workspace_id,root,configuration_only=TRUE,transaction=function(con,f)f())
  web_assert(identical(saved$project_id,project_id),"Evidence project identity changed.")
  web_exec(con,"INSERT INTO evidence_versions VALUES(?v,?p,?parent,?stage,?j,?actor,?now)",v=saved$version_id,p=project_id,parent=parent_version,stage=stage,j=if(is.null(job_id))NA_character_ else job_id,actor=actor,now=web_now())
  ops<-names(web_b2_operations())[web_b2_operations()==stage];kinds<-web_b2_gates()
  for(op in intersect(ops,names(kinds)))web_exec(con,"INSERT INTO gates(id,project_id,workspace_id,version_id,gate_type,required_operation,status,created_at) VALUES(?g,?p,?w,?v,?k,?op,'PENDING',?now)",g=web_id(),p=project_id,w=project$workspace_id,v=saved$version_id,k=kinds[[op]],op=op,now=web_now())
  web_audit(con,project,"EVIDENCE_VERSION_SAVED",saved$version_id,actor,list(parent_version=parent_version,stage=stage,source_job=job_id))
  saved
 })
}
web_b2_start <- function(con,settings,actor,project_id,version_id) {
 web_version_root(con,settings,actor,project_id,version_id)
 j<-web_query(con,"SELECT id,state FROM jobs WHERE project_id=?p AND version_id=?v AND operation='validate_database_inputs' ORDER BY created_at DESC LIMIT 1",p=project_id,v=version_id)
 web_assert(nrow(j)==1L&&j$state=="COMPLETE","Complete database preflight for this exact input version first.","USER_ACTION_REQUIRED")
 root<-tempfile("evidence-start-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE))
 web_copy_version(con,settings,actor,project_id,version_id,root)
 for(id in web_pipeline_sources(con,project_id,version_id,"validate_database_inputs"))web_overlay_job(con,settings,actor,project_id,version_id,id,root)
 web_b2_invalidate(root,"SCREENING")
 web_b2_write(root,list(stage="INPUT",source_version=version_id,preflight_job=j$id,tables=list(),history=list()))
 web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"INPUT")
}
web_b2_guard <- function(con,settings,actor,project_id,version_id,operation) {
 if(!operation %in% names(web_b2_operations()))return(invisible(TRUE))
 c<-web_b2_context(con,settings,actor,project_id,version_id)
 if(operation %in% names(web_b2_ai_operations())) {
  web_assert(nzchar(Sys.getenv(web_b2_credential_name(settings$engine_root,operation),"")),"AI assistance requires an operator credential. Manual Human review remains available.","USER_ACTION_REQUIRED")
 }
 web_science_settings(settings)
 web_assert(identical(c$row$stage,unname(web_b2_operations()[[operation]])),"This operation does not match the selected evidence stage.")
 kind<-web_b2_gates()[operation]
 if(!is.na(kind))web_assert(nrow(web_query(con,"SELECT id FROM gates WHERE project_id=?p AND version_id=?v AND gate_type=?k AND required_operation=?op AND status='APPROVED'",p=project_id,v=version_id,k=unname(kind),op=operation))==1L,"Explicit Human approval of this exact version is required.","USER_ACTION_REQUIRED")
 invisible(TRUE)
}
web_b2_accept <- function(con,settings,actor,project_id,version_id,job_id) {
 c<-web_b2_context(con,settings,actor,project_id,version_id)
 j<-web_scoped_job(con,actor,project_id,version_id,job_id,names(web_b2_operations()))
 web_assert(j$state=="COMPLETE","Wait for successful canonical processing before saving its outputs.")
 web_b2_guard(con,settings,actor,project_id,version_id,j$operation)
 old<-web_query(con,"SELECT version_id FROM evidence_versions WHERE project_id=?p AND source_job_id=?j",p=project_id,j=job_id)
 if(nrow(old))return(list(project_id=project_id,version_id=old$version_id))
 root<-tempfile("evidence-result-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE))
 web_copy_version(con,settings,actor,project_id,version_id,root)
 web_overlay_job(con,settings,actor,project_id,version_id,job_id,root)
 x<-web_b2_packet(root);x$history<-c(x$history,list(list(source_version=version_id,job_id=job_id,operation=j$operation,engine=j$engine_id)))
 web_b2_write(root,x)
 web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,x$stage,job_id)
}
# Edit contracts are UI/identity permissions, not scientific decision algorithms.
web_b2_edit_spec <- function() list(
 Reviewer_1=list(stage="SCREENING",fields=c("decision","exclusion_reason","confidence","notes")),
 Reviewer_2=list(stage="SCREENING",fields=c("decision","exclusion_reason","confidence","notes")),
 Conflict_Resolution=list(stage="SCREENING",fields=c("final_decision","final_exclusion_reason","resolution_notes")),
 Author_Final_Review=list(stage="FULLTEXT",fields=c("author_final_decision","author_final_exclusion_reason","author_notes","study_id","retrieval_status")),
 Target_Study_Verification=list(stage="EXTRACTION",fields=NULL),
 Target_Outcome_Verification=list(stage="EXTRACTION",fields=NULL),
 Domain_Review=list(stage="ROB",fields=c("author_domain_judgement","author_domain_rationale","author_override")),
 Overall_Review=list(stage="ROB",fields=c("author_final_overall_judgement","author_final_overall_rationale")),
 Signalling_Questions=list(stage="ROB",fields=c("human_response","human_evidence_location","human_rationale")))
web_b2_edit <- function(con,settings,actor,project_id,version_id,table,row_id,values) {
 c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet;spec<-web_b2_edit_spec()[[table]]
 web_assert(!is.null(spec)&&table %in% names(x$tables),"This review table is unavailable.")
 rows<-x$tables[[table]];hit<-which(vapply(rows,function(a)identical(a$.row_id,row_id),logical(1)))
 web_assert(length(hit)==1L,"This record does not belong to the selected review table/version.")
 i<-hit;row<-rows[[i]]
 allowed<-spec$fields
 if(is.null(allowed))allowed<-unlist(x$editable[[table]],use.names=FALSE)
 web_assert(is.list(values)&&length(values)>0&&!is.null(names(values))&&!anyDuplicated(names(values))&&all(names(values)%in%allowed),"Only displayed editable review fields can be saved.")
 web_assert(all(vapply(values,function(v)is.character(v)&&length(v)==1L&&!is.na(v)&&nchar(v)<=20000L,logical(1))),"Use a single bounded text value per field.")
 if(table %in% c("Reviewer_1","Reviewer_2")) {
  other<-x$tables[[if(table=="Reviewer_1")"Reviewer_2" else "Reviewer_1"]][[i]]
  web_assert(is.null(other$.actor)||!identical(other$.actor,actor),"Independent reviewer decisions must come from different people.")
 }
 if(table=="Conflict_Resolution") {
  first<-x$tables$Reviewer_1[[i]];second<-x$tables$Reviewer_2[[i]]
  web_assert(!is.null(first$.actor)&&!is.null(second$.actor)&&!actor %in% c(first$.actor,second$.actor),"Complete both independent reviews before a separate adjudicator resolves conflicts.")
 }
 if(table=="Author_Final_Review"&&!is.null(values$retrieval_status)&&values$retrieval_status=="Available") {
  web_assert(!is.null(row$.pdf_sha256),"Upload and register the PDF before marking it available.")
  f<-web_inside(file.path(c$version$root,row$.pdf_relative),c$version$root)
  web_assert(identical(web_sha(f),row$.pdf_sha256),"The registered PDF failed checksum verification.")
 }
 before<-row
 for(n in names(values))row[[n]]<-values[[n]]
 if(table %in% c("Target_Study_Verification","Target_Outcome_Verification")) {
  if(is.null(row$.values))row$.values<-list()
  for(n in setdiff(names(values),c("source_location","source_evidence","raw_value","transformation_rule","value_status"))) {
   old<-before$.values[[n]]
   row$.values[[n]]<-list(source_report=row$screening_id,source_pdf_sha256=row$.pdf_sha256,
    reported_value=if(!is.null(old$reported_value))old$reported_value else if((row$value_status %||b2% "REPORTED") %in% c("DERIVED","CALCULATED","IMPUTED"))row$raw_value else values[[n]],
    prior_value=before[[n]],human_value=values[[n]],status=row$value_status %||b2% "REPORTED",
    location=row$source_location,evidence=row$source_evidence,raw_value=row$raw_value,transformation=row$transformation_rule,
    actor=actor,at=web_now(),source_version=version_id,verification="UNVERIFIED")
  }
 }
 row$.actor<-actor;row$.timestamp<-web_now();row$.source<-"HUMAN DECISION"
 for(n in intersect(names(row),c("reviewer","adjudicator","author_reviewer","verifier")))row[[n]]<-actor
 for(n in intersect(names(row),c("screened_at","resolved_at","author_reviewed_at","verified_at")))row[[n]]<-row$.timestamp
 # Edits always clear verification; separate explicit verification is required.
 if(table %in% c("Target_Study_Verification","Target_Outcome_Verification")){row$verification_status<-"Needs clarification";row$.verified_by<-NULL}
 rows[[i]]<-row;x$tables[[table]]<-rows
 if(table %in% c("Reviewer_1","Reviewer_2")) {
  cr<-x$tables$Conflict_Resolution[[i]];cr$.actor<-NULL
  for(k in c("final_decision","final_exclusion_reason","resolution_notes","adjudicator","resolved_at"))cr[[k]]<-""
  x$tables$Conflict_Resolution[[i]]<-cr
 }
 if(table %in% c("Signalling_Questions","Domain_Review")) {
  for(t in if(table=="Signalling_Questions")c("Domain_Review","Overall_Review") else "Overall_Review")
   for(j in seq_along(x$tables[[t]]))if(identical(x$tables[[t]][[j]]$screening_id,row$screening_id)) {
    x$tables[[t]][[j]]$.actor<-NULL
    for(k in intersect(names(x$tables[[t]][[j]]),c("author_domain_judgement","author_final_overall_judgement")))x$tables[[t]][[j]][[k]]<-""
   }
 }
 x$stage<-spec$stage;x<-web_b2_invalidate_packet(x)
 x$history<-c(x$history,list(list(event="HUMAN_EDIT",table=table,row_id=row_id,actor=actor,at=web_now(),source_version=version_id)))
 root<-tempfile("review-edit-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root)
 web_b2_invalidate(root,x$stage);web_b2_write(root,x);web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,x$stage)
}
web_b2_verify <- function(con,settings,actor,project_id,version_id,table,row_id,approved,rationale) {
 web_assert(isTRUE(approved)&&is.character(rationale)&&length(rationale)==1&&nzchar(trimws(rationale)),"Explicit confirmation and verification rationale are required.")
 c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
 web_assert(x$stage=="EXTRACTION"&&table %in% c("Target_Study_Verification","Target_Outcome_Verification"),"Only draft extraction values can be verified.")
 i<-which(vapply(x$tables[[table]],function(a)identical(a$.row_id,row_id),logical(1)));web_assert(length(i)==1,"Unknown extraction row.")
 row<-x$tables[[table]][[i]]
 web_assert(!is.null(row$.actor)&&row$.actor!=actor,"A second person must independently verify extracted values.")
 web_assert(nzchar(row$source_location %||b2% "")&&nzchar(row$source_evidence %||b2% ""),"Record source location and evidence before verification.")
 row$verification_status<-"Verified";row$verifier<-actor;row$verified_at<-web_now();row$.verified_by<-actor;row$.source<-"HUMAN VERIFIED";row$.verification_rationale<-rationale;
 for(k in names(row$.values)){row$.values[[k]]$verification<-"HUMAN VERIFIED";row$.values[[k]]$verifier<-actor;row$.values[[k]]$verified_at<-web_now()};x$tables[[table]][[i]]<-row
 root<-tempfile("verify-edit-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root);web_b2_write(root,x)
 web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"EXTRACTION")
}
`%||b2%`<-function(x,y)if(is.null(x)||!length(x)||is.na(x[1]))y else x
web_b2_upload <- function(con,settings,actor,project_id,version_id,row_id,upload,upload_root) {
 c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
 web_assert(x$stage=="FULLTEXT","Upload full text while reviewing full-text eligibility.")
 i<-which(vapply(x$tables$Author_Final_Review,function(a)identical(a$.row_id,row_id),logical(1)));web_assert(length(i)==1,"Select a report belonging to this version.")
 web_assert(is.data.frame(upload)&&nrow(upload)==1&&all(c("name","datapath","size")%in%names(upload)),"Choose one PDF.")
 name<-upload$name[[1]];web_assert(grepl("^[A-Za-z0-9][A-Za-z0-9_. -]*[.]pdf$",name,ignore.case=TRUE)&&!grepl("..",name,fixed=TRUE),"Use a simple PDF filename without folders.")
 source<-web_inside(upload$datapath[[1]],upload_root);web_assert(!nzchar(Sys.readlink(upload$datapath[[1]])),"Symlink uploads are not accepted.")
 size<-file.info(source)$size;web_assert(is.finite(size)&&size>0&&size<=20*1024^2&&identical(as.numeric(upload$size[[1]]),as.numeric(size)),"The PDF must be nonempty and at most 20 MB.")
 f<-file(source,"rb");sig<-readBin(f,"raw",5);close(f);web_assert(identical(sig,charToRaw("%PDF-")),"The file does not have a valid PDF signature.")
 root<-tempfile("pdf-version-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root)
 row<-x$tables$Author_Final_Review[[i]];dest<-web_inside(file.path(root,row$.pdf_relative),root,exists=FALSE);dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE);web_assert(file.copy(source,dest,overwrite=TRUE),"PDF upload failed.")
 row$pdf_status<-"Available";row$retrieval_status<-"Available";row$pdf_path<-row$.pdf_relative;row$.pdf_sha256<-web_sha(dest);row$author_final_decision<-"";row$author_final_exclusion_reason<-"";row$.actor<-NULL;row$.source<-"SYSTEM OUTPUT";row$.uploaded_by<-actor;row$.uploaded_at<-web_now()
 x$tables$Author_Final_Review[[i]]<-row;x$tables$AI_Eligibility<-NULL;x<-web_b2_invalidate_packet(x);web_b2_invalidate(root,"FULLTEXT");web_b2_write(root,x)
 saved<-web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"FULLTEXT")
 web_audit(con,web_authorize(con,actor,project_id),"FULLTEXT_UPLOADED",saved$version_id,actor,list(report=row$screening_id,sha256=row$.pdf_sha256,source_version=version_id))
 saved
}
web_b2_view <- function(con,settings,actor,project_id,version_id) {
 c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
 # Keep independent votes hidden until both are recorded. Audit never includes PDF text.
 if(x$stage=="SCREENING")for(i in seq_along(x$tables$Reviewer_1)) {
  a<-x$tables$Reviewer_1[[i]];b<-x$tables$Reviewer_2[[i]]
  if(is.null(a$.actor)||is.null(b$.actor))for(t in c("Reviewer_1","Reviewer_2"))if(!is.null(x$tables[[t]][[i]]$.actor)&&x$tables[[t]][[i]]$.actor!=actor)for(k in c("decision","exclusion_reason","notes","confidence"))x$tables[[t]][[i]][[k]]<-"Independent review hidden"
 }
 x
}

web_b2_invalidate <- function(root,stage) {
 cleanup<-list(SCREENING=c("data/processed/full_text","data/processed/extraction","data/processed/meta_analysis_ready","results/full_text","results/extraction","results/meta_analysis_ready","results/risk_of_bias","results/analysis_ready_evidence.json"),
 FULLTEXT=c("data/processed/extraction","data/processed/meta_analysis_ready","results/extraction","results/meta_analysis_ready","results/risk_of_bias","results/analysis_ready_evidence.json"),
 EXTRACTION=c("data/processed/meta_analysis_ready","results/meta_analysis_ready","results/risk_of_bias","results/analysis_ready_evidence.json"),
 ROB=c("results/risk_of_bias/rob_final_combined.csv","results/analysis_ready_evidence.json"))
 for(p in cleanup[[stage]])unlink(web_inside(file.path(root,p),root,exists=FALSE),recursive=TRUE)
}

web_b2_invalidate_packet <- function(x) {
 order<-c(SCREENING=1L,FULLTEXT=2L,INCLUDED=3L,EXTRACTION=4L,VERIFIED=5L,ROB=6L,READY=7L)
 specs<-web_b2_edit_spec();rank<-order[[x$stage]]
 for(t in names(x$tables)) {
  stage<-if(!is.null(specs[[t]]))specs[[t]]$stage else if(t %in% c("Study_Result_Manifest","AI_Risk_of_Bias"))"ROB" else if(t=="AI_Eligibility")"FULLTEXT" else if(t %in% c("AI_study_characteristics","AI_group_characteristics","AI_outcome_data"))"EXTRACTION" else NULL
  if(!is.null(stage)&&order[[stage]]>rank)x$tables[[t]]<-NULL
 }
 if(rank<=2L){x$included_reports<-NULL;x$editable<-NULL}
 x
}
web_b2_add_row <- function(con,settings,actor,project_id,version_id,row_id) {
 c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
 web_assert(x$stage=="EXTRACTION","Add outcome/timepoint rows during draft extraction.")
 rows<-x$tables$Target_Outcome_Verification;i<-which(vapply(rows,function(a)identical(a$.row_id,row_id),logical(1)));web_assert(length(i)==1L,"Select an included report's existing outcome row.")
 row<-rows[[i]];row$.row_id<-web_id();row$outcome_row_id<-paste0(row$screening_id,"_",web_id());row$.actor<-NULL;row$.verified_by<-NULL;row$verification_status<-"Needs clarification";row$human_include_in_planned_analysis<-"No";row$verifier<-"";row$verified_at<-"";row$.source<-"HUMAN ENTRY REQUIRED";row$.timestamp<-NULL;row$.verification_rationale<-NULL;row$.values<-NULL
 x$tables$Target_Outcome_Verification<-c(rows,list(row))
 root<-tempfile("outcome-revision-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root);web_b2_invalidate(root,"EXTRACTION");web_b2_write(root,x)
 web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,"EXTRACTION")
}

# Reviewing a proposal never changes a scientific Human decision or verifies a value.
web_b2_review_proposal <- function(con,settings,actor,project_id,version_id,table,row_id,decision,rationale) {
 c<-web_b2_context(con,settings,actor,project_id,version_id);x<-c$packet
 web_assert(startsWith(table,"AI_")&&table %in% names(x$tables),"Select an AI recommendation from this version.")
 stage<-if(table=="AI_Screening")"SCREENING" else if(table=="AI_Eligibility")"FULLTEXT" else if(table=="AI_Risk_of_Bias")"ROB" else "EXTRACTION"
 web_assert(x$stage==stage,"Review proposals in their draft stage; create an evidence revision before revisiting finalized proposals.")
 web_assert(decision %in% c("ACCEPT_FOR_MANUAL_ENTRY","REJECT","UNCERTAIN")&&is.character(rationale)&&length(rationale)==1L&&!is.na(rationale)&&nzchar(trimws(rationale))&&nchar(rationale)<=20000,"Choose a proposal review and give a rationale.")
 i<-which(vapply(x$tables[[table]],function(a)identical(a$.row_id,row_id),logical(1)));web_assert(length(i)==1L,"This proposal is not in the selected version.")
 x$tables[[table]][[i]]$.human_review<-list(decision=decision,rationale=rationale,actor=actor,at=web_now(),version=version_id)
 root<-tempfile("proposal-review-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE))
 web_copy_version(con,settings,actor,project_id,version_id,root);web_b2_write(root,x)
 web_b2_save_snapshot(con,settings,actor,project_id,version_id,root,x$stage)
}
web_b2_credential_name <- function(engine_root,operation) {
 if(operation=="recommend_risk_of_bias")return("OPENAI_API_KEY")
 yaml::read_yaml(file.path(engine_root,"config/agent_screening.yml"))$agent_screening$provider$api_key_env %||b2% "OPENAI_API_KEY"
}
