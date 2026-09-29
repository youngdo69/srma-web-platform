# Append-only QC lineage on the approved manuscript ProjectVersion; never scientific edits.
web_c3_operations <- function() c(run_final_qc="APPROVED")
web_c3_ruleset <- function() "web-final-qc-v1"
web_c3_code_hash <- function(settings) {p<-c("web/R/final_qc.R","web/R/final_qc_checks.R","web/adapter/final_qc.R","web/R/manuscript.R","web/R/manuscript_render.R","web/adapter/manuscript.R");web_c2_hash(setNames(lapply(file.path(settings$engine_root,p),web_sha),p))}
web_c3_source <- function(con,settings,actor,project_id,version_id) {
 c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE)
 web_assert(c$packet$stage=="APPROVED","Final QC requires an explicitly approved manuscript.","USER_ACTION_REQUIRED")
 newer<-web_query(con,"SELECT m.version_id FROM manuscript_versions m JOIN project_versions v ON v.id=m.version_id WHERE m.project_id=?p AND v.version>(SELECT version FROM project_versions WHERE id=?v)",p=project_id,v=version_id)
 web_assert(!nrow(newer),"A newer manuscript exists. Review and approve it before Final QC.","STALE_UPSTREAM_VERSION")
 c
}
web_c3_artifact <- function(con,settings,actor,project_id,id,checksum=NULL,job=NULL) {
 web_authorize(con,actor,project_id)
 a<-web_query(con,"SELECT * FROM artifacts WHERE id=?i AND project_id=?p AND status='AVAILABLE'",i=id,p=project_id)
 web_assert(nrow(a)==1,"Required artifact is unavailable.","ARTIFACT_INTEGRITY_FAILED")
 if(!is.null(checksum))web_assert(identical(a$checksum,checksum),"Artifact checksum differs from the approved source.","ARTIFACT_INTEGRITY_FAILED")
 if(!is.null(job))web_assert(identical(a$job_id,job),"Artifact belongs to another source job.","ARTIFACT_INTEGRITY_FAILED")
 path<-web_store(settings)$get(a$storage_locator,a$checksum)
 list(row=a,path=path)
}
web_c3_collect <- function(con,settings,actor,project_id,version_id,root=NULL) {
 c<-web_c3_source(con,settings,actor,project_id,version_id);x<-c$packet
 ev<-web_c1_evidence(con,settings,actor,project_id,x$evidence_version)
 web_assert(web_c2_hash(x$source$evidence)==web_c2_hash(ev$packet),"Frozen evidence differs from its approved source.")
 proof<-list(manuscript_version=version_id,evidence_version=x$evidence_version,source_sha256=x$source_sha256,reference_sha256=x$reference_sha256,content_sha256=web_c2_hash(x$sections),spec_sha256=web_c2_hash(x$spec),docx=x$docx,analyses=list(),artifacts=list(),evidence_manifest=ev$version$manifest,ruleset=web_c3_ruleset(),web_sha256=web_c3_code_hash(settings),lock_sha256=web_sha(file.path(settings$engine_root,"renv.lock")))
 for(v in names(x$source$analyses)) {
  ac<-web_c1_context(con,settings,actor,project_id,v,TRUE)
  web_assert(ac$packet$stage=="APPROVED"&&identical(web_c2_hash(ac$packet),web_c2_hash(x$source$analyses[[v]])),"Analysis differs from the exact approved source.")
  j<-web_query(con,"SELECT * FROM jobs WHERE id=?j AND project_id=?p",j=ac$row$source_job_id,p=project_id)
  web_assert(nrow(j)==1&&j$state=="COMPLETE"&&j$version_id==ac$packet$source_plan_version,"Analysis job/plan lineage failed.")
  proof$analyses[[v]]<-list(job=j$id,version=j$version_id,engine=j$engine_id,attempt=web_parse(j$result_json)$provenance$attempt,plan_sha256=ac$packet$result$plan_sha256)
 }
 all<-c(x$source$artifacts,list(DOCX=list(id=x$docx$artifact_id,sha256=x$docx$docx_sha256,path="config/generated/final_qc_assets/manuscript.docx",type="docx")))
 for(a in all) {
  job<-if(a$type=="docx")x$docx$job_id else proof$analyses[[a$analysis_version]]$job
  ar<-web_c3_artifact(con,settings,actor,project_id,a$id,a$sha256,job)
  proof$artifacts[[a$id]]<-list(id=a$id,job=job,version=ar$row$version_id,engine=ar$row$engine_id,sha256=ar$row$checksum,path=a$path,type=a$type,analysis_version=a$analysis_version)
  if(!is.null(root)){dest<-web_inside(file.path(root,a$path),root,FALSE);dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE);web_assert(file.copy(ar$path,dest,overwrite=TRUE),"Cannot prepare QC inputs.")}
 }
 # C2's actual citation map, reviewed Markdown and rendered figure bytes belong to the DOCX job.
 j<-web_scoped_job(con,actor,project_id,x$docx$version_id,x$docx$job_id,"generate_manuscript_docx")
 web_assert(j$state=="COMPLETE","Approved Word source job is unavailable.")
 proof$docx_job<-list(id=j$id,version=j$version_id,attempt=web_parse(j$result_json)$provenance$attempt,R=web_parse(j$result_json)$provenance$R)
 proof$outputs<-list()
 for(a in web_parse(j$result_json)$artifacts) {
  if(!a$logical_name %in% c("manuscript.md","citation_map.json")&&!grepl("^figure-[0-9]+[.]png$",a$logical_name))next
  ar<-web_c3_artifact(con,settings,actor,project_id,a$artifact_id,a$checksum,j$id)
  dest<-file.path("config/generated/final_qc_assets",a$logical_name);proof$outputs[[a$logical_name]]<-list(id=a$artifact_id,sha256=a$checksum,path=dest)
  if(!is.null(root)){dir.create(dirname(file.path(root,dest)),recursive=TRUE,showWarnings=FALSE);file.copy(ar$path,file.path(root,dest),overwrite=TRUE)}
 }
 # Evidence snapshot is checksummed by web_version_root; preserve per-file source rather than derive counts.
 proof$evidence_root_version<-x$evidence_version
 proof
}
web_c3_guard <- function(con,settings,actor,project_id,version_id,operation) {
 if(operation!="run_final_qc")return(invisible(TRUE))
 web_c3_collect(con,settings,actor,project_id,version_id);invisible(TRUE)
}
web_c3_insert <- function(con,settings,actor,project_id,version_id,x,parent=NULL) {
 p<-web_authorize(con,actor,project_id);id<-web_id();x$id<-id;x$parent<-parent;x$actor<-actor;x$at<-web_now()
 web_exec(con,"INSERT INTO qc_versions VALUES(?i,?p,?v,?parent,?j,?e,?s,?x,?h,?a,?t)",i=id,p=project_id,v=version_id,parent=parent %||b2% NA_character_,j=x$job_id,e=x$engine,s=x$status,x=as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null="null",digits=NA)),h=web_c2_hash(x),a=actor,t=x$at)
 web_audit(con,p,paste0("FINAL_QC_",x$status),id,actor,list(manuscript=version_id,job=x$job_id,parent=parent))
 list(project_id=project_id,version_id=version_id,qc_id=id)
}
web_c3_start <- function(con,settings,actor,project_id,version_id) {
 proof<-web_c3_collect(con,settings,actor,project_id,version_id)
 job<-web_submit(con,settings,actor,project_id,version_id,"run_final_qc",idempotency_key=paste0("qc-",web_c2_hash(proof)))
 old<-web_query(con,"SELECT id FROM qc_versions WHERE project_id=?p AND source_job_id=?j AND parent_id IS NULL",p=project_id,j=job)
 if(nrow(old))return(list(project_id=project_id,version_id=version_id,qc_id=old$id[1]))
 web_tx(con,function()web_c3_insert(con,settings,actor,project_id,version_id,list(status="PENDING",job_id=job,engine=web_register_engine(con,settings),proof=proof,proof_sha256=web_c2_hash(proof),resolutions=list())))
}
web_c3_context <- function(con,settings,actor,project_id,qc_id,current=FALSE) {
 web_authorize(con,actor,project_id);r<-web_query(con,"SELECT * FROM qc_versions WHERE project_id=?p AND id=?i",p=project_id,i=qc_id);web_assert(nrow(r)==1,"QC decision is not accessible.")
 x<-web_parse(r$packet_json);web_assert(web_c2_hash(x)==r$packet_sha256&&x$id==qc_id,"QC evidence integrity failed.")
 if(current){proof<-web_c3_collect(con,settings,actor,project_id,r$version_id);web_assert(web_c2_hash(proof)==x$proof_sha256&&web_register_engine(con,settings)==x$engine,"QC sources or ruleset changed; run Final QC again.","STALE_UPSTREAM_VERSION")}
 list(row=r,packet=x)
}
web_c3_latest <- function(con,id)web_assert(!nrow(web_query(con,"SELECT id FROM qc_versions WHERE parent_id=?i",i=id)),"A newer QC decision exists. Load it first.")
web_c3_accept <- function(con,settings,actor,project_id,qc_id) {
 c<-web_c3_context(con,settings,actor,project_id,qc_id,TRUE);x<-c$packet;web_c3_latest(con,qc_id);web_assert(x$status=="PENDING","Load the current QC result.")
 j<-web_scoped_job(con,actor,project_id,c$row$version_id,x$job_id,"run_final_qc");web_assert(j$state=="COMPLETE","Wait for the QC worker to finish.")
 a<-web_query(con,"SELECT id,checksum FROM artifacts WHERE project_id=?p AND job_id=?j AND logical_name='final_qc.json' AND status='AVAILABLE'",p=project_id,j=j$id);web_assert(nrow(a)==1,"QC result artifact missing.")
 ar<-web_c3_artifact(con,settings,actor,project_id,a$id,a$checksum,j$id);result<-jsonlite::read_json(ar$path,simplifyVector=FALSE)
 web_assert(result$proof_sha256==x$proof_sha256&&result$version_id==c$row$version_id&&result$job_id==j$id&&result$ruleset==web_c3_ruleset(),"QC result binding failed.")
 x$result<-result;x$result_artifact<-a$id;x$result_sha256<-a$checksum;x$attempt<-j$attempts;x$status<-if(result$machine_pass)"USER_ACTION_REQUIRED" else "FAILED"
 web_tx(con,function(){web_c3_latest(con,qc_id);web_c3_context(con,settings,actor,project_id,qc_id,TRUE);x$gate<-web_id();p<-web_authorize(con,actor,project_id)
 web_exec(con,"INSERT INTO gates(id,project_id,workspace_id,version_id,gate_type,required_operation,status,created_at) VALUES(?g,?p,?w,?v,'final_qc','approve_final_qc','PENDING',?t)",g=x$gate,p=project_id,w=p$workspace_id,v=c$row$version_id,t=web_now())
 web_c3_insert(con,settings,actor,project_id,c$row$version_id,x,qc_id)})
}
web_c3_resolve <- function(con,settings,actor,project_id,qc_id,finding_id,decision,rationale) {
 c<-web_c3_context(con,settings,actor,project_id,qc_id,TRUE);x<-c$packet;web_c3_latest(con,qc_id)
 web_assert(x$status %in% c("FAILED","USER_ACTION_REQUIRED")&&decision %in% c("CONFIRMED","CORRECTION_REQUIRED"),"Choose an explicit Human review action.")
 f<-Filter(function(a)a$id==finding_id,x$result$findings);web_assert(length(f)==1&&f[[1]]$mode=="HUMAN","Deterministic findings cannot be waived. Correct the upstream source.")
 web_assert(nzchar(trimws(web_c2_text(rationale,settings$engine_root))),"Explain the Human scientific review.")
 x$resolutions[[finding_id]]<-list(decision=decision,rationale=rationale,reviewer=actor,at=web_now())
 web_tx(con,function(){web_c3_latest(con,qc_id);web_c3_context(con,settings,actor,project_id,qc_id,TRUE);web_c3_insert(con,settings,actor,project_id,c$row$version_id,x,qc_id)})
}
web_c3_approve <- function(con,settings,actor,project_id,qc_id,confirmed,rationale) {
 c<-web_c3_context(con,settings,actor,project_id,qc_id,TRUE);x<-c$packet;web_c3_latest(con,qc_id)
 web_assert(isTRUE(confirmed)&&nzchar(trimws(web_c2_text(rationale,settings$engine_root))),"Confirm numeric, artifact, citation, claim, visual and external-gate review explicitly.")
 web_assert(x$status=="USER_ACTION_REQUIRED"&&isTRUE(x$result$machine_pass),"Unresolved deterministic BLOCKER/MAJOR findings prohibit approval.")
 ids<-vapply(Filter(function(f)f$mode=="HUMAN",x$result$findings),`[[`,character(1),"id")
 web_assert(all(vapply(ids,function(i)identical(x$resolutions[[i]]$decision,"CONFIRMED"),logical(1))),"Complete each Human scientific/visual review item first.")
 web_c3_artifact(con,settings,actor,project_id,x$result_artifact,x$result_sha256,x$job_id)
 web_tx(con,function(){web_c3_latest(con,qc_id);web_c3_context(con,settings,actor,project_id,qc_id,TRUE)
 n<-web_exec(con,"UPDATE gates SET status='APPROVED',decision='APPROVED',reviewer_id=?a,decided_at=?t,rationale=?r WHERE id=?g AND project_id=?p AND version_id=?v AND gate_type='final_qc' AND status='PENDING'",a=actor,t=web_now(),r=rationale,g=x$gate,p=project_id,v=c$row$version_id);web_assert(n==1,"Final-QC gate is no longer pending.")
 x$status<-"PASSED";x$review<-list(actor=actor,at=web_now(),rationale=rationale);web_c3_insert(con,settings,actor,project_id,c$row$version_id,x,qc_id)})
}
web_c3_view <- function(con,settings,actor,project_id,qc_id) {
 c<-web_c3_context(con,settings,actor,project_id,qc_id);x<-c$packet
 current<-tryCatch({web_c3_context(con,settings,actor,project_id,qc_id,TRUE);if(!is.null(x$result_artifact))web_c3_artifact(con,settings,actor,project_id,x$result_artifact,x$result_sha256,x$job_id);TRUE},error=function(e)FALSE)
 engine<-web_register_engine(con,settings);a<-web_query(con,"SELECT kind,evidence_json FROM release_acceptances WHERE engine_id=?e AND lock_sha256=?h",e=engine,h=web_sha(file.path(settings$engine_root,"renv.lock")))
 a<-a$kind[vapply(a$evidence_json,function(p)identical(web_parse(p)$web_sha256,web_c3_code_hash(settings)),logical(1))]
 canonical<-"CANONICAL_RUNTIME" %in% a&&isTRUE(x$result$canonical_sources)
 pg<-"POSTGRESQL" %in% a&&settings$backend=="postgres"
 x$eligibility<-list(IMPLEMENTATION_COMPLETE=TRUE,QC_CHECKS_PASSED=isTRUE(x$result$machine_pass)&&current,HUMAN_QC_APPROVED=x$status=="PASSED"&&current,CANONICAL_RUNTIME_ACCEPTED=canonical,POSTGRESQL_ACCEPTED=pg,CURRENT=current)
 x$eligibility$RELEASE_ELIGIBLE<-all(unlist(x$eligibility))
 x$external<-list(POSTGRESQL=if(pg)"ACCEPTED" else "POSTGRESQL_EXTERNAL_ACCEPTANCE_PENDING",CANONICAL_RUNTIME=if(canonical)"ACCEPTED" else "CANONICAL_RUNTIME_ACCEPTANCE_PENDING",SCIENTIFIC_PROVIDER="CREDENTIALED_ACCEPTANCE_PENDING",AI="CREDENTIALED_AI_ACCEPTANCE_PENDING",MANUSCRIPT_PROVIDER="CREDENTIALED_MANUSCRIPT_ACCEPTANCE_PENDING")
 x$effective_status<-if(!current)"INVALIDATED" else if(x$status=="PENDING")web_query(con,"SELECT state FROM jobs WHERE id=?j",j=x$job_id)$state else x$status
 x
}
web_c3_list <- function(con,actor,project_id) {web_authorize(con,actor,project_id);web_query(con,"SELECT id,version_id,status,created_at FROM qc_versions WHERE project_id=?p ORDER BY created_at DESC",p=project_id)}
web_c3_invalidate_audit <- function(con,project,actor,new_version) {
 rows<-web_query(con,"SELECT id FROM qc_versions WHERE project_id=?p AND status='PASSED'",p=project$id)
 if(nrow(rows))web_audit(con,project,"FINAL_QC_INVALIDATED",new_version,actor,list(historical_qc=as.list(rows$id),reason="Upstream version changed; historical QC retained"))
}
