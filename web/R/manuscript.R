web_c2_hash <- function(x) digest::digest(as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=NA,dataframe="rows")),algo="sha256",serialize=FALSE)
# WEB-012 state and authorization; canonical statistics are never calculated here.
web_c2_operations <- function() c(generate_manuscript_docx="HUMAN_EDITED",generate_manuscript_section="DRAFT")
web_c2_sections <- function() c("Abstract","Keywords","Introduction","Methods","Results","Discussion","Limitations","Conclusions","Declarations")
web_c2_read <- function(root) jsonlite::read_json(file.path(root,"config/generated/web_manuscript.json"),simplifyVector=FALSE)
web_c2_write <- function(root,x) jsonlite::write_json(x,file.path(root,"config/generated/web_manuscript.json"),auto_unbox=TRUE,null="null",pretty=TRUE,digits=NA)
web_c2_defs <- function(engine,n) {
 if(!exists("web_b2_exports",mode="function"))source(file.path(engine,"web/adapter/evidence.R"))
 path<-list.files(file.path(engine,"R"),pattern=paste0("^",n,"_"),full.names=TRUE);web_assert(length(path)==1,"Canonical manuscript component unavailable.")
 web_b2_exports(path,new.env(parent=globalenv()),if(n==40)"numeric_token_pattern" else character())
}
web_c2_text <- function(x,engine) {
 web_assert(is.character(x)&&length(x)==1&&!is.na(x)&&nchar(x)<=50000,"Enter bounded manuscript text.")
 web_no_secrets(x,engine)
 web_assert(!grepl("(/workspace/|/home/|[A-Za-z]:[/\\\\]|<script|file://|data:|https?://)",x,ignore.case=TRUE),"Remove paths, raw links or executable markup; use verified citation tokens.")
 x
}
web_c2_fetch_references <- function(engine,pmids) {
 web_assert(requireNamespace("httr2",quietly=TRUE),"Reference verification needs the operator's manuscript dependencies.","DEPENDENCY_FAILED")
 e<-web_c2_defs(engine,39);e$ncbi_base<-"https://eutils.ncbi.nlm.nih.gov/entrez/eutils";e$ncbi_email<-Sys.getenv("NCBI_EMAIL");e$ncbi_api_key<-Sys.getenv("NCBI_API_KEY")
 raw<-tryCatch(e$pubmed_fetch_xml(pmids),error=function(err)web_error("PROVIDER_UNAVAILABLE","Reference verification is unavailable. Retry when PubMed is reachable."))
 rows<-e$parse_pubmed_articles(raw)
 web_assert(setequal(as.character(rows$pmid),pmids)&&all(rows$metadata_verified),"Some requested references could not be verified.","CITATION_VALIDATION_FAILED")
 list(records=lapply(seq_len(nrow(rows)),function(i)as.list(rows[i,,drop=FALSE])),provider="PubMed EFetch",response_sha256=digest::digest(raw,algo="sha256",serialize=FALSE),verified_at=web_now())
}
web_c2_normalize_refs <- function(refs,engine) {
 e<-web_c2_defs(engine,40);out<-list();seen<-character();aliases<-list()
 for(r in refs$records) {
  web_assert(isTRUE(r$metadata_verified)&&grepl("^[0-9]+$",r$pmid)&&all(vapply(r[c("title","authors","year","journal")],function(a)is.character(a)&&length(a)==1&&!is.na(a)&&nzchar(a),logical(1))),"Reference metadata is incomplete or unverified.","CITATION_VALIDATION_FAILED")
  doi<-e$normalize_doi(r$doi %||b2% "");key<-if(nzchar(doi))paste0("doi:",doi) else paste0("pmid:",r$pmid)
  hit<-which(vapply(out,function(a)a$pmid==r$pmid||nzchar(doi)&&identical(e$normalize_doi(a$doi),doi),logical(1)))
  if(length(hit)){aliases[[r$pmid]]<-out[[hit[1]]]$pmid;next}
  r$doi<-doi;out[[length(out)+1L]]<-r;seen<-c(seen,key)
 }
 refs$records<-out;refs$aliases<-aliases;refs
}
web_c2_current <- function(con,project_id,x,version_id=NULL,latest=FALSE) {
 web_c1_current(con,project_id,x$evidence_version)
 # Any later analysis plan/result is a material downstream revision; historical files are retained.
 newer<-web_query(con,"SELECT a.version_id FROM analysis_versions a JOIN project_versions v ON v.id=a.version_id WHERE a.project_id=?p AND v.version>?n",p=project_id,n=x$analysis_watermark)
 web_assert(!nrow(newer),"Analyses changed. Create and approve a new manuscript plan.","STALE_UPSTREAM_VERSION")
 refs<-web_query(con,"SELECT reference_sha256 FROM manuscript_versions WHERE project_id=?p AND stage='PLAN' ORDER BY created_at DESC,version_id DESC",p=project_id)
 if(nrow(refs)) {
  # Creation ordering uses ProjectVersion monotonic sequence, not wall-clock ties.
  refs<-web_query(con,"SELECT m.reference_sha256 FROM manuscript_versions m JOIN project_versions v ON v.id=m.version_id WHERE m.project_id=?p AND m.stage='PLAN' ORDER BY v.version DESC LIMIT 1",p=project_id)
  web_assert(refs$reference_sha256==x$reference_sha256,"Reference library changed. Review a new manuscript plan.","STALE_UPSTREAM_VERSION")
 }
 if(latest) {
  v<-web_query(con,"SELECT m.version_id FROM manuscript_versions m JOIN project_versions v ON v.id=m.version_id WHERE m.project_id=?p AND m.manuscript_id=?m ORDER BY v.version DESC LIMIT 1",p=project_id,m=x$manuscript_id)
  web_assert(nrow(v)==1&&v$version_id==version_id,"A newer manuscript edit exists. Load it; old jobs cannot replace Human edits.","STALE_UPSTREAM_VERSION")
 }
 invisible(TRUE)
}
web_c2_context <- function(con,settings,actor,project_id,version_id,current=FALSE,latest=FALSE) {
 v<-web_version_root(con,settings,actor,project_id,version_id)
 row<-web_query(con,"SELECT * FROM manuscript_versions WHERE project_id=?p AND version_id=?v",p=project_id,v=version_id)
 web_assert(nrow(row)==1,"Select a manuscript version in this project.")
 x<-web_c2_read(v$root)
 web_assert(x$manuscript_id==row$manuscript_id&&x$stage==row$stage&&web_c2_hash(x$source)==row$source_sha256&&web_c2_hash(x$spec)==row$spec_sha256&&web_c2_hash(x$source$references)==row$reference_sha256,"Manuscript source integrity failed.")
 if(current)web_c2_current(con,project_id,x,version_id,latest)
 list(version=v,row=row,packet=x)
}
web_c2_save <- function(con,settings,actor,project_id,parent,root,x,job=NULL,new_plan=FALSE) {
 p<-web_authorize(con,actor,project_id);x$source_sha256<-web_c2_hash(x$source);x$reference_sha256<-web_c2_hash(x$source$references);x$actor<-actor;x$updated_at<-web_now();x$engine<-web_register_engine(con,settings);web_c2_write(root,x)
 jsonlite::write_json(x$source,file.path(root,"config/generated/manuscript_source.json"),auto_unbox=TRUE,null="null",pretty=TRUE,digits=NA)
 web_tx(con,function(){
  web_exec(con,"UPDATE projects SET title=title WHERE id=?p",p=project_id)
  if(!new_plan)web_c2_current(con,project_id,x,parent,TRUE) else web_c1_current(con,project_id,x$evidence_version)
  v<-web_import_project(con,settings,actor,p$workspace_id,root,configuration_only=TRUE,transaction=function(con,f)f())
  web_assert(v$project_id==project_id,"Manuscript project changed.")
  web_exec(con,"INSERT INTO manuscript_versions VALUES(?v,?p,?m,?parent,?e,?stage,?source,?refs,?spec,?job,?engine,?actor,?now)",v=v$version_id,p=project_id,m=x$manuscript_id,parent=parent,e=x$evidence_version,stage=x$stage,source=x$source_sha256,refs=x$reference_sha256,spec=web_c2_hash(x$spec),job=job %||b2% NA_character_,engine=x$engine,actor=actor,now=web_now())
  if(x$stage %in% c("PLAN","REVIEW_REQUIRED"))web_exec(con,"INSERT INTO gates(id,project_id,workspace_id,version_id,gate_type,required_operation,status,created_at) VALUES(?g,?p,?w,?v,?k,?o,'PENDING',?now)",g=web_id(),p=project_id,w=p$workspace_id,v=v$version_id,k=if(x$stage=="PLAN")"manuscript_plan" else "manuscript_review",o=if(x$stage=="PLAN")"prepare_manuscript" else "approve_manuscript",now=web_now())
  web_audit(con,p,paste0("MANUSCRIPT_",x$stage),v$version_id,actor,list(source=x$source_sha256,reference=x$reference_sha256,job=job))
  if(new_plan)web_audit(con,p,"MANUSCRIPT_SOURCE_REVISION",v$version_id,actor,list(reference=x$reference_sha256))
  v
 })
}
web_c2_catalog <- function(con,settings,actor,project_id,version_id) {
 c<-web_c1_evidence(con,settings,actor,project_id,version_id)
 a<-web_query(con,"SELECT a.version_id,a.source_job_id,v.version FROM analysis_versions a JOIN project_versions v ON v.id=a.version_id WHERE a.project_id=?p AND a.evidence_version_id=?e AND a.stage='APPROVED' ORDER BY v.version",p=project_id,e=version_id)
 a$label<-vapply(a$version_id,function(v){x<-web_c1_context(con,settings,actor,project_id,v)$packet;paste(x$plan$role,x$plan$outcome,x$plan$timepoint)},character(1))
 arts<-lapply(seq_len(nrow(a)),function(i){t<-web_query(con,"SELECT id,logical_name,checksum FROM artifacts WHERE project_id=?p AND job_id=?j AND status='AVAILABLE'",p=project_id,j=a$source_job_id[i]);result<-web_c1_context(con,settings,actor,project_id,a$version_id[i])$packet$result;jr<-web_parse(web_query(con,"SELECT result_json FROM jobs WHERE id=?j",j=a$source_job_id[i])$result_json);allowed<-vapply(Filter(function(z)z$relative_path %in% c("results/meta_analysis/tables/meta_analysis_summary.csv",result$forest_path),jr$artifacts),`[[`,character(1),"artifact_id");t<-t[t$id %in% allowed,,drop=FALSE];t$analysis_version<-rep(a$version_id[i],nrow(t));t})
 list(analyses=a,artifacts=if(length(arts))do.call(rbind,arts) else data.frame(),evidence_version=version_id)
}
web_c2_plan <- function(con,settings,actor,project_id,version_id,spec) {
 web_assert(is.list(spec)&&setequal(names(spec),c("title","language","analyses","artifacts","pmids","rationale")),"Unsupported manuscript specification fields.")
 for(k in c("title","language","rationale"))web_assert(nzchar(trimws(web_c2_text(spec[[k]],settings$engine_root))),"Complete the manuscript plan.")
 ids<-unlist(spec$analyses,use.names=FALSE);pmids<-unique(unlist(spec$pmids,use.names=FALSE));arts<-unlist(spec$artifacts,use.names=FALSE)
 web_assert(length(ids)>0&&!anyDuplicated(ids)&&length(pmids)>0&&length(pmids)<=100&&all(grepl("^[0-9]{1,9}$",pmids)),"Choose approved analyses and valid PubMed IDs (maximum one hundred).")
 c<-web_c1_evidence(con,settings,actor,project_id,version_id);catalog<-web_c2_catalog(con,settings,actor,project_id,version_id)
 web_assert(all(ids %in% catalog$analyses$version_id)&&length(arts)>0&&!anyDuplicated(arts)&&all(arts %in% catalog$artifacts$id),"Choose registered figures/tables belonging to approved analyses.")
 a<-setNames(lapply(ids,function(v)web_c1_context(con,settings,actor,project_id,v,TRUE)$packet),ids)
 web_assert(any(vapply(a,function(x)x$plan$role=="PRIMARY",logical(1))),"Select an approved primary result.")
 chosen<-catalog$artifacts[match(arts,catalog$artifacts$id),,drop=FALSE];web_assert(all(chosen$analysis_version %in% ids),"An artifact belongs to an unselected analysis.")
 web_assert(all(ids %in% chosen$analysis_version[grepl("[.]csv$",chosen$logical_name)]),"Select the canonical result table for every included analysis.")
 for(t in a)if(t$plan$role %in% c("SENSITIVITY","SUBGROUP","EXPLORATORY"))web_assert(t$plan$parent_version %in% ids,"Include the approved parent of each linked analysis.")
 refs<-web_c2_normalize_refs(web_c2_fetch_references(settings$engine_root,pmids),settings$engine_root)
 root<-tempfile("manuscript-plan-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root)
 cfg<-yaml::read_yaml(file.path(root,"config/project_config.yml"));policy<-cfg$analysis$manuscript
 limits<-list(total=policy$minimum_unique_references %||b2% 25,introduction=policy$minimum_intro_references %||b2% 8,discussion=policy$minimum_discussion_references %||b2% 15)
 web_assert(all(vapply(limits,function(v)is.numeric(v)&&length(v)==1&&is.finite(v)&&v>=1&&v==as.integer(v),logical(1))),"Invalid canonical reference-count policy.")
 frozen<-list()
 for(i in seq_len(nrow(chosen))) {
  ar<-web_query(con,"SELECT * FROM artifacts WHERE id=?i AND project_id=?p",i=chosen$id[i],p=project_id)
  dest<-file.path("config/generated/manuscript_assets",paste0(ar$id,".",tools::file_ext(ar$logical_name)));dir.create(dirname(file.path(root,dest)),recursive=TRUE,showWarnings=FALSE)
  web_download(con,settings,actor,project_id,ar$id,file.path(root,dest))
  frozen[[ar$id]]<-list(id=ar$id,analysis_version=chosen$analysis_version[i],name=ar$logical_name,sha256=ar$checksum,path=dest,type=if(grepl("[.]csv$",ar$logical_name))"table" else "figure")
 }
 # Evidence tables and source-file hashes retain original record provenance; unavailable totals stay unavailable.
 source<-list(project_version=version_id,evidence_version=version_id,project=cfg$project,research_question=cfg$research_question,protocol=cfg$protocol,registration=cfg$registration,search=cfg$search,screening=cfg$screening,evidence=c$packet,evidence_manifest=c$version$manifest,analyses=a,artifacts=frozen,references=refs,reference_policy=limits)
 source$evidence_outputs<-list()
 for(path in names(c$version$manifest))if(grepl("(prisma|summary|crosswalk|included_studies|rob_final).*\\.csv$",path,ignore.case=TRUE)) {
  d<-web_c1_csv(file.path(root,path));source$evidence_outputs[[path]]<-list(version=version_id,sha256=c$version$manifest[[path]],rows=lapply(seq_len(nrow(d)),function(i)as.list(d[i,,drop=FALSE])))
 }
 source$facts<-web_c2_facts(source,settings$engine_root)
 watermark<-web_query(con,"SELECT MAX(v.version) AS n FROM analysis_versions a JOIN project_versions v ON v.id=a.version_id WHERE a.project_id=?p",p=project_id)$n
 x<-list(manuscript_id=web_id(),evidence_version=version_id,analysis_watermark=watermark,stage="PLAN",spec=spec,source=source,sections=list(),created_at=web_now())
 web_c2_save(con,settings,actor,project_id,version_id,root,x,new_plan=TRUE)
}
web_c2_plan_gate <- function(con,project_id,x) {
 g<-web_query(con,"SELECT id FROM gates WHERE project_id=?p AND version_id=?v AND gate_type='manuscript_plan' AND status='APPROVED'",p=project_id,v=x$plan_version)
 web_assert(nrow(g)==1,"Approve the exact manuscript plan and reference library first.","USER_ACTION_REQUIRED")
}
web_c2_begin <- function(con,settings,actor,project_id,version_id) {
 c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE);x<-c$packet;web_assert(x$stage=="PLAN","Select a manuscript plan.");x$plan_version<-version_id;web_c2_plan_gate(con,project_id,x)
 x$stage<-"DRAFT";x$sections<-setNames(lapply(web_c2_sections(),function(k)list(id=k,source_sha256=x$source_sha256,source="SYSTEM",original="[AUTHOR REQUIRED]",text="[AUTHOR REQUIRED]",status="REVIEW_REQUIRED",actor=actor,at=web_now(),history=list())),web_c2_sections())
 web_c2_mutate(con,settings,actor,project_id,version_id,c,x)
}
web_c2_mutate <- function(con,settings,actor,project_id,version_id,c,x,job=NULL) {
 root<-tempfile("manuscript-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root)
 web_c2_save(con,settings,actor,project_id,version_id,root,x,job)
}
web_c2_edit <- function(con,settings,actor,project_id,version_id,section,text,decision,rationale) {
 c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE);x<-c$packet;web_c2_plan_gate(con,project_id,x)
 web_assert(x$stage %in% c("DRAFT","HUMAN_EDITED","REVIEW_REQUIRED")&&section %in% web_c2_sections()&&decision %in% c("SAVE","APPROVE","REJECT"),"Select an editable section and explicit action.")
 text<-web_c2_text(text,settings$engine_root);web_assert(nzchar(trimws(web_c2_text(rationale,settings$engine_root))),"Record a Human editing/review rationale.")
 sec<-x$sections[[section]];sec$history<-c(sec$history,list(sec[setdiff(names(sec),"history")]));sec$text<-text;sec$source<-"HUMAN";sec$status<-if(decision=="SAVE")"REVIEW_REQUIRED" else if(decision=="APPROVE")"APPROVED" else "REJECTED";sec$actor<-actor;sec$at<-web_now();sec$rationale<-rationale;x$sections[[section]]<-sec
 x$stage<-"HUMAN_EDITED";x$docx<-NULL;x$generation<-NULL;x$review<-NULL
 web_c2_mutate(con,settings,actor,project_id,version_id,c,x)
}
web_c2_prepare_ai <- function(con,settings,actor,project_id,version_id,section) {
 c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE);x<-c$packet;web_c2_plan_gate(con,project_id,x)
 web_assert(section %in% web_c2_sections()&&x$stage %in% c("DRAFT","HUMAN_EDITED","REVIEW_REQUIRED"),"Select an editable section.")
 web_assert(nzchar(Sys.getenv("OPENAI_API_KEY")),"AI drafting is unavailable; enter Human-authored sections or ask the operator to configure credentials.","PROVIDER_UNAVAILABLE")
 x$generation<-list(section=section,source_sha256=x$source_sha256,model=Sys.getenv("SRMA_MANUSCRIPT_MODEL","gpt-5.6"),provider="OpenAI",template="web-manuscript-section-v1");x$stage<-"DRAFT";x$docx<-NULL
 web_c2_mutate(con,settings,actor,project_id,version_id,c,x)
}
web_c2_guard <- function(con,settings,actor,project_id,version_id,operation) {
 if(!operation %in% names(web_c2_operations()))return(invisible(TRUE))
 c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE);x<-c$packet;web_c2_plan_gate(con,project_id,x)
 if(operation=="generate_manuscript_docx")web_assert(web_c2_check(x,settings$engine_root)$status=="PASS","Resolve manuscript pre-check findings and approve all sections before Word generation.","USER_ACTION_REQUIRED")
 else web_assert(!is.null(x$generation)&&nzchar(Sys.getenv("OPENAI_API_KEY")),"Prepare an exact section request with available provider credentials.","PROVIDER_UNAVAILABLE")
 web_science_settings(settings);invisible(TRUE)
}
web_c2_accept <- function(con,settings,actor,project_id,version_id,job_id) {
 c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE);x<-c$packet;j<-web_scoped_job(con,actor,project_id,version_id,job_id,names(web_c2_operations()));web_assert(j$state=="COMPLETE","Only a completed job for this exact manuscript can be accepted.")
 root<-tempfile("manuscript-result-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE));web_copy_version(con,settings,actor,project_id,version_id,root);web_overlay_job(con,settings,actor,project_id,version_id,job_id,root)
 result<-jsonlite::read_json(file.path(root,"results/web_manuscript/result.json"),simplifyVector=FALSE)
 web_assert(result$version_id==version_id&&result$source_sha256==x$source_sha256&&result$job_id==job_id&&result$content_sha256==web_c2_hash(x$sections),"Manuscript result/version binding failed.")
 if(j$operation=="generate_manuscript_docx"){
  a<-web_query(con,"SELECT id,checksum FROM artifacts WHERE project_id=?p AND job_id=?j AND logical_name='manuscript.docx' AND status='AVAILABLE'",p=project_id,j=job_id)
  web_assert(nrow(a)==1&&a$checksum==result$docx_sha256,"DOCX checksum binding failed.");web_store(settings)$get(web_query(con,"SELECT storage_locator FROM artifacts WHERE id=?i",i=a$id)$storage_locator,a$checksum)
  x$docx<-c(result,list(artifact_id=a$id));x$stage<-"REVIEW_REQUIRED"
 } else {
  k<-x$generation$section;web_assert(result$section==k,"Wrong generated section.");sec<-x$sections[[k]];sec$history<-c(sec$history,list(sec[setdiff(names(sec),"history")]));sec$original<-web_c2_text(result$text,settings$engine_root);sec$text<-sec$original;sec$source<-"AI";sec$provenance<-result$generation;sec$status<-"REVIEW_REQUIRED";sec$actor<-actor;sec$at<-web_now();x$sections[[k]]<-sec;x$generation<-NULL;x$stage<-"DRAFT"
 }
 web_c2_save(con,settings,actor,project_id,version_id,root,x,job_id)
}
web_c2_approve <- function(con,settings,actor,project_id,version_id,confirmed,rationale) {
 c<-web_c2_context(con,settings,actor,project_id,version_id,TRUE,TRUE);x<-c$packet
 web_assert(isTRUE(confirmed)&&nzchar(trimws(web_c2_text(rationale,settings$engine_root))),"Explicitly confirm text, analyses, references, warnings and matching Word document.")
 web_assert(x$stage=="REVIEW_REQUIRED"&&!is.null(x$docx)&&web_c2_check(x,settings$engine_root)$status=="PASS","Complete section review, pre-check and Word generation before approval.")
 web_assert(x$docx$content_sha256==web_c2_hash(x$sections),"Word document no longer matches reviewed text.")
 a<-web_query(con,"SELECT * FROM artifacts WHERE id=?a AND project_id=?p AND status='AVAILABLE'",a=x$docx$artifact_id,p=project_id);web_assert(nrow(a)==1&&a$checksum==x$docx$docx_sha256,"Word artifact mismatch.");web_store(settings)$get(a$storage_locator,a$checksum)
 g<-web_query(con,"SELECT id,status FROM gates WHERE project_id=?p AND version_id=?v AND gate_type='manuscript_review'",p=project_id,v=version_id);web_assert(nrow(g)==1&&g$status %in% c("PENDING","APPROVED"),"Review gate unavailable.")
 if(g$status=="PENDING")web_decide_gate(con,actor,project_id,g$id,"APPROVED",rationale)
 x$stage<-"APPROVED";x$review<-list(actor=actor,at=web_now(),rationale=rationale,gate=g$id,meaning="Author-reviewed draft; NOT final release")
 web_c2_mutate(con,settings,actor,project_id,version_id,c,x,x$docx$job_id)
}
web_c2_view <- function(con,settings,actor,project_id,version_id) {
 c<-web_c2_context(con,settings,actor,project_id,version_id);x<-c$packet;x$current<-tryCatch({web_c2_current(con,project_id,x);TRUE},error=function(e)FALSE)
 x$precheck<-web_c2_check(x,settings$engine_root);if(!x$current){x$precheck$status<-"FAIL";x$precheck$issues<-c(x$precheck$issues,"Stale upstream sources")}
 x$preview<-tryCatch(web_c2_render(x,settings$engine_root,strict=FALSE)$text,error=function(e)"Resolve citation/placeholder issues to preview.")
 x
}

web_c2_invalidate_audit <- function(con,project,actor,new_version,reason) {
 a<-web_query(con,"SELECT version_id FROM manuscript_versions WHERE project_id=?p AND stage='APPROVED'",p=project$id)
 if(nrow(a))web_audit(con,project,"DOWNSTREAM_MANUSCRIPT_INVALIDATED",new_version,actor,list(historical_versions=as.list(a$version_id),reason=reason))
}
