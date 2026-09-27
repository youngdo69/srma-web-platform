# Version-bound orchestration. Scientific preparation/decisions remain canonical.
web_pipeline_operations <- function() c("initialize_project","generate_search","retrieve_pubmed")
web_provider_ready <- function(settings) {
  p <- yaml::read_yaml(file.path(settings$engine_root,"config/initialization_agent.yml"))$initialization_agent$provider
  is.character(p$api_key_env)&&length(p$api_key_env)==1L&&nzchar(Sys.getenv(p$api_key_env,""))
}
web_pipeline_prerequisites <- function(settings, operation) {
  if (operation %in% web_pipeline_operations()) {
    web_assert(web_provider_ready(settings),
      "This step requires an operator-configured provider credential. Your work is saved; manual concept review is available.","USER_ACTION_REQUIRED")
    web_science_settings(settings)
  }
  invisible(TRUE)
}
web_copy_version <- function(con,settings,actor,project_id,version_id,root,core_only=FALSE) {
  v <- web_version_root(con,settings,actor,project_id,version_id)
  names <- names(v$manifest)
  if(core_only) {
    cfg <- yaml::read_yaml(file.path(v$root,"config/project_config.yml"))
    names <- intersect(names,c("config/project_config.yml",cfg$paths$search_terms_file))
  }
  for(p in names) {dest<-web_inside(file.path(root,p),root,exists=FALSE);dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
    web_assert(file.copy(file.path(v$root,p),dest,overwrite=TRUE),"Could not prepare saved inputs.","SYSTEM_FAILED")}
  invisible(v)
}
web_scoped_job <- function(con,actor,project_id,version_id,job_id,operation=NULL) {
  web_authorize(con,actor,project_id)
  j <- web_query(con,"SELECT * FROM jobs WHERE id=?j AND project_id=?p AND version_id=?v",j=job_id,p=project_id,v=version_id)
  web_assert(nrow(j)==1L && (is.null(operation)||j$operation %in% operation),"This processing result does not belong to the selected version.")
  j
}
web_overlay_job <- function(con,settings,actor,project_id,version_id,job_id,root) {
  j <- web_scoped_job(con,actor,project_id,version_id,job_id)
  web_assert(j$state %in% c("COMPLETE","USER_ACTION_REQUIRED"),"This processing result is not ready.")
  r <- web_parse(j$result_json)
  a <- web_query(con,"SELECT * FROM artifacts WHERE job_id=?j AND project_id=?p AND version_id=?v AND status='AVAILABLE'",j=job_id,p=project_id,v=version_id)
  for(item in r$artifacts) {
    row <- a[a$id==item$artifact_id,,drop=FALSE]
    web_assert(nrow(row)==1L && identical(row$checksum,item$checksum),"Saved output integrity needs operator review.","SYSTEM_FAILED")
    rel <- item$relative_path
    web_assert(is.character(rel)&&length(rel)==1L&&!grepl("(^[/~]|^[A-Za-z]:|(^|/)\\.\\.(/|$)|^R/|^web/|(^|/)\\.)",rel),"Invalid generated output location.")
    path <- web_inside(file.path(root,rel),root,exists=FALSE);dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
    web_assert(file.copy(web_store(settings)$get(row$storage_locator,row$checksum),path,overwrite=TRUE),"Output could not be prepared.")
  }
  invisible(r)
}
web_latest_search <- function(con,project_id,version_id) {
  j <- web_query(con,"SELECT * FROM jobs WHERE project_id=?p AND version_id=?v AND operation='generate_search' ORDER BY created_at DESC LIMIT 1",p=project_id,v=version_id)
  web_assert(nrow(j)==1L && j$state=="COMPLETE", "Generate and review a passing search for this exact version first.","USER_ACTION_REQUIRED")
  r <- web_parse(j$result_json)
  web_assert(identical(r$provenance$search_quality,"PASS"),"Canonical search quality has not passed for this version.","USER_ACTION_REQUIRED")
  j$id
}
web_pipeline_sources <- function(con,project_id,version_id,operation) {
  if(operation=="retrieve_pubmed") return(web_latest_search(con,project_id,version_id))
  if(operation=="validate_database_inputs") {
    # Only selected-version generated inputs; failed/review-required search is never promoted.
    search <- tryCatch(web_latest_search(con,project_id,version_id),error=function(e)NULL)
    raw <- web_query(con,"SELECT j.id FROM jobs j JOIN job_sources s ON s.job_id=j.id WHERE j.project_id=?p AND j.version_id=?v AND j.operation='retrieve_pubmed' AND j.state='COMPLETE' AND s.source_job_id=?search ORDER BY j.created_at DESC LIMIT 1",p=project_id,v=version_id,search=if(is.null(search))"" else search)
    return(c(search,raw$id))
  }
  character()
}
web_require_activated <- function(con,project_id,version_id,operation) {
  if(operation %in% c("generate_search","retrieve_pubmed"))
    web_assert(nrow(web_query(con,"SELECT version_id FROM version_origins WHERE project_id=?p AND version_id=?v AND kind='ACTIVATED'",p=project_id,v=version_id))==1L,
      "Review and activate canonical concepts for this exact version first.","USER_ACTION_REQUIRED")
}
web_manual_concepts <- function(con,settings,actor,project_id,version_id,terms_yaml) {
  p <- web_authorize(con,actor,project_id)
  web_assert(is.character(terms_yaml)&&length(terms_yaml)==1L&&nchar(terms_yaml)>0&&nchar(terms_yaml)<=100000,
    "Enter a canonical search-concept document within the size limit.")
  root<-tempfile("concepts-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE),add=TRUE)
  web_copy_version(con,settings,actor,project_id,version_id,root,core_only=TRUE)
  r<-web_engine_call(settings,list(action="prepare_manual_concepts",project_dir=root,terms_yaml=terms_yaml))
  if(r$status!="COMPLETE")web_error(r$error_code,r$user_message,r$technical$diagnostic)
  source_key<-digest::digest(terms_yaml,algo="sha256",serialize=FALSE)
  web_tx(con,function(){
    web_exec(con,"UPDATE projects SET title=title WHERE id=?p",p=project_id)
    prior<-web_query(con,"SELECT version_id FROM version_origins WHERE project_id=?p AND parent_version_id=?v AND kind='MANUAL_DRAFT' AND source_key=?k",p=project_id,v=version_id,k=source_key)
    if(nrow(prior))return(list(project_id=project_id,version_id=prior$version_id))
    saved<-web_import_project(con,settings,actor,p$workspace_id,root,configuration_only=TRUE,draft_gates=TRUE,transaction=function(con,f)f())
    web_exec(con,"INSERT INTO version_origins VALUES(?v,?p,?parent,'MANUAL_DRAFT',?key,NULL,?actor,?now)",v=saved$version_id,p=project_id,parent=version_id,key=source_key,actor=actor,now=web_now())
    web_audit(con,p,"HUMAN_CONCEPT_DRAFT_SAVED",saved$version_id,actor,list(parent_version=version_id,source="HUMAN_PROVIDED",sha256=source_key))
    saved
  })
}
web_initialization_review <- function(con,settings,actor,project_id,version_id,job_id=NULL) {
  root<-tempfile("draft-review-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE),add=TRUE)
  web_copy_version(con,settings,actor,project_id,version_id,root)
  if(!is.null(job_id)&&nzchar(job_id)) {
    web_scoped_job(con,actor,project_id,version_id,job_id,"initialize_project")
    web_overlay_job(con,settings,actor,project_id,version_id,job_id,root)
  }
  names<-c("project_config_draft.yml","search_terms_draft.yml","initialization_decisions_required.yml")
  files<-file.path(root,"config/generated",names)
  web_assert(all(file.exists(files)),"Prepare canonical initialization or save human-provided concepts first.","USER_ACTION_REQUIRED")
  list(project_id=project_id,version_id=version_id,job_id=job_id,
    config=paste(readLines(files[1],warn=FALSE),collapse="\n"),terms=paste(readLines(files[2],warn=FALSE),collapse="\n"),
    decisions=yaml::read_yaml(files[3])$human_decisions_required)
}
web_activate_draft <- function(con,settings,actor,project_id,version_id,selections,rationale,human_approved,job_id=NULL) {
  p<-web_authorize(con,actor,project_id)
  web_assert(isTRUE(human_approved)&&is.character(rationale)&&length(rationale)==1L&&nzchar(trimws(rationale))&&nchar(rationale)<=10000,
    "Explicit approval and a review rationale are required.")
  web_assert(is.list(selections)&&length(selections)<=100L&&sum(nchar(unlist(selections)))<=100000,"Invalid decision selections.")
  review<-web_initialization_review(con,settings,actor,project_id,version_id,job_id)
  source_key<-if(is.null(job_id)||!nzchar(job_id))"manual" else job_id
  approval_hash<-digest::digest(as.character(web_json(list(selections=selections,rationale=rationale))),algo="sha256",serialize=FALSE)
  root<-tempfile("activation-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE),add=TRUE)
  web_copy_version(con,settings,actor,project_id,version_id,root,core_only=TRUE)
  dir.create(file.path(root,"config/generated"),recursive=TRUE,showWarnings=FALSE)
  writeLines(review$config,file.path(root,"config/generated/project_config_draft.yml"))
  writeLines(review$terms,file.path(root,"config/generated/search_terms_draft.yml"))
  yaml::write_yaml(list(human_decisions_required=review$decisions),file.path(root,"config/generated/initialization_decisions_required.yml"))
  r<-web_engine_call(settings,list(action="activate_initialization",project_dir=root,selections=selections,
    human_approved=TRUE,reviewer=actor,rationale=rationale))
  if(r$status!="COMPLETE")web_error(r$error_code,r$user_message,r$technical$diagnostic)
  # Copy only approved canonical inputs and explicit review record into activation snapshot.
  activated_config<-yaml::read_yaml(file.path(root,"config/project_config.yml"))
  keep<-c("config/project_config.yml",activated_config$paths$search_terms_file,"config/generated/web_human_review.yml")
  clean<-tempfile("approved-",settings$data_root);dir.create(clean);on.exit(unlink(clean,recursive=TRUE),add=TRUE)
  for(f in keep){dir.create(dirname(file.path(clean,f)),recursive=TRUE,showWarnings=FALSE);file.copy(file.path(root,f),file.path(clean,f))}
  web_tx(con,function(){
    web_exec(con,"UPDATE projects SET title=title WHERE id=?p",p=project_id)
    prior<-web_query(con,"SELECT version_id,approval_hash FROM version_origins WHERE project_id=?p AND parent_version_id=?v AND kind='ACTIVATED' AND source_key=?k",p=project_id,v=version_id,k=source_key)
    if(nrow(prior)){web_assert(identical(prior$approval_hash,approval_hash),"This draft was already activated with a different decision. Create a new draft.");return(list(project_id=project_id,version_id=prior$version_id))}
    saved<-web_import_project(con,settings,actor,p$workspace_id,clean,configuration_only=TRUE,draft_gates=TRUE,transaction=function(con,f)f())
    web_exec(con,"INSERT INTO version_origins VALUES(?v,?p,?parent,'ACTIVATED',?key,?hash,?actor,?now)",v=saved$version_id,p=project_id,parent=version_id,key=source_key,hash=approval_hash,actor=actor,now=web_now())
    gates<-web_query(con,"SELECT id FROM gates WHERE project_id=?p AND version_id=?v AND gate_type='configuration_human_review'",p=project_id,v=saved$version_id)
    for(g in gates$id){web_exec(con,"UPDATE gates SET status='APPROVED',decision='APPROVED',reviewer_id=?u,decided_at=?n,rationale=?r WHERE id=?g",u=actor,n=web_now(),r=rationale,g=g)
      web_audit(con,p,"GATE_DECIDED",g,actor,list(decision="APPROVED",rationale=rationale,parent_version=version_id))}
    web_audit(con,p,"CANONICAL_DRAFT_ACTIVATED",saved$version_id,actor,list(parent_version=version_id,source_key=source_key,approval_hash=approval_hash))
    saved
  })
}
