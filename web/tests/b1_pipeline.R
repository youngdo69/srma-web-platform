# Development-runtime integration, synthetic structural fixtures. No scientific PASS.
source("web/load.R");source("web/tests/fixture.R")
reject<-function(expr){e<-tryCatch({force(expr);NULL},error=identity);stopifnot(inherits(e,"error"));invisible(e)}
s<-web_settings(data_root=tempfile("pipeline-test-"));con<-web_db(s);web_migrate(con,s);web_migrate(con,s);web_local_identity(con)
f<-web_fixture(tempfile("pipeline-source-"),"pipeline-fixture")
cfg<-yaml::read_yaml(file.path(f,"config/project_config.yml"));cfg$search$databases<-list(pubmed=TRUE,embase=TRUE,central=TRUE)
yaml::write_yaml(cfg,file.path(f,"config/project_config.yml"))
v<-web_import_project(con,s,"local-researcher","local-workspace",f,configuration_only=TRUE,draft_gates=TRUE)
terms<-list(project=list(id="pipeline-fixture",title="Synthetic fixture",short_name="Fixture"),
 search=list(concepts=lapply(c("population","intervention"),function(x)list(concept_id=x,label=x,role=x,required=TRUE,
 free_text_terms=list(paste0("syntheticqa",x)),abbreviations=list(),spelling_variants=list(),
 controlled_vocabulary=list(pubmed_mesh_candidates=list(),embase_emtree_candidates=list()))),
 combination=list(operator_between_concepts="AND",notes="Explicit fixture"),outcome_terms_policy="Human supplied"),
 validation=list(known_relevant_ids=list(),controlled_vocabulary_requires_verification=TRUE))
manual<-function(value=yaml::as.yaml(terms),version=v)web_manual_concepts(con,s,"local-researcher",version$project_id,version$version_id,value)
draft<-manual();stopifnot(identical(manual(),draft))
review<-web_initialization_review(con,s,"local-researcher",draft$project_id,draft$version_id)
stopifnot(!length(review$decisions),identical(yaml::yaml.load(review$terms)$search,yaml::yaml.load(yaml::as.yaml(terms))$search))
bad<-terms;bad$project$id<-"other";reject(manual(yaml::as.yaml(bad)))
bad<-terms;bad$search$concepts<-list();reject(manual(yaml::as.yaml(bad)))
activate<-function(ver=draft,choices=list(),approved=TRUE)web_activate_draft(con,s,"local-researcher",ver$project_id,ver$version_id,choices,"Explicit synthetic reviewer decision",approved)
reject(activate(approved=FALSE));approved<-activate();stopifnot(identical(activate(),approved))
stopifnot(all(web_query(con,"SELECT status FROM gates WHERE version_id=?v AND gate_type='configuration_human_review'",v=approved$version_id)$status=="APPROVED"),
 all(web_query(con,"SELECT status FROM gates WHERE version_id=?v AND gate_type='protocol_registration'",v=approved$version_id)$status=="PENDING"),
 all(web_query(con,"SELECT status FROM gates WHERE version_id=?v",v=draft$version_id)$status=="PENDING"))
reject(web_exec(con,"UPDATE version_origins SET actor_id='changed' WHERE version_id=?v",v=approved$version_id))
reject(web_exec(con,"DELETE FROM version_origins WHERE version_id=?v",v=approved$version_id))
cat("PASS: canonical manual concepts and activation; idempotence, explicit approval, pending protocol and immutable lineage\n")
# Actual canonical decision patch and verifier, without an LLM or fabricated QC.
r<-tempfile("decision-draft-");dir.create(r);web_copy_version(con,s,"local-researcher",draft$project_id,draft$version_id,r)
decision<-list(decision_id="HD001",question="Choose explicit synthetic title",why_it_matters="Structural test only",
 suggested_options=list("Keep","Change"),recommended_option="Keep",recommendation_rationale="Test",
 option_patches=list(list(option="Keep",operations=list(list(target="project_config",path=list("project","title"),operation="set",value_json='"Synthetic structural validation fixture"'))),
 list(option="Change",operations=list(list(target="project_config",path=list("project","title"),operation="set",value_json='"Explicitly reviewed synthetic title"')))))
yaml::write_yaml(list(human_decisions_required=list(decision)),file.path(r,"config/generated/initialization_decisions_required.yml"))
decided<-web_import_project(con,s,"local-researcher","local-workspace",r,configuration_only=TRUE,draft_gates=TRUE)
reject(activate(decided));reject(activate(decided,list(HD001="Unknown")))
patched<-activate(decided,list(HD001="Change"))
stopifnot(web_edit_configuration(con,s,"local-researcher",patched$project_id,patched$version_id)$project$title=="Explicitly reviewed synthetic title")
cat("PASS: unresolved canonical decisions block activation; actual selected patch applied and verified\n")
provider<-yaml::read_yaml("config/initialization_agent.yml")$initialization_agent$provider$api_key_env
old<-Sys.getenv(provider,unset=NA_character_);Sys.unsetenv(provider)
for(op in web_pipeline_operations()) {
 e<-reject(web_submit(con,s,"local-researcher",approved$project_id,approved$version_id,op));stopifnot(e$code=="USER_ACTION_REQUIRED")
}
initial<-web_engine_call(s,list(action="initialize_project",project_dir=r,project_id=decided$project_id))
stopifnot(initial$status=="USER_ACTION_REQUIRED",!length(initial$artifacts),nrow(web_query(con,"SELECT id FROM jobs"))==0L)
# Sentinel is deliberately synthetic, never a real credential.
sentinel<-"synthetic-secret-sentinel-0123456789";do.call(Sys.setenv,setNames(list(sentinel),provider))
reject(manual(paste(yaml::as.yaml(terms),paste0("private_key: ",sentinel),sep="\n")))
stopifnot(identical(web_redact(list(value=sentinel),s$engine_root)$value,"[REDACTED]"))
secretfile<-tempfile();writeLines(sentinel,secretfile);reject(web_no_file_secrets(secretfile,s$engine_root));unlink(secretfile)
if(is.na(old))Sys.unsetenv(provider) else do.call(Sys.setenv,setNames(list(old),provider))
cat("PASS: missing credential fails closed with actionable state; synthetic secret rejected and redacted\n")
# Explicitly synthetic job/artifact rows exercise orchestration contracts only.
# These are NOT engine scientific quality or PubMed acceptance evidence.
fixture_job<-function(version,op,quality="PASS",source=NULL) {
 id<-web_id();eng<-web_register_engine(con,s)
 web_exec(con,"INSERT INTO jobs(id,project_id,version_id,engine_id,operation,idempotency_key,state,message,progress,created_at,updated_at) VALUES(?j,?p,?v,?e,?o,?j,'QUEUED','TEST CONTRACT FIXTURE',0,?n,?n)",j=id,p=version$project_id,v=version$version_id,e=eng,o=op,n=web_now())
 if(!is.null(source))web_exec(con,"INSERT INTO job_sources VALUES(?j,?p,?v,?s)",j=id,p=version$project_id,v=version$version_id,s=source)
 web_exec(con,"UPDATE jobs SET state='RUNNING' WHERE id=?j",j=id)
 result<-web_result(op,version$project_id,id,if(quality=="PASS")"COMPLETE" else "USER_ACTION_REQUIRED","TEST CONTRACT FIXTURE",eng,provenance=list(search_quality=quality))
 web_exec(con,"UPDATE jobs SET state=?s,result_json=?r WHERE id=?j",j=id,s=result$status,r=as.character(web_json(result)))
 id
}
search<-fixture_job(approved,"generate_search");raw<-fixture_job(approved,"retrieve_pubmed",source=search)
stopifnot(identical(web_pipeline_sources(con,approved$project_id,approved$version_id,"retrieve_pubmed"),search),
 setequal(web_pipeline_sources(con,approved$project_id,approved$version_id,"validate_database_inputs"),c(search,raw)))
reject(web_scoped_job(con,"local-researcher",approved$project_id,draft$version_id,search))
reject(web_exec(con,"UPDATE job_sources SET source_job_id=source_job_id WHERE job_id=?j",j=raw))
reject(web_exec(con,"INSERT INTO job_sources VALUES(?j,?p,?v,?s)",j=raw,p=approved$project_id,v=draft$version_id,s=search))
# Synthetic raw artifact contract: real object store/checksum/overlay, no PubMed retrieval claim.
artifact_id<-web_id();rawfile<-tempfile();writeLines("SYNTHETIC ARTIFACT BINDING TEST ONLY",rawfile)
object<-web_store(s)$put(rawfile,paste0(approved$project_id,"/",artifact_id));eng<-web_register_engine(con,s)
web_exec(con,"INSERT INTO artifacts VALUES(?a,?p,?v,?j,'contract.txt','raw_database_records',1,'AVAILABLE',?sha,?loc,?e,'UNREVIEWED',?n)",
 a=artifact_id,p=approved$project_id,v=approved$version_id,j=raw,sha=object$checksum,loc=object$locator,e=eng,n=web_now())
result<-web_parse(web_query(con,"SELECT result_json FROM jobs WHERE id=?j",j=raw)$result_json)
result$artifacts<-list(list(artifact_id=artifact_id,checksum=object$checksum,relative_path="data/raw/pubmed/contract.txt"))
web_exec(con,"UPDATE jobs SET result_json=?r WHERE id=?j",j=raw,r=as.character(web_json(result)))
isolated<-tempfile();dir.create(isolated)
web_overlay_job(con,s,"local-researcher",approved$project_id,approved$version_id,raw,isolated)
stopifnot(web_sha(file.path(isolated,"data/raw/pubmed/contract.txt"))==object$checksum)
reject(web_overlay_job(con,s,"local-researcher",approved$project_id,draft$version_id,raw,isolated))
result$artifacts[[1]]$relative_path<-"../escape"
web_exec(con,"UPDATE jobs SET result_json=?r WHERE id=?j",j=raw,r=as.character(web_json(result)))
reject(web_overlay_job(con,s,"local-researcher",approved$project_id,approved$version_id,raw,isolated))
cat("PASS: synthetic raw artifact binding, real store/checksum verification and cross-version/path rejection (not scientific retrieval evidence)\n")
newsearch<-fixture_job(approved,"generate_search")
stopifnot(identical(web_pipeline_sources(con,approved$project_id,approved$version_id,"validate_database_inputs"),newsearch))
fixture_job(approved,"generate_search","REVIEW_REQUIRED")
reject(web_pipeline_sources(con,approved$project_id,approved$version_id,"retrieve_pubmed"))
reject(web_require_activated(con,approved$project_id,draft$version_id,"generate_search"))
cat("PASS: contract fixtures prove exact-version and pinned-source binding; stale retrieval cannot join newer search; REVIEW_REQUIRED blocks retrieval\n")
# Exercise upload handoff with explicitly synthetic registry outputs, not QC evidence.
search3<-fixture_job(approved,"generate_search");raw3<-fixture_job(approved,"retrieve_pubmed",source=search3)
register_contract<-function(job,path,text,type) {
 file<-tempfile();writeLines(text,file);id<-web_id();obj<-web_store(s)$put(file,paste0(approved$project_id,"/",id))
 result<-web_parse(web_query(con,"SELECT result_json FROM jobs WHERE id=?j",j=job)$result_json)
 web_exec(con,"INSERT INTO artifacts VALUES(?a,?p,?v,?j,?name,?type,1,'AVAILABLE',?sha,?loc,?e,'UNREVIEWED',?n)",a=id,p=approved$project_id,v=approved$version_id,j=job,name=basename(path),type=type,sha=obj$checksum,loc=obj$locator,e=eng,n=web_now())
 result$artifacts<-c(result$artifacts,list(list(artifact_id=id,checksum=obj$checksum,relative_path=path)))
 web_exec(con,"UPDATE jobs SET result_json=?r WHERE id=?j",j=job,r=as.character(web_json(result)))
 obj$checksum
}
searchhash<-register_contract(search3,"search_strategies/pubmed_search.txt","SYNTHETIC QUERY CONTRACT ONLY","search_strategy")
rawhash<-register_contract(raw3,"data/raw/pubmed/pubmed.xml","SYNTHETIC RAW CONTRACT ONLY","raw_database_records")
upload_root<-tempfile();dir.create(upload_root);file<-file.path(upload_root,"manual.ris")
writeLines(c("TY  - JOUR","TI  - Synthetic import contract","ER  - "),file)
upload<-data.frame(name="manual.ris",datapath=file,size=file.info(file)$size)
import1<-web_upload_database(con,s,"local-researcher",approved$project_id,approved$version_id,"embase",upload,upload_root)
import2<-web_upload_database(con,s,"local-researcher",import1$project_id,import1$version_id,"central",upload,upload_root)
snapshot<-web_version_root(con,s,"local-researcher",import2$project_id,import2$version_id)
stopifnot(snapshot$manifest[["data/raw/pubmed/pubmed.xml"]]==rawhash,snapshot$manifest[["search_strategies/pubmed_search.txt"]]==searchhash,
 all(web_query(con,"SELECT status FROM gates WHERE version_id=?v",v=import2$version_id)$status=="PENDING"),
 yaml::read_yaml(file.path(snapshot$root,"config/generated/web_import_provenance.yml"))$source_version==approved$version_id)
stopifnot(!"data/raw/pubmed/pubmed.xml" %in% names(web_version_root(con,s,"local-researcher",approved$project_id,approved$version_id)$manifest))
cat("PASS: explicitly selected search/raw contract outputs and two uploads form new immutable input versions with provenance and pending gates; approved source snapshot unchanged\n")
source("web/adapter/search.R")
request<-list(action="generate_search",project_id="SYNTHETIC-CONTRACT",job_id="SYNTHETIC-CONTRACT")
produced<-list(quality="PASS",outputs=list(),script_hash="TEST",query_sha256="TEST",query_md5="TEST")
stopifnot(web_search_result(request,produced,list())$status=="COMPLETE")
produced$quality<-"REVIEW_REQUIRED";mapped<-web_search_result(request,produced,list())
stopifnot(mapped$status=="USER_ACTION_REQUIRED",mapped$provenance$search_quality=="REVIEW_REQUIRED")
produced$quality<-"FAILED";stopifnot(reject(web_search_result(request,produced,list()))$code=="ENGINE_FAILED")
cat("PASS: synthetic status contract preserves PASS, REVIEW_REQUIRED and structured failure; no scientific QC claim\n")
DBI::dbDisconnect(con)
