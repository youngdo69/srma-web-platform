# Invoked at a real C2 APPROVED checkpoint; no provider/runtime acceptance is fabricated.
cat("C3 Final QC reconciliation starting\n")
qc<-web_c3_start(con,s,actor,approved_ms$project_id,approved_ms$version_id)
stopifnot(identical(qc,web_c3_start(con,s,actor,approved_ms$project_id,approved_ms$version_id)))
qbase<-list(project_id=qc$project_id,qc_id=qc$qc_id)
reject(req("qc_view",qbase,auth="invalid"));reject(web_c3_view(con,s,"c1-outsider",qc$project_id,qc$qc_id));reject(web_c3_view(con,s,actor,other$project_id,qc$qc_id))
for(k in c("reviewer","actor","PASS","runtime_acceptance","engine_result","release_eligible"))reject(req("qc_approve",c(qbase,setNames(list(TRUE),k))))
reject(req("qc_accept",qbase,csrf_value="bad"));reject(web_c3_accept(con,s,actor,qc$project_id,qc$qc_id))
web_worker_once(con,s);qj<-web_c3_context(con,s,actor,qc$project_id,qc$qc_id)$packet$job_id
jq<-web_query(con,"SELECT * FROM jobs WHERE id=?j",j=qj)
if(jq$state!="COMPLETE"){print(web_parse(jq$result_json));stop("QC worker failed")}
qc<-web_c3_accept(con,s,actor,qc$project_id,qc$qc_id);q<-web_c3_view(con,s,actor,qc$project_id,qc$qc_id)
cat("QC machine checks:",paste(names(q$result$checks),unlist(q$result$checks),collapse="; "),"\n")
stopifnot(q$result$checks$numerics=="PASS",q$result$checks$tables_figures=="PASS",q$result$checks$citations=="PASS",q$result$checks$lineage=="PASS")
# Deterministic findings cannot be waived, in either a valid or intentionally invalid fixture.
reject(web_decide_gate(con,actor,qc$project_id,q$gate,"APPROVED","Cannot bypass via generic gate"))
reject(web_exec(con,"UPDATE qc_versions SET status='PASSED' WHERE id=?i",i=qc$qc_id));reject(web_exec(con,"DELETE FROM qc_versions WHERE id=?i",i=qc$qc_id))
if(identical(Sys.getenv("C3_VALID_FIXTURE"),"1")) {
 stopifnot(q$result$machine_pass,q$status=="USER_ACTION_REQUIRED",!q$eligibility$RELEASE_ELIGIBLE)
 reject(web_c3_approve(con,s,actor,qc$project_id,qc$qc_id,TRUE,"Queue not reviewed"))
 for(finding in Filter(function(f)f$mode=="HUMAN",q$result$findings))qc<-web_c3_resolve(con,s,actor,qc$project_id,qc$qc_id,finding$id,"CONFIRMED","Explicit non-private fixture Human scientific and visual review; this exercise is not actual publication acceptance")
 reject(web_c3_approve(con,s,actor,qc$project_id,qc$qc_id,FALSE,"Not confirmed"))
 qc<-web_c3_approve(con,s,actor,qc$project_id,qc$qc_id,TRUE,"All fixture numeric, table/figure, citation support, Methods and visual items reviewed; external gates remain pending")
 q<-web_c3_view(con,s,actor,qc$project_id,qc$qc_id)
 stopifnot(q$status=="PASSED",q$eligibility$QC_CHECKS_PASSED,q$eligibility$HUMAN_QC_APPROVED,!q$eligibility$RELEASE_ELIGIBLE,!q$eligibility$CANONICAL_RUNTIME_ACCEPTED,!q$eligibility$POSTGRESQL_ACCEPTED)
 reject(web_c3_approve(con,s,actor,qc$project_id,qc$qc_id,TRUE,"No second approval"))
 cat("PASS: actual positive automated QC -> explicit Human queue -> immutable Final-QC approval; scientific release remains FALSE\n")
} else {
 stopifnot(!q$eligibility$RELEASE_ELIGIBLE,!q$result$machine_pass,q$status=="FAILED")
 block<-Filter(function(f)f$mode=="MACHINE",q$result$findings)[[1]]
 reject(web_c3_resolve(con,s,actor,qc$project_id,qc$qc_id,block$id,"CONFIRMED","Cannot waive"))
 reject(web_c3_approve(con,s,actor,qc$project_id,qc$qc_id,TRUE,"Cannot bypass"))
 firsthuman<-Filter(function(f)f$mode=="HUMAN",q$result$findings)[[1]];oldqc<-qc
 qc<-web_c3_resolve(con,s,actor,qc$project_id,qc$qc_id,firsthuman$id,"CORRECTION_REQUIRED","Fixture contains publication-ineligible markers")
 reject(web_c3_resolve(con,s,actor,oldqc$project_id,oldqc$qc_id,firsthuman$id,"CONFIRMED","Stale review"))
}
f<-tempfile();web_download(con,s,actor,qc$project_id,q$result_artifact,f);stopifnot(web_sha(f)==q$result_sha256);reject(web_download(con,s,"c1-outsider",qc$project_id,q$result_artifact,tempfile()))
cat("PASS: actual QC worker, exact source/numeric/table/figure/citation checks, release blocks, immutable Human correction, security and downloads\n")
# Private copies for fault injection. Never alter approved source or registered artifact bytes.
testroot<-tempfile("c3-negative-");dir.create(testroot);web_copy_version(con,s,actor,approved_ms$project_id,approved_ms$version_id,testroot)
proof<-web_c3_collect(con,s,actor,approved_ms$project_id,approved_ms$version_id,testroot)
original<-web_c2_read(testroot)
failcheck<-function(y,code,p=proof){z<-web_c3_checks(y,p,testroot,s$engine_root);stopifnot(!z$machine_pass,any(vapply(z$findings,function(f)startsWith(f$check,code)&&f$mode=="MACHINE",logical(1))));invisible(z)}
y<-original;y$source$facts[[paste0("A",primary$version_id,".pooled_effect")]]$value<-999;failcheck(y,"numerics")
y<-original;y$source$references$records[[1]]$metadata_verified<-FALSE;failcheck(y,"numerics")
y<-original;y$sections$Results$text<-"[AUTHOR REQUIRED]";failcheck(y,"numerics")
y<-original;y$sections$Results$text<-"/workspace/private";failcheck(y,"leakage")
y<-original;y$sections$Results$text<-paste0("The primary effect was significantly increased {{FACT:A",primary$version_id,".pooled_effect}}.");failcheck(y,"direction");failcheck(y,"significance")
y<-original;y$sections$Methods$text<-"A fixed-effect model was used.";failcheck(y,"methods")
y<-original;y$sections$Methods$text<-"This review was registered in PROSPERO.";failcheck(y,"registration")
fig<-Filter(function(a)a$type=="figure",original$source$artifacts)[[1]];y<-original;y$source$analyses[[fig$analysis_version]]$result$consistency$figure_sha256<-"wrong";failcheck(y,"tables_figures")
tab<-Filter(function(a)a$type=="table",original$source$artifacts)[[1]];path<-file.path(testroot,tab$path);bytes<-readBin(path,"raw",file.info(path)$size);writeLines("forged",path);failcheck(original,"lineage");failcheck(original,"tables_figures");writeBin(bytes,path)
doc<-file.path(testroot,"config/generated/final_qc_assets/manuscript.docx");bytes<-readBin(doc,"raw",file.info(doc)$size);writeBin(charToRaw("forged"),doc);failcheck(original,"docx");writeBin(bytes,doc)
cat("PASS: wrong pooled value, unverified citation, placeholders, path leakage, direction/significance, Methods/protocol, stale forest, table and Word tampering negative fixtures\n")
# Required fault cases operate only on isolated copies of the approved packet.
for(k in c("lower_ci","upper_ci","i2","tau2")) {
 y<-original;y$source$facts[[paste0("A",primary$version_id,".",k)]]$value<-999
 failcheck(y,"numerics")
}
y<-original;y$sections$Discussion$text<-"Unknown reference [[PMID:999999999]]";failcheck(y,"numerics")
y<-original;y$sections$Discussion$text<-paste(y$sections$Discussion$text,"Synthetic test fixture");failcheck(y,"leakage")
p<-proof;p$manuscript_version<-"wrong-version";p$content_sha256<-"wrong";failcheck(original,"lineage",p)
reject(web_c3_artifact(con,s,actor,qc$project_id,fig$id,"wrong-checksum"))
reject(web_c3_artifact(con,s,actor,qc$project_id,fig$id,fig$sha256,"wrong-job"))
reject(web_c3_artifact(con,s,actor,other$project_id,fig$id,fig$sha256))
cat("PASS: CI/heterogeneity, invalid citation, synthetic marker, source binding, checksum/job and unauthorized artifact substitution blocked\n")
qc_history_hash<-web_c3_context(con,s,actor,qc$project_id,qc$qc_id)$row$packet_sha256
source("web/tests/c3_ui.R")
saveRDS(list(qc=qc,approved_ms=approved_ms,settings=s),file.path(s$data_root,"c3-state.rds"))
