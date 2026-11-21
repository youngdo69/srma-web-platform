source("web/load.R")
source("web/qc/runtime.R",encoding="UTF-8")
source("web/ui/final_qc.R",encoding="UTF-8")

srma_qc_ai<-function(...)stop("Unexpected AI request")
hash<-function(x)digest::digest(x,algo="sha256")
policy<-srma_qc_policy_load(getwd())
pid<-"project"
vid<-"manuscript"
ev<-"evidence"
selection<-list(status="HUMAN_CONFIRMED",study_ids=list("S1","S2"),
  actor="reviewer",at="2026-11-21")
manifest<-list("config/generated/web_evidence.json"=hash("packet"),
  "results/included_studies.csv"=hash("original selection"),
  "results/final_full_text_eligibility.csv"=hash("full text eligibility"),
  "results/excluded_after_full_text.csv"=hash("full text exclusions"))
packet<-list(final_study_selection=selection,
  human_outcome_harmonization=list(status="HUMAN_CONFIRMED"))
src_digest<-web_b2_synthesis_source_digest(packet)
packet$.synthesis_execution_fresh<-TRUE
packet$synthesis_execution<-list(source_digest=src_digest)
packet$narrative_batch_verification<-list(status="HUMAN_VERIFIED_EDITABLE",source_digest=src_digest)
packet$narrative_source_finalization<-list(status="FINALIZED",source_digest=src_digest,unresolved_rob=TRUE)
x<-list(evidence_version=ev,source=list(route="NARRATIVE",project_version=ev,
  references=list(records=list(list(pmid="123",title="Fixture",authors="Reviewer",
    year="2026",journal="Fixture Journal",metadata_verified=TRUE))),
  reference_discovery=list(review_status="HUMAN_SELECTION_REQUIRED"),
  narrative=list(source_digest=src_digest,source_manifest=manifest,
    human_final_selection=web_c2_9cd_freeze_selection(packet,ev,src_digest,manifest),
    analysis_groups=list("O1"),
    rows=list(list(study_id="S1")))),
  docx=list(version_id="generation",job_id="job",artifact_id="word",docx_sha256=hash("word")))
x$source_sha256<-web_c2_hash(x$source)
x$reference_sha256<-web_c2_hash(x$source$references)
x$docx$source_sha256<-x$source_sha256
x$production<-list(qc=list(publication_9cd=list(prisma_status="CANONICAL_R35_PASS",
  source_manifest_sha256=web_c2_hash(manifest),included_studies=2L)))

root<-tempfile("qc-evidence-")
dir.create(root)
artifact<-function(name,fields=list(),approval="APPROVED",project=pid,version=vid,
                     checksum=NULL,status="AVAILABLE") {
  record<-modifyList(list(project_id=pid,version_id=vid,evidence_version_id=ev,
    source_sha256=x$source_sha256,source_ref="reviewed-record",source_verified=TRUE,
    approval_state="APPROVED"),fields)
  path<-file.path(root,name)
  jsonlite::write_json(record,path,auto_unbox=TRUE)
  data.frame(id=name,project_id=project,version_id=version,logical_name=name,
    status=status,checksum=if(is.null(checksum))web_sha(path) else checksum,
    approval_state=approval,job_id="job")
}
word<-data.frame(id="word",project_id=pid,version_id="generation",logical_name="manuscript.docx",
  status="AVAILABLE",checksum=x$docx$docx_sha256,approval_state="UNREVIEWED",job_id="job")
prisma_names<-c("figure1_prisma.png","prisma_flow_audit.csv","human_final_prisma_reconciliation.json")
writeBin(as.raw(1L),file.path(root,prisma_names[[1L]]))
writeLines("count\n2",file.path(root,prisma_names[[2L]]))
jsonlite::write_json(list(evidence_version=ev,human_final_study_ids=list("S1","S2"),
  human_final_included_count=2L,staged_R24_included_count=2L,
  original_source_sha256=setNames(manifest[-1L],basename(names(manifest)[-1L]))),
  file.path(root,prisma_names[[3L]]),auto_unbox=TRUE)
prisma<-data.frame(id=prisma_names,project_id=pid,version_id="generation",logical_name=prisma_names,
  status="AVAILABLE",checksum=unname(vapply(file.path(root,prisma_names),web_sha,character(1))),
  approval_state="UNREVIEWED",job_id="job")
rows<-rbind(word,prisma,artifact("protocol.json"),artifact("grade.json",list(status="PASS",outcome_ids=list("O1"))),
  artifact("rob_draft.json",list(study_ids=list("S1","S2")),approval="UNREVIEWED"),
  artifact("rob_revoked.json",list(study_ids=list("S1","S2")),approval="REJECTED"),
  artifact("search_wrong_project.json",project="foreign"),
  artifact("search_wrong_content.json",list(project_id="foreign")),
  artifact("search_stale_registry.json",version="stale"),
  artifact("search_stale_content.json",list(version_id="stale")),
  artifact("search_missing_sha.json",checksum=""),
  artifact("search_mismatch_sha.json",checksum=hash("mismatch")),
  artifact("search_legacy.json",list(source_sha256="")),
  artifact("r40_wrong_docx.json",list(module="R40",status="PASS",docx_sha256=hash("other"))))
collect<-function(rows)list(evidence=list(packet=packet,version=list(manifest=manifest)),
  artifacts=srma_qc_link_artifact_metadata(rows,pid,c(vid,ev,"generation")),
  read_artifact=function(a)file.path(root,a$id),registry_available=TRUE)
snap<-srma_qc_link_snapshot(collect(rows),x,pid,vid)
stage<-function(s,f) s$links[[f]]$stage
stopifnot(all(vapply(c("protocol","certainty","selection","outcomes","prisma"),
  function(f) identical(stage(snap,f),"VERIFIED_SOURCE"),logical(1))),
  identical(snap$links$prisma$approval,"UNREVIEWED"),
  identical(snap$links$references$approval,"HUMAN_SELECTION_REQUIRED"),
  all(vapply(c("rob","r40","search"),function(f) identical(stage(snap,f),"UNVERIFIED"),logical(1))))
rows_ok<-rbind(rows,artifact("r40_exact.json",list(module="R40",status="PASS",
  docx_sha256=x$docx$docx_sha256)),
  artifact("rob_final.json",list(status="FINALIZED",study_ids=list("S1","S2"))))
snap_ok<-srma_qc_link_snapshot(collect(rows_ok),x,pid,vid)
stopifnot(stage(snap_ok,"r40")=="VERIFIED_SOURCE",stage(snap_ok,"rob")=="VERIFIED_SOURCE")
conflict<-x
conflict$production$qc$publication_9cd$included_studies<-3L
snap_bad<-srma_qc_link_snapshot(collect(rows),conflict,pid,vid)
stopifnot(isTRUE(snap_bad$meta$prisma_conflict),stage(snap_bad,"prisma")!="VERIFIED_SOURCE")
stale<-collect(rows)
stale$evidence$packet$final_study_selection$study_ids<-list("S1")
snap_stale<-srma_qc_link_snapshot(stale,x,pid,vid)
stopifnot(isTRUE(snap_stale$meta$selection_conflict),!snap_stale$meta$selection_final)

findings<-list(srma_qc_issue("C02","BLOCKED","Mixed comparator reported."),
  srma_qc_issue("C03","BLOCKED","No verified primary endpoint found in the packet."),
  srma_qc_issue("C04","NOT_ASSESSED","Risk-of-bias assessment unavailable."),
  srma_qc_issue("Q01","NOT_ASSESSED","Quantitative statistical analysis."),
  srma_qc_issue("PROTOCOL","NOT_ASSESSED","Protocol missing from the QC packet."))
qc<-list(number=1L,policy=policy$sha256,evidence_version=ev,status="BLOCKED",
  checks=list(),ai=list(checks=findings))
rec<-srma_qc_reconcile(qc,x,policy,pid,vid,1L)
linked<-srma_qc_link_build(rec,qc,x,policy,pid,vid,1L,snap_ok)
by_family<-setNames(linked$findings,vapply(linked$findings,`[[`,character(1),"family"))
stopifnot(by_family$analysis$status=="NOT_APPLICABLE",
  nzchar(by_family$pico$question),is.null(by_family$outcomes$question),
  is.null(by_family$rob$question),linked$original_qc_status=="BLOCKED",
  identical(qc$ai$checks,findings),identical(linked$publication_ready_release_allowed,FALSE))
html<-as.character(srma_qc_researcher_review(qc,linked))
stopifnot(grepl(by_family$pico$question,html,fixed=TRUE),
  !grepl("C02|SHA-256|NOT_APPLICABLE|BLOCKED|No verified primary endpoint",html),
  grepl("srma_qc_link_diagnostics",as.character(srma_qc_simple_ui()),fixed=TRUE))
cat("Canonical evidence, negative bindings and Final QC UI contract PASS\n")

# Existing, Human-approved plan authorizes reuse of the exact frozen library.
# All database fixtures are in memory.
con<-DBI::dbConnect(RSQLite::SQLite(),":memory:")
sql<-function(statement) DBI::dbExecute(con,statement)
invisible(lapply(c(
  "CREATE TABLE project_versions(id TEXT,project_id TEXT,version INTEGER)",
  "CREATE TABLE manuscript_versions(version_id TEXT,parent_version_id TEXT,project_id TEXT,evidence_version_id TEXT,reference_sha256 TEXT,stage TEXT)",
  "CREATE TABLE gates(id TEXT,project_id TEXT,version_id TEXT,gate_type TEXT,status TEXT,decision TEXT,reviewer_id TEXT,decided_at TEXT)",
  "INSERT INTO project_versions VALUES ('manuscript','project',2),('prior','project',1),('foreign','other',1),('other_evidence','project',1),('future','project',3)",
  "INSERT INTO gates VALUES ('foreign_gate','other','foreign','manuscript_plan','APPROVED','APPROVED','reviewer','now'),('different_evidence_gate','project','other_evidence','manuscript_plan','APPROVED','APPROVED','reviewer','now'),('future_gate','project','future','manuscript_plan','APPROVED','APPROVED','reviewer','now')"),
  sql))
for(v in c("foreign","other_evidence","future","prior"))
  DBI::dbExecute(con,"INSERT INTO manuscript_versions VALUES(?,?,?,?,?,?)",
    params=list(v,ev,if(v=="foreign")"other" else pid,
      if(v=="other_evidence")"different" else ev,x$reference_sha256,"PLAN"))
requested<-character()
request<-function(action,args) {
  stopifnot(action=="manuscript_view",identical(args$project_id,pid))
  requested<<-c(requested,args$version_id)
  x
}
stopifnot(is.null(srma_qc_link_reference_approval(request,con,pid,vid,x)),!length(requested))
sql("INSERT INTO gates VALUES ('prior_gate','project','prior','manuscript_plan','APPROVED','APPROVED','reviewer','now')")
ref_approval<-srma_qc_link_reference_approval(request,con,pid,vid,x)
stopifnot(identical(ref_approval$gate_id,"prior_gate"),identical(requested,"prior"))
changed<-x
changed$source$references$records[[1]]$pmid<-"999"
changed$reference_sha256<-web_c2_hash(changed$source$references)
stopifnot(is.null(srma_qc_link_reference_approval(request,con,pid,vid,changed)))

ready<-collect(rows_ok)
ready$reference_approval<-ref_approval
ready$artifacts<-lapply(ready$artifacts,function(a) {
  if(a$kind %in% prisma_names) {
    a$approval_state<-"APPROVED"
    a$approved<-TRUE
  }
  a
})
approved_snap<-srma_qc_link_snapshot(ready,x,pid,vid)
pass_qc<-qc
pass_qc$status<-"PASS"
pass_qc$ai$checks<-list(srma_qc_issue("C03","PASS","Verified primary outcome."))
allowed<-srma_qc_submission_evaluate(x,pass_qc,approved_snap)
web_c3_require_submission(allowed)
not_approved<-srma_qc_submission_evaluate(x,pass_qc,snap_ok)
stopifnot(all(c("REFERENCES_APPROVAL_PENDING","PRISMA_APPROVAL_PENDING") %in% unlist(not_approved$blockers)),
  inherits(tryCatch(web_c3_require_submission(not_approved),error=identity),"srma_web_error"))
blocked<-srma_qc_submission_evaluate(x,qc,approved_snap)
stopifnot(all(c("C02","C03") %in% unlist(blocked$blockers)),
  identical(qc$ai$checks,findings),
  inherits(tryCatch(web_c3_require_submission(blocked),error=identity),"srma_web_error"))
warning_qc<-pass_qc
warning_qc$ai$checks<-c(warning_qc$ai$checks,list(srma_qc_issue("C02","WARNING","Mixed comparator.")))
stopifnot(length(srma_qc_submission_evaluate(x,warning_qc,approved_snap)$human)==1L)
legacy<-x
legacy$source$route<-"OTHER"
legacy$production<-NULL
scoped<-approved_snap
scoped$links$certainty<-NULL
scoped$meta$approved_artifacts<-list()
stopifnot(!length(srma_qc_submission_evaluate(legacy,pass_qc,scoped)$blockers))
legacy$source$protocol<-list(certainty=list(required=TRUE))
stopifnot("CERTAINTY_ASSESSMENT_PENDING" %in%
  unlist(srma_qc_submission_evaluate(legacy,pass_qc,scoped)$blockers))
legacy$source$route<-"QUANTITATIVE"
stopifnot("R40_REQUIRED_RESULT_PENDING" %in%
  unlist(srma_qc_submission_evaluate(legacy,pass_qc,scoped)$blockers))

# Production entry points recheck publication blockers before any worker
# setup or approval write.
scope<-new.env(parent=environment(web_c3_collect))
scope$web_c3_source<-function(...)list(packet=x)
scope$web_c3_submission<-function(...)blocked
scope$web_c1_evidence<-scope$web_b2_context<-function(...)stop("Unexpected worker inputs")
check_collect<-web_c3_collect
environment(check_collect)<-scope
stopifnot(inherits(tryCatch(check_collect(con,NULL,"reviewer",pid,vid),error=identity),"srma_web_error"))
scope$web_c3_context<-function(...)list(packet=list(proof=list(publication_qc=blocked)))
scope$web_c3_latest<-function(...)NULL
scope$web_tx<-function(...)stop("Unexpected approval write")
check_approve<-web_c3_approve
environment(check_approve)<-scope
stopifnot(inherits(tryCatch(check_approve(con,NULL,"reviewer",pid,"qc",TRUE,"Reviewed"),error=identity),
  "srma_web_error"))
wrong_r40<-allowed
wrong_r40$r40[[1L]]$version<-"prior"
worker_proof<-list(manuscript_version=vid,docx=x$docx,publication_qc=wrong_r40)
worker<-web_c3_checks(x,worker_proof,root,getwd())
stopifnot(!worker$machine_pass,identical(worker$checks$canonical_r40,"FAIL"))

# Exact-version read path must reject an otherwise valid QC file copied under
# the selected cache key with foreign envelope identifiers.
scope_root<-list(data_root=root,engine_root=getwd())
key<-srma_qc_report_key(policy,pid,vid,x)
saved<-srma_qc_report_record(key,policy,pid,vid,x,qc)
foreign<-saved
foreign$project_id<-"other"
other_version<-saved
other_version$manuscript_version_id<-"prior"
other_source<-saved
other_source$source_sha256<-hash("other source")
stopifnot(srma_qc_report_save(scope_root,foreign),
  srma_qc_report_save(scope_root,other_version),srma_qc_report_save(scope_root,other_source),
  is.null(srma_qc_report_restore(scope_root,key,policy,pid,vid,x)),
  srma_qc_report_save(scope_root,saved),
  identical(srma_qc_report_restore(scope_root,key,policy,pid,vid,x)$ai$checks,findings))
DBI::dbDisconnect(con)
cat("Human approval inheritance, study applicability and submission enforcement PASS\n")
