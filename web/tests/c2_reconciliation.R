# Real B2/C1 state; genuine cached PubMed bibliography; no private data or fabricated statistics.
cat("C2 reconciliation starting\n")
fetch_original<-web_c2_fetch_references
web_c2_fetch_references<-function(engine,pmids){
 stopifnot(identical(pmids,"33782057"));e<-web_c2_defs(engine,39);raw<-paste(readLines("web/tests/fixtures/c2_reference.xml",warn=FALSE),collapse="\n");d<-e$parse_pubmed_articles(raw)
 list(records=lapply(seq_len(nrow(d)),function(i)as.list(d[i,,drop=FALSE])),provider="PubMed EFetch cached genuine metadata; development fixture",response_sha256=digest::digest(raw,algo="sha256",serialize=FALSE),verified_at="2026-09-27")
}
cat2<-web_c2_catalog(con,s,actor,ready$project_id,ready$version_id)
selected<-c(primary$version_id,sens_approved$version_id);aids<-cat2$artifacts$id[cat2$artifacts$analysis_version %in% selected]
mspec<-list(title="Author-review workflow demonstration",language="English",analyses=as.list(selected),artifacts=as.list(aids),pmids=list("33782057"),rationale="Non-private synthetic evidence; genuine reporting-guideline reference")
mp<-web_c2_plan(con,s,actor,ready$project_id,ready$version_id,mspec)
reject(web_c2_begin(con,s,actor,mp$project_id,mp$version_id))
g<-web_query(con,"SELECT id FROM gates WHERE version_id=?v AND gate_type='manuscript_plan'",v=mp$version_id)$id
web_decide_gate(con,actor,mp$project_id,g,"APPROVED","Reviewed selected primary/sensitivity, artifacts, language and genuine reference")
mv<-web_c2_begin(con,s,actor,mp$project_id,mp$version_id)
x<-web_c2_view(con,s,actor,mv$project_id,mv$version_id);stopifnot(x$current,x$stage=="DRAFT")
for(vv in selected)for(k in c("pooled_effect","lower_ci","upper_ci","i2","tau2","p_value","studies")) {
 actual<-x$source$facts[[paste0("A",vv,".",k)]]$value;expected<-web_c1_view(con,s,actor,mp$project_id,vv)$result$summary[[k]]
 stopifnot(identical(actual,expected))
}
cat("PASS: immutable primary and sensitivity numbers exactly equal approved C1 registry, no recomputation\n")
base<-list(project_id=mv$project_id,version_id=mv$version_id)
reject(req("manuscript_view",base,auth="bad"));reject(web_c2_view(con,s,"c1-outsider",mv$project_id,mv$version_id));reject(web_c2_view(con,s,actor,other$project_id,mv$version_id))
for(k in c("actor","reviewer","source","references","output_path","script"))reject(req("manuscript_edit",c(base,setNames(list("injected"),k))))
reject(req("manuscript_begin",base,csrf_value="stale"));reject(web_c2_edit(con,s,actor,mv$project_id,mv$version_id,"forged-section","text","SAVE","reason"))
reject(web_c2_edit(con,s,actor,mv$project_id,mv$version_id,"Methods","/workspace/private/key","SAVE","reason"))
Sys.setenv(OPENAI_API_KEY="c2-secret-fixture-never-publish");reject(web_c2_edit(con,s,actor,mv$project_id,mv$version_id,"Methods","c2-secret-fixture-never-publish","SAVE","reason"));Sys.unsetenv("OPENAI_API_KEY")
reject(web_c2_prepare_ai(con,s,actor,mv$project_id,mv$version_id,"Methods"))
reject(web_exec(con,"UPDATE manuscript_versions SET stage='APPROVED' WHERE version_id=?v",v=mv$version_id));reject(web_exec(con,"DELETE FROM manuscript_versions WHERE version_id=?v",v=mv$version_id))
bad<-mspec;bad$analyses<-list(res$version_id);reject(web_c2_plan(con,s,actor,ready$project_id,ready$version_id,bad))
bad<-mspec;bad$artifacts<-list("forged");reject(web_c2_plan(con,s,actor,ready$project_id,ready$version_id,bad))
bad<-mspec;bad$references<-list(verified=TRUE);reject(web_c2_plan(con,s,actor,ready$project_id,ready$version_id,bad))
cat("PASS: auth/project/version/CSRF/actor/reviewer/paths/secrets/reference-artifact substitution and immutable schema\n")
# Author text is manual; structured Methods/Results are injected separately by canonical presentation functions.
texts<-setNames(as.list(rep("Author-reviewed interpretation is limited to the selected evidence.",length(web_c2_sections()))),web_c2_sections())
texts$Introduction<-"Transparent reporting is supported by the cited reporting guideline. [[PMID:33782057]]"
texts$Discussion<-"Reporting should make evidence and uncertainty explicit. [[PMID:33782057]]"
texts$Declarations<-"Author declarations have been reviewed for this demonstration. Publication is not authorized."
texts$Keywords<-"Evidence synthesis; reproducibility; author review"
texts$Results<-paste0("The primary pooled effect is {{FACT:A",primary$version_id,".pooled_effect}}. See {{ARTIFACT:",aids[1],"}}.")
for(k in names(texts))mv<-web_c2_edit(con,s,actor,mv$project_id,mv$version_id,k,texts[[k]],"APPROVE","Human reviewed source grounding and wording")
x<-web_c2_view(con,s,actor,mv$project_id,mv$version_id);print(x$precheck);stopifnot(x$precheck$status=="PASS")
rr<-web_c2_render(x,s$engine_root);stopifnot(identical(rr$text,web_c2_render(x,s$engine_root)$text),grepl("PRISMA 2020",rr$text),grepl("PRIMARY",rr$text),grepl("SENSITIVITY",rr$text),nrow(rr$citations)==1)
for(badtext in c("Pooled effect is [999].","Pooled effect is nine.","{{ARTIFACT:forged}}",paste0("The PRIMARY effect is {{FACT:A",sens_approved$version_id,".pooled_effect}}."),"Pooled effect is 999.","Unknown [[PMID:999999999]]","{{FACT:missing}}")) {y<-x;y$sections$Results$text<-badtext;stopifnot(web_c2_check(y,s$engine_root)$status=="FAIL")}
y<-x;y$source$references$records[[1]]$metadata_verified<-FALSE;stopifnot(web_c2_check(y,s$engine_root)$status=="FAIL")
refs<-x$source$references;refs$records<-rep(refs$records,2);stopifnot(length(web_c2_normalize_refs(refs,s$engine_root)$records)==1)
cat("PASS: numeric conflict/unresolved fact/invalid citation/unverified metadata blocked; deterministic numbering and genuine DOI/PMID dedup\n")
# Section rejection/re-edit preserves original and history, no inherited approval.
old<-mv;mv<-web_c2_edit(con,s,actor,mv$project_id,mv$version_id,"Discussion",texts$Discussion,"REJECT","Reject wording for author revision")
stopifnot(web_c2_view(con,s,actor,mv$project_id,mv$version_id)$precheck$status=="FAIL")
reject(web_c2_edit(con,s,actor,old$project_id,old$version_id,"Discussion",texts$Discussion,"APPROVE","Stale edit"))
mv<-web_c2_edit(con,s,actor,mv$project_id,mv$version_id,"Discussion",paste(texts$Discussion,"The author retained the uncertainty statement."),"APPROVE","Human wording revised")
stopifnot(length(web_c2_view(con,s,actor,mv$project_id,mv$version_id)$sections$Discussion$history)>=3)
# Real worker and canonical Word paragraph/table helpers, no fabricated completion job.
docjob<-web_submit(con,s,actor,mv$project_id,mv$version_id,"generate_manuscript_docx")
stopifnot(identical(docjob,web_submit(con,s,actor,mv$project_id,mv$version_id,"generate_manuscript_docx")))
web_worker_once(con,s);job<-web_query(con,"SELECT * FROM jobs WHERE id=?j",j=docjob)
if(job$state!="COMPLETE"){print(web_parse(job$result_json));stop("C2 DOCX worker failed")}
# Later Human edit makes completed output stale; it must never overwrite the edit.
old<-mv;mv<-web_c2_edit(con,s,actor,mv$project_id,mv$version_id,"Conclusions","Interpretation remains bounded by the approved evidence.","APPROVE","Later Human text")
reject(web_c2_accept(con,s,actor,old$project_id,old$version_id,docjob));reject(web_c2_accept(con,s,actor,mv$project_id,mv$version_id,docjob))
docjob<-web_submit(con,s,actor,mv$project_id,mv$version_id,"generate_manuscript_docx");web_worker_once(con,s)
mv<-web_c2_accept(con,s,actor,mv$project_id,mv$version_id,docjob)
x<-web_c2_view(con,s,actor,mv$project_id,mv$version_id);stopifnot(x$stage=="REVIEW_REQUIRED")
doc<-file.path(s$data_root,"c2-manuscript.docx");web_download(con,s,actor,mv$project_id,x$docx$artifact_id,doc);stopifnot(web_sha(doc)==x$docx$docx_sha256)
reject(web_download(con,s,"c1-outsider",mv$project_id,x$docx$artifact_id,tempfile()))
tmp<-tempfile();dir.create(tmp);unzip(doc,exdir=tmp);xml<-xml2::read_xml(file.path(tmp,"word/document.xml"));ns<-xml2::xml_ns(xml)
stopifnot(length(xml2::xml_find_all(xml,"//w:p",ns))>10,length(xml2::xml_find_all(xml,"//w:tbl",ns))>=2,length(xml2::xml_find_all(xml,"//w:pStyle",ns))>0)
visible<-paste(xml2::xml_text(xml2::xml_find_all(xml,"//w:t",ns)),collapse=" ");stopifnot(grepl("PRISMA 2020",visible),!grepl("/workspace/|/home/|\\{\\{|\\[\\[PMID|c2-secret",visible))
cat("PASS: DOCX DEVELOPMENT SMOKE — actual worker, editable paragraphs/tables, headings, references, image artifacts, checksum and authorized download\n")
reject(web_c2_approve(con,s,actor,mv$project_id,mv$version_id,FALSE,"not confirmed"))
approved_ms<-web_c2_approve(con,s,actor,mv$project_id,mv$version_id,TRUE,"Reviewed text, primary/sensitivity, real reference, warnings and matching Word document; not Final QC")
x<-web_c2_view(con,s,actor,approved_ms$project_id,approved_ms$version_id);stopifnot(x$stage=="APPROVED",x$current)
msroot<-web_c2_context(con,s,actor,approved_ms$project_id,approved_ms$version_id)$version$root;mshash<-web_sha(file.path(msroot,"config/generated/web_manuscript.json"))
source("web/tests/c2_ui.R")
# New plan has no inherited gate, even for identical sources.
mp2<-web_c2_plan(con,s,actor,ready$project_id,ready$version_id,mspec);reject(web_c2_begin(con,s,actor,mp2$project_id,mp2$version_id))
revision<-web_c1_plan(con,s,actor,ready$project_id,ready$version_id,spec)
stopifnot(!web_c2_view(con,s,actor,approved_ms$project_id,approved_ms$version_id)$current,web_c2_view(con,s,actor,approved_ms$project_id,approved_ms$version_id)$stage=="APPROVED",web_sha(file.path(msroot,"config/generated/web_manuscript.json"))==mshash)
reject(web_c2_begin(con,s,actor,mp2$project_id,mp2$version_id))
stopifnot(nrow(web_query(con,"SELECT id FROM audit_events WHERE event_type='DOWNSTREAM_MANUSCRIPT_INVALIDATED' AND project_id=?p",p=primary$project_id))>0)
up<-web_b2_context(con,s,actor,ready$project_id,ready$version_id)$packet
new_evidence<-web_b2_edit(con,s,actor,ready$project_id,ready$version_id,"Target_Outcome_Verification",up$tables$Target_Outcome_Verification[[1]]$.row_id,list(intervention_events="1"))
stopifnot(!web_c1_view(con,s,actor,primary$project_id,primary$version_id)$current,!web_c2_view(con,s,actor,approved_ms$project_id,approved_ms$version_id)$current)
reject(web_c2_plan(con,s,actor,ready$project_id,ready$version_id,mspec))
web_c2_fetch_references<-fetch_original
cat("PASS: Human manuscript approval; plan non-inheritance; analysis revision invalidates current use while preserving historical bytes\n")
