# Editorial revisions are derived from the registered Markdown and Word pair.
web_revision_headings <- function() c("Statistical analysis and synthesis method","Statistical analysis")

web_revision_markdown <- function(markdown, target="Statistical Analysis") {
  lines<-strsplit(enc2utf8(markdown),"\n",fixed=TRUE)[[1L]]
  methods<-target %in% c("Statistical Analysis","Methods")
  level<-if(methods)3L else 2L
  headers<-which(grepl(sprintf("^#{1,%d} ",level),lines))
  labels<-if(methods)paste("###",web_revision_headings()) else paste("##",target)
  start<-which(tolower(lines) %in% tolower(labels))
  web_assert(length(start)==1L,"The archived section cannot be isolated.","USER_ACTION_REQUIRED")
  if(methods) {
    parent<-tail(which(grepl("^#{1,2} ",lines) & seq_along(lines)<start),1L)
    web_assert(identical(tolower(lines[parent]),"## methods"),"The Statistical Analysis is not under Methods.")
  }
  after<-c(headers[headers>start],length(lines)+1L)[[1L]]
  section<-if(after>start+1L)trimws(paste(lines[seq.int(start+1L,after-1L)],collapse="\n")) else ""
  web_assert(nzchar(section),"The archived section is empty.","USER_ACTION_REQUIRED")
  list(section=section,heading=sub("^#+ ","",lines[[start]]),
    before=paste(lines[seq_len(start)],collapse="\n"),
    after=if(after<=length(lines))paste(lines[seq.int(after,length(lines))],collapse="\n") else "")
}

web_revision_paragraphs <- function(text) {
  trimws(gsub("[[:space:]]+"," ",strsplit(trimws(text),"\n[[:blank:]]*\n+",perl=TRUE)[[1L]]))
}

web_revision_citations <- function(text)regmatches(text,gregexpr("\\[[0-9,; –-]+\\]",text,perl=TRUE))[[1L]]

web_revision_plain <- function(text) nzchar(text) &&
  !grepl("(?m)^\\s*(#{1,6} |[-*+] |[0-9]+[.)] |>|```|\\|)|\\{\\{|\\[\\[|[\\$*`<>]|__|\\b_[^_\\n]+_|\\]\\(|\\\\[][()]",text,perl=TRUE)

web_revision_apply <- function(markdown,revision) {
  parts<-web_revision_markdown(markdown,if(is.null(revision$section))"Methods" else revision$section)
  web_assert(identical(web_c2_hash(parts$section),revision$original_section_sha256) &&
    identical(web_c2_hash(revision$text),revision$section_sha256) &&
    identical(web_revision_citations(parts$section),web_revision_citations(revision$text)),
    "The revision does not match the archived source or changes its citations.","STALE_UPSTREAM_VERSION")
  paste(parts$before,revision$text,parts$after,sep="\n\n")
}

web_revision_archive <- function(con,settings,actor,project_id,x) {
  d<-x$docx
  job<-web_scoped_job(con,actor,project_id,d$version_id,d$job_id,
    c("generate_final_manuscript","generate_manuscript_docx"))
  result<-web_parse(job$result_json)
  web_assert(identical(job$state,"COMPLETE") && identical(result$provenance$project_version_id,d$version_id) &&
    identical(result$provenance$source_sha256,x$source_sha256),"The original publication job is not bound.","ARTIFACT_INTEGRITY_FAILED")
  kinds<-c("manuscript.md","manuscript.docx","citation_map.json")
  if(srma_qc_link_sha(d$canonical_source_sha256))kinds<-c(kinds,"manuscript_source.json")
  proofs<-setNames(lapply(kinds,function(kind) {
    hits<-Filter(function(a)identical(a$logical_name,kind),result$artifacts)
    web_assert(length(hits)==1L,"The original publication archive is incomplete.","ARTIFACT_INTEGRITY_FAILED")
    a<-hits[[1L]]
    ar<-web_c3_artifact(con,settings,actor,project_id,a$artifact_id,a$checksum,job$id)
    web_assert(identical(ar$row$version_id,d$version_id),"The archived artifact belongs to another version.")
    list(id=a$artifact_id,sha256=a$checksum,path=ar$path)
  }),kinds)
  word<-proofs[["manuscript.docx"]]
  web_assert(identical(word$id,d$artifact_id) && identical(word$sha256,d$docx_sha256),"The Word artifact differs from the registered archive.")
  if("manuscript_source.json" %in% kinds) {
    web_assert(identical(proofs[["manuscript_source.json"]]$sha256,d$canonical_source_sha256),"The canonical manuscript source differs from the registered archive.")
    source<-web_c2_source_read(proofs[["manuscript_source.json"]]$path,x,
      proofs[["manuscript.md"]]$path,proofs[["citation_map.json"]]$path)
    for(figure in source$layout$figures) {
      n<-figure$file
      hits<-Filter(function(a)identical(a$logical_name,n) && identical(a$checksum,figure$sha256),result$artifacts)
      web_assert(length(hits)==1L && !n %in% names(proofs),"The registered manuscript figure is missing or ambiguous.")
      a<-hits[[1L]]
      ar<-web_c3_artifact(con,settings,actor,project_id,a$artifact_id,a$checksum,job$id)
      web_assert(identical(ar$row$version_id,d$version_id),"The registered figure belongs to another version.")
      proofs[[n]]<-list(id=a$artifact_id,sha256=a$checksum,path=ar$path)
    }
  }
  proofs
}

web_revision_read_md <- function(path) {
  web_assert(isTRUE(file.info(path)$size<=2e6),"The registered Markdown cannot be read safely.","ARTIFACT_INTEGRITY_FAILED")
  paste(readLines(path,encoding="UTF-8",warn=FALSE),collapse="\n")
}

web_revision_view <- function(con,settings,actor,project_id,x) {
  a<-web_revision_archive(con,settings,actor,project_id,x)
  markdown<-web_revision_read_md(a[["manuscript.md"]]$path)
  targets<-"Statistical Analysis"
  if(!is.null(a[["manuscript_source.json"]]))targets<-c(targets,setdiff(web_c2_sections(),"Methods"))
  edits<-setNames(lapply(targets,function(k)tryCatch({
    section<-web_revision_markdown(markdown,k)$section
    if(web_revision_plain(section))list(text=section,sha256=web_c2_hash(section))
  },error=function(e)NULL)),targets)
  edits<-Filter(Negate(is.null),edits)
  if(!length(edits))return(NULL)
  default<-if("Statistical Analysis" %in% names(edits))"Statistical Analysis" else names(edits)[[1L]]
  c(edits[[default]],list(sections=edits,default=default,canonical=!is.null(a[["manuscript_source.json"]]),
    type=x$revision$type,prior=x$revision$original,methods=x$source$statistical_methods$records))
}

web_revision_affected <- function(section="Methods") {
  affected<-c("METHODS_PROTOCOL","STATISTICAL_METHODS_REPORTING","CROSS_SECTION_SEMANTICS",
    "NUMERIC_CLAIMS","CITATION_INTEGRITY","IDENTIFIER_INTEGRITY","NARRATIVE_INTERPRETATION",
    "PAIRWISE_ADDITIONAL_ANALYSES","PAIRWISE_DIRECTION","MIXED_CONSISTENCY","DOCUMENT_STRUCTURE")
  if(!identical(section,"Methods"))affected<-c(affected,"ARTIFACT_COVERAGE","STUDY_AND_PARTICIPANT_COUNTS",
    "NARRATIVE_STUDY_RESULTS","PAIRWISE_STUDY_COUNTS","PAIRWISE_ESTIMATES_CI","PAIRWISE_ARTIFACT_NUMERICS")
  affected
}

web_revision_existing <- function(con,settings,actor,pid,parent,fingerprint) {
  children<-web_query(con,paste0("SELECT version_id FROM manuscript_versions ",
    "WHERE project_id=?p AND parent_version_id=?v AND stage='HUMAN_EDITED'"),p=pid,v=parent)
  for(id in children$version_id) {
    saved<-web_c2_context(con,settings,actor,pid,id)$packet$revision
    if(identical(saved$fingerprint,fingerprint))return(list(project_id=pid,version_id=id))
  }
  NULL
}

web_revision_registered <- function(con,pid,draft,job) {
  rows<-web_query(con,paste0("SELECT version_id FROM manuscript_versions WHERE project_id=?p ",
    "AND parent_version_id=?v AND source_job_id=?j AND stage='REVIEW_REQUIRED'"),p=pid,v=draft,j=job)
  if(nrow(rows)==1L)list(project_id=pid,version_id=rows$version_id)
}

web_revision_save <- function(con,settings,actor,project_id,version_id,expected_section_sha256,type,text,rationale,
                              section="Statistical Analysis") {
  c<-web_c2_context(con,settings,actor,project_id,version_id)
  x<-c$packet
  web_assert(isTRUE(type %in% c("EDITORIAL","METHODS_DESCRIPTION","SCIENTIFIC_ANALYSIS")),"Choose the kind of revision.")
  if(identical(type,"SCIENTIFIC_ANALYSIS"))return(list(project_id=project_id,version_id=x$evidence_version,
    route="ANALYSIS_APPROVAL"))
  web_assert(x$stage %in% c("REVIEW_REQUIRED","APPROVED") && is.list(x$docx),"Open a registered Word manuscript first.")
  text<-trimws(gsub("\r\n?","\n",web_c2_text(text,settings$engine_root)))
  rationale<-trimws(web_c2_text(rationale,settings$engine_root))
  web_assert(web_revision_plain(text) && nzchar(rationale),"Use plain paragraphs and provide a revision rationale.")
  archive<-web_revision_archive(con,settings,actor,project_id,x)
  modern<-!is.null(archive[["manuscript_source.json"]])
  web_assert(identical(section,"Statistical Analysis") || modern && identical(type,"EDITORIAL") &&
    section %in% setdiff(web_c2_sections(),"Methods"),"This revision is outside the editable manuscript scope.")
  markdown<-web_revision_read_md(archive[["manuscript.md"]]$path)
  parts<-web_revision_markdown(markdown,section)
  web_assert(web_revision_plain(parts$section),"This section contains unsupported formatting.")
  original_sha<-web_c2_hash(parts$section)
  web_assert(identical(expected_section_sha256,original_sha),"The Statistical Analysis changed. Load the current text.","STALE_UPSTREAM_VERSION")
  web_assert(!identical(parts$section,text),"There is no revision to save.")
  keep<-function(a)a[c("id","sha256")]
  proof<-lapply(archive,keep)
  r<-list(section=if(section=="Statistical Analysis")"Methods" else section,canonical=modern,
    parent_version=version_id,project_id=project_id,reviewer=actor,rationale=rationale,
    type=type,original=parts$section,text=text,original_section_sha256=original_sha,section_sha256=web_c2_hash(text),
    parent_archive=proof,origin=x$revision$origin %||b2% list(version_id=version_id,docx=x$docx,
      archive=proof,production=x$production),source_sha256=x$source_sha256,reference_sha256=x$reference_sha256,
    evidence_version=x$evidence_version,affected=as.list(web_revision_affected(if(section=="Statistical Analysis")"Methods" else section)))
  r$fingerprint<-web_c2_hash(r)
  old<-web_revision_existing(con,settings,actor,project_id,version_id,r$fingerprint)
  if(!is.null(old))return(old)
  web_c2_current(con,project_id,x,version_id,TRUE)
  web_revision_apply(markdown,r)
  r$at<-web_now()
  x$revision<-r
  id<-r$section
  sec<-x$sections[[id]]
  x$sections[[id]]<-list(id=id,original=parts$section,text=text,source_sha256=x$source_sha256,
    source="HUMAN",status="REVIEW_REQUIRED",actor=actor,at=r$at,rationale=rationale,history=if(is.list(sec))
      c(sec$history,list(sec[setdiff(names(sec),"history")])) else list())
  x$stage<-"HUMAN_EDITED";x$docx<-NULL;x$review<-NULL;x$generation<-NULL
  root<-tempfile("manuscript-revision-",settings$data_root);dir.create(root,mode="0700")
  on.exit(unlink(root,recursive=TRUE))
  web_copy_version(con,settings,actor,project_id,version_id,root)
  dir<-file.path(root,"config/generated/manuscript_revision");dir.create(dir,recursive=TRUE,showWarnings=FALSE)
  for(k in names(archive)) {
    dest<-web_inside(file.path(dir,k),root,exists=FALSE)
    web_assert(file.copy(archive[[k]]$path,dest,overwrite=TRUE) && identical(web_sha(dest),proof[[k]]$sha256),
      "The parent manuscript could not be staged.","ARTIFACT_INTEGRITY_FAILED")
  }
  web_c2_save(con,settings,actor,project_id,version_id,root,x)
}

web_revision_docx <- function(source,dest,parts,text) {
  web_assert(requireNamespace("xml2",quietly=TRUE) && requireNamespace("zip",quietly=TRUE),
    "The pinned Word revision dependencies are unavailable.","DEPENDENCY_FAILED")
  inventory<-zip::zip_list(source)
  files<-inventory$filename[!endsWith(inventory$filename,"/")]
  web_assert(length(files)>0L && !anyDuplicated(tolower(inventory$filename)) &&
    !any(grepl("(^/|\\\\|:|(^|/)[.]{1,2}(/|$))",inventory$filename)) &&
    sum(inventory$uncompressed_size)<=250e6 && "word/document.xml" %in% files,
    "The registered Word archive is outside the supported document contract.","ARTIFACT_INTEGRITY_FAILED")
  root<-tempfile("rev-word-");dir.create(root,mode="0700")
  on.exit(unlink(root,recursive=TRUE))
  zip::unzip(source,files=files,exdir=root)
  paths<-setNames(vapply(files,function(p)web_inside(file.path(root,p),root),character(1)),files)
  hashes<-vapply(paths,web_sha,character(1))
  doc<-xml2::read_xml(paths[["word/document.xml"]],options="NONET")
  ns<-c(w="http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  body<-xml2::xml_find_first(doc,"/w:document/w:body",ns)
  nodes<-xml2::xml_children(body)
  node_text<-function(p)paste(xml2::xml_text(xml2::xml_find_all(p,".//w:t",ns)),collapse="")
  labels<-vapply(nodes,node_text,character(1))
  start<-which(labels==parts$heading & xml2::xml_name(nodes)=="p")
  old<-web_revision_paragraphs(parts$section)
  web_assert(length(start)==1L && start+length(old)<length(nodes),
    "The Statistical Analysis heading cannot be identified in Word.","ARTIFACT_INTEGRITY_FAILED")
  selected<-start+seq_along(old)
  plain<-vapply(nodes[selected],function(p) {
    xml2::xml_name(p)=="p" && all(xml2::xml_name(xml2::xml_children(p)) %in% c("pPr","r")) &&
      all(xml2::xml_name(xml2::xml_find_all(p,"./w:r/*",ns)) %in% c("rPr","t")) &&
      length(xml2::xml_find_all(p,"./w:r/w:t",ns))==1L
  },logical(1))
  web_assert(all(plain) && identical(unname(trimws(gsub("[[:space:]]+"," ",labels[selected]))),old),
    "The Word paragraphs differ from the archived Statistical Analysis or contain unsupported formatting.",
    "ARTIFACT_INTEGRITY_FAILED")
  outside<-vapply(nodes[-selected],as.character,character(1))
  new<-web_revision_paragraphs(text)
  for(paragraph in new) {
    item<-xml2::xml_add_sibling(nodes[[selected[[1L]]]],nodes[[selected[[1L]]]],.where="before",.copy=TRUE)
    spans<-xml2::xml_find_all(item,"./w:r/w:t",ns)
    web_assert(length(spans)>0L,"The archived paragraph has no editable text.")
    xml2::xml_set_text(spans[[1L]],paragraph)
    if(length(spans)>1L)xml2::xml_set_text(spans[-1L],"")
  }
  xml2::xml_remove(nodes[selected])
  changed<-xml2::xml_children(body)
  target<-start+seq_along(new)
  web_assert(identical(vapply(changed[target],node_text,character(1)),new) &&
    identical(vapply(changed[-target],as.character,character(1)),outside),
    "The Word change extended beyond the Statistical Analysis paragraphs.","ARTIFACT_INTEGRITY_FAILED")
  xml2::write_xml(doc,paths[["word/document.xml"]],options="as_xml")
  zip::zipr(dest,files,root=root,include_directories=FALSE,mode="mirror")
  rebuilt<-file.path(root,"verified");dir.create(rebuilt)
  web_assert(setequal(zip::zip_list(dest)$filename,files),"Word package membership changed.")
  zip::unzip(dest,files=files,exdir=rebuilt)
  names_unchanged<-setdiff(files,"word/document.xml")
  web_assert(identical(unname(vapply(file.path(rebuilt,names_unchanged),web_sha,character(1))),
    unname(hashes[names_unchanged])),"Word styles, references or other package parts changed.","ARTIFACT_INTEGRITY_FAILED")
  list(parts_sha256=web_c2_hash(as.list(hashes[names_unchanged])),outside_sha256=web_c2_hash(as.list(outside)))
}

web_revision_render <- function(root,out,x,result,engine=NULL) {
  r<-x$revision
  target<-if(is.null(r$section))"Methods" else r$section
  web_assert(identical(x$stage,"HUMAN_EDITED") && identical(r$source_sha256,x$source_sha256) &&
    identical(r$reference_sha256,x$reference_sha256) && identical(r$text,x$sections[[target]]$text),
    "The saved revision is inconsistent.","ARTIFACT_INTEGRITY_FAILED")
  inputs<-file.path(root,"config/generated/manuscript_revision")
  files<-setNames(lapply(names(r$parent_archive),function(n) {
    path<-web_inside(file.path(inputs,n),root)
    web_assert(identical(web_sha(path),r$parent_archive[[n]]$sha256),"Revision input hash mismatch.","ARTIFACT_INTEGRITY_FAILED")
    path
  }),names(r$parent_archive))
  markdown<-web_revision_read_md(files[["manuscript.md"]])
  updated<-web_revision_apply(markdown,r)
  dest<-normalizePath(out,mustWork=TRUE,winslash="/")
  word<-file.path(dest,"manuscript.docx")
  writeLines(updated,file.path(dest,"manuscript.md"),useBytes=TRUE)
  web_assert(file.copy(files[["citation_map.json"]],file.path(dest,"citation_map.json")),"Could not preserve the citation map.")
  if(isTRUE(r$canonical)) {
    source<-web_c2_source_read(files[["manuscript_source.json"]],x,files[["manuscript.md"]],files[["citation_map.json"]])
    assets<-web_c2_source_assets(source$layout,files,dest)
    web_c2_render_word(updated,engine,word,assets)
    result$canonical_source_sha256<-web_c2_source_write(dest,x,source$layout)
    scope<-list(layout_sha256=web_c2_hash(source$layout),method="CANONICAL_SOURCE")
  } else {
    scope<-web_revision_docx(files[["manuscript.docx"]],word,web_revision_markdown(markdown),r$text)
  }
  result$docx_sha256<-web_sha(word)
  result$revision<-list(fingerprint=r$fingerprint,section_sha256=r$section_sha256,
    markdown_sha256=web_sha(file.path(dest,"manuscript.md")),scope=scope)
  result
}

web_revision_binding <- function(con,settings,actor,pid,x) {
  r<-x$revision
  origin<-web_c2_context(con,settings,actor,pid,r$origin$version_id)$packet
  parent<-web_c2_context(con,settings,actor,pid,r$parent_version)$packet
  equal<-function(y)identical(y$source_sha256,x$source_sha256) && identical(y$reference_sha256,x$reference_sha256) &&
    identical(y$evidence_version,x$evidence_version) && identical(y$manuscript_id,x$manuscript_id) &&
    identical(web_c2_hash(y$spec),web_c2_hash(x$spec))
  web_assert(equal(origin) && equal(parent) && is.null(origin$revision) && identical(origin$docx,r$origin$docx) &&
    identical(r$project_id,pid) && identical(x$docx$revision$fingerprint,r$fingerprint),
    "The archived manuscript revision lineage differs from its sources.","STALE_UPSTREAM_VERSION")
  original<-web_revision_archive(con,settings,actor,pid,origin)
  prior<-web_revision_archive(con,settings,actor,pid,parent)
  revised<-web_revision_archive(con,settings,actor,pid,x)
  facts<-function(z)lapply(z,function(a)a[c("id","sha256")])
  old_markdown<-web_revision_read_md(original[["manuscript.md"]]$path)
  new_markdown<-web_revision_read_md(revised[["manuscript.md"]]$path)
  old<-web_revision_markdown(old_markdown,r$section)
  current<-web_revision_markdown(new_markdown,r$section)
  parent_markdown<-web_revision_read_md(prior[["manuscript.md"]]$path)
  parent_section<-web_revision_markdown(parent_markdown,r$section)
  web_assert(identical(facts(original),r$origin$archive) && identical(facts(prior),r$parent_archive) &&
    identical(original[["citation_map.json"]]$sha256,revised[["citation_map.json"]]$sha256) &&
    identical(new_markdown,web_revision_apply(parent_markdown,r)) &&
    identical(parent_section[c("before","heading","after")],current[c("before","heading","after")]) &&
    identical(revised[["manuscript.md"]]$sha256,x$docx$revision$markdown_sha256),
    "The revised document changed inputs outside Statistical Analysis.","ARTIFACT_INTEGRITY_FAILED")
  if(isTRUE(r$canonical)) {
    layout<-function(a,y)web_c2_source_read(a[["manuscript_source.json"]]$path,y,
      a[["manuscript.md"]]$path,a[["citation_map.json"]]$path)$layout
    h<-web_c2_hash(layout(revised,x))
    web_assert(identical(h,x$docx$revision$scope$layout_sha256) && identical(h,web_c2_hash(layout(prior,parent))) &&
      identical(h,web_c2_hash(layout(original,origin))),"The revision changed the frozen table or figure layout.")
  }
  list(origin_version=r$origin$version_id,origin_docx=r$origin$docx[c("job_id","version_id","artifact_id","docx_sha256")],
    origin_archive=facts(original),revised_archive=facts(revised),parent_version=r$parent_version,
    prior_sha256=r$original_section_sha256,section_sha256=r$section_sha256,section=current$section,target=r$section,
    whole_manuscript=!identical(r$section,"Methods") || !identical(old[c("before","after")],current[c("before","after")]),
    original_section=old$section,unchanged_sections_sha256=web_c2_hash(old[c("before","after")]),
    results=web_c3_r40(settings$engine_root)$extract_section(old_markdown,"Results"),
    source_sha256=x$source_sha256,evidence_version=x$evidence_version,reference_sha256=x$reference_sha256,
    reviewer=r$reviewer,at=r$at,fingerprint=r$fingerprint,type=r$type,affected=r$affected)
}

web_revision_numeric_context <- function(text,engine) {
  r40<-web_c2_defs(engine,40)
  paragraphs<-web_revision_paragraphs(text)
  paragraphs[vapply(paragraphs,function(p)nrow(r40$extract_numeric_tokens(p))>0L,logical(1))]
}

web_revision_selective_plan <- function(settings,plan,contract,base=NULL) {
  part<-contract$binding$inputs$revision$binding
  plan$revision<-list(bound=is.list(part),pending=web_revision_affected())
  if(!is.list(part))return(plan)
  base_plan<-base$r40_validation_plan
  same<-c("ruleset","policy_sha256","canonical_sha256","web_sha256","runtime_lock_sha256")
  inputs<-c("protocol","study_selection","outcome_harmonization","analyses","r38","r39","statistical_methods")
  stable<-!isTRUE(part$whole_manuscript) && is.list(base_plan) && identical(base_plan$binding[same],plan$binding[same]) &&
    identical(web_c2_hash(base$r40_input_contract$binding$inputs[inputs]),
      web_c2_hash(contract$binding$inputs[inputs]))
  reuse<-c("ARTIFACT_COVERAGE","CITATION_INTEGRITY","NARRATIVE_STUDY_RESULTS","PAIRWISE_STUDY_COUNTS",
    "PAIRWISE_ESTIMATES_CI","PAIRWISE_ARTIFACT_NUMERICS")
  old_numbers<-tryCatch(web_revision_numeric_context(part$original_section,settings$engine_root),error=function(e)NULL)
  new_numbers<-tryCatch(web_revision_numeric_context(part$section,settings$engine_root),error=function(e)NULL)
  same_numbers<-!is.null(old_numbers) && !is.null(new_numbers) && identical(old_numbers,new_numbers)
  if(same_numbers)reuse<-c(reuse,"NUMERIC_CLAIMS","STUDY_AND_PARTICIPANT_COUNTS")
  if(stable)for(id in intersect(reuse,names(plan$rules))) {
    old<-base$r40_rule_matrix[[id]]
    new<-plan$rules[[id]]
    if(identical(old$result,"PASS") && identical(old[c("id","basis","applicability")],new[c("id","basis","applicability")])) {
      plan$rules[[id]]$result<-"PASS";plan$rules[[id]]$execution<-"REUSED"
      plan$rules[[id]]$source<-list(receipt=base$r40,scope=part$unchanged_sections_sha256)
    }
  }
  protected<-vapply(plan$statistical_records,`[[`,character(1),"text")
  if(stable && !same_numbers && identical(base$r40_rule_matrix$NUMERIC_CLAIMS$result,"PASS") &&
     identical(base$r40_rule_matrix$NUMERIC_CLAIMS$basis,plan$rules$NUMERIC_CLAIMS$basis) &&
     !is.null(old_numbers) && !is.null(new_numbers) && all(new_numbers %in% c(old_numbers,protected))) {
    plan$rules$NUMERIC_CLAIMS$result<-"PASS"
    plan$rules$NUMERIC_CLAIMS$execution<-"EXECUTED"
    plan$rules$NUMERIC_CLAIMS$source<-list(base=base$r40,section_sha256=part$section_sha256,
      methods_records_sha256=web_stat_records_digest(plan$statistical_records))
  }
  relevant<-intersect(web_revision_affected(),
    c(names(web_r40_semantic_rules(plan$rules)),"CROSS_SECTION_SEMANTICS"))
  pending<-Filter(function(k)identical(plan$rules[[k]]$applicability,"REQUIRED") &&
    !identical(plan$rules[[k]]$result,"PASS"),relevant)
  plan$revision$pending<-pending
  plan$revision$receipt<-if(stable)base$r40 else NULL
  native_ids<-c(names(web_r40_semantic_rules(plan$rules)),"NUMERIC_CLAIMS","ARTIFACT_COVERAGE",
    "CITATION_INTEGRITY")
  remaining<-setdiff(native_ids,c(pending,"IDENTIFIER_INTEGRITY"))
  plan$revision$selective<-stable && all(vapply(plan$rules[remaining],function(r)
    r$applicability!="REQUIRED" || identical(r$result,"PASS"),logical(1)))
  plan$binding$revision_sha256<-web_c2_hash(list(part=part$fingerprint,receipt=plan$revision$receipt,
    pending=pending,reused=Filter(function(k)identical(plan$rules[[k]]$execution,"REUSED"),intersect(reuse,names(plan$rules)))))
  plan$fingerprint<-web_c2_hash(plan$binding)
  plan$execution_scope<-web_r40_execution_scope(plan,contract)
  plan
}

web_revision_base_receipt <- function(con,settings,actor,pid,origin) {
  tryCatch({
    base<-web_c2_context(con,settings,actor,pid,origin)$packet
    if(!identical(base$stage,"APPROVED") || is.list(base$revision))return(NULL)
    p<-web_c3_submission(con,settings,actor,pid,origin,base)
    receipt<-p$r40[[1L]]
    if(!isTRUE(p$r40_integration$R40_VALIDATION_COMPLETE) || !identical(receipt$status,"PASS"))return(NULL)
    checked<-web_c3_artifact(con,settings,actor,pid,receipt$id,receipt$sha256)
    if(identical(checked$row$approval_state,"APPROVED"))p
  },error=function(e)NULL)
}

web_revision_semantic_results <- function(reply,plan,section,evidence) {
  if(!is.list(reply$checks))return(list())
  ids<-plan$revision$pending
  answers<-setNames(lapply(ids,function(id) {
    hits<-Filter(function(r)identical(r$id,id),reply$checks)
    if(length(hits)!=1L)return(NULL)
    row<-hits[[1L]]
    if(identical(row$result,"FAIL"))return(list(result="FAIL"))
    if(!identical(row$result,"PASS") || !is.list(row$unsupported_claims) || length(row$unsupported_claims))return(NULL)
    if(identical(id,"STATISTICAL_METHODS_REPORTING")) {
      proof<-web_stat_methods_witnesses(row$witnesses,plan$statistical_records,section)
      if(!is.null(proof))return(c(list(result="PASS"),proof))
    } else {
      quote<-web_stat_text(row$manuscript_quote)
      supported<-web_stat_text(row$evidence_quote)
      if(nchar(quote)>=12L && nchar(supported)>=12L && grepl(quote,section,fixed=TRUE) &&
         grepl(supported,evidence,fixed=TRUE))return(list(result="PASS",witness_sha256=web_c2_hash(list(quote,supported))))
    }
    NULL
  }),ids)
  Filter(Negate(is.null),answers)
}

web_revision_worker_rules <- function(settings,plan,contract,policy,check_lease,
                                      transport=web_r40_ai_openai,mode="LIVE") {
  scope<-plan$execution_scope
  part<-contract$binding$inputs$revision$binding
  rules<-plan$rules
  result<-function(state,call=NULL)list(state=state,rules=rules,mode=mode,call=call)
  if(!isTRUE(plan$native_ready) || !is.list(part))return(result("REVISION_INPUTS_PENDING"))
  e<-web_c3_r40(settings$engine_root)
  safe<-tryCatch(!web_c3_leaks(part$section,e,settings$engine_root),error=function(e)NA)
  if(!is.na(safe)) {
    rules$IDENTIFIER_INTEGRITY$result<-if(safe)"PASS" else "FAIL"
    rules$IDENTIFIER_INTEGRITY$execution<-"EXECUTED"
    rules$IDENTIFIER_INTEGRITY$source<-list(section_sha256=part$section_sha256)
  }
  if(identical(safe,FALSE))return(result("REVISED_SECTION_IDENTIFIER_FAILURE"))
  # Missing baseline checks and genuinely new numeric claims remain pending.
  # Independently assess the scoped prose when authorization is available.
  ids<-plan$revision$pending
  scope_info<-lapply(plan$statistical_records,function(a)a[c("field","text")])
  evidence<-paste(as.character(jsonlite::toJSON(scope_info,auto_unbox=TRUE)),part$results,sep="\n\n")
  rules_text<-lapply(rules[ids],`[[`,"basis")
  request<-list(step="02_cross_section_consistency",policy_sha256=policy$sha256,
    system=paste("Validate only the revised Statistical Analysis against its frozen execution records.",
      "Return a checks array with id, result, unsupported_claims, manuscript_quote and evidence_quote.",
      "Use PASS only for evidence-supported, complete reporting, FAIL for contradictions and NOT_ASSESSED when uncertain.",
      "For STATISTICAL_METHODS_REPORTING include a witnesses array for all supplied record IDs with the exact full evidence passage and a quotation from the revised section."),
    user=paste("Required rules",as.character(jsonlite::toJSON(rules_text,auto_unbox=TRUE)),
      "Execution evidence",evidence,"Original text",part$original_section,"Revised text",part$section,sep="\n"))
  called<-web_r40_ai_call(settings,scope,request,policy,transport,mode,check_lease)
  audit<-called[intersect(names(called),c("id","state","model","provider_id","usage","cost_microusd","dispatched","reused"))]
  if(!identical(called$state,"COMPLETE"))return(result(called$state,audit))
  parsed<-tryCatch(jsonlite::fromJSON(called$text,simplifyVector=FALSE),error=function(e)NULL)
  found<-web_revision_semantic_results(parsed,plan,part$section,evidence)
  for(id in names(found)) {
    rules[[id]]$result<-found[[id]]$result
    rules[[id]]$execution<-"EXECUTED"
    rules[[id]]$source<-list(section_sha256=part$section_sha256,check=found[[id]])
  }
  result("COMPLETE",audit)
}
