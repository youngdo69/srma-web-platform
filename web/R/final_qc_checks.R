# Deterministic inspection only. No result repair, statistical recomputation or DOCX generation.
web_c3_r40 <- function(engine) {
 if(!exists("web_b2_exports",mode="function"))source(file.path(engine,"web/adapter/evidence.R"))
 p<-list.files(file.path(engine,"R"),pattern="^40_",full.names=TRUE)
 e<-web_b2_exports(p,new.env(parent=globalenv()),c("numeric_token_pattern","standard_manuscript_headings","machine_identifier_patterns"));e$numeric_relative_tolerance<-1e-6
 e$.expressions<-parse(p);e
}
web_c3_citation_audit <- function(e,text,mapping,records) {
 # Evaluate the actual R40 citation audit, not a parallel weaker implementation.
 e$manuscript_text<-text;e$citation_map<-mapping;e$selected_literature<-web_b2_df(records)
 e$references_text<-e$extract_section(text,"References");e$reference_lines<-strsplit(e$references_text,"\n",fixed=TRUE)[[1]];e$reference_lines<-e$reference_lines[grepl("^[0-9]+\\.\\s+",trimws(e$reference_lines))]
 e$reference_numbers<-as.integer(sub("^([0-9]+)\\..*$","\\1",trimws(e$reference_lines)))
 e$cited_numbers<-e$extract_citation_numbers(e$remove_references_and_after(text));e$map_numbers<-as.integer(mapping$citation_number);e$map_pmids<-as.character(mapping$pmid)
 web_assert(!anyDuplicated(e$map_numbers)&&!anyDuplicated(e$map_pmids)&&identical(sort(e$map_numbers),seq_along(e$map_numbers)),"Citation map is not one-to-one and contiguous.")
 for(ex in e$.expressions) {
  assignment<-is.call(ex)&&identical(ex[[1]],as.name("<-"))&&is.symbol(ex[[2]])
  if(assignment&&as.character(ex[[2]]) %in% c("citation_integrity_rows","all_numbers","citation_integrity"))eval(ex,e)
  if(is.call(ex)&&identical(ex[[1]],as.name("for"))&&identical(ex[[2]],as.name("num")))eval(ex,e)
 }
 e$citation_integrity
}
web_c3_coverage <- function(e,x) {
 arts<-x$source$artifacts
 e$required_artifacts<-data.frame(artifact_id=names(arts),artifact_type=vapply(arts,`[[`,character(1),"type"),title=unname(web_c2_artifact_labels(x)),manuscript_role="Results",manuscript_priority="required",source_relative_path=vapply(arts,`[[`,character(1),"path"),coverage_token=paste0("<!-- ARTIFACT: ",names(arts)," -->"))
 e$results_text<-x$sections$Results$text
 for(id in names(arts))e$results_text<-gsub(paste0("{{ARTIFACT:",id,"}}"),paste0("<!-- ARTIFACT: ",id," -->"),e$results_text,fixed=TRUE)
 e$results_artifact_markers<-e$extract_artifact_markers(e$results_text)
 for(ex in e$.expressions)if(is.call(ex)&&identical(ex[[1]],as.name("<-"))&&identical(ex[[2]],as.name("artifact_coverage_audit")))eval(ex,e)
 e$artifact_coverage_audit
}
web_c3_leaks <- function(text,e,engine) {
 canonical<-any(vapply(e$machine_identifier_patterns,function(p)grepl(p,text,ignore.case=TRUE,perl=TRUE),logical(1)))
 local<-grepl("/workspace/|/home/|/tmp/|[A-Za-z]:[/\\\\]|[.]env\\b|\\b[a-f0-9]{32}\\b|synthetic|test[-_ ]fixture|developer[-_ ]fixture",text,ignore.case=TRUE,perl=TRUE)
 secret<-tryCatch({web_no_secrets(text,engine);FALSE},error=function(err)TRUE)
 canonical||local||secret
}
web_c3_docx <- function(path,x,rendered,proof,root,engine) {
 entries<-utils::unzip(path,list=TRUE)
 web_assert(nrow(entries)>0&&all(!grepl("(^/|(^|/)\\.\\.(/|$)|^[A-Za-z]:)",entries$Name))&&sum(entries$Length)<100*1024^2,"Invalid or oversized Word container.")
 temp<-tempfile("qc-docx-");dir.create(temp);on.exit(unlink(temp,recursive=TRUE))
 utils::unzip(path,exdir=temp);xml<-xml2::read_xml(file.path(temp,"word/document.xml"));ns<-xml2::xml_ns(xml)
 text_node<-function(n)paste(xml2::xml_text(xml2::xml_find_all(n,".//w:t",ns)),collapse="")
 paras<-vapply(xml2::xml_find_all(xml,"/w:document/w:body/w:p",ns),text_node,character(1));paras<-trimws(paras);paras<-paras[nzchar(paras)]
 blocks<-web_c2_defs(engine,41)$parse_markdown_blocks(rendered$text)
 expected<-vapply(blocks,function(b)paste0(if(b$type=="bullet")"\u2022 " else "",b$text),character(1))
 web_assert(length(paras)>=length(expected)&&identical(paras[seq_along(expected)],expected),"Word manuscript text differs from the approved canonical rendering.","DOCX_TEXT_MISMATCH")
 captions<-unname(web_c2_artifact_labels(x));extra<-paras[-seq_along(expected)]
 web_assert(setequal(extra,captions)&&length(extra)==length(captions),"Word legends are missing or unexpected prose was appended.")
 tables<-xml2::xml_find_all(xml,"/w:document/w:body/w:tbl",ns);web_assert(length(tables)==length(x$source$analyses),"Unexpected Word table count.")
 fmt<-web_c2_defs(engine,38)$format_num
 for(i in seq_along(x$source$analyses)) {
  a<-x$source$analyses[[i]];values<-c(a$plan$role,a$plan$outcome,vapply(a$result$summary[c("studies","pooled_effect","lower_ci","upper_ci","p_value","i2","tau2")],fmt,character(1)))
  rows<-xml2::xml_find_all(tables[[i]],"./w:tr",ns);actual<-lapply(rows,function(row)vapply(xml2::xml_find_all(row,"./w:tc",ns),text_node,character(1)))
  web_assert(length(actual)==length(values)+1&&all(vapply(seq_along(values),function(j)length(actual[[j+1]])==2&&identical(actual[[j+1]][2],unname(values[j])),logical(1))),"Word table value/role differs from approved registry.","DOCX_TABLE_MISMATCH")
 }
 media<-list.files(file.path(temp,"word/media"),full.names=TRUE);figs<-Filter(function(a)grepl("[.]png$",a$path),proof$outputs)
 web_assert(length(media)==length(figs)&&setequal(vapply(media,web_sha,character(1)),vapply(figs,`[[`,character(1),"sha256")),"Word embedded figure bytes differ from the approved figure renderings.")
 # Inspect all publication text, including headers/footers, not only the visible main paragraphs.
 allxml<-list.files(file.path(temp,"word"),pattern="[.]xml$",full.names=TRUE,recursive=TRUE)
 visible<-paste(vapply(allxml,function(p){z<-xml2::read_xml(p);paste(xml2::xml_text(xml2::xml_find_all(z,"//*[local-name()='t']")),collapse=" ")},character(1)),collapse="\n")
 list(text=visible,paragraphs=length(paras),tables=length(tables),images=length(media),headings=length(xml2::xml_find_all(xml,"//w:pStyle",ns)))
}
web_c3_checks <- function(x,proof,root,engine) {
 e<-web_c3_r40(engine);findings<-list();checks<-list()
 add<-function(code,severity,affected,source,message,mode="MACHINE",status="FAIL") {
  id<-paste0(code,"-",substr(web_c2_hash(list(affected,source,message)),1,12))
  findings[[length(findings)+1L]]<<-list(id=id,check=code,severity=severity,affected=affected,source=source,machine_result=status,message=message,mode=mode,action=if(mode=="HUMAN")"Review source and record scientific/visual disposition; request upstream correction if unsupported" else "Correct the identified upstream source, approve a new version and rerun QC")
 }
 run<-function(code,source,expr){ok<-tryCatch({force(expr);TRUE},error=function(err){add(code,"BLOCKER",code,source,if(inherits(err,"srma_web_error"))conditionMessage(err) else "Required input is unreadable or violates its canonical contract.");FALSE});checks[[code]]<<-if(ok)"PASS" else "FAIL";ok}
 run("lineage",proof$manuscript_version,{
  web_assert(x$stage=="APPROVED"&&web_c2_hash(x$source)==proof$source_sha256&&web_c2_hash(x$source$references)==proof$reference_sha256&&web_c2_hash(x$sections)==proof$content_sha256,"Manuscript source/reference/content binding mismatch.")
  for(a in c(proof$artifacts,proof$outputs))web_assert(web_sha(web_inside(file.path(root,a$path),root))==a$sha256,"Required artifact bytes/checksum mismatch.")
 })
 run("numerics",proof$evidence_version,{
  web_assert(web_c2_check(x,engine)$status=="PASS","Manuscript numeric/citation/section pre-check is not satisfied.")
  facts<-web_c2_facts(x$source,engine);web_assert(web_c2_hash(facts)==web_c2_hash(x$source$facts),"Frozen numeric facts differ from upstream raw values.")
  for(f in facts)if(is.numeric(f$value)){tok<-e$extract_numeric_tokens(f$rendered);web_assert(nrow(tok)==1&&e$numeric_equivalent(tok$value,f$value,tok$digits_after_decimal),"Rendered numeric value differs beyond canonical rounding.")}
  for(v in names(x$source$analyses)){a<-x$source$analyses[[v]];web_assert(a$stage=="APPROVED"&&a$result$status=="PASS"&&a$evidence_version==proof$evidence_version&&a$result$plan_sha256==proof$analyses[[v]]$plan_sha256,"Unapproved/non-estimable or wrong-role analysis.")}
 })
 run("tables_figures",proof$source_sha256,{
  map<-c(pooled_effect="pooled_effect",lower_ci="pooled_lower_ci",upper_ci="pooled_upper_ci",i2="heterogeneity_i2",tau2="heterogeneity_tau2",p_value="p_value",q="heterogeneity_q",q_p_value="heterogeneity_p",studies="n_studies")
  for(v in names(x$source$analyses)){
   a<-x$source$analyses[[v]];arts<-Filter(function(z)identical(z$analysis_version,v),x$source$artifacts)
   table<-Filter(function(z)z$type=="table",arts);figure<-Filter(function(z)z$type=="figure",arts)
   web_assert(length(table)==1,"Canonical statistical table missing or ambiguous.")
   if(a$plan$role=="PRIMARY")web_assert(length(figure)>0,"Primary canonical figure is required.")
   t<-web_c1_csv(file.path(root,table[[1]]$path));web_assert(nrow(t)==1,"Unexpected canonical table shape.")
   for(k in names(map))web_assert(isTRUE(all.equal(as.numeric(t[[map[[k]]]]),as.numeric(a$result$summary[[k]]),tolerance=1e-12)),paste("Canonical table differs for",k))
   web_assert(identical(a$result$included_studies,a$plan$study_ids)&&identical(a$result$included_rows,a$plan$row_ids)&&identical(a$result$included_reports,a$plan$report_ids),"Analysis study/report/row membership mismatch.")
   for(f in figure)web_assert(f$sha256==a$result$consistency$figure_sha256&&proof$artifacts[[f$id]]$job==proof$analyses[[v]]$job&&f$path==x$source$artifacts[[f$id]]$path&&a$result$consistency$status=="PASS","Figure lineage differs from its approved table/result job.")
  }
 })
 run("evidence_counts_rob",proof$evidence_version,{
  ev<-x$source$evidence;included<-unlist(ev$included_reports);studies<-ev$tables$Target_Study_Verification
  web_assert(length(studies)>0&&setequal(vapply(studies,`[[`,character(1),"screening_id"),included),"Study characteristics do not cover the finalized included reports.")
  web_assert(all(vapply(studies,function(z)!is.null(z$.verified_by)&&z$verification_status %in% c("Verified","Corrected"),logical(1))),"Study characteristics are not Human verified.")
  for(v in names(x$source$analyses)){a<-x$source$analyses[[v]];web_assert(all(unlist(a$plan$report_ids) %in% included)&&a$result$summary$studies==length(unique(unlist(a$plan$study_ids))),"Analysis study count or report membership differs from its canonical source.")}
  paths<-names(proof$evidence_manifest)
  for(pattern in c("included_studies[.]csv$","report_study_crosswalk[.]csv$","rob_final_combined[.]csv$"))web_assert(any(grepl(pattern,paths)),"Required included-study/crosswalk/RoB artifact missing.")
  for(p in paths[grepl("(included_studies|report_study_crosswalk|rob_final_combined)[.]csv$",paths)])web_assert(web_sha(file.path(root,p))==proof$evidence_manifest[[p]],"Evidence count/RoB artifact checksum differs.")
  rob<-web_c1_csv(file.path(root,paths[grepl("rob_final_combined[.]csv$",paths)][1]));web_assert(all(included %in% rob$screening_id),"Finalized RoB coverage is incomplete.")
  for(t in c("Domain_Review","Overall_Review"))web_assert(length(ev$tables[[t]])>0&&all(vapply(ev$tables[[t]],function(z)!is.null(z$.actor),logical(1))),"RoB decisions lack Human provenance.")
 })
 paths<-names(proof$evidence_manifest);prisma<-paths[grepl("prisma.*[.]csv$",paths,ignore.case=TRUE)]
 if(!length(prisma)){checks$prisma<-"USER_ACTION_REQUIRED";add("prisma","MAJOR","Screening counts",proof$evidence_version,"Required canonical PRISMA counts are missing; complete upstream screening.")}else run("prisma",proof$evidence_version,{for(p in prisma)web_assert(web_sha(file.path(root,p))==proof$evidence_manifest[[p]],"Canonical PRISMA count checksum mismatch.")})
 run("search_artifacts",proof$evidence_version,{
  for(db in names(Filter(isTRUE,x$source$search$databases)))web_assert(any(grepl(paste0("search_strategies/",db,"_search[.]txt$"),paths)),"Configured database search strategy missing.")
  for(p in paths[grepl("search_strategies/.*[.]txt$",paths)])web_assert(web_sha(file.path(root,p))==proof$evidence_manifest[[p]],"Search strategy checksum mismatch.")
 })
 rendered<-tryCatch(web_c2_render(x,engine),error=function(err)NULL)
 if(is.null(rendered)){add("rendering","BLOCKER","Manuscript",proof$content_sha256,"Approved manuscript cannot be deterministically rendered.");checks$rendering<-"FAIL"}else {
  run("citations",proof$reference_sha256,{
   map<-jsonlite::read_json(file.path(root,proof$outputs[["citation_map.json"]]$path),simplifyVector=TRUE)
   web_assert(identical(web_c2_hash(map),web_c2_hash(rendered$citations)),"Stored citation map differs from approved references.")
   ca<-web_c3_citation_audit(e,rendered$text,map,x$source$references$records);web_assert(nrow(ca)>0&&all(ca$status=="PASS"),"Canonical citation/reference audit failed.")
  })
  run("artifact_coverage",proof$source_sha256,{coverage<-web_c3_coverage(e,x);web_assert(all(coverage$status=="PASS"),"Cite every selected table/figure with substantive Results context using its artifact token.")})
  run("docx",x$docx$artifact_id,{
   md<-paste(readLines(file.path(root,proof$outputs[["manuscript.md"]]$path),warn=FALSE),collapse="\n");web_assert(identical(md,rendered$text),"Reviewed Markdown differs from approved content.")
   doc<-web_c3_docx(file.path(root,"config/generated/final_qc_assets/manuscript.docx"),x,rendered,proof,root,engine)
   web_assert(doc$headings>=length(web_c2_sections()),"Required Word headings missing.")
   web_assert(!web_c3_leaks(doc$text,e,engine),"Machine identifiers, paths, secrets or synthetic markers occur in Word.")
   web_assert(!grepl("AUTHOR[- ]REQUIRED|TODO|FIXME|\\{\\{|\\[\\[PMID|TEMPLATE_PLACEHOLDER",doc$text,ignore.case=TRUE),"Unresolved publication placeholder.")
  })
  run("leakage_placeholders",proof$content_sha256,{web_assert(!web_c3_leaks(rendered$text,e,engine),"Machine paths/identifiers, credentials or fixture markers in manuscript.");web_assert(!grepl("AUTHOR[- ]REQUIRED|TODO|FIXME|\\{\\{|\\[\\[PMID",rendered$text,ignore.case=TRUE),"Unresolved manuscript placeholder.")})
 }
 # Actual text-based PDF inspection supplements provenance; no OCR or statistical inference.
 for(f in Filter(function(a)a$type=="figure",x$source$artifacts))run(paste0("figure_text_",f$id),f$id,{
  web_assert(nzchar(Sys.which("pdftotext")),"Figure text inspector unavailable; operator action required.")
  out<-tempfile();on.exit(unlink(out),add=TRUE);rc<-system2(Sys.which("pdftotext"),shQuote(c(file.path(root,f$path),out)),stdout=FALSE,stderr=FALSE)
  web_assert(rc==0L&&file.exists(out),"Canonical figure is unreadable.");text<-paste(readLines(out,warn=FALSE),collapse=" ")
  web_assert(nzchar(trimws(text))&&!web_c3_leaks(text,e,engine),"Figure contains internal identifiers/fixture markers or has no readable text.")
 })
 # Deterministically bind interpretation language where possible; never decide clinical relevance.
 for(k in names(x$sections))for(para in strsplit(x$sections[[k]]$text,"\n[[:space:]]*\n",perl=TRUE)[[1]]) {
  toks<-stringr::str_match_all(para,"\\{\\{FACT:A([a-f0-9]+)[.]([A-Za-z0-9_]+)\\}\\}")[[1]]
  if(nrow(toks)&&length(unique(toks[,2]))==1&&toks[1,2] %in% names(x$source$analyses)) {
   a<-x$source$analyses[[toks[1,2]]];s<-a$result$summary;low<-tolower(para)
   if(a$plan$effect_measure %in% c("RR","OR","HR")) {
    if((grepl("\\b(reduced|lower|decreased)\\b",low)&&s$pooled_effect>1)||(grepl("\\b(increased|higher)\\b",low)&&s$pooled_effect<1))add("direction","BLOCKER",k,toks[1,2],"Directional wording contradicts the bound canonical ratio; revise upstream prose.")
   }
   neg<-grepl("non[- ]significant|not (statistically )?significant",low)
   if((grepl("significant",low)&&!neg&&is.numeric(s$p_value)&&s$p_value>=.05)||(neg&&is.numeric(s$p_value)&&s$p_value<.05))add("significance","BLOCKER",k,toks[1,2],"Significance wording contradicts the bound canonical p value.")
  }
  refs<-web_c2_defs(engine,39)$extract_pmids(para)
  if(k %in% c("Introduction","Discussion")||grepl("superior|benefit|favou?r|increased|reduced|higher|lower|significant|registered|PROSPERO|protocol|reviewer|screened|extract|bias",para,ignore.case=TRUE))add("claim_support","MAJOR",k,paste(c(proof$evidence_version,refs),collapse=","),para,"HUMAN","REVIEW_REQUIRED")
 }
 methods<-x$sections$Methods$text
 if(grepl("fixed[- ]effect",methods,ignore.case=TRUE)&&all(vapply(x$source$analyses,function(a)a$plan$methods$model=="Random-effects",logical(1))))add("methods","BLOCKER","Methods",proof$evidence_version,"Fixed-effect claim contradicts the executed random-effects model.")
 alltext<-paste(vapply(x$sections,`[[`,character(1),"text"),collapse="\n")
 if(grepl("registered|PROSPERO|preregistered|protocol (publication|published)",gsub("not (registered|preregistered)|registration (not provided|pending)|not registered (in|with) PROSPERO","",alltext,ignore.case=TRUE),ignore.case=TRUE)&&!length(x$source$registration))add("registration","MAJOR","Methods/protocol",proof$evidence_version,"Registration claim lacks stored registration evidence. Correct the manuscript or provide verified upstream protocol evidence.")
 for(k in c("Methods/protocol","Study/report/PRISMA counts and RoB descriptions","Visual Word and all selected figures"))add("human_review","MAJOR",k,proof$source_sha256,"Compare every descriptive statement to frozen evidence, verify no unsupported claims or unreadable output; a severe visual failure requires upstream correction.","HUMAN","REVIEW_REQUIRED")
 pass<-!any(vapply(findings,function(f)f$mode=="MACHINE"&&f$severity %in% c("BLOCKER","MAJOR"),logical(1)))
 lock<-jsonlite::read_json(file.path(engine,"renv.lock"));runtime<-paste0("R version ",lock$R$Version)
 canonical<-startsWith(R.version.string,runtime)&&startsWith(proof$docx_job$R %||b2% "",runtime)&&all(vapply(x$source$analyses,function(a)startsWith(a$result$R,runtime)&&a$result$lock_sha256==proof$lock_sha256,logical(1)))
 list(machine_pass=pass,checks=checks,findings=findings,canonical_sources=canonical,ruleset=web_c3_ruleset(),proof_sha256=web_c2_hash(proof),R=R.version.string,lock_sha256=proof$lock_sha256,canonical_r40_sha256=web_sha(list.files(file.path(engine,"R"),pattern="^40_",full.names=TRUE)),semantic_route="Explicit Human scientific review; no R40 provider-semantic PASS claimed")
}
