# Deterministic presentation of approved values; no statistics or event aggregation.
web_c2_facts <- function(source,engine) {
 e<-web_c2_defs(engine,38);facts<-list()
 add<-function(key,value,version,field){if(length(value)==1&&!is.null(value)&&!is.na(value))facts[[key]]<<-list(value=value,rendered=if(is.numeric(value))e$format_num(value) else as.character(value),version=version,field=field)}
 for(v in names(source$analyses)) {
  a<-source$analyses[[v]]
  for(k in names(a$result$summary))if(is.numeric(a$result$summary[[k]]))add(paste0("A",v,".",k),a$result$summary[[k]],v,paste0("result.summary.",k))
 }
 # Approved row-level values may be cited, but never summed or used to recompute a pooled result.
 for(t in names(source$evidence$tables))for(row in source$evidence$tables[[t]])for(k in names(row)) {
  val<-row[[k]]
  if(is.character(val)&&length(val)==1&&grepl("^[0-9]+([.][0-9]+)?$",val)&&!startsWith(k,"."))add(paste0("E",row$.row_id,".",k),val,source$evidence_version,paste(t,row$.row_id,k,sep="."))
 }
 for(i in seq_along(source$evidence_outputs)) {
  t<-source$evidence_outputs[[i]]
  for(j in seq_along(t$rows))for(k in names(t$rows[[j]])) {
   val<-t$rows[[j]][[k]]
   if(length(val)==1&&is.character(val)&&grepl("^[0-9]+([.][0-9]+)?$",val))add(paste0("P",i,".row",j,".",k),val,t$version,paste(names(source$evidence_outputs)[i],j,k,sep=":"))
  }
 }
 facts
}
web_c2_expand <- function(text,x) {
 labels<-web_c2_artifact_labels(x)
 for(id in names(labels))text<-gsub(paste0("{{ARTIFACT:",id,"}}"),labels[[id]],text,fixed=TRUE)
 m<-stringr::str_match_all(text,"\\{\\{FACT:([A-Za-z0-9_.]+)\\}\\}")[[1]]
 if(nrow(m))for(k in unique(m[,2])) {
  web_assert(!is.null(x$source$facts[[k]]),"Unknown scientific fact token.","NUMERIC_CONSISTENCY_FAILED")
  text<-gsub(paste0("{{FACT:",k,"}}"),x$source$facts[[k]]$rendered,text,fixed=TRUE)
 }
 text
}
web_c2_system_blocks <- function(x,engine) {
 e<-web_c2_defs(engine,38);results<-character();methods<-character()
 for(v in names(x$source$analyses)) {
  a<-x$source$analyses[[v]];row<-as.data.frame(Filter(function(v)length(v)==1,a$result$summary),stringsAsFactors=FALSE)
  # Human-approved role/outcome label replaces only an internal engine group identifier.
  row$analysis_group<-paste(a$plan$role,a$plan$outcome,a$plan$timepoint)
  line<-paste(e$build_meta_result_sentences(row),collapse="\n")
  line<-gsub("^[-] ","",line);warnings<-unlist(a$result$warnings);technical<-grepl("conversion failure|mbcsToSbcs|font|glyph",warnings,ignore.case=TRUE);display<-c(warnings[!technical],if(any(technical))"Figure-label rendering warnings were recorded; inspect the selected figures before publication.");results<-c(results,line,paste(display,collapse=" "))
  methods<-c(methods,paste(a$plan$role,a$plan$outcome,a$plan$timepoint,":",paste(unlist(a$plan$methods),collapse="; "),paste0(". Executed with ",a$result$R,".")))
 }
 # Unsupported registration/reviewer statements are not added as boilerplate.
 db<-names(Filter(isTRUE,x$source$search$databases))
 if(length(db))methods<-c(paste("Configured database inputs:",paste(toupper(db),collapse=", "),"."),methods)
 tables<-x$source$evidence$tables
 if(length(tables$Reviewer_1)&&length(tables$Reviewer_1)==length(tables$Reviewer_2)&&all(vapply(seq_along(tables$Reviewer_1),function(i){a<-tables$Reviewer_1[[i]]$.actor;b<-tables$Reviewer_2[[i]]$.actor;!is.null(a)&&!is.null(b)&&a!=b},logical(1))))methods<-c(methods,"Title and abstract eligibility decisions were recorded by independent Human reviewers before conflict resolution.")
 list(Results=paste(results[nzchar(results)],collapse="\n\n"),Methods=paste(unique(methods),collapse="\n\n"))
}
web_c2_check <- function(x,engine) {
 issues<-character();add<-function(m)issues<<-c(issues,m);e<-web_c2_defs(engine,39);n<-web_c2_defs(engine,40)
 if(!setequal(names(x$sections),web_c2_sections()))add("Required manuscript sections are missing")
 if(!length(x$source$references$records))add("Verified references required")
 refs<-vapply(x$source$references$records,`[[`,character(1),"pmid");allcite<-character()
 for(k in names(x$sections)) {
  sec<-x$sections[[k]];text<-sec$text
  if(!identical(sec$source_sha256,x$source_sha256))add(paste(k,"source package mismatch"))
  if(!identical(sec$status,"APPROVED"))add(paste(k,"needs explicit Human section approval"))
  if(!nzchar(trimws(text))||grepl("AUTHOR[- ]REQUIRED|\\[TODO|\\[TBD",text,ignore.case=TRUE))add(paste(k,"author-required content unresolved"))
  expanded<-tryCatch(web_c2_expand(text,x),error=function(err){add(paste(k,"unknown fact token"));text})
  if(grepl("\\{\\{|\\}\\}",expanded))add(paste(k,"unresolved placeholder"))
  # Strict editing contract: all numeric claims are source tokens, including values that happen to match.
  free<-gsub("\\{\\{FACT:[A-Za-z0-9_.]+\\}\\}","",text);free<-gsub("\\{\\{ARTIFACT:[a-f0-9]+\\}\\}","",free);free<-gsub("\\[\\[PMID:[0-9]+\\]\\]","",free)
  if(nrow(n$extract_numeric_tokens(free)))add(paste(k,"unbound numerical claim: insert a protected source fact"))
  if(grepl("\\[\\[|\\]\\]",gsub("\\[\\[PMID:[0-9]+\\]\\]","",text)))add(paste(k,"unknown citation token"))
  if(grepl("\\[[0-9][0-9,; -]*\\]",free)||grepl("(?m)^#{1,6} ",text,perl=TRUE))add(paste(k,"use stable PMID citations and the existing section headings"))
  if(grepl("\\b(zero|one|two|three|four|five|six|seven|eight|nine|ten|hundred|thousand)\\b",free,ignore.case=TRUE,perl=TRUE))add(paste(k,"write numeric claims with protected facts, not number words"))
  paras<-strsplit(text,"\n[[:space:]]*\n",perl=TRUE)[[1]]
  for(para in paras) {
   tokens<-stringr::str_match_all(para,"\\{\\{FACT:A([a-f0-9]+)[.]")[[1]]
   if(nrow(tokens)) {
    vs<-unique(tokens[,2]);if(!all(vs %in% names(x$source$analyses))){add(paste(k,"unknown analysis fact"));next};roles<-vapply(x$source$analyses[vs],function(a)a$plan$role,character(1))
    stated<-regmatches(toupper(para),gregexpr("PRIMARY|SECONDARY|SENSITIVITY|SUBGROUP|EXPLORATORY",toupper(para)))[[1]]
    if(length(vs)>1||any(!stated %in% roles))add(paste(k,"analysis role/number binding is ambiguous or contradictory; use separate labeled paragraphs"))
   }
  }
  cites<-e$extract_pmids(text);allcite<-unique(c(allcite,cites))
  if(!all(cites %in% refs))add(paste(k,"unverified citation"))
  if(k %in% c("Introduction","Discussion")) {
   paragraphs<-strsplit(text,"\n[[:space:]]*\n",perl=TRUE)[[1]]
   if(any(!vapply(paragraphs,function(p)length(e$extract_pmids(p))>0,logical(1))))add(paste(k,"each literature paragraph needs a verified citation"))
   limit<-x$source$reference_policy[[tolower(k)]]
   if(length(cites)<limit)add(paste(k,"below canonical reference minimum",limit))
  }
 }
 if(length(allcite)<x$source$reference_policy$total)add("Below canonical unique-reference minimum")
 if(anyDuplicated(refs))add("Duplicate PMID")
 dois<-vapply(x$source$references$records,function(r)n$normalize_doi(r$doi),character(1));if(anyDuplicated(dois[nzchar(dois)]))add("Duplicate DOI")
 if(!all(vapply(x$source$references$records,function(r)isTRUE(r$metadata_verified),logical(1))))add("Unverified reference metadata")
 list(status=if(length(issues))"FAIL" else "PASS",label="WEB-012 Manuscript Pre-check — not Final QC",issues=as.list(unique(issues)))
}
web_c2_render <- function(x,engine,strict=TRUE) {
 if(strict)web_assert(web_c2_check(x,engine)$status=="PASS","Resolve manuscript pre-check issues.","INPUT_VALIDATION_FAILED")
 e<-web_c2_defs(engine,39);blocks<-web_c2_system_blocks(x,engine);parts<-c(paste0("# ",x$spec$title))
 for(k in web_c2_sections())parts<-c(parts,paste0("## ",k),blocks[[k]],web_c2_expand(x$sections[[k]]$text %||b2% "[AUTHOR REQUIRED]",x))
 text<-paste(parts,collapse="\n\n");order<-e$extract_pmids(text)
 refs<-setNames(x$source$references$records,vapply(x$source$references$records,`[[`,character(1),"pmid"))
 web_assert(all(order %in% names(refs)),"Unverified reference in manuscript.","CITATION_VALIDATION_FAILED")
 mapping<-data.frame(pmid=order,citation_number=seq_along(order));text<-e$replace_tokens_with_numbers(text,mapping)
 bibliography<-vapply(seq_along(order),function(i)paste0(i,". ",e$format_vancouver_reference(refs[[order[i]]])),character(1))
 legends<-unname(web_c2_artifact_labels(x))
 list(text=paste(c(text,"## References",bibliography,"## Table and Figure legends",legends),collapse="\n\n"),citations=mapping,legends=legends,blocks=blocks)
}

web_c2_artifact_labels <- function(x) {
 setNames(vapply(seq_along(x$source$artifacts),function(i){a<-x$source$artifacts[[i]];plan<-x$source$analyses[[a$analysis_version]]$plan;paste(if(a$type=="table")"Table" else "Figure",i,"—",plan$role,plan$outcome,plan$timepoint)},character(1)),names(x$source$artifacts))
}
