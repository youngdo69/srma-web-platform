# Deterministic presentation of approved values; no statistics or event aggregation.
web_c2_facts <- function(source,engine) {
  if (identical(source$route,"NARRATIVE")) {
    # No generic numeric-looking AI/evidence table values enter narrative facts.
    rows <- source$narrative$rows
    facts <- list()
    for(r in rows) {
      if(identical(r$decision,"NOT_REPORTED")) {
        web_assert(!nzchar(r$fact_id) && !nzchar(r$numeric_summary),
          "Not-reported narrative result must never create a numeric fact.")
        next
      }
      web_assert(r$decision %in% c("CONFIRMED","CORRECTED") &&
        nzchar(r$fact_id) && nzchar(r$numeric_summary) &&
        nzchar(r$verified_by) && nzchar(r$pdf_sha256) &&
        nzchar(r$source_location),
        "Unverified numerical narrative data reached the manuscript fact registry.")
      web_assert(is.null(facts[[r$fact_id]]),"Duplicate narrative numeric fact key.")
      facts[[r$fact_id]] <- list(value=r$numeric_summary,
        rendered=r$numeric_summary,version=source$evidence_version,
        field=paste0("narrative_numeric_reviews.",r$result_id),
        provenance=list(result_id=r$result_id,study_id=r$study_id,
          screening_id=r$screening_id,pdf_sha256=r$pdf_sha256,
          source_digest=r$source_digest,source_location=r$source_location,
          verified_by=r$verified_by,decision=r$decision))
    }
    return(facts)
  }
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
  if(identical(x$source$route,"NARRATIVE")) {
    web_assert(identical(x$source$narrative$finalization_scope,
      "NARRATIVE_ANALYSIS_LOCKED; NOT_CANONICAL_R26"),
      "Narrative source has not been finalized.")
    rows <- x$source$narrative$rows
    lines <- vapply(rows,function(r)paste0(
      "- ",r$study_label,"; original PDF endpoint: ",r$original_outcome,
      "; original PDF timepoint: ",
      if(nzchar(r$reported_timepoint))r$reported_timepoint else "not specified in source",
      "; Human-approved analysis variable: ",r$common_variable,
      "; analysis timepoint: ",r$analysis_timepoint,
      "; PDF location: ",r$source_location,"; result: ",
      if(identical(r$decision,"NOT_REPORTED"))
        "not reported (Human PDF check)" else
        paste0("{{FACT:",r$fact_id,"}}")),character(1))
    methods <- c("Structured narrative synthesis of Human-confirmed study/outcome/timepoint sets.",
      "Results originate from saved registered PDFs and exact source-bound Human verification.",
      "No pooled effect or canonical R/26 statistical inference was performed.")
    if(isTRUE(x$source$narrative$unresolved_rob))methods <- c(methods,
      "Risk of bias remains unresolved; this is not an approved publication result.")
    return(list(Results=paste(lines,collapse="\n\n"),
      Methods=if(is.null(x$source$statistical_methods))paste(methods,collapse="\n\n")))
  }
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
 list(Results=paste(results[nzchar(results)],collapse="\n\n"),
   Methods=if(is.null(x$source$statistical_methods))paste(unique(methods),collapse="\n\n"))
}
web_c2_check <- function(x,engine) {
  issues<-character();add<-function(m)issues<<-c(issues,m);e<-web_c2_defs(engine,39);n<-web_c2_defs(engine,40)
  if(identical(x$source$route,"NARRATIVE")) {
    nr <- x$source$narrative
    if(!identical(nr$finalization_scope,"NARRATIVE_ANALYSIS_LOCKED; NOT_CANONICAL_R26") ||
        !identical(nr$source_version,x$evidence_version))
      add("Narrative approval/version binding is invalid")
    if(isTRUE(nr$unresolved_rob)) add("Human-reviewed risk of bias remains pending")
    rr <- nr$rows
    if(!length(rr) || anyDuplicated(vapply(rr,function(r)r$result_id,character(1))))
      add("Narrative result inventory missing or duplicated")
    for(r in rr) {
      if(grepl("AUTHOR REQUIRED",r$study_label,fixed=TRUE))
        add("Study citation/identification needs author completion")
      if(identical(r$decision,"NOT_REPORTED")) {
        if(nzchar(r$numeric_summary)||nzchar(r$fact_id))
          add("Unreported result contains a numeric fact")
      } else {
        f <- x$source$facts[[r$fact_id]]
        if(is.null(f) || !identical(f$rendered,r$numeric_summary) ||
            !identical(f$provenance$pdf_sha256,r$pdf_sha256) ||
            !identical(f$provenance$result_id,r$result_id))
          add("Narrative fact token lacks exact Human/PDF provenance")
      }
    }
  }
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
 for(k in web_c2_sections())parts<-c(parts,paste0("## ",k),
    if(!is.null(blocks[[k]]))web_c2_expand(blocks[[k]],x) else NULL,
    web_c2_expand(x$sections[[k]]$text %||b2% "[AUTHOR REQUIRED]",x))
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

# Markdown, the numbering map, and the layout data form the render source.
# The index records stable section labels and version-local paragraph order.
web_c2_source_index <- function(markdown,layout) {
  lines<-strsplit(markdown,"\n",fixed=TRUE)[[1L]]
  headings<-which(grepl("^#{1,6} ",lines))
  stopifnot(length(headings)>0L,headings[[1L]]==1L)
  ends<-c(headings[-1L],length(lines)+1L)
  assets<-c(layout$tables,layout$figures)
  lapply(seq_along(headings),function(i) {
    first<-headings[[i]];end<-ends[[i]]
    title<-sub("^#+ ","",lines[[first]])
    body<-if(end>first+1L)trimws(paste(lines[seq.int(first+1L,end-1L)],collapse="\n")) else ""
    blocks<-strsplit(body,"\n[[:blank:]]*\n+",perl=TRUE)[[1L]]
    list(id=paste0("section-",i),heading=title,level=nchar(sub(" .*", "",lines[[first]])),
      paragraphs=lapply(seq_along(blocks),function(k) {
        b<-blocks[[k]]
        mentions<-regmatches(b,gregexpr("(?i)\\b(?:table|figure) [0-9]+\\b",b,perl=TRUE))[[1L]]
        linked<-Filter(function(a)any(tolower(a$label) %in% tolower(mentions)),assets)
        list(id=paste0("section-",i,"-paragraph-",k),sha256=web_c2_hash(b),
          citations=as.list(web_revision_citations(b)),
          display_assets=as.list(mentions),asset_ids=lapply(linked,`[[`,"id"))
      }))
  })
}

web_c2_source_layout <- function(assets,out) {
  label<-function(title)sub("^((?:Table|Figure) [0-9]+).*", "\\1",title,perl=TRUE)
  tables<-lapply(seq_along(assets$tables),function(i) {
    tab<-assets$tables[[i]]
    list(id=paste0("table-",i),label=label(tab$title),title=tab$title,origin=tab$origin,
      columns=lapply(tab$data,function(col)as.list(as.character(col))))
  })
  figures<-lapply(seq_along(assets$figures),function(i) {
    fig<-assets$figures[[i]]
    n<-paste0("render-figure-",i,".png")
    dest<-file.path(out,n)
    web_assert(file.copy(fig$path,dest),"Cannot freeze the rendered publication figure.")
    list(id=paste0("figure-",i),file=n,label=label(fig$title),title=fig$title,origin=fig$origin,sha256=web_sha(dest))
  })
  list(publication=!is.null(assets),tables=tables,figures=figures)
}

web_c2_source_write <- function(out,x,layout) {
  md<-file.path(out,"manuscript.md"); citations<-file.path(out,"citation_map.json")
  source<-list(schema="SRMA_MANUSCRIPT_SOURCE_V1",source_sha256=x$source_sha256,
    manuscript_id=x$manuscript_id,evidence_version=x$evidence_version,
    reference_sha256=x$reference_sha256,markdown_sha256=web_sha(md),citations_sha256=web_sha(citations),
    sections=web_c2_source_index(web_revision_read_md(md),layout),layout=layout)
  dest<-file.path(out,"manuscript_source.json")
  jsonlite::write_json(source,dest,auto_unbox=TRUE,null="null",pretty=TRUE)
  web_sha(dest)
}

web_c2_source_read <- function(path,x,md,citations) {
  web_assert(isTRUE(file.info(path)$size<=20e6),"Canonical manuscript source exceeds its size limit.")
  a<-jsonlite::read_json(path,simplifyVector=FALSE)
  web_assert(identical(a$schema,"SRMA_MANUSCRIPT_SOURCE_V1") && identical(a$source_sha256,x$source_sha256) &&
    identical(a$manuscript_id,x$manuscript_id) && identical(a$evidence_version,x$evidence_version) &&
    identical(a$reference_sha256,x$reference_sha256) && identical(a$markdown_sha256,web_sha(md)) &&
    identical(a$citations_sha256,web_sha(citations)) &&
    identical(web_c2_hash(a$sections),web_c2_hash(web_c2_source_index(web_revision_read_md(md),a$layout))) &&
    is.logical(a$layout$publication) && length(a$layout$publication)==1L && !is.na(a$layout$publication),
    "The canonical manuscript source does not match the frozen archive.","ARTIFACT_INTEGRITY_FAILED")
  a
}

web_c2_source_assets <- function(layout,files,out) {
  if(!isTRUE(layout$publication))return(NULL)
  tables<-lapply(layout$tables,function(tab) {
    values<-lapply(tab$columns,function(col)vapply(col,function(v)if(is.null(v))NA_character_ else v,character(1)))
    list(title=tab$title,data=as.data.frame(values,check.names=FALSE),origin=tab$origin)
  })
  figures<-lapply(layout$figures,function(fig) {
    web_assert(grepl("^render-figure-[0-9]+[.]png$",fig$file) && fig$file %in% names(files) &&
      identical(web_sha(files[[fig$file]]),fig$sha256),"The frozen figure is missing or unverified.")
    dest<-file.path(out,fig$file)
    web_assert(file.copy(files[[fig$file]],dest) && identical(web_sha(dest),fig$sha256),"Cannot stage the frozen figure.")
    list(title=fig$title,path=dest,origin=fig$origin)
  })
  list(tables=tables,figures=figures)
}

web_c2_render_word <- function(markdown,engine,dest,publication_assets=NULL) {
  production<-is.list(publication_assets)
  e<-web_c2_defs(engine,41)
  needed<-c("officer","flextable");web_assert(all(vapply(needed,requireNamespace,logical(1),quietly=TRUE)),"Editable Word dependencies unavailable.","DEPENDENCY_FAILED")
  doc<-officer::read_docx();styles<-e$available_paragraph_styles(doc);e$style_map<-list(title=e$pick_paragraph_style(styles,c("Title","graphic title","heading 1")),heading1=e$pick_paragraph_style(styles,c("heading 1")),heading2=e$pick_paragraph_style(styles,c("heading 2")),heading3=e$pick_paragraph_style(styles,c("heading 3")),normal=e$pick_paragraph_style(styles,"Normal"))
  # 9BCD.5: The body contains only cross-references to Tables/Figures.
  # Actual assets are appended at the very end, AFTER References.
  in_abstract<-FALSE
  for(block in e$parse_markdown_blocks(markdown)) {
    if(identical(block$type,"heading")) {
      heading<-trimws(as.character(block$text))
      if(identical(heading,"Abstract"))in_abstract<-TRUE
      if(identical(heading,"Keywords"))in_abstract<-FALSE
    }
    value<-trimws(as.character(block$text %||b2% ""))
    if(production && in_abstract && !identical(block$type,"heading") &&
       grepl("^(Background|Methods|Results|Conclusions|Conclusion|PROSPERO):[[:space:]]*",value)) {
      label<-sub("^([^:]+):.*$","\\1",value)
      detail<-sub("^[^:]+:[[:space:]]*","",value)
      doc<-officer::body_add_fpar(doc,officer::fpar(
        officer::ftext(paste0(label,": "),prop=officer::fp_text(bold=TRUE)),
        officer::ftext(detail)),style=e$style_map$normal)
    } else doc<-e$add_manuscript_block(doc,block)
  }
  if(production)
    doc<-web_c2_9e_append_endmatter(doc,publication_assets,e)
  # Preserve frozen scientific outputs; no recalculation or image manipulation.
  print(doc,target=dest)
  if(production)web_c2_9e_format_docx(dest)
  web_no_file_secrets(dest,engine)
  invisible(dest)
}
