# Read-only scientific comparisons and an append-only disposition record.
# These dispositions are not scientific approvals or release decisions.
web_qc_pdf_read <- function(path,first,last,max_bytes) {
  reader<-Sys.getenv("SRMA_PDFTOTEXT",unset="")
  if(!nzchar(reader))reader<-unname(Sys.which("pdftotext"))
  unavailable<-function(state)structure(character(),pdf_status=state)
  if(!grepl("^(/|[A-Za-z]:[/\\\\])",reader) || !file.exists(reader) || dir.exists(reader) ||
     !tolower(basename(reader)) %in% c("pdftotext","pdftotext.exe"))
    return(unavailable("READER_UNAVAILABLE"))
  target<-tempfile("qc-pdf-")
  on.exit(unlink(target),add=TRUE)
  status<-tryCatch(suppressWarnings(system2(reader,c("-f",first,"-l",last,"-layout","-enc","UTF-8",
    shQuote(path),shQuote(target)),stdout=FALSE,stderr=FALSE,timeout=8L)),error=function(e)NA_integer_)
  if(!identical(status,0L) || !file.exists(target))return(unavailable("READER_FAILED"))
  if(file.info(target)$size>max_bytes)return(unavailable("TEXT_SIZE_LIMIT"))
  contents<-sub("\f[\r\n]*$","",paste(readLines(target,encoding="UTF-8",warn=FALSE),collapse="\n"))
  # Keep empty interior pages so returned indices match PDF page numbers.
  pages<-head(strsplit(paste0(contents,"\fEND"),"\f",fixed=TRUE)[[1L]],-1L)
  structure(pages,pdf_status=if(any(nzchar(trimws(pages))))"TEXT_EXTRACTED" else "NO_TEXT_LAYER_IN_RANGE")
}

web_qc_pdf_page <- function(path,page)web_qc_pdf_read(path,page,page,80000L)
web_qc_pdf_pages <- function(path,limit=60L)web_qc_pdf_read(path,1L,limit,1024^2)

web_qc_pdf_row_context <- function(row,lines,normalize) {
  arms<-c("intervention","comparator")
  labels<-setNames(vapply(arms,function(a)normalize(web_b2_nr_one(row[[paste0(a,"_group")]])),character(1)),arms)
  time<-normalize(web_b2_nr_one(row$timepoint))
  counts<-setNames(lapply(arms,function(a) {
    values<-vapply(c("events","total"),function(k)web_b2_nr_one(row[[paste0(a,"_",k)]]),character(1))
    if(!all(grepl("^[0-9]+$",values)))return(NULL)
    pattern<-paste0("(?i)(?<![0-9.])",values[[1L]],"(?:\\s*/\\s*|\\s+of\\s+)",values[[2L]],"(?![0-9.])")
    locations<-gregexpr(pattern,lines,perl=TRUE)
    list(pattern=pattern,occurrences=sum(lengths(regmatches(lines,locations))))
  }),arms)
  function(raw_line,anchor) {
    clauses<-normalize(strsplit(raw_line,"[[:blank:]]{3,}|;",perl=TRUE)[[1L]])
    term<-function(x)paste0(" ",trimws(gsub("[^[:alnum:]]+"," ",x))," ")
    has<-function(needle) nzchar(needle) & grepl(term(needle),term(clauses),fixed=TRUE)
    if(!length(clauses))clauses<-""
    endpoint<-has(anchor)
    time_mentions<-gregexpr("(?i)\\b[0-9]+(?:\\s*[-\u2013]\\s*|\\s+)(?:day|week|month|year)s?\\b",
      clauses,perl=TRUE)
    compound<-grepl("\\b[0-9]+\\s*(?:,|and|or|to|[-\u2013])\\s*[0-9]+\\s*(?:days?|weeks?|months?|years?)\\b",
      clauses,perl=TRUE)
    single_time<-vapply(time_mentions,function(x)length(x)<=1L,logical(1)) & !compound
    exact_time<-has(time) & single_time
    definition<-normalize(web_b2_nr_one(row$outcome_definition))
    label<-normalize(web_b2_nr_one(row$outcome_name))
    exact_outcome<-has(if(nzchar(definition))definition else label)
    binds<-setNames(lapply(arms,function(a) {
      pair<-counts[[a]]
      if(is.null(pair))return("NOT_EXTRACTED")
      if(!any(grepl(pair$pattern,clauses,perl=TRUE)))return("NOT_LOCATED")
      other<-setdiff(arms,a)
      if(pair$occurrences!=1L || identical(pair$pattern,counts[[other]]$pattern) ||
         !all(nzchar(labels)) || identical(labels[[a]],labels[[other]]))
        return("REVIEW_REQUIRED")
      if(any(exact_outcome & exact_time & has(labels[[a]]) & !has(labels[[other]]) &
             grepl(pair$pattern,clauses,perl=TRUE)))"EXACT_CONTEXT_MATCH" else "REVIEW_REQUIRED"
    }),arms)
    list(definition=if(any(has(definition)))"EXACT_TEXT_MATCH" else "REVIEW_REQUIRED",
      timepoint=if(any(endpoint & exact_time))"EXACT_TEXT_MATCH" else "REVIEW_REQUIRED",events_total=binds)
  }
}

web_qc_search_primary_pdf <- function(row,pages) {
  normalize<-function(x) {
    x<-gsub("(?<=[[:alpha:]])[-\u00ad\u2010\u2011](?=[[:alpha:]])","",x,perl=TRUE)
    tolower(trimws(gsub("[[:space:]]+"," ",x)))
  }
  field<-web_b2_nr_one
  labels<-trimws(strsplit(field(row$outcome_name),";",fixed=TRUE)[[1L]])
  labels<-sub("^[0-9]+[.)][[:space:]]+","",labels)
  anchors<-unique(normalize(c(field(row$outcome_definition),labels,
    sub("^(freedom from|incidence of|rate of)[[:space:]]+","",labels,ignore.case=TRUE))))
  anchors<-anchors[nchar(anchors)>=5L]
  if(!length(anchors))return(list())
  fields<-c("outcome_definition","timepoint","analysis_population","intervention_group","comparator_group",
    "intervention_events","comparator_events","intervention_total","comparator_total",
    "intervention_n","comparator_n")
  page_lines<-strsplit(as.character(pages),"\n",fixed=TRUE)
  compare_context<-web_qc_pdf_row_context(row,unlist(page_lines,use.names=FALSE),normalize)
  found<-list()
  section<-"BODY"
  for(page in seq_along(page_lines)) {
    lines<-page_lines[[page]]
    text<-normalize(lines)
    for(i in seq_along(lines)) {
      if(grepl("^(supplement(al|ary) (materials?|appendix|results)|online supplement)$",text[[i]]))
        section<-"SUPPLEMENT"
      match_lines<-i
      present<-vapply(anchors,function(k)grepl(k,text[[i]],fixed=TRUE),logical(1))
      if(!any(present) && i<length(lines)) {
        wrapped<-normalize(paste(lines[c(i,i+1L)],collapse=" "))
        present<-vapply(anchors,function(k)grepl(k,wrapped,fixed=TRUE) &&
          !grepl(k,text[[i+1L]],fixed=TRUE),logical(1))
        if(any(present))match_lines<-c(i,i+1L)
      }
      if(!any(present))next
      # Values on neighboring rows or columns are never assigned to an arm.
      anchor<-anchors[which(present)[1L]]
      same_page<-Filter(function(hit)hit$page==page && identical(hit$anchor,anchor),found)
      if(length(same_page)>=2L || any(vapply(same_page,function(hit)
        i-max(unlist(hit$text_lines))<=4L,logical(1))))next
      window<-seq.int(max(1L,i-1L),min(length(lines),max(match_lines)+3L))
      excerpt<-paste(lines[window],collapse="\n")
      comparison<-setNames(lapply(fields,function(k){
        value<-normalize(field(row[[k]]))
        if(!nzchar(value))return("NOT_EXTRACTED")
        if(grepl("^[0-9]+$",value)) {
          numbers<-regmatches(text[[i]],gregexpr("(?<![0-9.])[0-9]+(?![0-9.])",text[[i]],perl=TRUE))[[1L]]
          if(!value %in% numbers)return("NOT_LOCATED")
          return("NUMERIC_TOKEN_REVIEW_REQUIRED")
        }
        if(grepl(value,normalize(excerpt),fixed=TRUE))"LOCATED_REVIEW_REQUIRED" else "NOT_LOCATED"
      }),fields)
      pairs<-setNames(lapply(c("intervention","comparator"),function(arm) {
        events<-field(row[[paste0(arm,"_events")]])
        total<-field(row[[paste0(arm,"_total")]])
        if(!all(grepl("^[0-9]+$",c(events,total))))return("NOT_EXTRACTED")
        if(grepl(paste0("(?<![0-9.])",events,"\\s*/\\s*",total,"(?![0-9.])"),
          text[[i]],perl=TRUE))"PAIR_LOCATED_ARM_UNCONFIRMED" else "NOT_LOCATED"
      }),c("intervention","comparator"))
      found[[length(found)+1L]]<-list(page=page,section=if(section=="SUPPLEMENT")section else
        if(any(grepl("^table[[:space:]]+[a-z]?[0-9]",text[seq.int(max(1L,i-6L),i)])))"TABLE" else "BODY",
        excerpt=substr(excerpt,1L,1000L),anchor=anchor,
        text_lines=as.list(match_lines),comparison=comparison,counts=pairs,
        source_comparison=compare_context(lines[[i]],anchors[which(present)[1L]]),
        reported_timepoints=as.list(unique(regmatches(excerpt,gregexpr(
          "(?i)\\b[0-9]+(?:\\s*[-\u2013]\\s*|\\s+)(?:day|week|month|year)s?\\b",excerpt,perl=TRUE))[[1L]])))
    }
  }
  # Give each configured outcome phrase coverage before filling the excerpt limit.
  by_anchor<-split(found,vapply(found,`[[`,character(1),"anchor"))
  rank<-order(sequence(lengths(by_anchor)), vapply(unlist(by_anchor,recursive=FALSE),`[[`,integer(1),"page"))
  unname(head(unlist(by_anchor,recursive=FALSE)[rank],12L))
}

web_qc_bound_pdf <- function(report,version,sha) {
  relative<-chartr("\\","/",web_b2_nr_one(report$.pdf_relative))
  if(!grepl("^[a-f0-9]{64}$",sha) || !identical(sha,web_b2_nr_one(report$.pdf_sha256)) ||
     !nzchar(relative) || grepl("^/|:|(^|/)[.]{1,2}(/|$)",relative) ||
     !identical(version$manifest[[relative]],sha))return(list(state="REGISTRY_BINDING_INVALID"))
  path<-tryCatch(web_inside(file.path(version$root,relative),version$root),error=function(e)NULL)
  if(is.null(path) || dir.exists(path))return(list(state="FILE_MISSING_OR_OUTSIDE_SNAPSHOT"))
  actual<-tryCatch(web_sha(path),error=function(e)NULL)
  if(!identical(actual,sha))return(list(state="FILE_HASH_UNVERIFIED"))
  list(state="HASH_VERIFIED",path=path,relative=relative)
}

web_qc_pdf_scan <- function(path,read_pages) {
  size<-file.info(path)$size
  if(!is.finite(size) || size>30*1024*1024)
    return(list(state="PDF_SIZE_LIMIT",pages=character()))
  pages<-tryCatch(read_pages(path),error=function(e)structure(character(),pdf_status="READER_FAILED"))
  state<-attr(pages,"pdf_status",exact=TRUE) %||b2% if(length(pages))"TEXT_EXTRACTED" else "TEXT_UNAVAILABLE"
  list(state=state,pages=pages)
}

web_qc_pdf_comparator_excerpt <- function(pages,comparator) {
  # Search using words from the frozen comparator. These excerpts are leads
  # for review, including therapies explicitly excluded by the protocol.
  value<-paste(unlist(comparator,use.names=FALSE),collapse=" ")
  terms<-unique(strsplit(tolower(value),"[^[:alpha:]]+")[[1L]])
  terms<-terms[nchar(terms)>=9L]
  if(!length(terms) || !length(pages))return(list())
  pattern<-paste0("\\b(",paste(terms,collapse="|"),")\\b")
  candidates<-lapply(seq_along(pages),function(page) {
    lines<-strsplit(pages[[page]],"\n",fixed=TRUE)[[1L]]
    # A counted procedure row offers stronger group context than prose that
    # may discuss methods, exclusions, or earlier studies.
    hits<-which(grepl(pattern,lines,ignore.case=TRUE) & grepl("[0-9][[:space:]]*%",lines))
    if(!length(hits))return(NULL)
    rows<-head(hits,8L)
    list(page=page,section="TABLE",excerpt=paste(substr(lines[rows],1L,160L),collapse="\n"),count=length(hits))
  })
  candidates<-Filter(Negate(is.null),candidates)
  if(length(candidates))candidates[[which.max(vapply(candidates,`[[`,integer(1),"count"))]]
}

web_qc_primary_comparison_row <- function(row,packet) {
  verified<-function(x)nzchar(web_b2_nr_one(x$.verified_by)) &&
    web_b2_nr_one(x$verification_status) %in% c("Verified","Corrected")
  if(verified(row))for(k in c("outcome_definition","timepoint","intervention_events","intervention_total",
    "comparator_events","comparator_total")) {
    actual<-web_b2_nr_one(row[[paste0("human_verified_",k)]])
    if(nzchar(actual))row[[k]]<-actual
  }
  study<-Filter(function(x)identical(x$screening_id,row$screening_id) &&
    identical(x$.pdf_sha256,row$.pdf_sha256) && verified(x),packet$tables$Target_Study_Verification)
  if(length(study)!=1L)return(row)
  study_fields<-c(analysis_population="human_verified_analysis_population",
    intervention_group="human_verified_intervention_or_exposure",comparator_group="human_verified_comparator")
  for(k in names(study_fields)) {
    actual<-web_b2_nr_one(study[[1L]][[study_fields[[k]]]])
    if(nzchar(actual))row[[k]]<-actual
  }
  row
}

web_qc_primary_source <- function(row,packet,version,read_page=web_qc_pdf_page,read_pages=web_qc_pdf_pages,
                                  comparator=NULL) {
  field<-web_b2_nr_one
  report_id<-field(row$screening_id)
  study<-field(row$study_id)
  report<-Filter(function(r)identical(field(r$screening_id),field(row$screening_id)),
    packet$tables$Author_Final_Review)
  key<-list(study=study,outcome_id=field(row$outcome_row_id),row_id=field(row$.row_id),report=report_id,
    sha256=field(row$.pdf_sha256))
  rows<-packet$tables$Target_Outcome_Verification
  unique_row<-sum(vapply(rows,function(r)identical(field(r$.row_id),key$row_id),logical(1)))==1L
  unavailable<-function(reason)c(key,list(state="PDF_NOT_BOUND",reader_state=reason))
  if(!nzchar(report_id) || length(report)!=1L ||
     (nzchar(field(report[[1L]]$study_id)) && !identical(field(report[[1L]]$study_id),study)))
    return(unavailable("REPORT_NOT_BOUND"))
  sha<-field(row$.pdf_sha256)
  source<-web_qc_bound_pdf(report[[1L]],version,sha)
  if(is.null(source$path))return(unavailable(source$state))
  compared<-web_qc_primary_comparison_row(row,packet)
  location<-field(row$source_location)
  page_pattern<-"(?i)\\b(?:page|p[.])\\s*([1-9][0-9]{0,3})\\b"
  pages<-regmatches(location,gregexpr(page_pattern,location,perl=TRUE))[[1L]]
  proof<-c(key,list(state="PDF_BOUND_LOCATION_PENDING",pdf_verified=TRUE,relative=source$relative,
    scope_verified=unique_row && all(nzchar(unlist(key))) && identical(field(report[[1L]]$study_id),study),
    location=location,excerpt=substr(field(row$source_evidence),1L,350L)))
  path<-source$path
  main<-web_qc_pdf_scan(path,read_pages)
  proof$reader_state<-main$state
  proof$pages_searched<-length(main$pages)
  proof$matches<-web_qc_search_primary_pdf(compared,main$pages)
  proof$comparator_match<-web_qc_pdf_comparator_excerpt(main$pages,comparator)
  labels<-trimws(strsplit(field(row$outcome_name),";",fixed=TRUE)[[1L]])
  if(identical(main$state,"TEXT_EXTRACTED") && length(labels)) {
    scanned<-vapply(labels,function(label) {
      endpoint<-compared
      endpoint$outcome_name<-label
      endpoint$outcome_definition<-NULL
      length(web_qc_search_primary_pdf(endpoint,main$pages))>0L
    },logical(1))
    proof$unlocated_outcomes<-as.list(labels[!scanned])
  }
  withhold_numeric_match<-function(matches)lapply(matches,function(m) {
    m$source_comparison$events_total<-lapply(m$source_comparison$events_total,
      function(x)if(identical(x,"EXACT_CONTEXT_MATCH"))"REVIEW_REQUIRED" else x)
    m
  })
  supplements<-Filter(function(a)nzchar(study) && identical(field(a$study_id),study) &&
    field(a$pdf_status)=="Available" && field(a$screening_id)!=proof$report &&
    tolower(field(a$publication_type)) %in% c("supplement","supplementary material","supplementary appendix"),
    packet$tables$Author_Final_Review)
  proof$registered_supplements<-length(supplements)
  proof$supplements<-lapply(head(supplements,2L),function(a) {
    hash<-field(a$.pdf_sha256)
    binding<-web_qc_bound_pdf(a,version,hash)
    hit<-list(report=field(a$screening_id),study=study,reader_state=binding$state)
    if(is.null(binding$path))return(hit)
    section<-web_qc_pdf_scan(binding$path,read_pages)
    matches<-lapply(web_qc_search_primary_pdf(compared,section$pages),function(m) {
      m$section<-"SUPPLEMENT"
      m
    })
    c(list(report=hit$report,study=study,sha256=hash,pdf_verified=TRUE,reader_state=section$state),
      list(pages_searched=length(section$pages),matches=withhold_numeric_match(matches)))
  })
  if(proof$pages_searched>=60L || proof$registered_supplements>0L)
    proof$matches<-withhold_numeric_match(proof$matches)
  page<-if(length(pages)==1L)as.integer(sub(page_pattern,"\\1",pages[[1L]],perl=TRUE))
  page_text<-if(length(page) && !identical(main$state,"PDF_SIZE_LIMIT")) {
    if(page<=length(main$pages))main$pages[[page]] else tryCatch(read_page(path,page),error=function(e)NULL)
  }
  located<-if(length(page) && page<=length(main$pages))
    Filter(function(m)m$page==page,proof$matches) else
    if(length(page_text))web_qc_search_primary_pdf(compared,list(page_text)) else list()
  if(!length(located)) {
    searched<-identical(main$state,"TEXT_EXTRACTED") || any(vapply(proof$supplements,function(a)
      identical(a$reader_state,"TEXT_EXTRACTED"),logical(1)))
    if(searched) {
      proof$state<-if(length(proof$matches) || any(vapply(proof$supplements,function(a)
        length(a$matches)>0L,logical(1))))"PDF_CANDIDATES_REQUIRE_REVIEW" else "PDF_NO_MATCH_IN_SEARCHED_PAGES"
      return(proof)
    }
    if(!length(page))return(proof)
  }
  proof$state<-"PDF_BOUND_TEXT_UNAVAILABLE"
  if(!length(page_text) || !nzchar(trimws(page_text)))return(proof)
  normalize<-function(x)tolower(gsub("[[:space:]]+"," ",trimws(x)))
  fields<-c("outcome_definition","timepoint","intervention_events","comparator_events",
    "intervention_total","comparator_total","intervention_n","comparator_n")
  proof$comparison<-setNames(lapply(fields,function(k){
    value<-field(row[[k]])
    if(!nzchar(value))return("NOT_EXTRACTED")
    text<-normalize(page_text)
    value<-normalize(value)
    if(grepl("^[0-9]+$",value)) {
      numbers<-regmatches(text,gregexpr("(?<![0-9.])[0-9]+(?![0-9.])",text,perl=TRUE))[[1L]]
      if(!value %in% numbers)return("NOT_LOCATED")
    } else if(!grepl(value,text,fixed=TRUE))return("NOT_LOCATED")
    "LOCATED_REVIEW_REQUIRED"
  }),fields)
  proof$state<-"PDF_TEXT_COMPARED"
  exact<-lapply(located,function(m) {
    m$page<-page
    m
  })
  if(page>length(main$pages))exact<-withhold_numeric_match(exact)
  proof$matches<-head(c(exact,Filter(function(m)m$page!=page,proof$matches)),6L)
  proof
}

web_qc_confirmation_items <- function(report,linked,evidence,configuration) {
  items<-list()
  searches<-new.env(parent=emptyenv())
  read_pages<-function(path) {
    key<-web_c2_hash(path)
    if(!exists(key,searches,inherits=FALSE))
      if(length(ls(searches))<4L)assign(key,web_qc_pdf_pages(path),searches) else
        return(structure(character(),pdf_status="DOCUMENT_SEARCH_LIMIT"))
    get(key,searches,inherits=FALSE)
  }
  put<-function(family,question,proposal,sources,impact) {
    items[[family]]<<-list(family=family,question=question,proposal=proposal,sources=sources,impact=as.list(impact))
  }
  questions<-Filter(function(f)nzchar(web_b2_nr_one(f$question)),linked$findings)
  for(f in questions) {
    if(f$family=="pico") {
      rules<-Filter(function(r)grepl("^C02|COMPARATOR|PICO|ELIGIB",web_b2_nr_one(r$rule_id),ignore.case=TRUE) &&
        r$status %in% c("WARNING","BLOCKED","NOT_ASSESSED"),report$ai$checks)
      put("pico",f$question,"선정 결정을 유지한 상태에서 실제 비교군과 간접성을 원문으로 확인하고 해석 한계를 명시하는 안을 검토해 주세요.",
        lapply(rules,function(r)list(rule=r$rule_id,status=r$status,scope=r$scope,references=r$evidence_refs,
          excerpt=substr(web_b2_nr_one(r$detail %||b2% r$observed),1L,350L))),
        c("protocol","synthesis","manuscript"))
    }
    if(f$family=="rob") {
      # Configured tools are choices by design. Never classify a study from
      # a recommendation or populate an unassessed domain as Low risk.
      put("rob",f$question,"승인된 연구설계에 맞는 도구를 선택하고 원문 근거를 인용해 미완료 영역을 판정해 주세요.",
        list(list(tools=configuration$risk_of_bias,
          reports=as.list(evidence$packet$final_study_selection$study_ids))),
        c("risk_of_bias","certainty","tables","manuscript"))
      selected<-unlist(evidence$packet$final_study_selection$study_ids,use.names=FALSE)
      rows<-Filter(function(r)web_b2_nr_one(r$study_id) %in% selected,evidence$packet$tables$Author_Final_Review)
      items$rob$review_notes<-unlist(lapply(head(rows,4L),function(r) {
        pdf<-web_qc_bound_pdf(r,evidence$version,web_b2_nr_one(r$.pdf_sha256))
        if(is.null(pdf$path))return(NULL)
        scan<-web_qc_pdf_scan(pdf$path,read_pages)
        locations<-lapply(seq_len(min(length(scan$pages),6L)),function(i) {
          lines<-strsplit(scan$pages[[i]],"\n",fixed=TRUE)[[1L]]
          lines<-trimws(unlist(strsplit(lines,"[[:blank:]]{3,}"),use.names=FALSE))
          hits<-grep("retrospective observational|prospective cohort|randomi[sz]ed (controlled|trial)|propensity score",
            lines,value=TRUE,ignore.case=TRUE)
          if(length(hits))list(study=r$study_id,report=r$screening_id,page=i,excerpt=substr(hits[[1L]],1L,220L),sha256=r$.pdf_sha256)
        })
        head(Filter(Negate(is.null),locations),3L)
      }),recursive=FALSE)
    }
    if(f$family=="outcomes")
      put("synthesis",f$question,"서로 다른 결과 정의나 측정 시점을 별도로 보고하는 안을 검토해 주세요.",
        linked$provenance$primary_outcome_inventory,c("synthesis","tables","figures","manuscript"))
  }
  inventory<-linked$provenance$primary_outcome_inventory
  labels<-vapply(Filter(function(a)identical(a$state,"REVIEW_PENDING"),inventory),
    function(a)web_b2_nr_one(a$label),character(1))
  selected<-as.character(unlist(evidence$packet$final_study_selection$study_ids,use.names=FALSE))
  candidates<-Filter(function(r)tolower(web_b2_nr_one(r$outcome_name)) %in% tolower(labels) &&
    web_b2_nr_one(r$study_id) %in% selected,evidence$packet$tables$Target_Outcome_Verification)
  if(length(candidates) && length(candidates)<=12L) {
    sources<-lapply(candidates,web_qc_primary_source,packet=evidence$packet,version=evidence$version,
      read_pages=read_pages,comparator=configuration$research_question$comparator)
    put("primary","일차 결과지표 후보의 정의, 시점, 사건 수와 분석 대상자를 원문에서 확인해 주세요.",
      "확인된 정의와 실제 측정 시점을 기록해 주세요. 원문에 보고되지 않은 값은 추정 없이 미보고로 남겨 주세요.",
      sources,c("evidence","synthesis","tables","figures","manuscript"))
  }
  items
}

web_qc_pending_reporting_approvals <- function(links,profiles,production) {
  needed<-function(k)identical(links[[k]]$stage,"VERIFIED_SOURCE") && !identical(links[[k]]$approval,"APPROVED")
  c(if((production || "NARRATIVE_SWIM" %in% profiles) && needed("prisma"))
      "PRISMA 선정 흐름은 기존 근거와 일치합니다. 최종 선정·제외 기록에 대한 승인이 필요합니다.",
    if(needed("references"))"참고문헌 자료는 검증되었습니다. 원고 검토에서 현재 인용 목록의 적합성을 확인해 주세요.")
}

web_qc_protocol_review_draft <- function(configuration,links,upstream,x,evidence) {
  field<-function(value,source,approved=FALSE)list(
    state=if(!length(value))"MISSING" else if(approved)"VERIFIED" else "DRAFT",source=source,value=value)
  screened<-identical(links$protocol_definition$approval,"APPROVED")
  question<-configuration$research_question
  list(
    question=field(question[intersect(c("framework","question","population","intervention","comparator"),
      names(question))],"Frozen configuration / approved screening definition",screened),
    eligibility=field(configuration$eligibility,"Frozen configuration; full Protocol approval pending"),
    outcomes=field(question$outcomes,"Frozen configuration; preserve original outcome wording",screened),
    planned_search=field(configuration$search,"Frozen configuration; confirm complete search coverage"),
    search_history=field(links$search_execution$provenance,"Version-bound search execution",
      identical(links$search_execution$stage,"VERIFIED_SOURCE") &&
        all(vapply(links$search_execution$provenance,function(a)identical(a$execution_approval,"APPROVED"),logical(1)))),
    synthesis=field(evidence$synthesis_plan$decisions,"Finalized Evidence synthesis plan; actual mode used",
      identical(links$outcomes$approval,"APPROVED")),
    risk_of_bias=field(configuration$risk_of_bias,"Frozen configuration; applicability pending"),
    certainty=field(configuration$certainty_of_evidence,"Frozen configuration; applicability pending"),
    statistical_plan=field(x$source$protocol$analysis %||b2% configuration$analysis,
      "Protocol/statistical plan, including sensitivity analysis"),
    registration_draft=field(upstream$registration_drafts,"Bound registration preparation records"))
}

web_qc_registered_protocol <- function(con,project_id,proof,registration) {
  if(!is.list(registration) || !is.list(proof) || !length(proof$approval_ids) ||
     !identical(toupper(web_b2_nr_one(registration$status)),"REGISTERED"))return(FALSE)
  ids<-trimws(as.character(c(registration$prospero_id,registration$registration_id,registration$url)))
  ids<-ids[!is.na(ids) & nzchar(ids)]
  if(!length(ids))return(FALSE)
  gates<-web_query(con,paste("SELECT id,rationale FROM gates WHERE project_id=?p AND version_id=?v",
    "AND gate_type='protocol_registration' AND status='APPROVED'"),p=project_id,v=proof$approval_version)
  gate_ids<-unlist(proof$approval_ids,use.names=FALSE)
  gates<-gates[gates$id %in% gate_ids,,drop=FALSE]
  setequal(gates$id,gate_ids) && all(vapply(gates$rationale,function(note) {
    lines<-trimws(strsplit(web_b2_nr_one(note),"\n",fixed=TRUE)[[1L]])
    "Registration status: Registered" %in% lines && any(paste0("Registration ID or URL: ",ids) %in% lines)
  },logical(1)))
}

web_qc_decision_package <- function(payload,snapshot,x,evidence,items,production,registration_confirmed=FALSE) {
  links<-snapshot$links
  approved<-function(key)identical(links[[key]]$stage,"VERIFIED_SOURCE") &&
    identical(links[[key]]$approval,"APPROVED")
  present<-function(key)identical(links[[key]]$stage,"VERIFIED_SOURCE")
  status<-function(key) {
    switch(web_b2_nr_one(links[[key]]$approval),APPROVED="승인됨",REJECTED="반려됨",
      DRAFT_ONLY="초안",UNREVIEWED="검토 대기",HUMAN_SELECTION_REQUIRED="인용 선택 검토 대기","미확인")
  }
  has_item<-function(key)any(vapply(items,function(i)identical(i$family,key),logical(1)))
  draft<-web_qc_protocol_review_draft(payload$configuration,links,payload$upstream,x,evidence)
  entry<-function(title,current,decision,blocked,approval,page=NULL,details=NULL)
    list(title=title,current=current,decision=decision,blocked=blocked,approval=approval,page=page,details=details)
  required_protocol<-production || length(x$source$protocol)>0L || present("protocol_definition")
  registration<-x$source$registration
  registration_policy<-x$source$protocol$registration
  registration_required<-length(registration)>0L || present("registration_draft") ||
    (is.list(registration_policy) && isTRUE(registration_policy$required))
  rob_na<-identical(snapshot$adaptive$rob$overall$status,"NOT_APPLICABLE")
  certainty<-x$source$protocol$certainty
  grade_na<-approved("protocol") && is.list(certainty) && identical(certainty$applicable,FALSE) &&
    nzchar(web_b2_nr_one(certainty$rationale))
  grades<-!grade_na && (isTRUE(snapshot$adaptive$certainty_required) ||
    (!approved("protocol") && length(payload$configuration$certainty_of_evidence)>0L))
  outstanding<-Filter(function(a)!identical(a$state,"VERIFIED_ROWS"),snapshot$meta$primary_outcome_inventory)
  selection<-length(unlist(evidence$final_study_selection$study_ids,use.names=FALSE))
  reporting<-production || "NARRATIVE_SWIM" %in% unlist(snapshot$adaptive$profiles,use.names=FALSE)
  open<-list(
    protocol=if(required_protocol && !approved("protocol"))entry("연구계획서",
      if(approved("protocol_definition"))"승인된 선정 기준과 동결 설정을 검토 초안에 반영했습니다." else "승인된 전체 연구계획서가 연결되지 않았습니다.",
      "최초 계획서와 날짜를 확인하고, 변경점과 그 날짜·사유를 구분해 확정해 주세요. 검색 범위와 합성 계획을 포함해 주세요.",
      "R/38 근거 묶음 · 최종 제출",status("protocol"),"Human Review Required",draft),
    registration=if(registration_required && !isTRUE(registration_confirmed))entry("연구 등록",
      if(present("registration_draft"))"등록 준비 기록이 있습니다. 등록 완료 증빙은 확인되지 않았습니다." else "등록 완료 증빙이 확인되지 않았습니다.",
      "등록기관의 실제 등록 상태를 확인하고, 등록이 완료되었다면 등록번호 또는 공식 링크를 제출해 주세요.",
      "최종 제출",if(present("registration_draft"))status("registration_draft") else "미확인",
      "Human Review Required",draft$registration_draft),
    primary=if(length(outstanding) && !has_item("primary"))entry("일차 결과지표",
      "계획서 설정과 최종 결과에서 일치가 확인되지 않은 지표가 있습니다.",
      "미확인 지표를 원문에서 확인하고, 보고되지 않았다면 그 근거를 기록해 주세요.",
      "결과 근거 확정 · 최종 제출","미확인","Screening / Evidence",outstanding),
    rob=if(!rob_na && !approved("rob") && !has_item("rob") && length(snapshot$adaptive$rob$studies))entry(
      "비뚤림 위험","포함 연구에 대한 최종 비뚤림 위험 평가가 연결되지 않았습니다.",
      "연구설계에 맞는 도구를 확인하고 미완료 영역의 원문 근거와 판단을 기록해 주세요.",
      "R/38 근거 묶음 · 최종 제출",status("rob"),"Screening / Evidence",snapshot$adaptive$rob$studies),
    grade=if(grades && !approved("certainty"))entry("근거 확실성 (GRADE)",
      "현재 기록에 근거 확실성 평가 계획이 있으며 최종 평가표는 확인되지 않았습니다.",
      "계획서에 따른 평가 대상을 확정하고, 필요한 지표별 확실성 및 판단 근거를 기록해 주세요.",
      "R/38 근거 묶음 · 최종 제출",status("certainty"),details=list(plan=draft$certainty,groups=evidence$synthesis_plan$decisions)),
    prisma=if(reporting && !approved("prisma"))entry("PRISMA 흐름도",
      if(present("prisma"))sprintf("검증된 흐름도의 포함 연구 수는 %d건으로 최종 선정과 일치합니다.",selection) else
        "최종 선정과 연결된 PRISMA 근거를 아직 검증하지 못했습니다.",
      if(present("prisma"))"최종 흐름과 제외 사유의 보고 내용을 검토해 주세요." else "누락된 흐름 수치와 제외 사유의 원본 근거를 확인해 주세요.",
      "R/38 근거 묶음 · 최종 제출",status("prisma"),details=links$prisma$provenance),
    references=if(present("references") && !approved("references"))entry("원고 인용",
      "기존 서지 확인 결과를 사용할 수 있습니다.",
      "원고 검토에서 아직 승인되지 않은 인용의 선택과 해당 문장에 대한 적합성을 확인해 주세요.",
      "원고 검토 · 최종 제출",status("references"),"Manuscript",links$references$provenance))
  list(open=Filter(Negate(is.null),open),protocol_draft=draft,
    assessment_pages=if(has_item("rob"))"Screening / Evidence")
}

web_qc_reused_decision_version <- function(con,settings,project_id,evidence_version,field,decision) {
  source<-web_b2_nr_one(decision$source_version)
  if(!nzchar(source) || !identical(decision$status,"HUMAN_CONFIRMED"))return(NULL)
  rows<-web_query(con,paste0("WITH RECURSIVE lineage(version_id,parent_version_id) AS (",
    "SELECT version_id,parent_version_id FROM evidence_versions WHERE project_id=?p AND version_id=?e UNION ",
    "SELECT ev.version_id,ev.parent_version_id FROM evidence_versions ev JOIN lineage l ",
    "ON ev.version_id=l.parent_version_id WHERE ev.project_id=?p) ",
    "SELECT pv.id,pv.version,pv.snapshot_locator,pv.manifest_json FROM lineage l ",
    "JOIN project_versions pv ON pv.id=l.version_id AND pv.project_id=?p WHERE l.parent_version_id=?s"),
    p=project_id,e=evidence_version,s=source)
  if(nrow(rows)!=1L)return(NULL)
  relative<-"config/generated/web_evidence.json"
  sha<-web_parse(rows$manifest_json)[[relative]]
  if(!grepl("^[a-f0-9]{64}$",web_b2_nr_one(sha)))return(NULL)
  tryCatch({
    root<-web_inside(file.path(settings$data_root,rows$snapshot_locator),settings$data_root)
    path<-web_inside(file.path(root,relative),root)
    if(!identical(web_sha(path),sha))return(NULL)
    archived<-jsonlite::read_json(path,simplifyVector=FALSE)[[field]]
    if(!is.null(archived) && identical(web_c2_hash(archived),web_c2_hash(decision)))
      list(version_id=rows$id,version=as.integer(rows$version),source_version=source,sha256=sha)
  },error=function(e)NULL)
}

web_qc_confirmation_context <- function(con,settings,actor,project_id,version_id,for_save=FALSE) {
  if(!exists("srma_qc_link_snapshot",mode="function"))
    source(file.path(settings$engine_root,"web/qc/runtime.R"),local=TRUE,encoding="UTF-8")
  m<-web_c2_context(con,settings,actor,project_id,version_id,current=for_save,latest=for_save)
  x<-m$packet
  request<-function(action,args) {
    web_assert(identical(args$project_id,project_id),"Project scope mismatch.")
    f<-switch(action,manuscript_view=web_c2_view,evidence_view=web_b2_view,configuration=web_edit_configuration)
    do.call(f,c(list(con=con,settings=settings,actor=actor),args))
  }
  q<-srma_qc_restore_selected(request,settings,con,project_id,version_id)
  web_assert(is.list(q),"The selected manuscript has no saved Scientific QC.","USER_ACTION_REQUIRED")
  policy<-srma_qc_policy_load(settings$engine_root)
  p<-srma_qc_link_collect(request,con,settings,project_id,version_id,x)
  snap<-srma_qc_link_snapshot(p,x,project_id,version_id)
  linked<-srma_qc_link_build(srma_qc_reconcile(q,x,policy,project_id,version_id,q$number),
    q,x,policy,project_id,version_id,q$number,snap)
  ev<-if(is.list(p$evidence))web_b2_context(con,settings,actor,project_id,x$evidence_version) else NULL
  items<-web_qc_confirmation_items(q,linked,ev,p$configuration)
  production<-length(x$production$qc$publication_9cd)>0L
  registered<-identical(snap$links$registration$approval,"APPROVED")
  decision_package<-web_qc_decision_package(p,snap,x,ev$packet,items,production,registered)
  prisma<-decision_package$open$prisma
  if(!is.null(prisma) && identical(snap$links$prisma$stage,"VERIFIED_SOURCE")) {
    items$prisma<-list(family="prisma",question="PRISMA 흐름과 제외 이유의 보고 내용을 확인해 주세요.",
      proposal=paste(prisma$current,"선정·제외의 보고 내용을 판단 이력에 기록해 주세요."),
      sources=list(c(prisma$details,list(location="PRISMA 흐름도 및 제외 기록",excerpt=prisma$current))),
      impact=list("prisma","manuscript"))
  }
  binding<-linked[c("project_id","manuscript_version_id","evidence_version_id",
    "source_sha256","docx_sha256","policy_sha256","qc_finding_sha256","canonical_snapshot_sha256")]
  scope<-web_c2_hash(binding)
  items<-lapply(items,function(i) {
    key<-i
    key$review_notes<-NULL
    if(identical(i$family,"primary"))key$sources<-lapply(key$sources,function(s) {
      strip<-function(m)m[setdiff(names(m),"source_comparison")]
      if(length(s$matches))s$matches<-lapply(s$matches,strip)
      if(length(s$supplements))s$supplements<-lapply(s$supplements,function(a) {
        if(length(a$matches))a$matches<-lapply(a$matches,strip)
        a
      })
      s
    })
    c(i,list(id=web_c2_hash(list(scope,key))))
  })
  dashboard<-web_qc_review_items(items,decision_package,scope)
  certainty<-x$source$protocol$certainty
  if(identical(snap$links$protocol$approval,"APPROVED") && is.list(certainty) &&
     identical(certainty$required,FALSE) && nzchar(web_b2_nr_one(certainty$rationale)))
    dashboard<-lapply(dashboard,function(i) {
      if(identical(i$family,"grade"))i$allow_na<-TRUE
      i
    })
  dashboard_ids<-vapply(dashboard,function(i)i$id,character(1))
  items<-c(dashboard,Filter(function(i)!i$id %in% dashboard_ids,items))
  reusable<-snap$links[intersect(c("selection","outcomes","protocol","registration","rob","certainty","prisma","references"),names(snap$links))]
  reusable<-Filter(function(a)identical(a$stage,"VERIFIED_SOURCE") && identical(a$approval,"APPROVED"),reusable)
  if(!for_save) {
    fields<-c(selection="final_study_selection",outcomes="human_outcome_harmonization")
    for(k in intersect(names(fields),names(reusable)))
      reusable[[k]]$decision_version<-web_qc_reused_decision_version(con,settings,project_id,
        x$evidence_version,fields[[k]],ev$packet[[fields[[k]]]])
  }
  drafts<-list()
  if(length(p$configuration$risk_of_bias) && !identical(snap$links$rob$approval,"APPROVED"))
    drafts$rob<-list(state="NO_JUDGMENT",configured_tools=p$configuration$risk_of_bias,
      selected_reports=ev$packet$final_study_selection$study_ids,
      study_requirements=snap$adaptive$rob$studies,
      requires=c("approved_study_design","applicable_tool","source_location","domain_judgment"))
  if(length(p$configuration$certainty_of_evidence) && !identical(snap$links$certainty$approval,"APPROVED"))
    drafts$certainty<-list(state="NO_RATING",plan=p$configuration$certainty_of_evidence,
      outcomes=snap$meta$primary_outcome_inventory,
      requires=c("outcome_specific_synthesis","effect_and_uncertainty","risk_of_bias","domain_rationales"))
  list(binding=binding,scope=scope,route=x$source$route,items=unname(items),drafts=drafts,
    dashboard_ids=dashboard_ids,reused=reusable,
    decision_package=decision_package,
    pending_approvals=web_qc_pending_reporting_approvals(snap$links,unlist(snap$adaptive$profiles,use.names=FALSE),
      production))
}

web_qc_confirmation_log <- function(settings,context) {
  root<-settings$data_root
  dir<-web_inside(file.path(root,"qc-confirmation",context$scope),root,exists=FALSE)
  files<-list.files(dir,pattern="^[a-f0-9]{32}[.]json$",full.names=TRUE)
  web_assert(length(files)<=256L,"Review history requires operator attention.")
  events<-lapply(files,function(path){
    path<-web_inside(path,dir)
    web_assert(file.info(path)$size<=256L*1024L,"Invalid review record.")
    envelope<-jsonlite::read_json(path,simplifyVector=FALSE)
    event<-envelope$event
    web_assert(identical(envelope$sha256,web_c2_hash(event)) &&
      identical(event$binding,context$binding) &&
      identical(basename(path),paste0(event$id,".json")),"Review history integrity check failed.")
    event
  })
  events<-events[order(vapply(events,function(x)paste0(x$at,x$id),character(1)))]
  list(dir=dir,events=events)
}

web_qc_confirmation_view <- function(con,settings,actor,project_id,version_id) {
  context<-web_qc_confirmation_context(con,settings,actor,project_id,version_id)
  history<-web_qc_confirmation_log(settings,context)$events
  for(i in seq_along(context$items)) {
    item<-context$items[[i]]
    events<-Filter(function(x)identical(x$item_id,item$id),history)
    context$items[[i]]$latest<-if(length(events))events[[length(events)]]
    if(item$id %in% context$dashboard_ids)
      context$items[[i]]$working_draft<-web_qc_review_draft(settings,context,item$id,actor)
  }
  modifications<-Filter(function(e)identical(e$decision,"MODIFY") && identical(e$family,"primary"),history)
  followup<-lapply(tail(modifications,12L),
    function(e)web_qc_confirmation_track(con,settings,actor,context,e))
  list(items=context$items,history=history,drafts=context$drafts,followup=Filter(Negate(is.null),followup),
    binding=context$binding,
    dashboard_ids=context$dashboard_ids,reused=context$reused,
    decision_package=context$decision_package,
    pending_approvals=context$pending_approvals)
}

web_qc_bound_revision_choices <- function(item) {
  if(!identical(item$family,"primary"))return(list())
  choices<-list()
  for(source in item$sources) {
    if(!nzchar(web_b2_nr_one(source$row_id)))next
    for(m in source$matches) {
      # Only original report pages can be written to this extraction row.
      # Supplementary candidates stay available for the separate source workflow.
      record<-list(row_id=source$row_id,report=source$report,sha256=source$sha256,
        page=m$page,excerpt=trimws(m$excerpt))
      if(!nzchar(record$excerpt))next
      choices[[web_c2_hash(record)]]<-record
    }
  }
  choices
}

web_qc_confirmation_handoff <- function(context,event,draft_version,row_id) {
  route<-web_b2_nr_one(context$route)
  analysis<-switch(route,NARRATIVE="web_b2_execute_synthesis",
    QUANTITATIVE=names(web_c1_operations())[[1L]],"SELECT_APPROVED_SYNTHESIS_ROUTE")
  list(decision_id=event$id,project_id=context$binding$project_id,
    previous_evidence_version=context$binding$evidence_version_id,
    previous_manuscript_version=context$binding$manuscript_version_id,draft_version=draft_version,
    outcome_row=row_id,route=route,
    outcome="INDEPENDENT_VERIFICATION_REQUIRED",
    analysis=list(operation=analysis,state="WAITING_FOR_VERIFIED_EVIDENCE"),
    outputs=list(tables="WAITING_FOR_SYNTHESIS",figures="WAITING_FOR_SYNTHESIS_IF_APPLICABLE",
      manuscript="WAITING_FOR_REVISED_SOURCES"),
    final_qc=list(operation=names(web_c3_operations())[[1L]],state="WAITING_FOR_APPROVED_MANUSCRIPT"),
    r40=list(state="NOT_VERIFIED",requires="APPROVED_EXACT_VERSION_R40_PROOF"))
}

web_qc_confirmation_track <- function(con,settings,actor,context,event) {
  p<-context$binding$project_id
  evidence<-context$binding$evidence_version_id
  marker<-paste("QC confirmation record",event$id)
  choices<-web_qc_bound_revision_choices(list(family=event$family,sources=event$evidence))
  # The existing canonical edit records the exact event ID in the immutable child.
  children<-web_query(con,paste0("SELECT version_id FROM evidence_versions ",
    "WHERE project_id=?p AND parent_version_id=?e AND stage='EXTRACTION' ",
    "AND actor_id=?a ORDER BY created_at DESC LIMIT 12"),p=p,e=evidence,a=event$actor)
  for(draft in children$version_id) {
    ev<-web_b2_context(con,settings,actor,p,draft)
    rows<-Filter(function(r) {
      last<-tail(strsplit(web_b2_nr_one(r$extraction_notes),"\n",fixed=TRUE)[[1L]],1L)
      identical(last,marker) && any(vapply(choices,function(choice)identical(r$.row_id,choice$row_id) &&
        identical(r$screening_id,choice$report) && identical(r$.pdf_sha256,choice$sha256) &&
        identical(r$source_location,paste("PDF page",choice$page)) &&
        identical(r$source_evidence,choice$excerpt),logical(1)))
    },ev$packet$tables$Target_Outcome_Verification)
    if(length(rows)==1L) {
      followup<-web_qc_confirmation_handoff(context,event,draft,rows[[1L]]$.row_id)
      return(web_qc_confirmation_worker_state(con,p,followup))
    }
  }
  NULL
}

web_qc_confirmation_worker_state <- function(con,project_id,followup) {
  # Only descendants of the draft can satisfy its follow-up. Match each job
  # to its typed version and expose worker state separately from approval.
  descendants<-paste0("WITH RECURSIVE lineage(version_id) AS (SELECT version_id FROM evidence_versions ",
    "WHERE project_id=?p AND version_id=?d UNION SELECT e.version_id FROM evidence_versions e ",
    "JOIN lineage l ON e.parent_version_id=l.version_id WHERE e.project_id=?p) ")
  jobs<-web_query(con,paste0(descendants,"SELECT j.version_id,j.operation,j.state,CASE ",
    "WHEN j.state IN ('QUEUED','RUNNING') THEN 'PENDING_EXECUTION' ",
    "WHEN j.state<>'COMPLETE' THEN 'EXECUTION_NOT_COMPLETE' ",
    "WHEN j.operation='run_meta_analysis' AND EXISTS (SELECT 1 FROM analysis_versions a ",
    "WHERE a.project_id=?p AND a.parent_version_id=j.version_id AND a.source_job_id=j.id AND a.stage='RESULT') ",
    "THEN 'REGISTERED_FOR_HUMAN_REVIEW' ",
    "WHEN j.operation='generate_manuscript_docx' AND EXISTS (SELECT 1 FROM manuscript_versions m ",
    "WHERE m.project_id=?p AND m.parent_version_id=j.version_id AND m.source_job_id=j.id AND m.stage='REVIEW_REQUIRED') ",
    "THEN 'REGISTERED_FOR_HUMAN_REVIEW' ELSE 'VALIDATION_PENDING' END AS validation_state, ",
    "CASE WHEN j.state='COMPLETE' AND j.operation='run_meta_analysis' AND EXISTS (SELECT 1 ",
    "FROM analysis_versions a JOIN analysis_versions r ON a.parent_version_id=r.version_id ",
    "WHERE a.project_id=?p AND r.project_id=?p AND a.evidence_version_id=r.evidence_version_id ",
    "AND r.parent_version_id=j.version_id AND r.source_job_id=j.id AND r.stage='RESULT' AND a.stage='APPROVED') ",
    "THEN 'APPROVED' WHEN j.state='COMPLETE' AND j.operation='generate_manuscript_docx' AND EXISTS (SELECT 1 ",
    "FROM manuscript_versions a JOIN manuscript_versions r ON a.parent_version_id=r.version_id ",
    "WHERE a.project_id=?p AND r.project_id=?p AND a.evidence_version_id=r.evidence_version_id ",
    "AND r.parent_version_id=j.version_id AND r.source_job_id=j.id AND r.stage='REVIEW_REQUIRED' AND a.stage='APPROVED') ",
    "THEN 'AUTHOR_APPROVED' ELSE 'PENDING' END AS approval_state FROM jobs j ",
    "WHERE j.project_id=?p AND ((j.operation='run_meta_analysis' AND EXISTS (SELECT 1 ",
    "FROM analysis_versions a JOIN lineage l ON a.evidence_version_id=l.version_id ",
    "WHERE a.project_id=?p AND a.version_id=j.version_id AND a.stage='PLAN')) OR ",
    "(j.operation IN ('run_final_qc','generate_manuscript_docx') AND EXISTS (SELECT 1 FROM manuscript_versions m ",
    "JOIN lineage l ON m.evidence_version_id=l.version_id WHERE m.project_id=?p ",
    "AND m.version_id=j.version_id AND ((j.operation='run_final_qc' AND m.stage='APPROVED') OR ",
    "(j.operation='generate_manuscript_docx' AND m.stage='HUMAN_EDITED'))))) ",
    "ORDER BY j.created_at DESC LIMIT 12"),p=project_id,d=followup$draft_version)
  followup$worker_jobs<-jobs
  followup
}

web_qc_confirmation_apply <- function(con,settings,actor,project_id,version_id,item_id,choice_id,expected_event=NULL) {
  web_assert(is.character(choice_id) && length(choice_id)==1L &&
    grepl("^[a-f0-9]{64}$",choice_id),"Select a current, source-bound PDF excerpt.")
  context<-web_qc_confirmation_context(con,settings,actor,project_id,version_id,for_save=TRUE)
  hits<-Filter(function(i)identical(i$id,item_id),context$items)
  web_assert(length(hits)==1L,"The selected review item changed.","STALE_UPSTREAM_VERSION")
  item<-hits[[1L]]
  web_assert(is.null(expected_event) || !isTRUE(item$review_only),
    "Choose the study-specific correction before editing evidence.")
  web_qc_review_require_source(item,"MODIFY")
  choice<-web_qc_bound_revision_choices(item)[[choice_id]]
  web_assert(is.list(choice),"Select a current, source-bound PDF excerpt.")
  events<-Filter(function(e)identical(e$item_id,item_id),web_qc_confirmation_log(settings,context)$events)
  latest<-if(length(events))events[[length(events)]] else NULL
  web_assert(identical(latest$decision,"MODIFY") && identical(latest$actor,actor),
    "Record your correction before creating a draft Evidence Version.")
  web_assert(is.null(expected_event) || identical(latest$id,expected_event),
    "A newer correction exists. Reload it before creating a draft Evidence Version.","STALE_UPSTREAM_VERSION")
  evidence_version<-context$binding$evidence_version_id
  e<-web_b2_context(con,settings,actor,project_id,evidence_version)
  rows<-Filter(function(r)identical(r$.row_id,choice$row_id) &&
    identical(r$screening_id,choice$report) && identical(r$.pdf_sha256,choice$sha256) &&
    (is.null(item$study_binding) || (identical(r$study_id,item$study_binding$study_id) &&
      any(vapply(item$study_binding$outcomes,function(outcome)identical(r$outcome_row_id,outcome$outcome_id) &&
        identical(r$.row_id,outcome$row_id),logical(1))))),
    e$packet$tables$Target_Outcome_Verification)
  web_assert(length(rows)==1L && identical(e$packet$stage,"EXTRACTION") &&
    !nzchar(web_b2_nr_one(rows[[1L]]$.verified_by)) &&
    !web_b2_nr_one(rows[[1L]]$verification_status) %in% c("Verified","Corrected"),
    "Use the existing source workflow for already verified or unavailable rows.")
  notes<-paste(Filter(nzchar,c(web_b2_nr_one(rows[[1L]]$extraction_notes),
    if(nzchar(web_b2_nr_one(latest$text)))paste("Pending correction:",latest$text),
    paste("QC confirmation record",latest$id))),collapse="\n")
  # The canonical edit creates a new draft, clears row verification, and
  # invalidates downstream source lineage. No quantitative values are edited.
  draft<-web_b2_edit(con,settings,actor,project_id,evidence_version,
    "Target_Outcome_Verification",choice$row_id,list(
      source_location=paste("PDF page",choice$page),source_evidence=choice$excerpt,
      extraction_notes=notes),require_current=TRUE,
    qc_confirmation=list(scope=context$scope,event_id=latest$id,choice_id=choice_id))
  draft$followup<-web_qc_confirmation_handoff(context,latest,draft$version_id,choice$row_id)
  draft
}

web_qc_confirmation_save <- function(con,settings,actor,project_id,version_id,item_id,decision,text="",
                                     context=NULL,review=NULL) {
  web_assert(is.character(decision) && length(decision)==1L &&
    decision %in% c("CONFIRM","MODIFY","PROPOSE_CORRECTION","DEFER","NOT_APPLICABLE"),"Choose a review decision.")
  web_assert(is.character(text) && length(text)==1L && !is.na(text) && nchar(text)<=4000L,
    "Keep the review note within 4000 characters.")
  if(is.null(context))context<-web_qc_confirmation_context(con,settings,actor,project_id,version_id,for_save=TRUE)
  hits<-Filter(function(i)identical(i$id,item_id),context$items)
  web_assert(length(hits)==1L,"The review item or its evidence changed. Reload the current version.","STALE_UPSTREAM_VERSION")
  item<-hits[[1L]]
  web_qc_review_require_source(item,decision)
  web_assert(decision!="NOT_APPLICABLE" || isTRUE(item$allow_na),"This review does not permit an inapplicability proposal.")
  text<-trimws(text)
  web_assert(decision!="MODIFY" || nzchar(text),"Enter the revised judgment.")
  if(decision=="PROPOSE_CORRECTION" && !nzchar(text))text<-item$proposal
  history<-web_qc_confirmation_log(settings,context)
  web_assert(length(history$events)<256L,"Review history requires operator attention.")
  event<-list(id=web_id(),binding=context$binding,item_id=item$id,family=item$family,
    actor=actor,at=web_now(),decision=decision,text=text,proposal=item$proposal,
    evidence=item$sources,evidence_sha256=web_c2_hash(item$sources),affected_outputs=item$impact,
    revalidation=switch(decision,CONFIRM="CONFIRMED_FOR_REVIEW",
      MODIFY="SOURCE_WORKFLOW_REQUIRED",PROPOSE_CORRECTION="PROPOSAL_ONLY",
      DEFER="WAITING_FOR_EVIDENCE",NOT_APPLICABLE="PROPOSAL_ONLY"))
  if(length(item$review_notes))event$source_candidates<-item$review_notes
  if(!is.null(item$study_binding))event$study_binding<-item$study_binding
  if(is.list(review))event$review<-review
  web_qc_review_write(settings,history$dir,event)
  event
}

web_qc_review_write <- function(settings,dir,event) {
  if(!dir.exists(dir))dir.create(dir,recursive=TRUE,showWarnings=FALSE,mode="0700")
  dir<-web_inside(dir,settings$data_root)
  dest<-file.path(dir,paste0(event$id,".json"))
  temp<-tempfile(".qc-review-",tmpdir=dir)
  on.exit(unlink(temp),add=TRUE)
  jsonlite::write_json(list(event=event,sha256=web_c2_hash(event)),temp,auto_unbox=TRUE,null="null")
  web_assert(file.info(temp)$size<=256L*1024L,"The review record exceeds the allowed size.")
  web_assert(!file.exists(dest) && file.rename(temp,dest),"Could not record the review disposition.","SYSTEM_FAILED")
  invisible(event)
}

web_qc_study_finding <- function(finding,source) {
  ids<-unlist(finding$scope$study_ids,use.names=FALSE)
  outcomes<-unlist(finding$scope$outcome_ids,use.names=FALSE)
  if(!nzchar(web_b2_nr_one(source$relative)) || !identical(ids,source$study) ||
    (length(outcomes) && !source$outcome_id %in% outcomes))return(FALSE)
  refs<-lapply(as.character(unlist(finding$references,use.names=FALSE)),function(ref) {
    fields<-trimws(strsplit(ref,";",fixed=TRUE)[[1L]])
    study<-sub("^study_id=","",grep("^study_id=",fields,value=TRUE))
    if(length(study) && !identical(study,source$study))return(FALSE)
    source$relative %in% fields && paste0("sha256=",source$sha256) %in% fields
  })
  any(unlist(refs,use.names=FALSE))
}

web_qc_review_items <- function(items,package,scope) {
  titles<-c(primary="일차 결과지표 정의",pico="연구 및 비교군 적격성",synthesis="결과지표 합성",
    rob="최종 비뚤림 위험 평가",prisma="PRISMA 흐름도 승인",protocol="전체 연구계획서 및 개정 이력",
    registration="등록 상태와 검색 근거",grade="근거 확실성 (GRADE)",references="참고문헌 및 인용 적합성")
  keyed<-setNames(items,vapply(items,function(i)i$family,character(1)))
  for(key in names(package$open)) {
    entry<-package$open[[key]]
    if(is.null(keyed[[key]]))keyed[[key]]<-list(
      family=key,id=web_c2_hash(list(scope,key)),question=entry$decision,proposal=entry$current,
      sources=list(list(provenance=entry$details)),review_only=TRUE)
    keyed[[key]]$current<-entry$current
    keyed[[key]]$canonical_state<-entry$approval
    keyed[[key]]$page<-entry$page
  }
  if(!is.null(keyed$rob))keyed$rob$page<-"Screening / Evidence"
  source<-keyed$primary$sources
  studies<-unique(Filter(nzchar,vapply(source,function(s)web_b2_nr_one(s$study),character(1))))
  studies_items<-lapply(studies,function(study) {
    evidence<-Filter(function(s)identical(s$study,study),source)
    bindings<-lapply(evidence,function(s)s[c("study","outcome_id","row_id","report","sha256")])
    source_ready<-all(vapply(evidence,function(s)isTRUE(s$scope_verified),logical(1))) &&
      !anyDuplicated(vapply(evidence,function(s)web_b2_nr_one(s$outcome_id),character(1))) &&
      !anyDuplicated(vapply(evidence,function(s)web_b2_nr_one(s$row_id),character(1)))
    notes<-if(source_ready)Filter(function(n)any(vapply(evidence,function(s)
      web_qc_study_finding(n,s),logical(1))),keyed$pico$sources)
    list(family="primary",id=web_c2_hash(list(scope,bindings,source_ready)),
      title=paste(gsub("_"," ",study,fixed=TRUE),"· 결과지표와 비교군"),
      question="이 연구에서 실제 보고된 결과의 정의·시점·비교군을 확인하고, 연구 적격성과 해당 결과의 합성 적격성을 따로 판단해 주세요.",
      proposal=if(any(vapply(evidence,function(a)length(a$matches)>0L,logical(1))))
        "아래 원문 후보에서 실제 보고 결과와 비교 방향을 확인해 주세요." else
        "자동 검색 결과만으로 미보고나 사건 0건을 판단할 수 없습니다.",
      sources=if(source_ready)lapply(evidence,function(s) {s$excerpt<-NULL;s}) else list(),
      study_binding=list(study_id=study,outcomes=bindings),source_ready=source_ready,review_notes=notes,
      impact=keyed$primary$impact,page="Screening / Evidence")
  })
  if(length(studies_items)) {
    keyed$pico<-NULL
    keyed$primary$review_only<-TRUE
    keyed$primary$source_ready<-all(vapply(studies_items,function(i)isTRUE(i$source_ready),logical(1)))
    keyed$primary$sources<-unlist(lapply(studies_items,`[[`,"sources"),recursive=FALSE)
  }
  for(key in names(keyed))keyed[[key]]$title<-if(key %in% names(titles))titles[[key]] else keyed[[key]]$question
  ordered<-c("protocol","registration","primary","pico","synthesis","rob","grade","prisma","references")
  c(unname(keyed[intersect(ordered[1:3],names(keyed))]),studies_items,
    unname(keyed[intersect(ordered[-(1:3)],names(keyed))]),unname(keyed[setdiff(names(keyed),ordered)]))
}

web_qc_review_require_source <- function(item,decision) {
  if(!identical(decision,"DEFER"))web_assert(!identical(item$source_ready,FALSE),
    "The study, outcome and PDF source could not be verified. Only deferral is available.","SOURCE_BINDING_UNVERIFIED")
}

web_qc_review_draft_dir <- function(settings,context,item_id,actor) {
  web_inside(file.path(settings$data_root,"qc-review-drafts",web_c2_hash(list(context$scope,actor,item_id))),
    settings$data_root,exists=FALSE)
}

web_qc_review_draft <- function(settings,context,item_id,actor) {
  dir<-web_qc_review_draft_dir(settings,context,item_id,actor)
  paths<-list.files(dir,pattern="^[a-f0-9]{32}[.]json$",full.names=TRUE)
  web_assert(length(paths)<=256L,"Draft history requires operator attention.")
  events<-lapply(paths,function(path) {
    path<-web_inside(path,dir)
    web_assert(file.info(path)$size<=256L*1024L,"Invalid review draft.")
    z<-jsonlite::read_json(path,simplifyVector=FALSE)
    e<-z$event
    web_assert(identical(z$sha256,web_c2_hash(e)) && identical(e$binding,context$binding) &&
      identical(e$actor,actor) && identical(e$item_id,item_id) &&
      identical(basename(path),paste0(e$id,".json")),"Review draft integrity check failed.")
    e
  })
  if(length(events))events[[tail(order(vapply(events,function(e)paste0(e$at,e$id),character(1))),1L)]]
}

web_qc_review_upload <- function(upload,upload_root,project_id) {
  if(is.null(upload))return(NULL)
  web_assert(is.character(upload_root) && length(upload_root)==1L && is.data.frame(upload) && nrow(upload)==1L &&
    all(c("name","datapath","size") %in% names(upload)),"Select one PDF from this session.")
  name<-upload$name[[1L]]
  web_assert(is.character(name) && !is.na(name) && nchar(name)<=180L &&
    grepl("^[^/\\\\:<>?*|\r\n]+[.]pdf$",name,ignore.case=TRUE),"Choose a PDF with a simple filename.")
  path<-web_inside(upload$datapath[[1L]],upload_root)
  size<-file.info(path)$size
  web_assert(is.finite(size) && size>=5 && size<=20*1024^2 && identical(as.numeric(size),as.numeric(upload$size[[1L]])) &&
    identical(readBin(path,"raw",5L),charToRaw("%PDF-")),"The evidence file must be a valid PDF up to 20 MB.")
  sha<-web_sha(path)
  list(path=path,metadata=list(name=name,locator=web_c2_hash(list(project_id,sha)),sha256=sha,size=size))
}

web_qc_review_submit <- function(con,settings,actor,project_id,version_id,item_id,mode,expected_event,
                                 expected_draft,decision="",text="",reason="",comment="",upload=NULL,upload_root=NULL) {
  string<-function(s,max=4000L)is.character(s) && length(s)==1L && !is.na(s) && nchar(s)<=max
  web_assert(string(mode,8L) && mode %in% c("DRAFT","SUBMIT") && string(expected_event,32L) &&
    string(expected_draft,32L) && string(text) && string(reason) && string(comment) && string(decision,24L),
    "Review fields are incomplete or too long.")
  web_assert(decision %in% c("","CONFIRM","MODIFY","PROPOSE_CORRECTION","DEFER","NOT_APPLICABLE"),"Choose a review decision.")
  text<-trimws(text); reason<-trimws(reason); comment<-trimws(comment)
  if(mode=="SUBMIT") {
    web_assert(nzchar(decision),"Choose a review decision before submitting.")
    web_assert(decision!="MODIFY" || nzchar(text),"Enter the revised judgment.")
    web_assert(!decision %in% c("MODIFY","DEFER","NOT_APPLICABLE") || nzchar(reason),"Enter the reason for your decision.")
  }
  web_tx(con,function() {
    web_authorize(con,actor,project_id)
    if(!inherits(con,"SQLiteConnection"))web_query(con,"SELECT id FROM projects WHERE id=?p FOR UPDATE",p=project_id)
    context<-web_qc_confirmation_context(con,settings,actor,project_id,version_id,for_save=TRUE)
    rows<-Filter(function(i)identical(i$id,item_id),context$items)
    web_assert(length(rows)==1L && item_id %in% context$dashboard_ids,
      "The item or its source changed. Open the current review.","STALE_UPSTREAM_VERSION")
    item<-rows[[1L]]
    if(mode=="SUBMIT")web_qc_review_require_source(item,decision)
    web_assert(decision!="NOT_APPLICABLE" || isTRUE(item$allow_na),"This review does not permit an inapplicability proposal.")
    history<-web_qc_confirmation_log(settings,context)
    current<-tail(Filter(function(e)identical(e$item_id,item_id),history$events),1L)
    current<-if(length(current))current[[1L]] else NULL
    draft_dir<-web_qc_review_draft_dir(settings,context,item_id,actor)
    draft<-web_qc_review_draft(settings,context,item_id,actor)
    file<-web_qc_review_upload(upload,upload_root,project_id)
    attachment<-if(is.list(file))file$metadata else
      draft$input$attachment %||b2% if(identical(current$actor,actor))current$review$input$attachment
    input<-list(decision=decision,text=text,reason=reason,comment=comment,attachment=attachment)
    intent<-web_c2_hash(input)
    if(mode=="SUBMIT" && identical(current$actor,actor) && identical(current$review$intent,intent))return(current)
    if(mode=="DRAFT" && identical(draft$intent,intent) && identical(draft$prior_event,expected_event))return(draft)
    web_assert(identical(web_b2_nr_one(current$id),expected_event) &&
      identical(web_b2_nr_one(draft$id),expected_draft),"Another review was saved. Refresh before submitting again.",
      "STALE_UPSTREAM_VERSION")
    if(is.list(file))web_store(settings)$put(file$path,attachment$locator)
    if(is.list(attachment))web_store(settings)$get(attachment$locator,attachment$sha256)
    if(mode=="DRAFT") {
      event<-list(id=web_id(),binding=context$binding,item_id=item_id,actor=actor,at=web_now(),input=input,
        intent=intent,prior_event=expected_event,approval="DRAFT")
      if(!is.null(item$study_binding))event$study_binding<-item$study_binding
      web_qc_review_write(settings,draft_dir,event)
      return(event)
    }
    metadata<-list(input=input,intent=intent,prior_event=expected_event,draft_id=expected_draft,
      before=current$review$input %||b2% list(value=item$current %||b2% item$proposal,source=item$canonical_state),
      approval="SUBMITTED_FOR_REVIEW")
    web_qc_confirmation_save(con,settings,actor,project_id,version_id,item_id,decision,text,context,metadata)
  })
}

web_qc_review_pdf <- function(con,settings,actor,project_id,version_id,report,sha256) {
  manuscript<-web_c2_context(con,settings,actor,project_id,version_id)$packet
  e<-web_b2_context(con,settings,actor,project_id,manuscript$evidence_version)
  selected<-unlist(e$packet$final_study_selection$study_ids,use.names=FALSE)
  rows<-Filter(function(r)identical(r$screening_id,report) &&
    (web_b2_nr_one(r$study_id) %in% selected || (!length(selected) && report %in% unlist(e$packet$included_reports))),
    e$packet$tables$Author_Final_Review)
  web_assert(length(rows)==1L,"The PDF is unavailable for the selected evidence.")
  source<-web_qc_bound_pdf(rows[[1L]],e$version,sha256)
  web_assert(identical(source$state,"HASH_VERIFIED"),"The PDF failed source verification.")
  source$path
}

web_qc_author_review_ready <- function(con,settings,actor,project_id,version_id,x=NULL) {
  if(is.null(x))x<-web_c2_context(con,settings,actor,project_id,version_id,current=TRUE,latest=TRUE)$packet
  if(!identical(x$stage,"REVIEW_REQUIRED") || is.null(x$docx))return(FALSE)
  p<-web_c3_submission(con,settings,actor,project_id,version_id,x)
  checks_required<-length(x$production$qc$publication_9cd)>0L || length(x$source$analyses)>0L ||
    !isFALSE(p$r40_integration$standalone_required)
  !checks_required || isTRUE(p$r40_integration$R40_VALIDATION_COMPLETE)
}
