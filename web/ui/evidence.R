# Pure display summaries for the selected immutable evidence version.
# An AI proposal must never be counted as a human reviewer decision.
web_b2_screening_counts <- function(rows, human_only=FALSE, decision_field="decision") {
 total <- length(rows)
 if(!total)return(c(Total=0L, Human_entries=0L, Include=0L, Exclude=0L,
                    Uncertain=0L, Other=0L, Pending=0L))
 human <- vapply(rows,function(a) {
  v <- a$.actor
  !is.null(v) && length(v)==1L && !is.na(v) && nzchar(trimws(as.character(v)))
 },logical(1))
 decisions <- vapply(rows,function(a) {
  v <- a[[decision_field]]
  if(is.null(v) || !length(v) || is.na(v[[1L]]))return("")
  tolower(trimws(as.character(v[[1L]])))
 },character(1))
 # Do not infer decisions from proposal-review flags or model predictions.
 selected <- if(isTRUE(human_only))human else rep(TRUE,total)
 actual <- decisions[selected]
 inc <- sum(actual=="include")
 exc <- sum(actual=="exclude")
 unc <- sum(actual %in% c("unclear","uncertain","unsure","awaiting full-text clarification",
                           "needs clarification","pending","maybe"))
 other <- sum(nzchar(actual)) - inc - exc - unc
 pending <- if(isTRUE(human_only))sum(!human | !nzchar(decisions)) else sum(!nzchar(actual))
 c(Total=as.integer(total),Human_entries=as.integer(sum(human)),
   Include=as.integer(inc),Exclude=as.integer(exc),Uncertain=as.integer(unc),
   Other=as.integer(other),Pending=as.integer(pending))
}

web_b2_screening_summary_panel <- function(title,subtitle,values,show_human=FALSE) {
 fields <- if(isTRUE(show_human))
  c("Total","Human_entries","Include","Exclude","Uncertain","Pending","Other") else
  c("Total","Include","Exclude","Uncertain","Pending","Other")
 labels <- c(Total="Records",Human_entries="Human reviewed",Include="Include",
             Exclude="Exclude",Uncertain="Uncertain",Pending="Pending",
             Other="Other decisions")
 shiny::div(style="border:1px solid #d1d5db;border-radius:6px;padding:12px 14px;margin:10px 0 14px;",
  shiny::strong(title),
  shiny::p(style="font-size:0.9em;color:#4b5563;margin:4px 0 10px;",subtitle),
  shiny::div(style="display:flex;flex-wrap:wrap;gap:18px;",
   lapply(fields,function(k) shiny::div(
    shiny::div(style="font-size:0.82em;color:#4b5563;",labels[[k]]),
    shiny::strong(style="font-size:1.28em;",format(values[[k]],big.mark=","))
   ))
  )
 )
}


# Version-lineage resolver. The saved immutable evidence packet records each
# source job in history; a completed job belongs to its SOURCE version, not to
# the new evidence version created by accepting it. Do not query by current
# version alone or fabricate job provenance.
web_b2_ui_saved_job_history <- function(packet,jobs) {
  empty<-data.frame(id=character(),operation=character(),source_version=character(),
                    stringsAsFactors=FALSE)
  if(is.null(packet)||!is.list(packet$history)||!length(packet$history)||
     is.null(jobs)||!is.data.frame(jobs)||!nrow(jobs))return(empty)
  required<-c("id","operation","version_id","state")
  if(!all(required %in% names(jobs)))return(empty)
  result<-list()
  for(h in packet$history){
    if(!is.list(h))next
    id<-as.character(h$job_id %||b2% "")
    op<-as.character(h$operation %||b2% "")
    src<-as.character(h$source_version %||b2% "")
    if(length(id)!=1L||length(op)!=1L||length(src)!=1L||
       anyNA(c(id,op,src))||!nzchar(id)||!nzchar(op)||!nzchar(src))next
    hit<-jobs[!is.na(jobs$id)&jobs$id==id &
      !is.na(jobs$operation)&jobs$operation==op &
      !is.na(jobs$version_id)&jobs$version_id==src &
      !is.na(jobs$state)&jobs$state=="COMPLETE",,drop=FALSE]
    if(nrow(hit)!=1L)next
    result[[length(result)+1L]]<-data.frame(id=id,operation=op,
      source_version=src,stringsAsFactors=FALSE)
  }
  if(!length(result))return(empty)
  ans<-do.call(rbind,result)
  ans[!duplicated(ans$id),,drop=FALSE]
}

web_b2_ui_design_summary <- function(packet) {
  if(is.null(packet)||!identical(packet$stage,"SCREENING")||
     !length(packet$tables$AI_Screening))return(NULL)
  d<-tryCatch(web_b2_study_design_validate(packet,required=TRUE),error=function(e)NULL)
  if(is.null(d)||!length(d$classes))return(NULL)
  counts<-table(factor(d$classes,
    levels=c("COMPARATIVE","NON_COMPARATIVE","UNCLEAR")))
  list(total=length(d$classes),counts=counts)
}

# RC28.8 UX — count the actually persisted final Human decisions and explicit
# policy/AI-only exclusions. Never infer a Human Include from AI recommendations.
web_b2_ui_final_screening_counts <- function(packet) {
 if(is.null(packet)||is.null(packet$screening_excel_review)||
    is.null(packet$tables$Reviewer_2))return(NULL)
 rows<-packet$tables$Reviewer_2
 actor<-vapply(rows,function(r){
  a<-as.character(r$.actor %||b2% "")
  length(a)==1L && !is.na(a) && nzchar(trimws(a)) && !identical(a,"AI_AGENT_REVIEWER_1")
 },logical(1))
 decision<-vapply(rows,function(r)as.character(r$decision %||b2% ""),character(1))
 source<-vapply(rows,function(r)as.character(r$.source %||b2% ""),character(1))
 human_include<-sum(actor & decision=="Include")
 human_exclude<-sum(actor & decision=="Exclude")
 policy_only<-sum(!actor & source=="POLICY-ONLY AUTO EXCLUSION" & decision=="Exclude")
 ai_only<-sum(!actor & source=="AI-ONLY AUTO EXCLUSION" & decision=="Exclude")
 pending<-length(rows)-human_include-human_exclude-policy_only-ai_only
 meta<-packet$screening_excel_review
 expected<-c(
  human_include=as.integer(meta$human_include %||b2% -1L),
  human_exclude=as.integer(meta$human_exclude %||b2% -1L),
  ai_only=as.integer(meta$ai_only_exclude_count %||b2% -1L),
  policy_only=as.integer(meta$policy_auto_exclude_count %||b2% -1L))
 actual<-c(human_include=human_include,human_exclude=human_exclude,
           ai_only=ai_only,policy_only=policy_only)
 list(total=length(rows),human_include=human_include,human_exclude=human_exclude,
      ai_only=ai_only,policy_only=policy_only,
      total_include=human_include,total_exclude=human_exclude+policy_only+ai_only,
      human_reviewed=human_include+human_exclude,pending=pending,
      policy=as.character(meta$comparative_option %||b2% "PASS"),
      consistent=isTRUE(all(actual==expected)) && pending==0L)
}

# Display-only context control rules. Invalid/non-selected rows never expose actions.
# All actual writes remain guarded by the existing server-side version/stage/actor checks.
web_b2_ui_review_control_visibility <- function(stage,table,has_row=FALSE,
                                                  has_editable_fields=FALSE,
                                                  has_human_entry=FALSE) {
  valid<-isTRUE(has_row) && is.character(stage) && length(stage)==1L &&
    !is.na(stage) && is.character(table) && length(table)==1L && !is.na(table)
  proposal_stage<-if(valid && startsWith(table,"AI_")) {
    if(table %in% c("AI_Screening","AI_Study_Design"))"SCREENING"
    else if(identical(table,"AI_Eligibility"))"FULLTEXT"
    else if(identical(table,"AI_Risk_of_Bias"))"ROB"
    else "EXTRACTION"
  } else ""
  # Only the current canonical stage may expose a Human save button.
  save_stage<-switch(table,
    Author_Final_Review="FULLTEXT",
    Target_Study_Verification="EXTRACTION",
    Target_Outcome_Verification="EXTRACTION",
    Audit_Checks="EXTRACTION",
    Signalling_Questions="ROB",
    Domain_Review="ROB",
    Overall_Review="ROB",
    "")
  list(
    proposal=valid && nzchar(proposal_stage) && identical(stage,proposal_stage),
    add_outcome=valid && identical(stage,"EXTRACTION") &&
      identical(table,"Target_Outcome_Verification"),
    save=valid && isTRUE(has_editable_fields) && nzchar(save_stage) &&
      identical(stage,save_stage),
    verify=valid && identical(stage,"EXTRACTION") &&
      table %in% c("Target_Study_Verification","Target_Outcome_Verification") &&
      isTRUE(has_human_entry),
    pdf=valid && identical(stage,"FULLTEXT") &&
      identical(table,"Author_Final_Review")
  )
}

# After PDF intake, restore the Full-text section and the same report if it
# still exists in the newly saved immutable packet. Never infer a study ID.
web_b2_ui_pdf_restore_target <- function(stage,tables,row_id) {
 if(!identical(stage,"FULLTEXT") || is.null(tables) ||
    is.null(tables$Author_Final_Review))return(NULL)
 rows<-tables$Author_Final_Review
 ids<-vapply(rows,function(a)as.character(a$.row_id %||b2% ""),character(1))
 requested<-as.character(row_id %||b2% "")
 valid<-length(requested)==1L && !is.na(requested) &&
   nzchar(requested) && requested %in% ids
 list(table="Author_Final_Review",row_id=if(valid)requested else NULL)
}

# Return to the exact editable row after a human revision. Saved Human entries
# remain editable on FULLTEXT/EXTRACTION/ROB until their respective finalization.
# Never infer fields, permissions or a Study ID from an earlier Evidence Version.
web_b2_ui_human_edit_restore_target <- function(stage,tables,table,row_id) {
 stage_map<-c(Author_Final_Review="FULLTEXT",
  Target_Study_Verification="EXTRACTION",Target_Outcome_Verification="EXTRACTION",
  Audit_Checks="EXTRACTION",Signalling_Questions="ROB",Domain_Review="ROB",Overall_Review="ROB")
 t<-as.character(table %||b2% "")
 if(length(t)!=1L||is.na(t)||!nzchar(t)||!t %in% names(stage_map)||
    !identical(stage,unname(stage_map[[t]]))||is.null(tables[[t]]))return(NULL)
 ids<-vapply(tables[[t]],function(a)as.character(a$.row_id %||b2% ""),character(1))
 rid<-as.character(row_id %||b2% "")
 if(length(rid)!=1L||is.na(rid)||!nzchar(rid)||!rid %in% ids)return(NULL)
 list(table=t,row_id=rid)
}

# Restore the ONE newly created outcome row after immutable-version creation.
# Identity comes only from an exact set difference of saved row IDs. Never
# select an outcome by position or infer outcome equivalence from its label.
web_b2_ui_new_outcome_restore_target <- function(stage,tables,previous_ids,
                                                  source_row_id,source_screening_id) {
 if(!identical(stage,"EXTRACTION") || !is.list(tables) ||
    !is.list(tables$Target_Outcome_Verification))return(NULL)
 old<-as.character(previous_ids)
 old_source<-as.character(source_row_id %||b2% "")
 sid<-as.character(source_screening_id %||b2% "")
 if(!length(old)||anyNA(old)||any(!nzchar(old))||anyDuplicated(old) ||
    length(old_source)!=1L||is.na(old_source)||!old_source %in% old ||
    length(sid)!=1L||is.na(sid)||!nzchar(sid))return(NULL)
 rows<-tables$Target_Outcome_Verification
 ids<-vapply(rows,function(a)as.character(a$.row_id %||b2% ""),character(1))
 if(anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids) ||
    length(ids)!=length(old)+1L || !all(old %in% ids))return(NULL)
 added<-setdiff(ids,old)
 if(length(added)!=1L)return(NULL)
 row<-rows[[match(added,ids)]]
 if(!identical(row$screening_id,sid))return(NULL)
 list(table="Target_Outcome_Verification",row_id=added)
}

# RC28.10 — display-only bridge from existing canonical R/25 AI output to
# draft Human study-characteristics fields. This NEVER writes an Evidence
# Version, asserts Human approval, or substitutes for R/25B / R/26.
web_b2_ui_ai_study_draft <- function(packet, human_row, editable_fields) {
  if(is.null(packet) || !identical(packet$stage,"EXTRACTION") ||
     !is.list(human_row) || !is.list(packet$tables) ||
     identical(packet$targeted_verification_source,"CANONICAL_R25B"))return(NULL)
  source_rows<-packet$tables$AI_study_characteristics
  if(is.null(source_rows) || !length(source_rows))return(NULL)
  id<-human_row$screening_id
  if(!is.character(id) || length(id)!=1L || is.na(id) || !nzchar(id))return(NULL)
  matches<-Filter(function(a) {
    is.list(a) && identical(a$screening_id,id) &&
      identical(a$.source,"AI RECOMMENDATION")
  },source_rows)
  # A unique exact screening-ID match is mandatory. Never select first-by-order.
  if(length(matches)!=1L)return(NULL)
  ai<-matches[[1L]]
  a_sid<-as.character(ai$study_id %||b2% "")
  h_sid<-as.character(human_row$study_id %||b2% "")
  if(length(a_sid)!=1L || length(h_sid)!=1L || is.na(a_sid) || is.na(h_sid) ||
     (nzchar(a_sid) && nzchar(h_sid) && !identical(a_sid,h_sid)))return(NULL)
  permitted<-setdiff(unique(as.character(editable_fields)),
    c("source_location","source_evidence","verification_status","verifier",
      "verified_at","study_id","screening_id"))
  permitted<-permitted[!is.na(permitted) & nzchar(permitted)]
  fields<-intersect(permitted,names(ai))
  values<-list()
  for(k in fields){
    v<-ai[[k]]
    if(is.character(v) && length(v)==1L && !is.na(v) &&
       nzchar(trimws(v)) && nchar(v)<=20000L)values[[k]]<-v
  }
  if(!length(values))return(NULL)
  list(screening_id=id,values=values)
}

# RC28.16 — conservative display-only flag, never a clinical endpoint parser.
# A conjunction might describe a valid composite endpoint; only the researcher
# can determine whether a numerical result applies to that exact endpoint.
web_b2_ui_outcome_label_needs_disambiguation <- function(label) {
  if(!is.character(label)||length(label)!=1L||is.na(label)||!nzchar(trimws(label)))
    return(FALSE)
  segments<-strsplit(label,"\\s*;\\s*",perl=TRUE)[[1L]]
  numbered<-gregexpr("(?:^|;\\s*)[0-9]+\\s*[.)]",label,perl=TRUE)[[1L]]
  length(segments)>=2L || sum(numbered>=0L)>=2L ||
    grepl("\\b(and|or)\\b|[&]",label,perl=TRUE,ignore.case=TRUE)
}

# Topic-independent, read-only candidate selection from a SAVED R/25 result.
# No clinical synonym inference or row-order matching. A different/compound
# label requires human selection of EACH individual field after PDF inspection.
# In R/25B mode, original Agent values are read-only; proposals may populate
# matching human_verified_* draft fields, never the protected Agent columns.
web_b2_ui_ai_outcome_drafts <- function(packet,human_row,editable_fields) {
  if(is.null(packet)||!identical(packet$stage,"EXTRACTION")||
     !is.list(human_row)||!is.list(packet$tables))return(NULL)
  sid<-as.character(human_row$screening_id %||b2% "")
  outcome<-as.character(human_row$outcome_name %||b2% "")
  if(length(sid)!=1L||is.na(sid)||!nzchar(trimws(sid))||
     length(outcome)!=1L||is.na(outcome)||!nzchar(trimws(outcome)))return(NULL)
  source_rows<-packet$tables$AI_outcome_data
  if(!is.list(source_rows)||!length(source_rows))return(NULL)
  matches<-Filter(function(a) {
    is.list(a)&&identical(a$screening_id,sid)&&
      identical(a$.source,"AI RECOMMENDATION")
  },source_rows)
  hs<-as.character(human_row$study_id %||b2% "")
  matches<-Filter(function(a) {
    asid<-as.character(a$study_id %||b2% "")
    length(asid)==1L&&!is.na(asid)&&
      (length(hs)!=1L||is.na(hs)||!nzchar(hs)||!nzchar(asid)||identical(asid,hs))
  },matches)
  if(!length(matches))return(NULL)
  ids<-vapply(matches,function(a)as.character(a$.row_id %||b2% ""),character(1))
  if(anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids))return(NULL)

  protected<-c("screening_id","study_id","outcome_row_id","outcome_name",
    "verification_status","verifier","verified_at","value_status",
    "analysis_group","meta_analysis_role","parent_analysis_group",
    "human_include_in_planned_analysis","raw_value","transformation_rule",
    "corrected_value_notes")
  permitted<-setdiff(unique(as.character(editable_fields)),protected)
  permitted<-permitted[!is.na(permitted)&nzchar(permitted)&
    !startsWith(permitted,".")]
  # Never copy human judgments from the AI source, even when names happen to match.
  mapping<-function(source_key) {
    if(source_key %in% protected||startsWith(source_key,".")||
       startsWith(source_key,"human_")||startsWith(source_key,"final_"))return("")
    if(source_key %in% permitted)return(source_key)
    h<-paste0("human_verified_",source_key)
    if(h %in% permitted)return(h)
    ""
  }
  candidates<-lapply(seq_along(matches),function(i) {
    a<-matches[[i]];values<-list()
    for(k in names(a)) {
      target<-mapping(k)
      if(!nzchar(target))next
      v<-a[[k]]
      if(is.character(v)&&length(v)==1L&&!is.na(v)&&
         nzchar(trimws(v))&&nchar(v)<=20000L)values[[target]]<-v
    }
    label<-as.character(a$outcome_name %||b2% "")
    if(length(label)!=1L||is.na(label))label<-""
    exact<-identical(label,outcome)
    manual<-!exact||web_b2_ui_outcome_label_needs_disambiguation(label)||
      web_b2_ui_outcome_label_needs_disambiguation(outcome)
    short<-function(k) {
      v<-as.character(a[[k]] %||b2% "")
      if(length(v)!=1L||is.na(v)||!nzchar(trimws(v)))"not specified" else
        substr(gsub("[\\r\\n]+"," ",v),1L,70L)
    }
    display<-paste0(if(exact)"Exact label" else "Different label / manual mapping",
      " | ",if(nzchar(label))substr(label,1L,95L) else "(no AI outcome name)",
      " | ",short("timepoint")," | ",short("data_type"),
      " | source ",substr(ids[[i]],1L,10L),
      " | ",length(values)," proposal fields")
    list(id=ids[[i]],label=display,source_outcome_name=label,
      exact_label=exact,requires_manual=manual,values=values)
  })
  names(candidates)<-ids
  exact_first<-order(!vapply(candidates,function(z)isTRUE(z$exact_label),logical(1)))
  candidates<-candidates[exact_first]
  # Every candidate-specific field is explicitly cleared before each load;
  # values for unselected or empty fields cannot leak between outcomes.
  clear_fields<-unique(unlist(lapply(candidates,function(z)names(z$values)),use.names=FALSE))
  list(screening_id=sid,outcome_name=outcome,candidates=candidates,
       clear_fields=clear_fields)
}

# Researcher-facing read-only explanation when a draft-import selection is hidden.
# Never infer clinical outcome equivalence; mismatched labels cannot be silently
# treated as the same outcome. No version writes or remote calls.
web_b2_ui_outcome_visibility <- function(packet,human_row) {
 if(is.null(packet)||!identical(packet$stage,"EXTRACTION")||
    !is.list(human_row)||!is.list(packet$tables))return(NULL)
 sid<-as.character(human_row$screening_id %||b2% "")
 outcome<-as.character(human_row$outcome_name %||b2% "")
 if(length(sid)!=1L||is.na(sid)||!nzchar(sid))return(NULL)
 rows<-packet$tables$AI_outcome_data %||b2% list()
 if(!is.list(rows)||!length(rows))return(list(
   message="This saved version has no AI outcome rows. Check the AI-extracted outcomes review section or the saved extraction artifacts.",labels=character()))
 study_rows<-Filter(function(a)is.list(a)&&identical(a$screening_id,sid),rows)
 if(!length(study_rows))return(list(
   message="There are no saved AI outcome rows for this report. The study characteristics may have been extracted even if outcome values were not.",labels=character()))
 values<-unique(vapply(study_rows,function(a){
   v<-as.character(a$outcome_name %||b2% "")
   if(length(v)!=1L||is.na(v)||!nzchar(trimws(v)))"(no outcome label)" else v
 },character(1)))
 exact<-Filter(function(a)identical(a$outcome_name,outcome),study_rows)
 if(length(exact))return(list(
   message="Saved AI rows exist for this outcome, but their record identity or provenance did not pass the safe import checks. Review the source extraction before importing.",labels=values))
 list(message="AI outcome rows exist for this report, but the outcome names differ from this Human review row. For safety, values were not copied into a different outcome. Compare the labels below in the AI-extracted outcomes review section.",labels=values)
}

# Human review UI; labels describe decisions, never engine script numbers.
web_b2_ui <- function() shiny::tabPanel("Screening / Evidence",
 shiny::tags$style(shiny::HTML("
/* RC28.8: universal, accessible Human Reviewer 2 screening Include accordion */
.srma-includes-accordion { margin:14px 0 16px; border:1px solid #b6cde5; border-radius:12px; background:#fff; overflow:hidden; }
.srma-includes-accordion[open] { border-color:#2876bb; box-shadow:0 2px 12px rgba(31,86,137,.09); }
.srma-includes-accordion > summary { cursor:pointer; display:flex; flex-wrap:wrap; align-items:center; gap:10px; padding:15px 17px; min-height:58px; background:#eaf4ff; color:#15395c; list-style:none; font-weight:650; line-height:1.4; transition:background-color .15s ease; }
.srma-includes-accordion > summary::-webkit-details-marker { display:none; }
.srma-includes-accordion > summary::before { content:'▶'; color:#2167a3; font-size:.8em; }
.srma-includes-accordion[open] > summary::before { content:'▼'; }
.srma-includes-accordion > summary:hover { background:#d9ebfc; }
.srma-includes-accordion > summary:focus-visible { outline:3px solid #155fa9; outline-offset:-3px; border-radius:8px; }
.srma-includes-count { display:inline-block; padding:3px 10px; border-radius:999px; color:#fff; background:#205f96; white-space:nowrap; font-size:.84em; font-weight:700; }
.srma-includes-label { font-weight:700; }
.srma-includes-hint { flex-basis:100%; padding-left:20px; color:#42627d; font-size:.91em; font-weight:400; }
.srma-includes-list { padding:8px 20px 0 44px; margin:8px 0; max-height:380px; overflow-y:auto; overflow-wrap:anywhere; }
.srma-includes-list > li { padding:7px 4px; line-height:1.55; border-bottom:1px solid #e8eef5; }
.srma-includes-list > li:last-child { border-bottom:none; }
.srma-includes-id { font-family:ui-monospace,SFMono-Regular,Consolas,monospace; font-weight:700; white-space:nowrap; color:#15568c; }
.srma-includes-accordion > p { padding:0 17px; margin:8px 0 14px; color:#455d74; line-height:1.5; }
@media (max-width:640px) { .srma-includes-accordion > summary { padding:13px; } .srma-includes-list { padding-left:28px; } }
\n        .srma-action-clicked { outline:2px solid #93c5fd!important;outline-offset:2px; }\n        .srma-action-clicked::before { content:'◌ ';font-weight:800; }\n        .srma-action-ack { display:inline-block;margin-left:8px;font-size:.86em;\n          color:#315f7d;vertical-align:middle;line-height:1.3; }\n        @media (prefers-reduced-motion: no-preference) {\n          .srma-action-clicked::before { display:inline-block;animation:srma-turn 1.1s linear infinite; }\n          @keyframes srma-turn { from{transform:rotate(0)} to{transform:rotate(360deg)} }\n        }\n      ")),
 shiny::tags$script(shiny::HTML("        (function(){\n          if (window.__srmaUniversalClickFeedback) return;\n          window.__srmaUniversalClickFeedback = true;\n          document.addEventListener('click', function(event) {\n            var btn = event.target && event.target.closest &&\n              event.target.closest('button, input[type=submit], a.shiny-download-link');\n            if (!btn || btn.disabled || btn.getAttribute('aria-disabled') === 'true') return;\n            var id = btn.id || '';\n            var text = (btn.innerText || btn.value || '').trim();\n            var kind = /save|approve|record|confirm|import|upload|verify/i.test(text) ?\n              'Save clicked — checking…' :\n              (/download/i.test(text) ? 'Download clicked — preparing…' : 'Click received — processing…');\n            var parent = btn.parentNode;\n            if (!parent) return;\n            btn.classList.add('srma-action-clicked');\n            var feedback = document.getElementById(id ? 'srma-ack-' + id : '');\n            if (!feedback || !id) {\n              feedback = document.createElement('span');\n              feedback.className = 'srma-action-ack';\n              feedback.setAttribute('role','status');\n              feedback.setAttribute('aria-live','polite');\n              if(id) feedback.id='srma-ack-'+id;\n              btn.insertAdjacentElement('afterend',feedback);\n            }\n            feedback.textContent = kind;\n            var serial = String(Date.now())+'-'+Math.random();\n            feedback.setAttribute('data-serial',serial);\n            window.setTimeout(function(){\n              if(feedback && feedback.getAttribute('data-serial')===serial){\n                if(feedback.parentNode) feedback.parentNode.removeChild(feedback);\n                btn.classList.remove('srma-action-clicked');\n              }\n            },4000);\n          },true);\n        })();\n      \n
Shiny.addCustomMessageHandler('srmaEvidenceRunButton', function(x) {
  var el = document.getElementById('evidence_run');
  if (!el) return;
  el.disabled = !!x.disabled;
  el.innerText = x.label || 'Queue processing';
});
Shiny.addCustomMessageHandler('srmaEvidenceSaveButton', function(x) {
  var el = document.getElementById('evidence_accept');
  if (!el) return;
  el.disabled = !!x.disabled;
  el.innerText = x.label || 'Save result as new evidence version';
});
Shiny.addCustomMessageHandler('srmaFinalStudyButton', function(x) {
  var el = document.getElementById('evidence_final_study_save');
  if (!el) return;
  el.disabled = !!x.disabled;
  el.innerText = x.label || 'Save study selection';
});
Shiny.addCustomMessageHandler('srmaPdfDiscoveryButton', function(x) {
  var el = document.getElementById('evidence_pdf_discover_run');
  if (!el) return;
  el.disabled = !!x.disabled;
  el.innerText = x.label || 'Find results in PDFs';
});
Shiny.addCustomMessageHandler('srmaSynthesisPlanButton', function(x) {
  var el=document.getElementById('evidence_synthesis_save');
  if(!el) return;
  el.disabled=!!x.disabled;
  el.innerText=x.label||'Save synthesis plan';
});
Shiny.addCustomMessageHandler('srmaTargetOutcomeSaveButton', function(x) {
  var el = document.getElementById(x.id);
  if (!el) return;
  el.disabled = !!x.disabled;
  el.innerText = x.label || (x.id === 'evidence_inventory_save' ?
    'Save selected outcomes' : 'Save common outcomes');
});
")),
 shiny::p("Review the selected saved version. Each edit creates a new version; approval is never copied."),
 shiny::actionButton("evidence_start","Start screening from checked database inputs"),
 shiny::actionButton("evidence_refresh","Load selected evidence version"),shiny::textOutput("evidence_stage"),
 shiny::uiOutput("evidence_gate_ui"),
 shiny::tableOutput("evidence_progress"),
 shiny::uiOutput("evidence_screening_summary"),
 shiny::uiOutput("evidence_final_import_summary"),
 shiny::uiOutput("evidence_design_workflow"),
 shiny::selectInput("evidence_operation","Next processing step",character()),shiny::actionButton("evidence_run","Queue processing",onclick="this.disabled=true;this.innerText='Submitting…';"),
 shiny::uiOutput("evidence_job_status"),
 shiny::selectInput("evidence_job","Completed processing result",character()),shiny::actionButton("evidence_accept","Save result as new evidence version",onclick="this.disabled=true;this.innerText='Saving…';"),shiny::uiOutput("evidence_saved_job_note"),
 shiny::uiOutput("evidence_selective_excel_ui"),
 shiny::p("Before finalizing a stage, explicitly approve its pending gate under Human Review Required. Automated outputs and AI recommendations are not human approvals."),
 shiny::uiOutput("evidence_final_studies_ui"),
 shiny::uiOutput("evidence_study_outcome_matrix_ui"),
 shiny::uiOutput("evidence_study_to_analysis_ui"),
 shiny::tags$details(style="margin:15px 0;border:1px solid #dbe3ec;border-radius:8px;padding:12px;",
   shiny::tags$summary(style="cursor:pointer;font-size:1.18em;font-weight:600;",
     "Step 5 — Human Numeric Verification (required for quantitative analysis)"),
 shiny::p("Open this section only when verifying source numbers or correcting an extraction. The R/26 independent Human gate remains mandatory for quantitative analyses."),
 shiny::selectInput("evidence_table","Review section",character()),
 shiny::uiOutput("evidence_reviewer_summary"),shiny::textInput("evidence_filter","Find title, ID or review status"),
 shiny::selectInput("evidence_queue","Review queue",c("All records"="all","AI Include / Unclear priority"="ai_priority","Needs Human entry"="pending","Unclear / needs attention"="uncertain")),
 shiny::selectInput("evidence_row","Record / result",character()),shiny::uiOutput("evidence_context"),shiny::tableOutput("evidence_details"),shiny::uiOutput("evidence_ai_study_draft_ui"),shiny::uiOutput("evidence_ai_outcome_draft_ui"),shiny::uiOutput("evidence_fields"),
 shiny::uiOutput("evidence_proposal_controls"),
 shiny::uiOutput("evidence_add_outcome_control"),
 shiny::uiOutput("evidence_save_control"),
 shiny::uiOutput("evidence_verify_controls"),
 shiny::uiOutput("evidence_pdf_controls")))
web_b2_server <- function(input,output,session,request,safe,notice,refresh) {
 packet<-shiny::reactiveVal(NULL)
 output$evidence_study_to_analysis_ui <- shiny::renderUI({
   x <- packet()
   if(is.null(x) || !identical(x$synthesis_plan$status,"HUMAN_CONFIRMED"))return(NULL)
   st <- srma_ui_analysis_status(x)
   if(isTRUE(st$ready))
     shiny::tags$div(style="margin:15px 0;padding:16px;border:1px solid #bfdbfe;border-radius:9px;background:#f8fbff;",
       shiny::tags$strong("Studies complete — continue to Analysis"),
       shiny::tags$p(style="margin:7px 0;color:#475569;",st$message),
       shiny::actionButton("evidence_go_analysis","Go to Analysis",class="btn-primary"))
   else shiny::tags$div(style="margin:14px 0;color:#9a3412;",
     shiny::tags$strong("Analysis handoff not yet ready: "),st$message)
 })
 pending_import_load<-shiny::reactiveVal(NULL)
 # UI feedback is session-local and not an Evidence Version decision.
 final_study_feedback<-shiny::reactiveVal(NULL)
 pdf_discovery_feedback<-shiny::reactiveVal(NULL)
 inventory_feedback<-shiny::reactiveVal(NULL)
 harmonization_feedback<-shiny::reactiveVal(NULL)
 synthesis_feedback<-shiny::reactiveVal(NULL)
 synthesis_execution_feedback<-shiny::reactiveVal(NULL)
 # Inline status plus toast: never make a save failure invisible off-screen.
 target_save_feedback<-function(which,kind,message,version_id=NULL,project_id=NULL) {
   current_version<-as.character(version_id %||b2% input$version %||b2% "")
   current_project<-as.character(project_id %||b2% input$project %||b2% "")
   value<-list(kind=kind,text=message,version_id=current_version,project_id=current_project)
   if(identical(which,"inventory")) inventory_feedback(value) else harmonization_feedback(value)
   notice(message)
   shiny::showNotification(message,
     type=if(identical(kind,"error"))"error" else "message",duration=7)
 }
 target_save_button<-function(id,label,disabled=FALSE) {
   session$sendCustomMessage("srmaTargetOutcomeSaveButton",
     list(id=id,label=label,disabled=isTRUE(disabled)))
 }
 set_run_button<-function(disabled=FALSE,label="Queue processing") {
  session$sendCustomMessage("srmaEvidenceRunButton",list(disabled=isTRUE(disabled),label=label))
 }
 set_save_button<-function(disabled=TRUE,label="Save result as new evidence version") {
  session$sendCustomMessage("srmaEvidenceSaveButton",list(disabled=isTRUE(disabled),label=label))
 }
 labels<-c(generate_scope_advisory="Optional: AI research scope and analysis strategy advice (AI cost)",prepare_targeted_verification="Prepare focused verification of studies and outcomes (no AI cost)",recommend_screening="AI title/abstract screening",recommend_study_design="Classify included studies by design (AI)",recommend_eligibility="AI full-text eligibility review",recommend_extraction="Extract study and outcome data from full-text PDFs (AI)",discover_pdf_results="Find PDF-reported outcomes (AI)",recommend_risk_of_bias="AI risk-of-bias assessment proposals",prepare_screening="Prepare records for screening",finalize_screening="Finalize title/abstract screening",register_fulltext="Register full-text PDFs",finalize_included_studies="Finalize eligible studies and Study IDs",prepare_extraction="Prepare human extraction forms",finalize_extraction="Finalize human-verified extraction data",prepare_risk_of_bias="Prepare risk-of-bias review",finalize_evidence="Finalize approved evidence package")
 # Resolve the same canonical data root used by the background Worker.
 # The Shiny server may not have SRMA_WEB_DATA set, and the authorized
 # project view need not expose internal diagnostic_locator paths.
 screening_data_root <- tryCatch(as.character(web_settings()$data_root),
                                  error=function(e) character())
 design_checkpoint_progress<-function(j) {
  if(is.null(j)||!nrow(j)||!"id" %in% names(j))return(NULL)
  job_id<-as.character(j$id[[1L]])
  if(length(job_id)!=1L||is.na(job_id)||!grepl("^[0-9a-fA-F]{32}$",job_id))return(NULL)
  root<-screening_data_root
  if(length(root)!=1L||is.na(root)||!nzchar(root)||!dir.exists(root))return(NULL)
  job_dir<-file.path(root,"jobs",job_id)
  if(!dir.exists(job_dir))return(NULL)
  attempts<-list.files(job_dir,full.names=TRUE,recursive=FALSE)
  attempts<-attempts[dir.exists(attempts)&grepl("^[0-9a-fA-F]{32}$",basename(attempts))]
  paths<-unlist(lapply(attempts,function(a) {
    dir<-file.path(a,"project","results","logs")
    if(!dir.exists(dir))return(character())
    list.files(dir,pattern="^rc286_design_checkpoint_[0-9]{5}[.]rds$",full.names=TRUE)
  }),use.names=FALSE)
  if(!length(paths))return(NULL)
  for(f in paths[order(file.info(paths)$mtime,decreasing=TRUE,na.last=TRUE)]){
    cp<-tryCatch(readRDS(f),error=function(e)NULL)
    if(!is.list(cp)||!is.list(cp$rows)||!is.character(cp$include_ids))next
    total<-length(cp$include_ids);done<-length(cp$rows)
    if(done>total||total<1L)next
    ids<-vapply(cp$rows,function(r)as.character(r$screening_id %||b2% ""),character(1))
    if(!identical(ids,head(cp$include_ids,done)))next
    return(list(completed=done,total=total,pct=round(100*done/total,1),
      mtime=file.info(f)$mtime[[1L]]))
  }
  NULL
 }
 screening_checkpoint_progress<-function(j){
  if(is.null(j)||!nrow(j)||!"id" %in% names(j))return(NULL)
  job_id<-as.character(j$id[[1L]])
  if(length(job_id)!=1L||is.na(job_id)||!grepl("^[0-9a-fA-F]{32}$",job_id))return(NULL)
  root<-screening_data_root
  if(length(root)!=1L||is.na(root)||!nzchar(root)||!dir.exists(root))return(NULL)
  # Only inspect attempts under this authorized Job ID; never trust a path from the browser.
  job_dir<-file.path(root,"jobs",job_id)
  if(!dir.exists(job_dir))return(NULL)
  attempts<-list.files(job_dir,full.names=TRUE,recursive=FALSE)
  attempts<-attempts[dir.exists(attempts)&grepl("^[0-9a-fA-F]{32}$",basename(attempts))]
  if(!length(attempts))return(NULL)
  files<-file.path(attempts,"project","results","logs","agent_screening_checkpoint.csv")
  files<-files[file.exists(files)]
  if(!length(files))return(NULL)
  files<-files[order(file.info(files)$mtime,decreasing=TRUE,na.last=TRUE)]
  for(f in files){
   d<-tryCatch(utils::read.csv(f,stringsAsFactors=FALSE,check.names=FALSE),
               error=function(e)NULL)
   if(is.null(d))next
   raw_rows<-nrow(d)
   has_validation<-all(c("screening_id","validation_status") %in% names(d))
   if(has_validation){
    # Match the Worker's resume-count contract exactly: valid unique screening IDs.
    ok<-!is.na(d$screening_id)&nzchar(trimws(as.character(d$screening_id)))&
        as.character(d$validation_status)=="valid"
    ok[is.na(ok)]<-FALSE
    valid_rows<-sum(ok)
    completed<-length(unique(as.character(d$screening_id[ok])))
    nonvalid_rows<-raw_rows-valid_rows
    duplicate_valid_rows<-valid_rows-completed
   }else{
    # Legacy CSV lacking validation metadata: rows are NOT verified decisions.
    completed<-raw_rows
    valid_rows<-NA_integer_
    nonvalid_rows<-NA_integer_
    duplicate_valid_rows<-NA_integer_
   }
   x<-packet()
   total<-if(!is.null(x)&&!is.null(x$tables$Reviewer_1))
     length(x$tables$Reviewer_1) else NA_integer_
   pct<-if(is.finite(total)&&total>0)round(100*completed/total,1) else NA_real_
   mtime<-tryCatch(file.info(f)$mtime[[1L]],error=function(e)NA)
   return(list(completed=completed,total=total,pct=pct,mtime=mtime,
               raw_rows=raw_rows,has_validation=has_validation,
               valid_rows=valid_rows,nonvalid_rows=nonvalid_rows,
               duplicate_valid_rows=duplicate_valid_rows))
  }
  NULL
 }
 # Keep raw saved rows separate from the valid-unique count used for resume.
 screening_checkpoint_note<-function(cp){
  if(is.null(cp))return(NULL)
  if(!isTRUE(cp$has_validation))return(shiny::p(
   style="font-size:0.9em;margin-top:4px;",
   paste0("Checkpoint: ",cp$raw_rows,
          " saved rows (validation metadata unavailable; counts are not verified).")))
  parts<-c(paste0("Checkpoint CSV: ",cp$raw_rows," saved rows"),
           paste0(cp$completed," unique valid decisions"))
  if(cp$nonvalid_rows>0L)parts<-c(parts,paste0(cp$nonvalid_rows," non-valid rows"))
  if(cp$duplicate_valid_rows>0L)parts<-c(parts,
    paste0(cp$duplicate_valid_rows," duplicate valid-ID rows"))
  shiny::p(style="font-size:0.9em;margin-top:4px;",
           paste(parts,collapse=" · "),
           ". Only unique valid decisions count toward completion.")
 }
 design_summary<-shiny::reactive({web_b2_ui_design_summary(packet())})
 # One cached authorized project view per refresh cycle; avoid multiple
 # SQLite read connections for every status/list/button reactive.
 job_view<-shiny::reactive({
  shiny::req(input$project)
  shiny::invalidateLater(2000,session)
  safe(request("view",list(project_id=input$project)))
 })
 saved_history<-shiny::reactive({
  x<-packet()
  if(is.null(x)||is.null(input$project))return(data.frame(id=character(),operation=character(),source_version=character()))
  view<-job_view()
  if(is.null(view))return(data.frame(id=character(),operation=character(),source_version=character()))
  web_b2_ui_saved_job_history(x,view$jobs)
 })
 names_table<-c(Verification_Plan="Focused verification plan",Included_Not_Targeted="Included studies not selected for focused verification",Audit_Checks="Additional study and cohort audit checks",Advisory_Meta_Analysis_Sets="Recommended meta-analysis study sets",Advisory_Outcome_Harmonization="Suggested outcome definitions and time windows",Advisory_Study_Roles="Recommended study roles",AI_study_characteristics="AI-extracted study characteristics (read-only)",AI_group_characteristics="AI-extracted treatment group characteristics (read-only)",AI_outcome_data="AI-extracted outcomes and effect data (read-only)",Reviewer_1="AI Reviewer 1 (read-only)",AI_Study_Design="AI study-design triage (Include only)",Reviewer_2="Human Reviewer 2",Author_Final_Review="Full-text eligibility and Study IDs",Target_Study_Verification="Study characteristics — human review",Target_Outcome_Verification="Outcome values — human review",Signalling_Questions="Bias assessment questions",Domain_Review="Bias assessment by domain",Overall_Review="Overall bias assessment")
 output$evidence_gate_ui<-shiny::renderUI({
  refresh()
  shiny::req(input$project,input$version)
  x<-safe(request("view",list(project_id=input$project)))
  if(is.null(x)||is.null(x$gates)||!nrow(x$gates))return(NULL)
  g<-x$gates[
    x$gates$version_id==input$version &
    x$gates$status=="PENDING" &
    grepl("^b2_",x$gates$gate_type),
    ,drop=FALSE
  ]
  if(!nrow(g))return(
    shiny::div(
      style="border-left:4px solid #15803d;background:#f0fdf4;padding:10px 14px;margin:10px 0 14px;border-radius:6px;",
      shiny::strong("✓ Ready to continue"),
      shiny::span(" — No human approval is pending for this saved version.")
    )
  )
  gate_labels<-c(
    b2_protocol="Approve screening start",
    b2_screening="Approve title/abstract screening",
    b2_inclusion="Approve final included studies",
    b2_extraction="Approve verified data extraction",
    b2_rob="Approve risk-of-bias assessment"
  )
  labs<-unname(gate_labels[g$gate_type])
  miss<-is.na(labs)
  if(any(miss))labs[miss]<-gsub("_"," ",g$gate_type[miss])
  shiny::div(
    style="border-left:4px solid #d97706;background:#fff7ed;padding:12px 14px;margin:10px 0 14px;border-radius:6px;",
    shiny::strong("! ACTION REQUIRED — Human approval needed"),
    shiny::p("This saved version requires your approval before the next scientific processing step."),
    shiny::selectInput(
      "evidence_gate","Pending human review",
      choices=stats::setNames(g$id,labs),
      selected=if(nrow(g))g$id[[1L]] else character()
    ),
    shiny::selectInput(
      "evidence_gate_decision","Your decision",
      c("Choose a decision"="","Approve"="APPROVED","Reject"="REJECTED")
    ),
    shiny::textAreaInput("evidence_gate_rationale","Reason / rationale",rows=2),
    shiny::actionButton("evidence_gate_record","Record Human decision",class="btn-primary")
  )
 })
 shiny::observeEvent(input$evidence_gate_record,{
  shiny::req(input$project,input$version,input$evidence_gate)
  decision<-input$evidence_gate_decision %||b2% ""
  rationale<-trimws(input$evidence_gate_rationale %||b2% "")
  if(!decision %in% c("APPROVED","REJECTED")||!nzchar(rationale)){
    notice("Choose Approve or Reject and provide a rationale.")
    return()
  }
  x<-safe(request("view",list(project_id=input$project)))
  if(is.null(x)||is.null(x$gates)||!nrow(x$gates))return()
  valid<-x$gates$id==input$evidence_gate &
    x$gates$version_id==input$version &
    x$gates$status=="PENDING" &
    grepl("^b2_",x$gates$gate_type)
  if(!any(valid)){
    notice("That Human Gate is no longer pending for this exact evidence version. Reload the selected evidence version.")
    return()
  }
  done<-safe({
    request("decide_gate",list(
      project_id=input$project,
      gate_id=input$evidence_gate,
      decision=decision,
      rationale=rationale
    ))
    TRUE
  })
  if(isTRUE(done)){
    refresh(refresh()+1L)
    shiny::updateSelectInput(session,"evidence_gate_decision",selected="")
    shiny::updateTextAreaInput(session,"evidence_gate_rationale",value="")
    notice("Human decision recorded for this exact evidence version.")
  }
 })

 load<-function(){x<-safe(request("evidence_view",list(project_id=input$project,version_id=input$version)));packet(x);x}
 selected<-shiny::reactive({x<-packet();shiny::req(x,input$evidence_table,input$evidence_row);rows<-x$tables[[input$evidence_table]];hits<-Filter(function(a)identical(a$.row_id,input$evidence_row),rows);shiny::req(length(hits)==1);hits[[1]]})
 saved<-function(v,auto_load=FALSE,return_table=NULL,return_row=NULL,return_kind=NULL,
                 previous_ids=NULL,source_screening_id=NULL){if(!is.null(v)){
  packet(NULL)
  refresh(refresh()+1L)
  all<-safe(request("view",list(project_id=v$project_id)))
  if(isTRUE(auto_load)){
   pending_import_load(list(project_id=as.character(v$project_id),
                            version_id=as.character(v$version_id),
                            return_table=return_table,return_row=return_row,
                            return_kind=return_kind,previous_ids=previous_ids,
                            source_screening_id=source_screening_id))
  }
  if(!is.null(all)&&nrow(all$versions)){
   choices<-stats::setNames(all$versions$id,paste("Version",all$versions$version))
   session$onFlushed(function(){
    shiny::updateSelectInput(session,"version",choices=choices,selected=v$version_id)
   },once=TRUE)
  }
  if(!isTRUE(auto_load))
   notice("Saved a new evidence version. The new version is selected automatically; load it to continue. Pending approvals must be decided explicitly.")
 }}
 shiny::observeEvent(list(input$project,input$version),packet(NULL))
 # Auto-load the exact immutable version produced by Human Excel or PDF intake.
 # Run AFTER the generic version-change reset; never race packet(NULL).
 shiny::observeEvent(input$version,{
  target<-pending_import_load()
  if(is.null(target)||!identical(as.character(input$project),target$project_id)||
     !identical(as.character(input$version),target$version_id))return()
  pending_import_load(NULL)
  x<-safe(request("evidence_view",list(project_id=target$project_id,
                                       version_id=target$version_id)))
  if(is.null(x)){
   if(identical(target$return_kind,"PDF"))
    notice("PDF saved in a new Evidence Version, but automatic loading failed. Load selected evidence version; do not upload the same PDF again.")
   else if(identical(target$return_kind,"HUMAN_EDIT"))
    notice("Human edit saved in a new Evidence Version, but automatic loading failed. Load selected evidence version; do not re-enter the decision.")
   else if(identical(target$return_kind,"OUTCOME_ADD"))
    notice("The new outcome row was saved, but this Evidence Version could not be loaded automatically. Load selected evidence version; do not add the row twice.")
   else if(identical(target$return_kind,"DISCOVERY"))
    notice("PDF Discovery was saved, but the Evidence Version did not load automatically. Click Load selected evidence version; do not rerun AI.")
   else if(identical(target$return_kind,"INVENTORY"))
    target_save_feedback("inventory","error","Selected outcomes were saved, but the new Evidence Version did not load. Click Load selected evidence version; do not save again.",target$version_id,target$project_id)
   else if(identical(target$return_kind,"HARMONIZATION"))
    target_save_feedback("harmonization","error","Common outcomes were saved, but the new Evidence Version did not load. Click Load selected evidence version; do not save again.",target$version_id,target$project_id)
   else if(identical(target$return_kind,"SYNTHESIS_PLAN"))
    notice("Synthesis decisions were saved, but the new Evidence Version did not load. Click Load selected evidence version; do not save again.")
   else if(identical(target$return_kind,"SYNTHESIS_EXECUTION"))
    notice("Automatic synthesis was saved, but the Evidence Version did not load. Load the latest version; do not regenerate.")
   else
    notice("Human Excel was saved, but this Evidence Version could not be loaded automatically. Use Load selected evidence version; do NOT import again.")
   return()
  }
  packet(x)
  if(identical(target$return_kind,"DISCOVERY")) {
    ready<-tryCatch(isTRUE(web_b2_discovery_ready(x)),error=function(e)FALSE)
    if(ready) {
      n<-length(x$pdf_result_discovery$results %||b2% list())
      notice(paste0("PDF Discovery saved and loaded: ",n,
        " paper-reported results. Review Step 2; AI results are not Human approvals."))
    } else {
      notice("PDF Discovery saved and version loaded, but PDF source checksums or scan coverage require review. Do not rerun AI.")
    }
    return()
  }
  if(identical(target$return_kind,"INVENTORY")) {
    ok<-identical(x$result_inventory_selection$status,"HUMAN_SELECTED")
    n<-length(x$result_inventory_selection$group_ids %||b2% list())
    msg<-if(ok)paste0("Selected outcomes saved and loaded: ",n,
      ". Continue to Step 3 — Common outcomes.") else
      "A new Evidence Version was created, but the selected outcome state requires review. Do not save twice."
    target_save_feedback("inventory",if(ok)"ok" else "error",msg,
      target$version_id,target$project_id)
    return()
  }
  if(identical(target$return_kind,"HARMONIZATION")) {
    ok<-identical(x$human_outcome_harmonization$status,"HUMAN_CONFIRMED")
    n<-length(x$human_outcome_harmonization$mappings %||b2% list())
    msg<-if(ok)paste0("Common outcomes saved and loaded: ",n,
      ". All numerical values still require separate Human Verification.") else
      "A new Evidence Version was created, but the common outcome state requires review. Do not save twice."
    target_save_feedback("harmonization",if(ok)"ok" else "error",msg,
      target$version_id,target$project_id)
    return()
  }
  if(identical(target$return_kind,"SYNTHESIS_PLAN")) {
    ok<-identical(x$synthesis_plan$status,"HUMAN_CONFIRMED")
    n<-length(x$synthesis_plan$decisions %||b2% list())
    synthesis_feedback(list(version_id=target$version_id,project_id=target$project_id,
      kind=if(ok)"ok" else "error",text=if(ok)
      paste0("Synthesis plan and automated source-indexed outputs saved and loaded for ",n,
        " timepoint-specific sets. Quantitative candidates still need Human Numeric Verification.")
      else "The new version loaded, but synthesis plan integrity must be reviewed."))
    return()
  }
  if(identical(target$return_kind,"SYNTHESIS_EXECUTION")) {
    done<-is.list(x$synthesis_execution) &&
      identical(x$synthesis_execution$status,"AUTO_PREPARED")
    synthesis_execution_feedback(list(kind=if(done)"ok" else "error",
      text=if(done)"Synthesis matrix and structured narrative draft saved and loaded; unverified numbers remain flagged."
        else "Synthesis output could not be confirmed. Do not repeat the operation; inspect the saved version.",
      project_id=target$project_id,version_id=target$version_id))
    return()
  }
  if(isTRUE(target$return_kind %in% c("PDF","HUMAN_EDIT","OUTCOME_ADD"))){
   restore<-if(identical(target$return_kind,"PDF"))
    web_b2_ui_pdf_restore_target(x$stage,x$tables,target$return_row)
   else if(identical(target$return_kind,"OUTCOME_ADD"))
    web_b2_ui_new_outcome_restore_target(x$stage,x$tables,target$previous_ids,
      target$return_row,target$source_screening_id)
   else web_b2_ui_human_edit_restore_target(x$stage,x$tables,target$return_table,target$return_row)
   if(is.null(restore)){
    if(identical(target$return_kind,"OUTCOME_ADD"))
     notice("The new Evidence Version is loaded, but the exact new outcome row could not be identified safely. Select the new row from All records; do not add it again.")
    else
     notice("New Evidence Version was loaded, but the previously selected review row is not available. Choose the appropriate Review section and record again.")
    return()
   }
   # Restore both the section and row after the version-reset observer.
   # Human decisions already saved in this immutable version remain editable.
   session$onFlushed(function(){
    shiny::updateSelectInput(session,"evidence_table",selected=restore$table)
    shiny::updateSelectInput(session,"evidence_row",selected=restore$row_id)
   },once=TRUE)
   if(identical(target$return_kind,"PDF"))
    notice("Full-text PDF saved. The new Evidence Version is loaded; select the next report to upload another PDF.")
   else if(identical(target$return_kind,"OUTCOME_ADD"))
    notice("Blank outcome/timepoint row added and selected. Only report identity was retained; no AI numbers, outcome name, or Human approval was inherited. Enter a single outcome name, then select an AI source explicitly.")
   else
    notice("Human entry saved in a new Evidence Version. The saved record is loaded and remains editable; select another record to continue.")
   return()
  }
  shiny::updateSelectInput(session,"evidence_table",selected="Reviewer_2")
  result<-web_b2_ui_final_screening_counts(x)
  if(is.null(result))notice("Human Excel saved; load this version to verify the screening summary.")
  else if(!isTRUE(result$consistent))notice("Human Excel saved, but final screening counts need verification before finalization.")
  else notice(paste0("Human decisions imported and saved — Include ",result$human_include,
                      ", Exclude ",result$human_exclude,
                      ", policy Exclude ",result$policy_only,
                      ", AI-only Exclude ",result$ai_only,
                      ", pending ",result$pending,". New Evidence Version is loaded."))
 },priority=-100,ignoreInit=TRUE)
 shiny::observeEvent(input$evidence_refresh,{load()})
 shiny::observeEvent(input$evidence_start,saved(safe(request("evidence_start",list(project_id=input$project,version_id=input$version)))))
 output$evidence_stage<-shiny::renderText({x<-packet();if(is.null(x))return("Load this version to review its evidence. Start screening only from checked database inputs.");stages<-c(INPUT="Research setup",SCREENING="Title and abstract screening",FULLTEXT="Full-text eligibility",INCLUDED="Included studies",EXTRACTION="Data extraction and human verification",VERIFIED="Verified research data",ROB="Risk-of-bias review",READY="Approved evidence package");stage<-as.character(x$stage %||b2% "");friendly<-unname(stages[stage]);if(length(friendly)!=1L||is.na(friendly))friendly<-"Current research step";paste("Current step:",friendly,"— review evidence, save decisions, then continue.")})
 output$evidence_progress<-shiny::renderTable({x<-packet();shiny::req(x);do.call(rbind,lapply(names(x$tables),function(t){rows<-x$tables[[t]];data.frame(Section=unname(names_table[t]) %||b2% t,Rows=length(rows),Human_entries=sum(vapply(rows,function(a)!is.null(a$.actor) && !identical(a$.actor,"AI_AGENT_REVIEWER_1"),logical(1))),Verified=sum(vapply(rows,function(a)identical(a$verification_status,"Verified"),logical(1))))}))})
 output$evidence_screening_summary<-shiny::renderUI({
  x<-packet()
  if(is.null(x) || is.null(x$tables$AI_Screening))return(NULL)
  rows<-x$tables$AI_Screening
  s<-if(!is.null(x$ai_screening_aggregate))unlist(x$ai_screening_aggregate,use.names=TRUE) else web_b2_screening_counts(rows)
  web_b2_screening_summary_panel(
   "AI title/abstract screening — saved version",
   "AI Reviewer 1 recommendations; Human Reviewer 2 decisions are recorded separately.",s)
 })
 output$evidence_final_import_summary<-shiny::renderUI({
  x<-packet()
  c<-web_b2_ui_final_screening_counts(x)
  if(is.null(c))return(NULL)
  # These are Human-selected *screening* includes. Full-text eligibility and
  # analysis-ready endpoint checks still determine quantitative meta-analysis.
  included<-Filter(function(r) {
    actor<-as.character(r$.actor %||b2% "")
    identical(as.character(r$decision %||b2% ""),"Include") &&
      length(actor)==1L && !is.na(actor) && nzchar(trimws(actor)) &&
      !identical(actor,"AI_AGENT_REVIEWER_1")
  },x$tables$Reviewer_2)
  included_lines<-lapply(seq_len(min(length(included),15L)),function(i) {
    r<-included[[i]]
    id<-as.character(r$screening_id %||b2% "")
    title<-as.character(r$title %||b2% "")
    shiny::tags$li(
      shiny::tags$span(class="srma-includes-id",id),
      if(nzchar(title))paste0(" — ",title) else "")
  })
  entries<-c(
   "Human Reviewer 2 — Include"=c$human_include,
   "Human Reviewer 2 — Exclude"=c$human_exclude,
   "Policy-only Exclude"=c$policy_only,
   "AI-only Exclude"=c$ai_only,
   "Final Include"=c$total_include,
   "Final Exclude"=c$total_exclude,
   "Pending"=c$pending,
   "Total screened"=c$total)
  shiny::div(style="border:1px solid #cbd5e1;border-radius:12px;padding:15px 17px;margin:12px 0;background:#fff;box-shadow:0 1px 4px rgba(24,52,82,.05);",
   shiny::strong("Final title/abstract screening — imported Human decisions"),
   shiny::p(paste0("Evidence Version ",substr(input$version %||b2% "",1,10),
                   " | Researcher exclusion policy: ",c$policy,
                   " | Human-reviewed ",c$human_reviewed," records")),
   shiny::tags$table(class="table table-sm",shiny::tags$tbody(
    lapply(names(entries),function(label)shiny::tags$tr(
     shiny::tags$td(label),shiny::tags$td(format(entries[[label]],big.mark=",")))))),
   if(isTRUE(c$consistent) && length(included))
    shiny::tags$details(class="srma-includes-accordion",open=if(length(included)<=5L)"open" else NULL,
      shiny::tags$summary(
       shiny::tags$span(class="srma-includes-count",paste0(length(included)," included")),
       shiny::tags$span(class="srma-includes-label","Human Reviewer 2 screening Includes"),
       shiny::tags$span(class="srma-includes-hint","Click to expand or collapse study IDs and titles")
      ),
      shiny::tags$ol(class="srma-includes-list",included_lines),
      if(length(included)>15L)shiny::p(paste0("Showing the first 15 of ",length(included)," records.")),
      shiny::p("These records proceed to full-text eligibility; only full-text-eligible studies with analysis-ready data can enter meta-analysis.")),
   if(isTRUE(c$consistent))
    shiny::p("Saved decision rows and import audit counts agree. A separate Human approval is required before finalization.")
   else shiny::p(style="color:#b91c1c;",
                 "Saved rows and import audit totals do not agree, or some decisions remain pending. Review before finalization.")
  )
 })
 output$evidence_design_workflow<-shiny::renderUI({
  x<-packet()
  if(is.null(x)||!identical(x$stage,"SCREENING")||!length(x$tables$AI_Screening))return(NULL)
  z<-web_b2_selective_review_targets(x)
  summary<-design_summary()
  ready<-!is.null(summary)&&identical(as.integer(summary$total),as.integer(z$count$include))
  count_note<-if(ready)shiny::p(style="font-weight:600;",
    paste0("Classifications: COMPARATIVE ",summary$counts[["COMPARATIVE"]],
      " | NON_COMPARATIVE ",summary$counts[["NON_COMPARATIVE"]],
      " | UNCLEAR ",summary$counts[["UNCLEAR"]],
      " (total ",summary$total,").")) else NULL
  shiny::div(style="border-top:1px solid #e2e8f0;margin-top:12px;padding-top:12px;",
    shiny::strong("Step 2 — AI Include study-design classification (required for policy ②/④)"),
    shiny::p(if(ready)
      paste0("COMPLETED & SAVED — Validated classification for ",summary$total,
        " AI Include records in Evidence Version ",substr(input$version %||b2% "",1,10),
        ". Proceed to Step 3. Do not rerun or re-save the AI classification.")
      else paste0("For comparator-dependent policy ②/④, classify only the ",z$count$include,
        " AI Include records using the operation below, then save the completed result as a new Evidence Version. The ",z$count$uncertain," AI Unclear and ",z$count$exclude," AI Exclude records are not reprocessed.")),
    count_note
  )
 })
 output$evidence_selective_excel_ui<-shiny::renderUI({
  x<-packet()
  if(is.null(x)||!identical(x$stage,"SCREENING")||!length(x$tables$AI_Screening))return(NULL)
  # An imported, immutable Human Reviewer 2 decision set is already final for
  # title/abstract screening. Do not show another upload/Import workflow.
  # Keep the pending Human Gate separate, and do not bypass full-text eligibility.
  if(!is.null(x$screening_excel_review)) {
    c<-web_b2_ui_final_screening_counts(x)
    return(shiny::div(
      style="border:1px solid #cbd5e1;border-radius:8px;padding:14px;margin:12px 0;background:#f8fafc;",
      shiny::strong("Human Reviewer 2 — final decisions saved"),
      if(!is.null(c) && isTRUE(c$consistent))
        shiny::p(paste0("Screening decisions COMPLETE: ",c$human_include,
          " Human Include, ",c$human_exclude," Human Exclude, ",
          c$policy_only," policy-only Exclude, ",c$ai_only,
          " AI-only Exclude. Pending: ",c$pending,"."))
      else shiny::p(style="color:#b91c1c;",
        "Import counts require verification; do not finalize screening."),
      shiny::p("The Excel was already imported into this Evidence Version. Do not upload or import it again."),
      shiny::p("Next: explicitly approve the pending title/abstract screening Human Gate, then finalize screening to prepare full-text eligibility. Screening Include does not automatically mean inclusion in quantitative meta-analysis.")
    ))
  }
  z<-web_b2_selective_review_targets(x)
  # Human Review remains optional for non-comparator projects. Do not expose
  # SINGLE_ARM/BOTH before a validated saved design result exists.
  design_ready<-tryCatch({
    d<-web_b2_study_design_validate(x,required=TRUE)
    length(d$ids)==z$count$include && z$count$include>0L
  },error=function(e)FALSE)
  policy_choices<-c("① Pass — no additional exclusions"="PASS",
    "② Exclude clearly non-comparative / single-arm studies"="SINGLE_ARM",
    "③ Exclude AI Unclear records"="AI_UNCLEAR",
    "④ Exclude both non-comparative and AI Unclear"="BOTH")
  if(!design_ready)policy_choices<-policy_choices[policy_choices %in% c("PASS","AI_UNCLEAR")]
  shiny::div(style="border:1px solid #cbd5e1;border-radius:8px;padding:14px;margin:12px 0;background:#f8fafc;",
    shiny::strong("Step 3 — Optional protocol-specific comparative triage"),
    shiny::p(if(design_ready)
      "AI Include study-design classification is saved and validated. Options ②/④ are now available."
      else "Options ②/④ are locked. First complete Step 2 and save the AI Include classification as a new Evidence Version; then load that version."),
    shiny::selectInput("evidence_comparative_option","Researcher-selected exclusion policy (default: Pass)",
      choices=policy_choices,selected=as.character(x$screening_excel_review$comparative_option %||b2% "PASS")),
    shiny::p("Choose ②/④ only if this review requires an analyzable comparator. AI Unclear (③/④) is not proof of ineligibility and requires a separate explicit protocol justification."),
    shiny::p("AI Unclear is NOT reclassified. Bulk exclusions are researcher-selected policy decisions, not individually verified study reasons."),
    shiny::textAreaInput("evidence_comparative_rationale", "Researcher justification for bulk exclusions (15+ characters)",
      value=as.character(x$screening_excel_review$comparative_rationale %||b2% ""),rows=2,placeholder="Why these exclusions comply with this review's protocol"),
    shiny::checkboxInput("evidence_comparative_confirm",
      "I approve the selected bulk exclusions and accept the risk of omitting eligible studies",FALSE),
    shiny::uiOutput("evidence_comparative_preview"),
    shiny::tags$hr(),
    shiny::strong("Human Reviewer 2 — final decision Excel (one-time submission)"),
    shiny::p(paste0("Total AI screened: ",z$count$total," | Baseline Human Excel before option: ",
      z$count$include," AI Include + ",z$count$uncertain," AI Unclear = ",length(z$review_ids),
      " required rows. AI Exclude: ",z$count$exclude," (not sent to Human Excel; retained as AI-only).")),
    shiny::p("Fill yellow cells: G final Include/Exclude, H reason for Exclude. Keep the Screening IDs and template columns; titles/abstracts may be corrected. An earlier Toolkit template is accepted when its study IDs match this review and the current required rows are complete."),
    shiny::downloadButton("evidence_selective_excel_download","Download Human Review Excel"),
    shiny::fileInput("evidence_selective_excel_file","Upload completed Human Review XLSX",accept=".xlsx"),
    shiny::actionButton("evidence_selective_excel_import","Import final Human decisions",class="btn-primary"),
    if(!is.null(x$screening_excel_review))shiny::p(
      paste0("Imported Human decisions: ",x$screening_excel_review$target_count,
        "; AI-only Exclude: ",x$screening_excel_review$ai_only_exclude_count,
        "; comparative option: ",x$screening_excel_review$comparative_option %||b2% "PASS",
        "; additional option excludes: ",x$screening_excel_review$policy_auto_exclude_count %||b2% 0,
        ". Approve the SCREENING Human Gate before finalizing."))
  )
 })
 output$evidence_comparative_preview<-shiny::renderUI({
  x<-packet();if(is.null(x)||!identical(x$stage,"SCREENING")||!length(x$tables$AI_Screening))return(NULL)
  policy<-input$evidence_comparative_option %||b2% "PASS"
  # Preview does not save an exclusion; rationale is required at download/import.
  p<-tryCatch(web_b2_comparative_policy_targets(x,policy,
    if(identical(policy,"PASS"))"" else "Preview only; researcher approval pending"),
    error=function(e)e)
  if(inherits(p,"error"))return(shiny::p(conditionMessage(p),style="color:#b91c1c;"))
  shiny::div(style="padding:9px;background:#f1f5f9;border-radius:6px;",
    shiny::strong(paste0("Preview: ",p$counts[["human_review"]]," Human Review rows; ",
      p$counts[["policy_auto_exclude"]]," additional policy exclusions; ",
      p$counts[["ai_exclude"]]," original AI Exclude")),
    shiny::p(paste0("Policy single-arm exclusions: ",p$counts[["policy_single_arm"]],
      "; policy AI Unclear exclusions: ",p$counts[["policy_ai_unclear"]],
      ". Non-comparative classifications among AI Include only: ",p$classified_single_arm,
      ". Supplementary AI Include rows classified: ",p$design_classified,
      ". Previous Human judgments protected: ",p$protected_human,".")),
    shiny::p("The researcher authorizes protocol-dependent exclusions. AI design labels are recommendations, not verified human judgments. The workbook includes a separate Design_Triage audit sheet."),
    shiny::p("All original screening IDs remain in the final audit."))
 })
 output$evidence_selective_excel_download<-shiny::downloadHandler(
  filename=function()paste0("SRMA_Human_Review_",input$version,".xlsx"),
  content=function(file){x<-packet();shiny::req(x,input$version)
    policy<-input$evidence_comparative_option %||b2% "PASS"
    reason<-if(identical(policy,"PASS"))"" else as.character(input$evidence_comparative_rationale %||b2% "")
    if(!identical(policy,"PASS"))shiny::req(isTRUE(input$evidence_comparative_confirm))
    web_b2_export_selective_excel(x,input$version,file,policy,reason)
  },contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
 shiny::observeEvent(input$evidence_selective_excel_import,{
   shiny::req(input$project,input$version,input$evidence_selective_excel_file)
   policy<-as.character(input$evidence_comparative_option %||b2% "PASS")
   rationale<-if(identical(policy,"PASS"))"" else
     as.character(input$evidence_comparative_rationale %||b2% "")
   approved<-isTRUE(input$evidence_comparative_confirm)
   if(!identical(policy,"PASS") && !approved) {
     shiny::showNotification("Approve the selected comparative exclusions before importing.",type="error")
   } else {
     saved(safe(request("evidence_upload",list(project_id=input$project,
       version_id=input$version,row_id="__SELECTIVE_SCREENING_XLSX_RC28__",
       upload=input$evidence_selective_excel_file,
       comparative_option=policy,comparative_rationale=rationale,
       comparative_approved=approved))),auto_load=TRUE)
   }
 })
 shiny::observeEvent(input$evidence_table,{
  if(identical(input$evidence_table,"Reviewer_2"))
    shiny::updateSelectInput(session,"evidence_queue",selected="ai_priority")
 },ignoreInit=FALSE)
 # Final Study Selection is distinct from historical full-text eligibility.
 # A simple checkbox decision controls all analysis-facing views in this version.
 output$evidence_final_studies_ui<-shiny::renderUI({
  x<-packet();if(is.null(x)||!identical(x$stage,"EXTRACTION")||
    !length(x$included_reports %||b2% list()))return(NULL)
  catalog<-tryCatch(web_b2_final_study_catalog(x),error=function(e)NULL)
  if(is.null(catalog))return(shiny::p("Final Study Selection: unique Study IDs and full-text included reports must be registered first."))
  all_ids<-vapply(catalog,`[[`,character(1),"study_id")
  chosen<-if(is.null(x$final_study_selection))all_ids else
    tryCatch(web_b2_final_active_study_ids(x,TRUE),error=function(e)character())
  labs<-vapply(catalog,function(a)paste0(a$study_id,"  |  ",
    if(length(a$titles))substr(a$titles[[1L]],1L,100L) else "(title unavailable)",
    "  [",paste(a$screening_ids,collapse=", "),"]"),character(1))
  shiny::div(style="border:1px solid #c8d6e7;border-radius:13px;padding:20px;margin:17px 0;background:#f8fbfe;",
   shiny::h3("Step 1 — Final Study Selection (Human confirmation)"),
   shiny::p("Uncheck any study that must NOT enter the Study–Outcome Matrix, Human Outcome Verification or meta-analysis. No exclusion reason is required. The original screening and extraction records remain unchanged."),
   shiny::checkboxGroupInput("evidence_final_study_ids","Final analysis studies",
     choices=stats::setNames(all_ids,labs),selected=chosen),
   shiny::p(if(is.null(x$final_study_selection))
     "Not yet confirmed. Save an explicit final study set before finalizing extraction."
    else paste0("Saved Human selection: ",length(chosen)," of ",length(all_ids),
      " studies in this Evidence Version.")),
   shiny::actionButton("evidence_final_study_save","Save study selection",
     class="btn-primary",onclick="this.disabled=true;this.innerText='Saving…';"),
   shiny::uiOutput("evidence_final_study_feedback")
  )
 })
 output$evidence_final_study_feedback<-shiny::renderUI({
  f<-final_study_feedback()
  if(is.null(f)||!identical(f$version_id,as.character(input$version)))return(NULL)
  col<-if(identical(f$kind,"error"))"#b91c1c" else "#166534"
  shiny::tags$small(style=paste0("display:block;margin-top:8px;color:",col,";"),f$text)
 })
 shiny::observeEvent(input$evidence_final_study_save,{
  x<-packet();shiny::req(x,input$project,input$version,identical(x$stage,"EXTRACTION"))
  current_version<-as.character(input$version)
  restore<-function()session$sendCustomMessage("srmaFinalStudyButton",
    list(disabled=FALSE,label="Save study selection"))
  catalog<-web_b2_final_study_catalog(x)
  valid<-vapply(catalog,`[[`,character(1),"study_id")
  selected_ids<-as.character(input$evidence_final_study_ids %||b2% character())
  if(!length(selected_ids)||anyDuplicated(selected_ids)||!all(selected_ids %in% valid)) {
    final_study_feedback(list(version_id=current_version,kind="error",
      text="Select at least one eligible study."))
    restore()
    return()
  }
  if(!is.null(x$final_study_selection) && identical(sort(selected_ids),
       sort(web_b2_final_active_study_ids(x,TRUE)))) {
    final_study_feedback(list(version_id=current_version,kind="ok",
      text="Already saved — selection unchanged."))
    restore()
    return()
  }
  final_study_feedback(list(version_id=current_version,kind="ok",text="Saving selection…"))
  v<-safe(request("evidence_edit",list(project_id=input$project,version_id=input$version,
    table="__FINAL_STUDY_SELECTION__",row_id="__FINAL_STUDIES__",
    values=list(study_ids_json=as.character(jsonlite::toJSON(selected_ids,auto_unbox=FALSE))))))
  if(is.null(v)) {
    problem<-as.character(notice() %||b2% "")
    if(!nzchar(problem))problem<-"Save failed. Check the Terminal 2 App log."
    final_study_feedback(list(version_id=current_version,kind="error",text=problem))
    restore()
    return()
  }
  final_study_feedback(list(version_id=current_version,kind="ok",
    text="Selection saved. Opening new Evidence Version…"))
  saved(v,auto_load=TRUE)
  restore()
 })
 # RC28.19: paper-specific PDF results, simple checkboxes. Optional source details
 # stay collapsed, and unextracted protocol-template rows are never repeated.
 output$evidence_study_outcome_matrix_ui<-shiny::renderUI({
  x<-packet();if(is.null(x)||!identical(x$stage,"EXTRACTION"))return(NULL)
  if(is.null(x$final_study_selection))return(NULL)
  catalog<-web_b2_final_study_catalog(x)
  chosen<-web_b2_final_active_study_ids(x,TRUE)
  catalog<-Filter(function(r)r$study_id %in% chosen,catalog)
  brief<-function(v,n=170L){v<-as.character(v %||b2% "");
    if(length(v)!=1L||is.na(v))return("")
    substr(gsub("[\r\n\t]+"," ",trimws(v)),1L,n)}
  if(!web_b2_discovery_ready(x)){
    done<-length(x$pdf_result_discovery$report_checksums %||b2% list())
    return(shiny::div(
      style="border:1px solid #dbe3ec;border-radius:9px;padding:15px;margin:14px 0;",
      shiny::h4("Step 2 — Results in each paper"),
      if(done)shiny::tags$small(paste(done,"/",length(web_b2_final_active_reports(x,TRUE)),"PDFs scanned")) else NULL,
      shiny::actionButton("evidence_pdf_discover_run","Find results in PDFs",
        class="btn-primary",onclick="this.disabled=true;this.innerText='Submitting…';"),
      shiny::uiOutput("evidence_pdf_discovery_feedback"),
      shiny::tags$small(style="display:block;margin-top:7px;color:#64748b;",
        "AI processing costs apply. Save the completed job to see the results.")))
  }
  groups<-web_b2_result_inventory(x)
  old<-as.character(unlist(x$result_inventory_selection$group_ids %||b2% list(),use.names=FALSE))
  blocks<-lapply(catalog,function(study){
    subset<-Filter(function(g)g$screening_id %in% study$screening_ids,groups)
    keys<-vapply(subset,`[[`,character(1),"id")
    labels<-vapply(subset,function(g)paste0(brief(g$reported_name,140L),
      if(nzchar(g$reported_timepoint))paste0(" · ",brief(g$reported_timepoint,45L)) else ""),character(1))
    details<-if(length(subset))shiny::tags$details(style="margin:3px 0 8px 20px;",
      shiny::tags$summary(style="cursor:pointer;color:#64748b;font-size:.88em;","Definitions and PDF sources"),
      shiny::tags$table(class="table table-sm",shiny::tags$tbody(lapply(subset,function(g)
        shiny::tags$tr(shiny::tags$td(brief(g$reported_name,85L)),
          shiny::tags$td(brief(g$reported_definition,150L)),
          shiny::tags$td(brief(g$source_location,80L))))))) else NULL
    shiny::tags$div(style="padding:10px 0;border-bottom:1px solid #edf2f7;",
      shiny::tags$div(style="font-weight:600;margin-bottom:6px;",
        paste0(brief(study$study_id,65L), "  |  ",
          if(length(study$titles))brief(study$titles[[1L]],125L) else "(title unavailable)")),
      if(length(subset))shiny::checkboxGroupInput(
        paste0("evidence_inventory_study_",substr(digest::digest(study$study_id,algo="sha256"),1L,12L)),
        label=NULL,choices=stats::setNames(keys,labels),selected=intersect(keys,old)) else
        shiny::tags$span("No reportable results were identified."),
      details)
  })
  shiny::div(style="border:1px solid #dbe3ec;border-radius:9px;padding:15px;margin:14px 0;",
    shiny::h4("Step 2 — Results in each paper"),
    shiny::tags$p(style="margin:4px 0 4px;color:#334155;",
      "Select the outcomes you want to analyze in this review (meta-analysis or structured synthesis). If multiple papers report the same target outcome, check it in each paper."),
    shiny::tags$p(style="margin:0 0 12px;color:#64748b;font-size:.92em;",
      "These are analysis targets only. Outcome definitions, timepoints and source numbers require harmonization and human verification before pooling."),
    blocks,
    if(length(groups))shiny::actionButton("evidence_inventory_save", "Save selected outcomes",
      class="btn-primary",onclick="this.disabled=true;this.innerText='Saving…';"),
    shiny::uiOutput("evidence_inventory_feedback"),
    if(!is.null(x$result_inventory_selection))shiny::tags$small(
      style="display:block;margin-top:8px;color:#166534;",
      paste(length(old),"outcomes selected")),
    shiny::uiOutput("evidence_harmonization_ui"))
 })
 # The dedicated Step 2 status is independent of the unrelated processing
 # dropdown above Step 1. Poll the *actual* jobs table every two seconds.
 output$evidence_pdf_discovery_feedback<-shiny::renderUI({
  version<-as.character(input$version %||b2% "")
  project<-as.character(input$project %||b2% "")
  f<-pdf_discovery_feedback()
  if(!is.null(f) && (!identical(f$version_id,version) ||
                     !identical(f$project_id,project)))f<-NULL
  j<-NULL
  view<-job_view()
  if(!is.null(view) && is.data.frame(view$jobs) && nrow(view$jobs)) {
    all<-view$jobs
    hit<-all[!is.na(all$version_id)&all$version_id==version &
      !is.na(all$operation)&all$operation=="discover_pdf_results",,drop=FALSE]
    if(nrow(hit)) {
      if(!is.null(f) && nzchar(as.character(f$job_id %||b2% ""))) {
        exact<-hit[!is.na(hit$id)&hit$id==f$job_id,,drop=FALSE]
        if(nrow(exact))hit<-exact else hit<-hit[0L,,drop=FALSE]
      }
      if(nrow(hit)>1L && "created_at" %in% names(hit))
        hit<-hit[order(as.character(hit$created_at),decreasing=TRUE),,drop=FALSE]
      if(nrow(hit))j<-hit[1L,,drop=FALSE]
    }
  }
  if(!is.null(f) && identical(f$state,"SUBMITTING"))return(shiny::tags$small(
    style="display:block;margin-top:8px;","Submitting request…"))
  if(!is.null(f) && identical(f$state,"ERROR"))return(shiny::tags$small(
    style="display:block;margin-top:8px;color:#b91c1c;",f$text))
  if(!is.null(j)) {
    state<-as.character(j$state[[1L]] %||b2% "")
    detail<-as.character(j$message[[1L]] %||b2% "")
    message<-switch(state,
      QUEUED="Queued — waiting for Worker.",
      RUNNING="Running — analyzing the selected PDFs.",
      COMPLETE="Completed — select this job above and save its result as a new Evidence Version.",
      USER_ACTION_REQUIRED=paste0("Action required: ",detail),
      FAILED=paste0("Failed: ",detail),
      paste0("Job status: ",state))
    color<-if(state %in% c("USER_ACTION_REQUIRED","FAILED"))"#b91c1c" else
      if(identical(state,"COMPLETE"))"#166534" else "#334155"
    return(shiny::tags$small(style=paste0("display:block;margin-top:8px;color:",color,";"),
      paste0(message," Job ",substr(as.character(j$id[[1L]]),1L,10L),".")))
  }
  if(is.null(f)||!nzchar(as.character(f$text %||b2% "")))return(NULL)
  col<-if(identical(f$state,"ERROR"))"#b91c1c" else "#334155"
  shiny::tags$small(style=paste0("display:block;margin-top:8px;color:",col,";"),f$text)
 })
 shiny::observeEvent(input$evidence_pdf_discover_run,{
  x<-packet();shiny::req(x,input$project,input$version,
    identical(x$stage,"EXTRACTION"),!is.null(x$final_study_selection))
  p<-as.character(input$project)
  v<-as.character(input$version)
  pdf_discovery_feedback(list(project_id=p,version_id=v,state="SUBMITTING",
    text="Submitting PDF Discovery…",job_id=""))
  id<-safe(request("submit",list(project_id=p,version_id=v,
    operation="discover_pdf_results")))
  if(is.null(id)) {
    problem<-as.character(notice() %||b2% "")
    if(!nzchar(problem))problem<-"No job was queued. Check the Terminal 2 App log."
    pdf_discovery_feedback(list(project_id=p,version_id=v,state="ERROR",
      text=paste0("Not queued: ",problem),job_id=""))
    session$sendCustomMessage("srmaPdfDiscoveryButton",
      list(disabled=FALSE,label="Find results in PDFs"))
    return()
  }
  job_id<-as.character(id[[1L]])
  pdf_discovery_feedback(list(project_id=p,version_id=v,state="QUEUED",
    text=paste0("Queued — job ",substr(job_id,1L,10L),"."),job_id=job_id))
  session$sendCustomMessage("srmaPdfDiscoveryButton",
    list(disabled=TRUE,label="Queued…"))
  shiny::updateSelectInput(session,"evidence_operation",selected="discover_pdf_results")
  notice("PDF Result Discovery queued. Step 2 shows the live job status.")
 })
 # Re-enable retry after Worker errors; do not let an old failed job trap
 # the researcher on a permanently disabled "Queued..." button.
 last_pdf_button_update<-shiny::reactiveVal("")
 shiny::observe({
  f<-pdf_discovery_feedback()
  if(is.null(f)||!nzchar(as.character(f$job_id %||b2% "")) ||
     !identical(f$project_id,as.character(input$project)) ||
     !identical(f$version_id,as.character(input$version)))return()
  view<-job_view()
  if(is.null(view)||!is.data.frame(view$jobs)||!nrow(view$jobs))return()
  j<-view$jobs
  hit<-j[!is.na(j$id)&j$id==f$job_id &
    !is.na(j$version_id)&j$version_id==f$version_id &
    !is.na(j$operation)&j$operation=="discover_pdf_results",,drop=FALSE]
  if(nrow(hit)!=1L)return()
  state<-as.character(hit$state[[1L]])
  if(identical(state,last_pdf_button_update()))return()
  last_pdf_button_update(state)
  retry<-state %in% c("FAILED","USER_ACTION_REQUIRED")
  label<-if(retry)"Retry PDF discovery" else
    if(identical(state,"RUNNING"))"Processing PDFs…" else
    if(identical(state,"COMPLETE"))"Completed — save result" else "Queued…"
  session$sendCustomMessage("srmaPdfDiscoveryButton",list(disabled=!retry,label=label))
 })
 # Status is scoped to the exact project and evidence version so stale errors
 # from a previous version never appear as if they apply to another version.
 target_feedback_ui<-function(f) {
   if(is.null(f)||!identical(f$version_id,as.character(input$version %||b2% ""))||
       !identical(f$project_id,as.character(input$project %||b2% "")))return(NULL)
   col<-if(identical(f$kind,"error"))"#b91c1c" else "#166534"
   shiny::tags$small(role="status",style=paste0("display:block;margin:8px 0;color:",col,";"),f$text)
 }
 output$evidence_inventory_feedback<-shiny::renderUI(target_feedback_ui(inventory_feedback()))
 output$evidence_harm_feedback<-shiny::renderUI(target_feedback_ui(harmonization_feedback()))
 shiny::observeEvent(input$evidence_inventory_save,{
  version<-as.character(input$version %||b2% "")
  project<-as.character(input$project %||b2% "")
  done<-function(kind,msg) {
    target_save_feedback("inventory",kind,msg,version,project)
    target_save_button("evidence_inventory_save","Save selected outcomes")
  }
  x<-packet()
  if(is.null(x)||!nzchar(project)||!nzchar(version)||
      !identical(x$stage,"EXTRACTION")||is.null(x$final_study_selection)) {
    done("error","Load the current extraction Evidence Version and confirm Final Study Selection first.")
    return()
  }
  catalog<-tryCatch(web_b2_final_study_catalog(x),error=function(e)NULL)
  if(is.null(catalog)){
    done("error","Study catalog is unavailable in this Evidence Version. Reload the current version.")
    return()
  }
  chosen<-web_b2_final_active_study_ids(x,TRUE)
  selected<-unlist(lapply(Filter(function(r)r$study_id %in% chosen,catalog),function(study)
    as.character(input[[paste0("evidence_inventory_study_",substr(digest::digest(study$study_id,algo="sha256"),1L,12L))]] %||b2% character())),
    use.names=FALSE)
  allowed<-names(web_b2_result_group_lookup(x))
  if(!length(selected)||anyDuplicated(selected)||!all(selected %in% allowed)){
    done("error","Select at least one valid PDF-reported outcome in Step 2 before saving.")
    return()
  }
  previous<-as.character(unlist(x$result_inventory_selection$group_ids %||b2% list(),use.names=FALSE))
  if(identical(sort(selected),sort(previous))){
    done("ok","Already saved — selected outcomes have not changed. No new Evidence Version was created.")
    return()
  }
  target_save_feedback("inventory","ok","Saving selected outcomes…",version,project)
  v<-safe(request("evidence_edit",list(project_id=project,version_id=version,
    table="__RESULT_INVENTORY_SELECTION__",row_id="__RESULT_GROUPS__",
    values=list(result_group_ids_json=as.character(jsonlite::toJSON(selected,auto_unbox=FALSE))))))
  if(is.null(v)){
    problem<-as.character(notice() %||b2% "")
    if(!nzchar(problem))problem<-"The save was rejected. Review the App log for the exact reason."
    done("error",paste0("Selected outcomes not saved: ",problem))
    return()
  }
  target_save_feedback("inventory","ok","Selected outcomes saved. Opening the new Evidence Version…",version,project)
  saved(v,auto_load=TRUE,return_kind="INVENTORY")
  target_save_button("evidence_inventory_save","Save selected outcomes")
 })
 output$evidence_harmonization_ui<-shiny::renderUI({
  x<-packet();if(is.null(x)||!identical(x$stage,"EXTRACTION")||
    !identical(x$result_inventory_selection$status,"HUMAN_SELECTED"))return(NULL)
  selected<-web_b2_result_selected_groups(x)
  old<-x$human_outcome_harmonization$mappings %||b2% list()
  prior<-stats::setNames(old,vapply(old,function(a)as.character(a$group_id),character(1)))
  brief<-function(v,n=200L) {
    v<-as.character(v %||b2% "")
    if(length(v)!=1L||is.na(v))return("")
    substr(trimws(v),1L,n)
  }
  advisory<-x$tables$Advisory_Outcome_Harmonization %||b2% list()
  advisory_panel<-if(length(advisory))shiny::tags$details(
    style="margin:10px 0;padding:10px;border:1px solid #e2e8f0;",
    shiny::tags$summary("Optional R/25A AI harmonization recommendations — advisory, NOT verified PDF facts"),
    shiny::tags$table(class="table table-sm",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th("Canonical Outcome"),
        shiny::tags$th("AI-proposed common endpoint"),
        shiny::tags$th("AI-proposed time window"))),
      shiny::tags$tbody(lapply(advisory,function(r){
        shiny::tags$tr(
          shiny::tags$td(as.character(r$canonical_outcome %||b2% "")),
          shiny::tags$td(as.character(r$recommended_harmonized_endpoint %||b2% "")),
          shiny::tags$td(as.character(r$recommended_time_window %||b2% "")))
      })))
  ) else NULL
  cards<-lapply(selected,function(g){
    key<-g$id;p<-prior[[key]] %||b2% list()
    source_ids<-vapply(g$sources,function(r)as.character(r$.row_id %||b2% ""),character(1))
    source_labels<-vapply(seq_along(g$sources),function(i){r<-g$sources[[i]]
      paste0(brief(r$outcome_name,60L)," | ",brief(r$outcome_definition,60L),
        " | ",brief(r$timepoint,24L)," | ",brief(r$source_location,45L))
    },character(1))
    source_choice<-as.character(p$source_row_id %||b2% "")
    if(!source_choice %in% source_ids)source_choice<-""
    guess<-as.character(p$final_name %||b2% g$reported_name)
    reported_window<-as.character(g$reported_timepoint %||b2% "")
    window_guess<-if(nzchar(reported_window))reported_window else "Not reported"
    def_guess<-as.character(p$final_definition %||b2% if(nzchar(g$reported_definition))g$reported_definition else g$reported_name)
    measure_info<-web_b2_resolve_effect_measure(g,source_choice)
    measure_guess<-as.character(p$effect_measure %||b2% measure_info$measure)
    measure_label<-if(nzchar(measure_guess))measure_guess else "Pending numeric verification"
    measure_detail<-if(nzchar(as.character(p$effect_measure %||b2% "")))
      "Previously saved mapping; source and numerical verification remain required." else measure_info$detail
    shiny::div(style="border:1px solid #e2e8f0;border-radius:7px;padding:11px;margin:8px 0;",
      shiny::tags$small(style="display:block;color:#64748b;margin-bottom:5px;",
        brief(g$reported_name,150L)," · ",brief(g$screening_id,28L)),
      shiny::textInput(paste0("evidence_harm_name_",key),"Common outcome name",value=guess),
      shiny::textInput(paste0("evidence_harm_window_",key),"Analysis timepoint",value=as.character(p$analysis_window %||b2% window_guess)),
      shiny::tags$p(style="color:#334155;font-size:.9em;margin:3px 0 7px 0;",
        shiny::tags$strong("Effect measure: "),measure_label,
        if(identical(measure_info$status,"ANALYSIS_CANDIDATE"))" (analysis candidate, not PDF-reported)" else NULL),
      shiny::tags$details(style="margin:5px 0;",
        shiny::tags$summary(style="cursor:pointer;color:#64748b;font-size:.9em;","Definition and analysis settings"),
        shiny::textAreaInput(paste0("evidence_harm_def_",key),"Operational definition",value=def_guess,rows=2),
        shiny::selectInput(paste0("evidence_harm_src_",key),"R/25 numerical source (optional)",
          choices=c("No matched numbers — verify manually"="",stats::setNames(source_ids,source_labels)),
          selected=source_choice),
        shiny::tags$div(style="padding:7px 9px;background:#f8fafc;border:1px solid #e2e8f0;border-radius:5px;",
          shiny::tags$strong("Automatically identified effect measure: "),
          shiny::tags$span(measure_label),
          shiny::tags$div(style="color:#64748b;font-size:.87em;margin-top:3px;",measure_detail)),
        shiny::selectInput(paste0("evidence_harm_role_",key),"Analysis role",
          choices=c(Primary="Primary",Secondary="Secondary",Sensitivity="Sensitivity",
            Exploratory="Exploratory","Narrative only"="Narrative only"),
          selected=as.character(p$meta_analysis_role %||b2%
            if(g$result_kind %in% c("binary","continuous","time_to_event"))"Exploratory" else "Narrative only")),
        shiny::textInput(paste0("evidence_harm_parent_",key),"Sensitivity parent (optional)",
          value=as.character(p$parent_analysis_group %||b2% "")),
        shiny::tags$small(style="color:#64748b;","Numerical data remain unverified until Human review.")))
  })
  shiny::div(style="border-top:2px solid #e2e8f0;margin-top:16px;padding-top:15px;",
    shiny::h4("Step 3 — Common outcomes"),
    shiny::tags$p(style="color:#475569;",
      "Effect measures are identified automatically from PDF-reported statistics or labelled analysis candidates. ",
      "Unknown measures remain pending for Human Numeric Verification; no manual choice is required here."),
    advisory_panel,
    cards,
    shiny::checkboxInput("evidence_harm_confirm",
      "I checked the outcome definitions and timepoints.",FALSE),
    shiny::actionButton("evidence_harm_save","Save common outcomes",
      class="btn-primary",onclick="this.disabled=true;this.innerText='Saving…';"),
    shiny::uiOutput("evidence_harm_feedback"),
    if(identical(x$human_outcome_harmonization$status,"HUMAN_CONFIRMED")) {
      idx<-web_b2_synthesis_catalog(x)
      prior_plan<-x$synthesis_plan$decisions %||b2% list()
      past<-if(length(prior_plan))stats::setNames(
        vapply(prior_plan,function(a)as.character(a$mode),character(1)),
        vapply(prior_plan,function(a)as.character(a$analysis_group),character(1))) else character()
      study_count<-length(unique(unlist(lapply(idx,function(z)z$studies),use.names=FALSE)))
      strata_count<-sum(vapply(idx,function(z)length(z$strata),integer(1)))
      cards<-lapply(idx,function(v){
        rows<-lapply(v$strata,function(s){
          n<-length(s$study_ids)
          possible<-isTRUE(s$quantitative_candidate)
          input_id<-paste0("evidence_synthesis_mode_",substr(digest::digest(s$id,algo="sha256"),1L,14L))
          choices<-c("Narrative / single-study synthesis"="NARRATIVE",
            "Needs harmonization or further evidence"="RECONCILE")
          if(possible)choices<-c("Quantitative meta-analysis candidate"="QUANTITATIVE_CANDIDATE",choices)
          # An unsaved Step 4 has NO prior synthesis plan.  On an atomic
          # character() lookup, [[missing_name]] raises a Shiny render error.
          # Fall back to Toolkit's auto-first route; never require re-saving
          # Step 2 or rerunning PDF Discovery just to render the plan.
          prior_mode<-if(length(past)>0L && s$id %in% names(past))
            as.character(past[[s$id]]) else ""
          initial<-if(nzchar(prior_mode)) prior_mode else
            if(possible) "QUANTITATIVE_CANDIDATE" else "NARRATIVE"
          if(!initial %in% unname(choices))initial<-"RECONCILE"
          names_text<-lapply(s$sources,function(a)shiny::tags$li(
            shiny::tags$strong(as.character(a$study_id))," — ",
            as.character(a$pdf_outcome)," (PDF: ",as.character(a$pdf_timepoint),")",
            shiny::tags$small(style="color:#64748b;display:block;",
              as.character(a$screening_id)," | ",substr(as.character(a$title),1L,105L),
              " | ",as.character(a$source_location))))
          shiny::tags$div(style="margin:8px 0;padding:10px;border:1px solid #e2e8f0;border-radius:7px;",
            shiny::tags$div(style="font-weight:600;",as.character(s$timepoint),
              " | ",if(nzchar(s$effect_measure))as.character(s$effect_measure) else "Effect measure pending"),
            shiny::tags$small(style="color:#64748b;",paste0(n,
              if(n==1L)" contributing study; no pooling." else " contributing studies; quantitative eligibility pending verification.")),
            shiny::tags$ul(style="padding-left:20px;margin:6px 0;",names_text),
            if(!possible)shiny::tags$small(style="display:block;color:#92400e;margin-bottom:5px;",
              "Pooling not selectable: fewer than two compatible studies or unresolved definition/timepoint."),
            shiny::tags$div(style="color:#475569;font-size:.9em;margin:5px 0;",
              shiny::tags$strong("Toolkit suggested route: "),
              if(identical(initial,"QUANTITATIVE_CANDIDATE"))
                "Quantitative meta-analysis candidate (numeric verification required)" else
              if(identical(initial,"NARRATIVE"))
                "Narrative / single-study synthesis" else
                "Reconcile before analysis"),
            shiny::tags$details(style="margin:6px 0;",
              shiny::tags$summary(style="cursor:pointer;color:#64748b;",
                "Change suggested route (optional; exceptions only)"),
              shiny::selectInput(input_id,"Synthesis route override",
                choices=choices,selected=initial,width="360px")))
        })
        shiny::tags$div(style="border:1px solid #dbe3ec;border-radius:8px;padding:12px;margin:10px 0;",
          shiny::tags$strong(style="font-size:1.05em;",as.character(v$name)),
          shiny::tags$small(style="display:block;color:#64748b;",
            paste(length(v$studies),"distinct studies;",length(v$strata),"timepoint-specific sets")),
          rows)
      })
      shiny::tagList(
        shiny::h4("Step 4 — Common outcome variables and synthesis plan"),
        shiny::p(paste(length(idx),"unique common outcome variables;",strata_count,
          "analysis sets across",study_count,"selected studies.")),
        shiny::p(style="color:#64748b;",
          "Toolkit suggests each synthesis route automatically. Review the listed studies once and save the whole plan; open Change suggested route only for exceptions. Different timepoints remain separate, and quantitative candidates still require Human Numeric Verification."),
        if(!any(vapply(do.call(base::c,lapply(idx,function(a)unname(a$strata))),
             function(s)isTRUE(s$quantitative_candidate),logical(1))))
          shiny::tags$p(style="color:#92400e;",
            "No compatible multi-study pool is currently identified. Keep distinct timepoints separate; narrative synthesis can be planned, but the canonical R/26–R/27 quantitative branch is not applicable without eligible, verified quantitative targets."),
        cards,
        shiny::actionButton("evidence_synthesis_save","Save synthesis plan",
          class="btn-primary",onclick="this.disabled=true;this.innerText='Saving…';"),
        shiny::uiOutput("evidence_synthesis_feedback"),
        if(identical(x$synthesis_plan$status,"HUMAN_CONFIRMED")) {
          run<-x$synthesis_execution
          fresh<-is.list(run)&&identical(run$status,"AUTO_PREPARED") &&
            isTRUE(x$.synthesis_execution_fresh)
          shiny::tagList(
            shiny::tags$p(style="color:#166534;",
              "Human synthesis routes are saved. Evidence matrix and narrative draft are generated automatically with the plan; older saved plans may need one generation run."),
            if(!fresh)shiny::actionButton("evidence_synthesis_execute","Generate synthesis outputs",
              class="btn-primary",onclick="this.disabled=true;this.innerText='Generating…';"),
            shiny::uiOutput("evidence_synthesis_execution_feedback"),
            if(fresh)shiny::tags$div(style="margin:12px 0;padding:11px;border:1px solid #cbd5e1;border-radius:8px;",
              shiny::tags$strong("Automatic synthesis prepared — source-indexed draft"),
              shiny::tags$p(paste(run$counts$variables,"common variables,",
                run$counts$analysis_sets,"timepoint-specific sets,",
                run$counts$evidence_rows,"source results.")),
              shiny::tags$p(style="color:#92400e;",run$disclosure),
              shiny::downloadButton("evidence_synthesis_matrix_csv","Download evidence matrix (CSV)"),
              shiny::downloadButton("evidence_synthesis_narrative_md","Download narrative draft (MD)"),
              shiny::tableOutput("evidence_synthesis_execution_table"))
            else if(is.list(run))shiny::tags$p(style="color:#92400e;",
              "Existing synthesis output is stale after a source edit; regenerate from this Evidence Version."))
        } else NULL)
    } else NULL)
 })
 output$evidence_synthesis_execution_feedback<-shiny::renderUI({
   f<-synthesis_execution_feedback()
   if(is.null(f)||!identical(as.character(f$version_id),as.character(input$version))||
      !identical(as.character(f$project_id),as.character(input$project)))return(NULL)
   shiny::tags$small(style=paste0("display:block;margin:8px 0;color:",
     if(identical(f$kind,"error"))"#b91c1c" else "#166534",";"),as.character(f$text))
 })
 output$evidence_synthesis_execution_table<-shiny::renderTable({
   x<-packet();shiny::req(x,identical(x$synthesis_execution$status,"AUTO_PREPARED"))
   web_b2_synthesis_execution_table(x$synthesis_execution)[,
     c("common_variable","analysis_timepoint","study_id","route","numeric_status","issues"),drop=FALSE]
 },striped=TRUE,bordered=TRUE,spacing="xs")
 output$evidence_synthesis_matrix_csv<-shiny::downloadHandler(
   filename=function(){paste0("srma-synthesis-evidence-matrix-",Sys.Date(),".csv")},
   content=function(file){
     x<-packet();shiny::req(x,identical(x$synthesis_execution$status,"AUTO_PREPARED"))
     web_assert(isTRUE(x$.synthesis_execution_fresh),
       "Synthesis is stale; regenerate before export.")
     utils::write.csv(web_b2_synthesis_execution_table(x$synthesis_execution),
       file,row.names=FALSE,fileEncoding="UTF-8",na="")
   })
 output$evidence_synthesis_narrative_md<-shiny::downloadHandler(
   filename=function(){paste0("srma-synthesis-draft-",Sys.Date(),".md")},
   content=function(file){
     x<-packet();shiny::req(x,identical(x$synthesis_execution$status,"AUTO_PREPARED"))
     web_assert(isTRUE(x$.synthesis_execution_fresh),
       "Synthesis is stale; regenerate before export.")
     writeLines(web_b2_synthesis_execution_markdown(x$synthesis_execution),file,useBytes=TRUE)
   })
 shiny::observeEvent(input$evidence_synthesis_execute,{
   p<-as.character(input$project %||b2% "")
   v<-as.character(input$version %||b2% "")
   reset<-function(kind,msg){
     synthesis_execution_feedback(list(kind=kind,text=msg,project_id=p,version_id=v))
     session$sendCustomMessage("srmaTargetOutcomeSaveButton",
       list(id="evidence_synthesis_execute",label="Generate synthesis outputs",disabled=FALSE))
     if(identical(kind,"error"))notice(msg)
   }
   x<-packet()
   if(is.null(x)||!identical(x$synthesis_plan$status,"HUMAN_CONFIRMED")){
     reset("error","Save the Step 4 synthesis plan before generating outputs.")
     return()
   }
   if(is.list(x$synthesis_execution)&&identical(x$synthesis_execution$status,"AUTO_PREPARED")&&
      isTRUE(x$.synthesis_execution_fresh)){
     reset("ok","Already generated for this Evidence Version. Download the outputs below; no additional run is needed.")
     return()
   }
   synthesis_execution_feedback(list(kind="ok",text="Generating source-indexed synthesis outputs…",
     project_id=p,version_id=v))
   result<-safe(request("evidence_edit",list(project_id=p,version_id=v,
     table="__SYNTHESIS_EXECUTION__",row_id="__EXECUTE__",values=list())))
   if(is.null(result)){
     msg<-as.character(notice() %||b2% "")
     reset("error",paste0("Synthesis execution failed: ",if(nzchar(msg))msg else
       "See the App log for diagnostics."))
     return()
   }
   if(identical(as.character(result$version_id),v)){
     reset("ok","Synthesis outputs already exist in this Evidence Version.")
     return()
   }
   synthesis_execution_feedback(list(kind="ok",text="Synthesis outputs saved; loading the new Evidence Version…",
     project_id=p,version_id=v))
   saved(result,auto_load=TRUE,return_kind="SYNTHESIS_EXECUTION")
   session$sendCustomMessage("srmaTargetOutcomeSaveButton",
     list(id="evidence_synthesis_execute",label="Generate synthesis outputs",disabled=FALSE))
 })
 output$evidence_synthesis_feedback<-shiny::renderUI({
   f<-synthesis_feedback()
   if(is.null(f)||!identical(as.character(f$version_id),as.character(input$version))||
      !identical(as.character(f$project_id),as.character(input$project)))return(NULL)
   shiny::tags$small(style=paste0("display:block;margin:8px 0;color:",
     if(identical(f$kind,"error"))"#b91c1c" else "#166534",";"),as.character(f$text))
 })
 shiny::observeEvent(input$evidence_synthesis_save,{
   p<-as.character(input$project %||b2% "")
   v<-as.character(input$version %||b2% "")
   done<-function(kind,msg){
     synthesis_feedback(list(kind=kind,text=msg,version_id=v,project_id=p))
     session$sendCustomMessage("srmaSynthesisPlanButton",list(disabled=FALSE,label="Save synthesis plan"))
     notice(msg)
   }
   x<-packet()
   if(is.null(x)||!nzchar(p)||!nzchar(v)||
      !identical(x$human_outcome_harmonization$status,"HUMAN_CONFIRMED")){
     done("error","Save and load the Step 3 common outcomes before planning synthesis.")
     return()
   }
   idx<-tryCatch(web_b2_synthesis_catalog(x),error=function(e)NULL)
   if(is.null(idx)||!length(idx)){
     done("error","The saved common outcome variables could not be loaded.")
     return()
   }
   strata<-do.call(base::c,lapply(idx,function(a)unname(a$strata)))
   names(strata)<-vapply(strata,function(s)as.character(s$id),character(1))
   decisions<-lapply(strata,function(s){
     id<-paste0("evidence_synthesis_mode_",substr(digest::digest(s$id,algo="sha256"),1L,14L))
     list(analysis_group=as.character(s$id),mode=as.character(input[[id]] %||b2% ""))
   })
   if(any(!vapply(decisions,function(a)a$mode %in% c("NARRATIVE","RECONCILE","QUANTITATIVE_CANDIDATE"),logical(1)))){
     done("error","Choose a synthesis route for every timepoint-specific set.")
     return()
   }
   if(any(vapply(seq_along(decisions),function(i)
      identical(decisions[[i]]$mode,"QUANTITATIVE_CANDIDATE")&&
      !isTRUE(strata[[i]]$quantitative_candidate),logical(1)))){
     done("error","A quantitative candidate must contain at least two compatible study results.")
     return()
   }
   synthesis_feedback(list(kind="ok",text="Saving Human synthesis plan…",version_id=v,project_id=p))
   saved_version<-safe(request("evidence_edit",list(project_id=p,version_id=v,
     table="__SYNTHESIS_PLAN__",row_id="__SYNTHESIS__",
     values=list(decisions_json=as.character(jsonlite::toJSON(decisions,auto_unbox=TRUE))))))
   if(is.null(saved_version)){
     problem<-as.character(notice() %||b2% "")
     done("error",paste0("Synthesis plan not saved: ",if(nzchar(problem))problem else "Check Terminal 2 App log."))
     return()
   }
   synthesis_feedback(list(kind="ok",text="Synthesis plan saved. Loading Evidence Version…",version_id=v,project_id=p))
   saved(saved_version,auto_load=TRUE,return_kind="SYNTHESIS_PLAN")
   session$sendCustomMessage("srmaSynthesisPlanButton",list(disabled=FALSE,label="Save synthesis plan"))
 })

 shiny::observeEvent(input$evidence_harm_save,{
  version<-as.character(input$version %||b2% "")
  project<-as.character(input$project %||b2% "")
  done<-function(kind,msg) {
    target_save_feedback("harmonization",kind,msg,version,project)
    target_save_button("evidence_harm_save","Save common outcomes")
  }
  x<-packet()
  if(is.null(x)||!nzchar(project)||!nzchar(version)||
      !identical(x$stage,"EXTRACTION")||
      !identical(x$result_inventory_selection$status,"HUMAN_SELECTED")){
    done("error","Save the Step 2 selected outcomes and load that Evidence Version before saving common outcomes.")
    return()
  }
  if(!isTRUE(input$evidence_harm_confirm)){
    done("error","Check 'I checked the outcome definitions and timepoints' before saving common outcomes.")
    return()
  }
  groups<-tryCatch(web_b2_result_selected_groups(x),error=function(e)NULL)
  if(is.null(groups)||!length(groups)){
    done("error","No valid selected outcomes were found in this Evidence Version. Reload Step 2.")
    return()
  }
  previous<-x$human_outcome_harmonization$mappings %||b2% list()
  prior<-stats::setNames(previous,vapply(previous,function(a)as.character(a$group_id),character(1)))
  mappings<-lapply(groups,function(g){
    key<-g$id
    list(group_id=key,
      source_row_id=as.character(input[[paste0("evidence_harm_src_",key)]] %||b2% ""),
      final_name=as.character(input[[paste0("evidence_harm_name_",key)]] %||b2% ""),
      final_definition=as.character(input[[paste0("evidence_harm_def_",key)]] %||b2% ""),
      analysis_window=as.character(input[[paste0("evidence_harm_window_",key)]] %||b2% ""),
      effect_measure=as.character(if(nzchar(as.character(
        (prior[[key]] %||b2% list())$effect_measure %||b2% "")))
        prior[[key]]$effect_measure else
        web_b2_resolve_effect_measure(g,as.character(input[[paste0("evidence_harm_src_",key)]] %||b2% ""))$measure),
      meta_analysis_role=as.character(input[[paste0("evidence_harm_role_",key)]] %||b2% ""),
      parent_analysis_group=as.character(input[[paste0("evidence_harm_parent_",key)]] %||b2% ""))
  })
  # Validate locally before the backend so each error is attached to Step 3,
  # not just the distant shared notification. The backend remains authoritative.
  for(i in seq_along(mappings)){
    a<-mappings[[i]]
    name<-trimws(a$final_name)
    definition<-trimws(a$final_definition)
    window<-trimws(a$analysis_window)
    role<-a$meta_analysis_role
    measure<-a$effect_measure
    label<-as.character(groups[[i]]$reported_name %||b2% paste("Outcome",i))
    if(!nzchar(name)||!nzchar(definition)||!nzchar(window)){
      done("error",paste0("Complete common name, operational definition and timepoint for: ",label))
      return()
    }
    # Missing reported measures stay pending until Human Numeric Verification.
    # Never force the reviewer to invent an effect measure or choose Narrative only.
    if(nzchar(measure)&&!measure %in% c("RR","OR","HR","RD","MD","SMD")){
      done("error",paste0("Unsupported source effect measure for: ",label))
      return()
    }
    # Unreported timepoints remain pending in Human Numeric Verification.
    # No forced Narrative-only selection or invented follow-up timepoint.
  }
  target_save_feedback("harmonization","ok","Validating and saving common outcomes…",version,project)
  v<-safe(request("evidence_edit",list(project_id=project,version_id=version,
    table="__HUMAN_OUTCOME_HARMONIZATION__",row_id="__HARMONIZATION__",
    values=list(mappings_json=as.character(jsonlite::toJSON(mappings,auto_unbox=TRUE))))))
  if(is.null(v)){
    problem<-as.character(notice() %||b2% "")
    if(!nzchar(problem))problem<-"The save was rejected. Check the Terminal 2 App log."
    done("error",paste0("Common outcomes not saved: ",problem))
    return()
  }
  target_save_feedback("harmonization","ok","Common outcomes saved. Opening the new Evidence Version…",version,project)
  saved(v,auto_load=TRUE,return_kind="HARMONIZATION")
  target_save_button("evidence_harm_save","Save common outcomes")
 })
 output$evidence_reviewer_summary<-shiny::renderUI({
  x<-packet();table<-input$evidence_table
  if(is.null(x) || is.null(table) || !table %in% c("Reviewer_1","Reviewer_2","Conflict_Resolution") ||
     is.null(x$tables[[table]]))return(NULL)
  field<-if(identical(table,"Conflict_Resolution"))"final_decision" else "decision"
  is_ai<-identical(table,"Reviewer_1")
  s<-if(is_ai && !is.null(x$ai_screening_aggregate))
    unlist(x$ai_screening_aggregate,use.names=TRUE)
   else web_b2_screening_counts(x$tables[[table]],human_only=TRUE,decision_field=field)
  if(identical(table,"Reviewer_2")) {
    # Count ONLY the AI Include/Unclear selective review set, not AI-only exclusions.
    z<-web_b2_selective_review_targets(x)
    policy_ids<-as.character(unlist(x$screening_excel_review$policy_excluded_ids %||b2% list(),use.names=FALSE))
    human_ids<-setdiff(z$review_ids,policy_ids)
    human_rows<-Filter(function(r)!is.null(r$.actor) &&
       as.character(r$screening_id %||b2% "")%in%human_ids,x$tables$Reviewer_2)
    s<-c(Total=as.integer(length(human_ids)),
         Human_entries=as.integer(length(human_rows)),
         Include=sum(vapply(human_rows,function(r)identical(r$decision,"Include"),logical(1))),
         Exclude=sum(vapply(human_rows,function(r)identical(r$decision,"Exclude"),logical(1))),
         Uncertain=0L,Pending=as.integer(length(human_ids)-length(human_rows)),Other=0L)
  }
  lab<-c(Reviewer_1="AI Reviewer 1",Reviewer_2="Human Reviewer 2",Conflict_Resolution="Human adjudication")
  web_b2_screening_summary_panel(
   paste0(lab[[table]],if(is_ai)" — AI decision summary" else " — Human decision summary"),
   if(is_ai) "AI Reviewer 1 screening recommendations; these are not Human decisions."
   else "Only explicitly recorded Human decisions are counted; AI-only exclusions have no Human actor.",
   s,show_human=!is_ai)
 })
 shiny::observe({x<-packet();shiny::req(x)
  tables<-setdiff(names(x$tables),"Conflict_Resolution")
  if(is.null(tables))tables<-character()
  labs<-unname(names_table[tables])
  if(length(labs)){miss<-is.na(labs);labs[miss]<-tables[miss]}
  shiny::updateSelectInput(session,"evidence_table",
    choices=if(length(tables))stats::setNames(tables,labs) else character(),
    selected=shiny::isolate(input$evidence_table))
  ops<-names(web_b2_operations())[web_b2_operations()==x$stage]
  if(is.null(ops))ops<-character()
  op_labs<-unname(labels[ops])
  # When the saved Evidence Version already contains the completed AI Include
  # classification, default to that completed operation. A generic first
  # choice such as recommend_screening would show a misleading Not started
  # status and an enabled Queue button even though Step 2 is complete.
  # Preserve the other available operations for later Human-approved steps.
  selected_op<-if(identical(x$stage,"EXTRACTION") &&
      identical(x$targeted_verification_source,"CANONICAL_R25B")) {
    "finalize_extraction"
  } else if(identical(x$stage,"EXTRACTION") &&
      identical(x$advisory_source,"CANONICAL_R25A")) {
    "prepare_targeted_verification"
  } else if(identical(x$stage,"EXTRACTION") &&
      length(x$tables$AI_study_characteristics %||b2% list())>0L) {
    "generate_scope_advisory"
  } else if(!is.null(design_summary()) &&
      "recommend_study_design" %in% ops) {
    "recommend_study_design"
  } else {
    shiny::isolate(input$evidence_operation)
  }
  if(!length(selected_op) || is.na(selected_op[[1L]]) ||
     !(selected_op[[1L]] %in% ops))
    selected_op<-if(length(ops))ops[[1L]] else character()
  shiny::updateSelectInput(session,"evidence_operation",
    choices=if(length(ops))stats::setNames(ops,op_labs) else character(),
    selected=selected_op)
 })
 # Include COMPLETE results from this version AND lineage-verified source jobs
 # embedded in the selected saved Evidence Version's immutable history.
 # The cached project view still polls every 2 seconds for job state updates.
 # Do NOT re-send an unchanged selectInput choice list: Shiny will close an
 # open native dropdown whenever updateSelectInput() is called.
 last_job_choices<-shiny::reactiveVal(NULL)
 shiny::observe({
  x<-packet();shiny::req(x,input$project,input$version)
  view<-job_view()
  shiny::req(view,!is.null(view$jobs))
  all_jobs<-view$jobs
  ops<-names(web_b2_operations())
  now<-all_jobs[all_jobs$version_id==input$version &
    all_jobs$operation %in% ops & all_jobs$state=="COMPLETE",,drop=FALSE]
  prior<-web_b2_ui_saved_job_history(x,all_jobs)
  candidates<-unique(c(as.character(now$id),as.character(prior$id)))
  found<-all_jobs[all_jobs$id %in% candidates & all_jobs$state=="COMPLETE",,drop=FALSE]
  labels_for_job<-function(j) {
    id<-as.character(j$id)
    op<-as.character(j$operation)
    name<-unname(labels[op]);if(is.na(name))name<-op
    stamp<-as.character(j$created_at %||b2% "")
    if(id %in% prior$id)
      paste0("SAVED — ",name," — ",stamp," [",substr(id,1,10),"]")
    else paste0("READY TO SAVE — ",name," — ",stamp," [",substr(id,1,10),"]")
  }
  choices<-if(nrow(found))stats::setNames(as.character(found$id),
    vapply(seq_len(nrow(found)),function(i)labels_for_job(found[i,,drop=FALSE]),character(1))) else character()
  chosen<-shiny::isolate(input$evidence_job)
  # Prefer the latest unsaved complete job rather than older saved history.
  ready_ids<-as.character(found$id[found$version_id==input$version &
    !found$id %in% prior$id])
  if(length(ready_ids) && (!length(chosen) ||
    chosen %in% prior$id || !chosen %in% candidates)) {
    chosen<-ready_ids[[1L]]
  } else if(!length(chosen)||!chosen %in% candidates) {
    preferred<-found$id[found$operation==(input$evidence_operation %||b2% "")]
    chosen<-if(length(preferred))as.character(preferred[[1L]]) else if(nrow(found))as.character(found$id[[1L]]) else character()
  }
  # Only refresh this dropdown when its option labels/IDs or selected version
  # actually change. Ordinary two-second status polling must not interrupt
  # user interaction or reset a manually chosen completed Job.
  signature<-list(project=as.character(input$project),
    version=as.character(input$version),choices=choices)
  if(!identical(signature,shiny::isolate(last_job_choices()))) {
    last_job_choices(signature)
    shiny::updateSelectInput(session,"evidence_job",choices=choices,selected=chosen)
  }
 })
 output$evidence_saved_job_note<-shiny::renderUI({
  x<-packet();shiny::req(x)
  history<-saved_history()
  if(nrow(history)&&length(input$evidence_job)==1L&&
     input$evidence_job %in% history$id)
    return(shiny::p(style="color:#15803d;",
      paste0("This completed Job ",substr(input$evidence_job,1,10),
        " was already saved into the selected Evidence Version's lineage. Do not save it again.")))
  NULL
 })
 latest_evidence_job<-shiny::reactive({
  shiny::req(input$project,input$version,input$evidence_operation)
  if(!nzchar(input$evidence_operation))return(NULL)
  x<-job_view()
  if(is.null(x)||is.null(x$jobs)||!nrow(x$jobs))return(NULL)
  j<-x$jobs[x$jobs$version_id==input$version&x$jobs$operation==input$evidence_operation,,drop=FALSE]
  if(!nrow(j))return(NULL)
  j[1,,drop=FALSE]
 })
 output$evidence_job_status<-shiny::renderUI({
  # A completed result is attached to the preceding source version. On its
  # saved descendant, show the persisted classification rather than Not started.
  d<-design_summary()
  if(identical(input$evidence_operation,"recommend_study_design")&&!is.null(d)) {
    hist<-saved_history()
    hit<-hist[hist$operation=="recommend_study_design",,drop=FALSE]
    label<-if(nrow(hit))paste0(" | Job ",substr(hit$id[[nrow(hit)]],1,10)) else ""
    return(shiny::div(style="border-left:4px solid #15803d;background:#f0fdf4;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",
      shiny::strong("Completed & Saved — ",d$total," / ",d$total," classified"),
      shiny::p(paste0("Evidence Version ",substr(input$version %||b2% "",1,10),label,
        ". Continue to Step 3; no further AI call is needed."))))
  }
  j<-latest_evidence_job()
  if(is.null(j))return(shiny::div(
    style="border-left:4px solid #6b7280;background:#f9fafb;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",
    shiny::strong("Not started"),shiny::span(" — Queue the selected processing step when ready.")
  ))
  state<-as.character(j$state[[1L]])
  msg<-as.character(j$message[[1L]] %||b2% "")
  progress<-as.integer(j$progress[[1L]] %||b2% 0L)
  is_screening<-identical(as.character(j$operation[[1L]]),"recommend_screening")
  is_design<-identical(as.character(j$operation[[1L]]),"recommend_study_design")
  cp<-if(is_screening)screening_checkpoint_progress(j) else if(is_design)design_checkpoint_progress(j) else NULL
  long_note<-if(is_screening)shiny::p(style="margin-top:8px;","AI title/abstract screening is usually the longest processing step. Large reviews may take several hours. You may leave this page; completed decisions are checkpointed and preserved.") else if(is_design)shiny::p(style="margin-top:8px;","Only the AI Reviewer 1 Include set is classified. Each successful eight-record batch is preserved. The original AI decisions remain unchanged.") else NULL
  if(state=="QUEUED")return(shiny::div(style="border-left:4px solid #2563eb;background:#eff6ff;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",shiny::strong("Queued — waiting for worker"),shiny::p(msg),long_note))
  if(state=="RUNNING") {
   headline<-"Processing…"
   detail<-msg
   progress_bar<-NULL
   checkpoint_note<-NULL
   if(is_design){
     if(is.null(cp)) {
       headline<-"AI Include study-design classification — awaiting first completed batch"
       detail<-"No completed classification checkpoint yet; no processed-record count is available."
     } else {
       headline<-paste0("AI Include study-design classification — ",cp$completed," / ",cp$total,
         " records (",format(cp$pct,nsmall=1),"%)")
       detail<-paste0("Last completed batch: ",format(cp$mtime,"%Y-%m-%d %H:%M:%S"))
       progress_bar<-shiny::tags$div(style="height:9px;background:#dbeafe;border-radius:5px;overflow:hidden;margin:7px 0;",
         shiny::tags$div(style=sprintf("height:100%%;width:%.1f%%;background:#2563eb;",cp$pct)))
     }
   }
   if(is_screening){
    if(is.null(cp)){
     headline<-"AI screening in progress — waiting for checkpoint data"
     detail<-"This job's checkpoint is not yet available to the web view; this is not a processed-record count."
    } else {
     headline<-if(is.finite(cp$total)&&cp$total>0)
       paste0("AI screening in progress — ",cp$completed," / ",cp$total,
              " records (",format(cp$pct,nsmall=1),"%)") else
       paste0("AI screening in progress — ",cp$completed," decisions checkpointed")
     if(!is.na(cp$mtime))detail<-paste0("Last checkpoint: ",format(cp$mtime,"%Y-%m-%d %H:%M:%S"))
     checkpoint_note<-screening_checkpoint_note(cp)
     if(is.finite(cp$pct)){
      width<-max(0,min(100,cp$pct))
      progress_bar<-shiny::tags$div(
       style="height:9px;background:#dbeafe;border-radius:5px;overflow:hidden;margin:7px 0;",
       shiny::tags$div(style=sprintf("height:100%%;width:%.1f%%;background:#2563eb;",width))
      )
     }
    }
   }
   return(shiny::div(style="border-left:4px solid #2563eb;background:#eff6ff;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",
                     shiny::strong(headline),progress_bar,shiny::p(detail),checkpoint_note,long_note))
  }
  if(state=="COMPLETE")return(shiny::div(style="border-left:4px solid #15803d;background:#f0fdf4;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",shiny::strong("✓ Processing completed"),shiny::p("Select the completed result below and save it as a new evidence version to continue.")))
  if(state=="USER_ACTION_REQUIRED")return(shiny::div(style="border-left:4px solid #d97706;background:#fff7ed;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",shiny::strong("! Action required"),shiny::p(msg)))
  if(state=="FAILED") {
   preserved<-if(!is.null(cp)&&cp$completed>0)shiny::p(paste0(
     "This job contains ",cp$completed," checkpointed ",
     if(is_design)"design classifications" else "decisions",
     if(is.finite(cp$total))paste0(" out of ",cp$total) else "",
     ". A new job can reuse them only when the resume conditions are met.")) else NULL
   return(shiny::div(style="border-left:4px solid #b91c1c;background:#fef2f2;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",shiny::strong("✕ Processing failed"),shiny::p(msg),preserved,if(is_screening)screening_checkpoint_note(cp) else NULL,shiny::p("Correct the reported problem, then queue this step again.")))
  }
  shiny::div(style="border-left:4px solid #6b7280;background:#f9fafb;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",shiny::strong(state),shiny::p(msg))
 })
 shiny::observe({
  j<-latest_evidence_job()
  d<-design_summary()
  h<-saved_history()
  op<-input$evidence_operation %||b2% ""
  if(identical(op,"recommend_study_design")&&!is.null(d))
    set_run_button(TRUE,"Completed & Saved — Step 3 next")
  else if(is.null(j))set_run_button(FALSE,"Queue processing")
  else {
    state<-as.character(j$state[[1L]])
    if(state=="QUEUED")set_run_button(TRUE,"Queued…")
    else if(state=="RUNNING")set_run_button(TRUE,"Processing…")
    else if(state=="COMPLETE")set_run_button(TRUE,"Completed — save result")
    else if(state=="FAILED")set_run_button(FALSE,"Retry processing")
    else set_run_button(FALSE,"Queue processing")
  }
  selected_id<-input$evidence_job %||b2% ""
  is_prior<-length(selected_id)==1L&&nzchar(selected_id)&&selected_id %in% h$id
  ready<-!is.null(j)&&identical(as.character(j$state[[1L]]),"COMPLETE")&&
    identical(as.character(j$id[[1L]]),selected_id)&&!is_prior
  # A completed job selected from the original version is eligible for save
  # only on its own source version, never on an already-saved descendant.
  if(!ready&&length(selected_id)==1L&&nzchar(selected_id)&&!is_prior) {
    # Another complete job may be selected in the dropdown; check it exactly.
    view<-job_view()
    if(!is.null(view)&&!is.null(view$jobs)) {
      k<-view$jobs
      hit<-k[k$id==selected_id & k$version_id==input$version &
        k$state=="COMPLETE" & k$operation %in% names(web_b2_operations()),,drop=FALSE]
      ready<-nrow(hit)==1L
    }
  }
  if(is_prior)set_save_button(TRUE,"Already saved — no action needed")
  else if(ready)set_save_button(FALSE,"Save result as new evidence version")
  else set_save_button(TRUE,"No unsaved completed result")
 })
 shiny::observe({x<-packet();shiny::req(x,input$evidence_table);rows<-x$tables[[input$evidence_table]];queue<-input$evidence_queue %||b2% "all"
  if(queue=="ai_priority" && identical(input$evidence_table,"Reviewer_2")) {
    z<-web_b2_selective_review_targets(x)
    rows<-Filter(function(a)as.character(a$screening_id %||b2% "")%in%z$review_ids,rows)
  }
  if(queue=="pending")rows<-Filter(function(a)is.null(a$.actor),rows)
  if(queue=="uncertain")rows<-Filter(function(a){i<-which(vapply(x$tables$Reviewer_1,function(r)identical(r$screening_id,a$screening_id),logical(1)));disagree<-FALSE
   if(length(i)==1L){aa<-x$tables$Reviewer_1[[i]];bb<-x$tables$Reviewer_2[[i]];disagree<-!is.null(aa$.actor)&&!is.null(bb$.actor)&&(!identical(aa$decision,bb$decision)||!identical(aa$exclusion_reason,bb$exclusion_reason))}
   disagree||grepl("uncertain|unclear|awaiting|clarification",paste(unlist(a),collapse=" "),ignore.case=TRUE)
  },rows)
  q<-tolower(input$evidence_filter %||b2% "");if(nzchar(q))rows<-Filter(function(a)grepl(q,tolower(paste(unlist(a),collapse=" ")),fixed=TRUE),rows);shiny::updateSelectInput(session,"evidence_row",choices=if(length(rows))stats::setNames(vapply(rows,`[[`,character(1),".row_id"),vapply(rows,function(a)paste(a$screening_id %||b2% "",a$title %||b2% a$outcome_name %||b2% a$domain %||b2% a$question_id %||b2% ""),character(1))) else character(),selected=shiny::isolate(input$evidence_row))})
 output$evidence_details<-shiny::renderTable({a<-selected();keys<-names(a)[!startsWith(names(a),".")];data.frame(Field=gsub("_"," ",keys),Value=vapply(a[keys],function(v)paste(unlist(v),collapse="; "),character(1)))})
 output$evidence_context<-shiny::renderUI({a<-selected();shiny::tagList(shiny::strong(if(identical(a$.source,"AI RECOMMENDATION"))"AI RECOMMENDATION — HUMAN REVIEW REQUIRED / UNVERIFIED" else a$.source %||b2% "SYSTEM OUTPUT"),shiny::p(paste("Report:",a$screening_id %||b2% ""," | Source location:",a$source_location %||b2% "")),shiny::p(a$title %||b2% ""),shiny::p(a$abstract %||b2% ""),shiny::p(a$agent_rationale %||b2% a$source_evidence %||b2% a$evidence_summary %||b2% ""),shiny::p(paste("Bias instrument:",a$rob_tool %||b2% "Not applicable")),shiny::p(if(!is.null(a$.human_review))paste("Proposal review:",a$.human_review$decision,a$.human_review$rationale) else ""),shiny::p("Registered PDFs and canonical outputs are available under Results / Artifacts."))})
 field_names<-shiny::reactive({
  x<-packet();shiny::req(x)
  spec<-web_b2_edit_spec()[[input$evidence_table]]
  if(is.null(spec))return(character())
  fields<-spec$fields %||b2% unlist(x$editable[[input$evidence_table]])
  # Permit identity definition only for a NEW Human-owned outcome row.
  if(identical(input$evidence_table,"Target_Outcome_Verification")){
   rid<-as.character(input$evidence_row %||b2% "")
   rows<-x$tables$Target_Outcome_Verification %||b2% list()
   matches<-Filter(function(a)identical(a$.row_id,rid),rows)
   if(length(matches)==1L&&identical(matches[[1L]]$.source,"HUMAN ENTRY REQUIRED"))
    fields<-unique(c(fields,"outcome_name"))
   if(length(matches)==1L&&isTRUE(matches[[1L]]$.web_harmonized_result))
    fields<-unique(c(fields,"source_location","source_evidence","value_status",
      "human_verified_outcome_definition","human_verified_timepoint",
      "human_verified_data_type","human_verified_intervention_events",
      "human_verified_intervention_total","human_verified_comparator_events",
      "human_verified_comparator_total","human_verified_intervention_mean",
      "human_verified_intervention_sd","human_verified_intervention_n",
      "human_verified_comparator_mean","human_verified_comparator_sd",
      "human_verified_comparator_n","human_verified_unit",
      "human_verified_effect_measure","human_verified_effect_estimate",
      "human_verified_lower_ci","human_verified_upper_ci",
      "human_verified_adjusted","human_verified_adjustment_variables",
      "human_verified_source_location","human_outcome_notes"))
  }
  fields
 })
 # Only an explicit UI click copies this version's existing R/25 proposal
 # into the browser's UNPERSISTED Human form. The normal human-save and
 # independent-verification contracts remain the exclusive write path.
 output$evidence_ai_study_draft_ui<-shiny::renderUI({
  x<-packet()
  if(is.null(x) || !identical(input$evidence_table,"Target_Study_Verification") ||
     !identical(x$stage,"EXTRACTION"))return(NULL)
  r<-selected()
  draft<-web_b2_ui_ai_study_draft(x,r,field_names())
  if(is.null(draft))return(NULL)
  shiny::div(style="border:1px solid #a6c5e4;background:#f1f7fe;border-radius:9px;padding:13px 15px;margin:14px 0;",
   shiny::strong("AI extraction proposal available — review before use"),
   shiny::p(style="margin:6px 0 9px;",
    paste0(length(draft$values)," matching study-characteristics fields from saved AI extraction for ",draft$screening_id,".")),
   shiny::actionButton("evidence_ai_study_draft_load","Load AI values into editable draft",class="btn-primary"),
   shiny::p(style="font-size:.9em;color:#475569;margin:8px 0 0;",
    "This only fills unsaved browser fields. It does not accept, approve, verify or save any result. Existing unsaved edits to matching fields will be replaced when clicked. Check the original PDF and enter page/source evidence before saving a Human entry."))
 })
 shiny::observeEvent(input$evidence_ai_study_draft_load,{
  shiny::req(identical(input$evidence_table,"Target_Study_Verification"),
             isTRUE(control_visibility()$save))
  x<-packet()
  shiny::req(x,identical(x$stage,"EXTRACTION"))
  r<-selected()
  fields<-as.character(field_names())
  draft<-web_b2_ui_ai_study_draft(x,r,fields)
  shiny::req(!is.null(draft),identical(draft$screening_id,r$screening_id))
  for(k in names(draft$values)) {
   i<-match(k,fields)
   if(!is.na(i))shiny::updateTextAreaInput(session,paste0("evf_",i),
                                          value=draft$values[[k]])
  }
  notice(paste0(length(draft$values)," AI proposal values loaded into the unsaved Human draft for ",draft$screening_id,
    ". Compare against the PDF, add source location/evidence, then explicitly save as a new Evidence Version."))
 })
 # Each proposal row has a stable saved .row_id; selection is always
 # explicit, including when an outcome has multiple timepoints or measures.
 output$evidence_ai_outcome_draft_ui<-shiny::renderUI({
  x<-packet()
  if(is.null(x)||!identical(input$evidence_table,"Target_Outcome_Verification")||
     !identical(x$stage,"EXTRACTION"))return(NULL)
  r<-selected()
  drafts<-web_b2_ui_ai_outcome_drafts(x,r,field_names())
  if(is.null(drafts)) {
    diagnosis<-web_b2_ui_outcome_visibility(x,r)
    if(is.null(diagnosis))return(NULL)
    show_labels<-head(diagnosis$labels,8L)
    return(shiny::div(style="border:1px solid #d4dce7;background:#f8fafc;border-radius:9px;padding:12px 15px;margin:14px 0;",
      shiny::strong("AI outcome suggestions — no candidate for this report"),
      shiny::p(diagnosis$message),
      if(length(show_labels))shiny::tags$ul(lapply(show_labels,function(v)shiny::tags$li(v))) else NULL,
      shiny::p("The saved AI outputs remain available in the read-only extraction review section.")))
  }
  labels<-vapply(drafts$candidates,function(z)z$label,character(1))
  choices<-c("Select one AI extraction source row"="",
    stats::setNames(names(drafts$candidates),labels))
  shiny::div(style="border:1px solid #a6c5e4;background:#f1f7fe;border-radius:9px;padding:13px 15px;margin:14px 0;",
   shiny::strong("One-by-one AI outcome reconciliation — Human review required"),
   shiny::p(paste0(length(drafts$candidates)," saved source row(s) for report ",
     drafts$screening_id,". These are NOT automatically equivalent to ‘",
     drafts$outcome_name,"’.")),
   shiny::selectInput("evidence_ai_outcome_choice","Saved AI source outcome / timepoint",
     choices=choices,selected=""),
   shiny::uiOutput("evidence_ai_outcome_fieldmap_ui"),
   shiny::actionButton("evidence_ai_outcome_load","Load selected AI fields into unsaved Human draft",class="btn-primary"),
   shiny::p(style="font-size:.9em;color:#475569;margin:8px 0 0;",
    "This never creates a Human approval or saves an Evidence Version. Where labels differ or combine multiple endpoints, select each value independently and confirm its PDF correspondence. An outcome requires its own Human row; use Add another outcome/timepoint for additional endpoints."))
 })
 output$evidence_ai_outcome_fieldmap_ui<-shiny::renderUI({
  if(!identical(input$evidence_table,"Target_Outcome_Verification"))return(NULL)
  x<-packet();if(is.null(x)||!identical(x$stage,"EXTRACTION"))return(NULL)
  r<-selected();drafts<-web_b2_ui_ai_outcome_drafts(x,r,field_names())
  if(is.null(drafts))return(NULL)
  chosen<-as.character(input$evidence_ai_outcome_choice %||b2% "")
  if(length(chosen)!=1L||is.na(chosen)||!chosen %in% names(drafts$candidates))return(NULL)
  entry<-drafts$candidates[[chosen]]
  if(!length(entry$values))return(shiny::p("This source row has no matching editable proposal fields."))
  if(!isTRUE(entry$requires_manual))return(shiny::p(
    "Exact single-outcome label. The nonempty source fields will be loaded into the unsaved draft; verify every value against the PDF."))
  # Scope confirmation controls to the exact Evidence Version, Human row and AI source.
  key<-gsub("[^A-Za-z0-9_]","_",paste(input$version,input$evidence_row,chosen,sep="_"))
  labs<-vapply(names(entry$values),function(k)paste0(gsub("_"," ",k),
    " = ",substr(gsub("[\\r\\n]+"," ",entry$values[[k]]),1L,125L)),character(1))
  shiny::div(style="border-left:4px solid #b45309;margin:10px 0;padding:10px 12px;",
   shiny::strong("Ambiguous or different outcome labels — field-by-field selection required"),
   shiny::p(paste0("AI source label: ",entry$source_outcome_name,
     " | Human target label: ",drafts$outcome_name)),
   shiny::p(paste0("Saved AI source row ID: ",chosen)),
   shiny::p("The AI row may contain one number set for several named outcomes. Do not select a number unless the PDF proves it belongs to THIS exact outcome, group and timepoint. Unselected fields remain blank. Record the source row ID in Human outcome notes for traceability."),
   shiny::checkboxGroupInput(paste0("evidence_ai_outcome_selected_fields_",key),
      "Individually supported AI values to load",
      choices=stats::setNames(names(labs),labs),selected=character()),
   shiny::checkboxInput(paste0("evidence_ai_outcome_source_confirm_",key),
      "I matched every selected field independently to this Human outcome in the source PDF",FALSE))
 })
 shiny::observeEvent(input$evidence_ai_outcome_load,{
  shiny::req(identical(input$evidence_table,"Target_Outcome_Verification"),
             isTRUE(control_visibility()$save))
  x<-packet();shiny::req(x,identical(x$stage,"EXTRACTION"))
  r<-selected();fields<-as.character(field_names())
  drafts<-web_b2_ui_ai_outcome_drafts(x,r,fields)
  shiny::req(!is.null(drafts))
  chosen<-as.character(input$evidence_ai_outcome_choice %||b2% "")
  shiny::req(length(chosen)==1L,!is.na(chosen),nzchar(chosen),
             chosen %in% names(drafts$candidates))
  entry<-drafts$candidates[[chosen]]
  if(!length(entry$values)){
   notice("The AI source row has no usable matching fields; nothing was imported.")
   return()
  }
  selected_fields<-names(entry$values)
  if(isTRUE(entry$requires_manual)){
   key<-gsub("[^A-Za-z0-9_]","_",paste(input$version,input$evidence_row,chosen,sep="_"))
   selected_fields<-as.character(input[[paste0("evidence_ai_outcome_selected_fields_",key)]] %||b2% character())
   confirmed<-isTRUE(input[[paste0("evidence_ai_outcome_source_confirm_",key)]])
   if(!confirmed||!length(selected_fields)||
      anyNA(selected_fields)||anyDuplicated(selected_fields)||
      !all(selected_fields %in% names(entry$values))){
     notice("For ambiguous/different endpoints, select individually verified fields and confirm their PDF mapping before importing.")
     return()
   }
  }
  # Clear any stale proposal values before loading the SELECTED fields only.
  # This is a browser draft action, not a persisted decision or verification.
  for(k in drafts$clear_fields) {
   i<-match(k,fields)
   if(!is.na(i))shiny::updateTextAreaInput(session,paste0("evf_",i),value="")
  }
  for(k in selected_fields) {
   i<-match(k,fields)
   if(!is.na(i))shiny::updateTextAreaInput(session,paste0("evf_",i),
       value=entry$values[[k]])
  }
  vi<-match("value_status",fields)
  if(!is.na(vi))shiny::updateSelectInput(session,paste0("evf_",vi),selected="")
  notice(paste0(length(selected_fields)," AI proposal fields loaded into UNSAVED draft for report ",
     drafts$screening_id,". Source outcome: ",entry$source_outcome_name,
     ". Verify the single endpoint, counts, denominators, timepoint and PDF citation; then explicitly save and independently verify."))
 })
 # Context-aware review actions: show only when the current stage/table/row
 # permits that action. The canonical service still enforces every write gate.
 control_visibility<-shiny::reactive({
  x<-packet();table<-input$evidence_table %||b2% ""
  id<-input$evidence_row %||b2% ""
  empty<-web_b2_ui_review_control_visibility("","",FALSE)
  if(is.null(x)||!nzchar(table)||!nzchar(id)||
     is.null(x$tables[[table]]))return(empty)
  matches<-Filter(function(r)identical(as.character(r$.row_id %||b2% ""),id),
                  x$tables[[table]])
  if(length(matches)!=1L)return(empty)
  spec<-web_b2_edit_spec()[[table]]
  editable<-FALSE
  if(!is.null(spec) && identical(spec$stage,x$stage)){
    f<-spec$fields %||b2% unlist(x$editable[[table]],use.names=FALSE)
    editable<-length(f)>0L
  }
  actor<-as.character(matches[[1L]]$.actor %||b2% "")
  web_b2_ui_review_control_visibility(x$stage,table,TRUE,editable,
    length(actor)==1L && !is.na(actor) && nzchar(trimws(actor)))
 })
 output$evidence_proposal_controls<-shiny::renderUI({
  if(!isTRUE(control_visibility()$proposal))return(NULL)
  shiny::tagList(
   shiny::selectInput("proposal_decision","AI proposal review (does not approve scientific values)",
     c("Choose explicitly"="","Accept for manual entry"="ACCEPT_FOR_MANUAL_ENTRY",
       "Reject"="REJECT","Uncertain"="UNCERTAIN")),
   shiny::textInput("proposal_rationale","Proposal review rationale"),
   shiny::actionButton("proposal_review","Record proposal review"))
 })
 output$evidence_add_outcome_control<-shiny::renderUI({
  if(!isTRUE(control_visibility()$add_outcome))return(NULL)
  shiny::tagList(
   shiny::actionButton("evidence_add_row","Add another outcome/timepoint for selected report"),
   shiny::p(style="font-size:.9em;color:#475569;margin:6px 0 0;",
    "Save the current reviewed outcome first. Adding a row creates a new Evidence Version; unsaved browser edits will not be included. Verify any copied fields in the new row against the intended outcome."))
 })
 output$evidence_save_control<-shiny::renderUI({
  if(input$evidence_table %in% c("Reviewer_1","Reviewer_2"))
    return(shiny::p("Screening decisions are read-only here. Use the selective Human Review Excel above."))
  if(!isTRUE(control_visibility()$save))return(NULL)
  shiny::actionButton("evidence_save","Save human entry as new version")
 })
 output$evidence_verify_controls<-shiny::renderUI({
  if(!isTRUE(control_visibility()$verify))return(NULL)
  shiny::tagList(
   shiny::checkboxInput("evidence_confirm","I independently verified these extracted values against the source",FALSE),
   shiny::textInput("evidence_rationale","Independent verification rationale"),
   shiny::actionButton("evidence_verify","Verify extraction as a second reviewer"))
 })
 output$evidence_pdf_controls<-shiny::renderUI({
  if(!isTRUE(control_visibility()$pdf))return(NULL)
  shiny::tagList(
   shiny::fileInput("evidence_pdf","Full-text PDF for selected report",accept=".pdf"),
   shiny::actionButton("evidence_upload","Save PDF in a new version"))
 })
 output$evidence_fields<-shiny::renderUI({
  x<-packet();a<-selected();fields<-field_names()
  if(!length(fields))return(NULL)
  human_editor<-identical(x$stage,"FULLTEXT") &&
    identical(input$evidence_table,"Author_Final_Review")
  outcome_editor<-identical(x$stage,"EXTRACTION") &&
    identical(input$evidence_table,"Target_Outcome_Verification")
  # The project-specific outcome definition can contain a prose list of
  # endpoints. Such a label must not silently be equated with one event count
  # or effect estimate. The warning changes no data or Human decision.
  ambiguous<-outcome_editor &&
    web_b2_ui_outcome_label_needs_disambiguation(a$outcome_name %||b2% "")
  controls<-lapply(seq_along(fields),function(i){
   k<-fields[i];val<-a[[k]] %||b2% "";choices<-NULL
   if(k=="decision")choices<-unlist(x$choices$decisions)
   if(k %in% c("author_final_decision","final_decision"))
    choices<-if(k=="final_decision")
      c("Include","Exclude","Awaiting full-text clarification") else c("Include","Exclude")
   if(grepl("exclusion_reason$",k))choices<-unlist(x$choices$reasons)
   if(k=="human_include_in_planned_analysis")choices<-c("No","Yes")
   if(k=="audit_status")choices<-c("Confirmed","No overlap","Needs clarification")
   if(k=="value_status")choices<-c("REPORTED","DERIVED","CALCULATED","IMPUTED","MISSING","NOT APPLICABLE")
   if(k=="retrieval_status")choices<-c("Missing","Available","Not retrieved")
   if(k=="human_response")choices<-c("Y","PY","PN","N","NI","NA")
   width<-if(outcome_editor)"100%" else NULL
   if(!is.null(choices))
    shiny::selectInput(paste0("evf_",i),gsub("_"," ",k),
      c("Choose explicitly"="",choices),selected=val,width=width)
   else shiny::textAreaInput(paste0("evf_",i),gsub("_"," ",k),val,
     rows=if(outcome_editor && k %in% c(
      "outcome_name","outcome_definition","source_location","source_evidence",
      "extraction_notes","adjustment_variables","human_outcome_notes",
      "human_verified_outcome_definition","human_verified_source_location"))4 else 2,
     width=width)
  })
  shiny::tagList(
   if(human_editor)shiny::div(
    style="margin:10px 0;padding:9px 12px;border-left:3px solid #6b7280;background:#f8fafc;",
    shiny::strong("Full-text decision — editable"),
    shiny::p(style="margin:4px 0 0;",
     "Select any report, including one previously saved, to revise its Include/Exclude decision, Study ID, or notes. Saving creates a new Evidence Version.")),
   if(ambiguous)shiny::div(
    style="border-left:4px solid #b45309;padding:10px 14px;margin:10px 0 18px;max-width:1080px;",
    shiny::strong("Possible multiple endpoints in one outcome name — check before use"),
    shiny::p(style="margin:6px 0 0;",
     "This outcome label appears to list several endpoints. The AI event counts, denominators, and effect estimates must each refer to one precisely defined endpoint and timepoint. Do not assume a value represents every item in the label."),
    shiny::p(style="margin:4px 0 0;",
     "Check the project's defined outcomes and the source PDF. Do not approve or include this row in planned analysis until the endpoint-to-value correspondence is clear. No AI result has been split or changed.")),
   if(outcome_editor)
     shiny::div(style="width:100%;max-width:1100px;",controls)
   else controls
  )
 })
 shiny::observeEvent(input$proposal_review,saved(safe(request("evidence_review_proposal",list(project_id=input$project,version_id=input$version,table=input$evidence_table,row_id=input$evidence_row,decision=input$proposal_decision,rationale=input$proposal_rationale)))))
 # Adding an outcome creates an immutable version. Auto-load it and select
 # the newly generated row by its exact ID difference (never row order).
 shiny::observeEvent(input$evidence_add_row,{
  x<-packet();shiny::req(x,identical(x$stage,"EXTRACTION"),
    identical(input$evidence_table,"Target_Outcome_Verification"))
  r<-selected();rows<-x$tables$Target_Outcome_Verification
  old_ids<-vapply(rows,function(a)as.character(a$.row_id %||b2% ""),character(1))
  shiny::req(length(old_ids)>0L,any(old_ids==input$evidence_row))
  added<-safe(request("evidence_add_row",list(project_id=input$project,
    version_id=input$version,row_id=input$evidence_row)))
  saved(added,auto_load=TRUE,return_kind="OUTCOME_ADD",
    return_table="Target_Outcome_Verification",return_row=input$evidence_row,
    previous_ids=old_ids,source_screening_id=r$screening_id)
 })
 shiny::observeEvent(input$evidence_save,{
  shiny::req(packet(),input$project,input$version,input$evidence_table,input$evidence_row)
  # Remember the exact selected row, before the new immutable version exists.
  table<-as.character(input$evidence_table)
  row_id<-as.character(input$evidence_row)
  shiny::req(isTRUE(control_visibility()$save))
  fields<-field_names()
  shiny::req(length(fields)>0L)
  values<-stats::setNames(lapply(seq_along(fields),function(i)
   input[[paste0("evf_",i)]] %||b2% ""),fields)
  new_version<-safe(request("evidence_edit",list(project_id=input$project,
   version_id=input$version,table=table,row_id=row_id,values=values)))
  saved(new_version,auto_load=TRUE,return_kind="HUMAN_EDIT",
        return_table=table,return_row=row_id)
 })
 # Stay in the review queue after independent verification; do not mark
 # other rows as verified or repeat the scientific finalization.
 shiny::observeEvent(input$evidence_verify,{
  shiny::req(input$evidence_table,input$evidence_row)
  table<-as.character(input$evidence_table)
  row<-as.character(input$evidence_row)
  result<-safe(request("evidence_verify",list(project_id=input$project,
    version_id=input$version,table=table,row_id=row,
    approved=isTRUE(input$evidence_confirm),rationale=input$evidence_rationale)))
  saved(result,auto_load=TRUE,return_kind="HUMAN_EDIT",
    return_table=table,return_row=row)
 })
 shiny::observeEvent(input$evidence_upload,{
  shiny::req(input$project,input$version,input$evidence_row,input$evidence_pdf)
  report_id<-input$evidence_row
  result<-safe(request("evidence_upload",list(project_id=input$project,
    version_id=input$version,row_id=report_id,upload=input$evidence_pdf)))
  saved(result,auto_load=TRUE,return_table="Author_Final_Review",return_row=report_id,return_kind="PDF")
 })
 shiny::observeEvent(input$evidence_run,{
  shiny::req(input$project,input$version,input$evidence_operation)
  if(identical(input$evidence_operation,"recommend_study_design")&&
     !is.null(design_summary())) {
    set_run_button(TRUE,"Completed & Saved — Step 3 next")
    notice("AI Include classification is already saved in this Evidence Version. Proceed to Step 3; no new AI processing was queued.")
    return()
  }
  id<-safe(request("submit",list(project_id=input$project,version_id=input$version,operation=input$evidence_operation)))
  if(is.null(id))set_run_button(FALSE,"Queue processing")
  else {
    if(identical(input$evidence_operation,"recommend_screening")) {
      all<-safe(request("view",list(project_id=input$project)))
      if(!is.null(all) && !is.null(all$jobs) && nrow(all$jobs)) {
        hit<-all$jobs[all$jobs$id==id,,drop=FALSE]
        if(nrow(hit)==1L && !identical(as.character(hit$version_id[[1L]]),as.character(input$version))) {
          versions<-all$versions
          choices<-stats::setNames(versions$id,paste("Version",versions$version))
          packet(NULL);refresh(refresh()+1L)
          target_version<-as.character(hit$version_id[[1L]])
          session$onFlushed(function() shiny::updateSelectInput(session,"version",
            choices=choices,selected=target_version),once=TRUE)
          notice("A compatible Luna screening version was created automatically; existing decisions will be restored without re-screening.")
        } else notice("AI screening queued. Valid checkpoint decisions are preserved.")
      } else notice("AI screening queued. Valid checkpoint decisions are preserved.")
    } else notice("Processing queued. Save the completed result as a new evidence version.")
  }
 })
 shiny::observeEvent(input$evidence_accept,{
  shiny::req(input$project,input$version,input$evidence_job)
  h<-saved_history()
  if(input$evidence_job %in% h$id){
    notice("This completed job is already saved in the selected Evidence Version's lineage. No duplicate version was created.")
    set_save_button(TRUE,"Already saved — no action needed")
    return()
  }
  view<-safe(request("view",list(project_id=input$project)))
  if(is.null(view)||is.null(view$jobs))return()
  j<-view$jobs
  hit<-j[j$id==input$evidence_job & j$version_id==input$version &
    j$state=="COMPLETE" & j$operation %in% names(web_b2_operations()),,drop=FALSE]
  if(nrow(hit)!=1L){
    notice("This job is not an unsaved completed result of the selected Evidence Version. Reload the latest version.")
    set_save_button(TRUE,"No unsaved completed result")
    return()
  }
  result<-safe(request("evidence_accept",list(project_id=input$project,
    version_id=input$version,job_id=input$evidence_job)))
  if(is.null(result)){
    set_save_button(FALSE,"Save result as new evidence version")
    return()
  }
  if(identical(as.character(hit$operation[[1L]]),"discover_pdf_results"))
    saved(result,auto_load=TRUE,return_kind="DISCOVERY")
  else
    saved(result)
 })
}
