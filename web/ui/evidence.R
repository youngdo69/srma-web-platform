# Human review UI; labels describe decisions, never engine script numbers.
web_b2_ui <- function() shiny::tabPanel("Screening / Evidence",
 shiny::tags$script(shiny::HTML("
Shiny.addCustomMessageHandler('srmaEvidenceRunButton', function(x) {
  var el = document.getElementById('evidence_run');
  if (!el) return;
  el.disabled = !!x.disabled;
  el.innerText = x.label || 'Queue processing';
});
")),
 shiny::p("Review the selected saved version. Each edit creates a new version; approval is never copied."),
 shiny::actionButton("evidence_start","Start screening from checked database inputs"),
 shiny::actionButton("evidence_refresh","Load selected evidence version"),shiny::textOutput("evidence_stage"),
 shiny::uiOutput("evidence_gate_ui"),
 shiny::tableOutput("evidence_progress"),
 shiny::selectInput("evidence_operation","Next processing step",character()),shiny::actionButton("evidence_run","Queue processing",onclick="this.disabled=true;this.innerText='Submitting…';"),
 shiny::uiOutput("evidence_job_status"),
 shiny::selectInput("evidence_job","Completed processing result",character()),shiny::actionButton("evidence_accept","Save result as new evidence version"),
 shiny::p("Before finalizing a stage, explicitly approve its pending gate under Human Review Required. Automated outputs and AI recommendations are not human approvals."),
 shiny::selectInput("evidence_table","Review section",character()),shiny::textInput("evidence_filter","Find title, ID or review status"),
 shiny::selectInput("evidence_queue","Review queue",c("All records"="all","Needs Human entry"="pending","Uncertain / potential conflict"="uncertain")),
 shiny::selectInput("evidence_row","Record / result",character()),shiny::uiOutput("evidence_context"),shiny::tableOutput("evidence_details"),shiny::uiOutput("evidence_fields"),
 shiny::selectInput("proposal_decision","AI proposal review (does not approve scientific values)",c("Choose explicitly"="","Accept for manual entry"="ACCEPT_FOR_MANUAL_ENTRY","Reject"="REJECT","Uncertain"="UNCERTAIN")),shiny::textInput("proposal_rationale","Proposal review rationale"),shiny::actionButton("proposal_review","Record proposal review"),
 shiny::actionButton("evidence_add_row","Add another outcome/timepoint for selected report"),
 shiny::actionButton("evidence_save","Save human entry as new version"),
 shiny::checkboxInput("evidence_confirm","I independently verified these extracted values against the source",FALSE),
 shiny::textInput("evidence_rationale","Independent verification rationale"),shiny::actionButton("evidence_verify","Verify extraction as a second reviewer"),
 shiny::fileInput("evidence_pdf","Full-text PDF for selected report",accept=".pdf"),shiny::actionButton("evidence_upload","Save PDF in a new version"))
web_b2_server <- function(input,output,session,request,safe,notice,refresh) {
 packet<-shiny::reactiveVal(NULL)
 set_run_button<-function(disabled=FALSE,label="Queue processing") {
  session$sendCustomMessage("srmaEvidenceRunButton",list(disabled=isTRUE(disabled),label=label))
 }
 labels<-c(recommend_screening="Request AI screening recommendations",recommend_eligibility="Request AI full-text recommendations",recommend_extraction="Request AI extraction proposals",recommend_risk_of_bias="Request AI bias-assessment proposals",prepare_screening="Prepare deduplicated screening set",finalize_screening="Finalize title/abstract review and prepare full text",register_fulltext="Register uploaded full-text PDFs",finalize_included_studies="Finalize Human inclusion decisions",prepare_extraction="Prepare structured human extraction",finalize_extraction="Validate Human-verified extraction",prepare_risk_of_bias="Prepare design-specific bias review",finalize_evidence="Finalize Human-approved evidence package")
 screening_checkpoint_progress<-function(j){
  if(is.null(j)||!nrow(j))return(NULL)
  loc<-as.character(j$diagnostic_locator[[1L]] %||b2% "")
  root<-Sys.getenv("SRMA_WEB_DATA","")
  if(!nzchar(root)||!nzchar(loc))return(NULL)
  project_root<-file.path(root,loc,"project")
  if(!dir.exists(project_root))return(NULL)
  hits<-list.files(project_root,pattern="^agent_screening_checkpoint[.]csv$",recursive=TRUE,full.names=TRUE)
  if(!length(hits))return(NULL)
  f<-hits[[1L]]
  d<-tryCatch(utils::read.csv(f,stringsAsFactors=FALSE,check.names=FALSE),error=function(e)NULL)
  if(is.null(d))return(NULL)
  completed<-if(all(c("screening_id","validation_status") %in% names(d))) {
   ok<-!is.na(d$screening_id)&nzchar(trimws(as.character(d$screening_id)))&as.character(d$validation_status)=="valid"
   length(unique(as.character(d$screening_id[ok])))
  } else nrow(d)
  x<-packet();total<-if(!is.null(x)&&!is.null(x$tables$Reviewer_1))length(x$tables$Reviewer_1) else NA_integer_
  pct<-if(is.finite(total)&&total>0)round(100*completed/total,1) else NA_real_
  mtime<-tryCatch(file.info(f)$mtime[[1L]],error=function(e)NA)
  list(completed=completed,total=total,pct=pct,mtime=mtime)
 }
 names_table<-c(Reviewer_1="Independent reviewer 1",Reviewer_2="Independent reviewer 2",Conflict_Resolution="Conflict resolution",Author_Final_Review="Full-text eligibility and study mapping",Target_Study_Verification="Study characteristics",Target_Outcome_Verification="Outcome values",Signalling_Questions="RoB signalling questions",Domain_Review="RoB domain judgments",Overall_Review="Overall RoB judgments")
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
 saved<-function(v){if(!is.null(v)){
  packet(NULL)
  refresh(refresh()+1L)
  all<-safe(request("view",list(project_id=v$project_id)))
  if(!is.null(all)&&nrow(all$versions)){
   choices<-stats::setNames(all$versions$id,paste("Version",all$versions$version))
   session$onFlushed(function(){
    shiny::updateSelectInput(session,"version",choices=choices,selected=v$version_id)
   },once=TRUE)
  }
  notice("Saved a new evidence version. The new version is selected automatically; load it to continue. Pending approvals must be decided explicitly.")
 }}
 shiny::observeEvent(list(input$project,input$version),packet(NULL))
 shiny::observeEvent(input$evidence_refresh,{load()})
 shiny::observeEvent(input$evidence_start,saved(safe(request("evidence_start",list(project_id=input$project,version_id=input$version)))))
 output$evidence_stage<-shiny::renderText({x<-packet();if(is.null(x))return("Load this version to review its evidence. Start a new screening set only from checked database inputs.");paste("Selected stage:",if(x$stage=="READY")"FINAL / APPROVED evidence — no meta-analysis performed" else x$stage,"— review the source, save your decision, then request the next processing step.")})
 output$evidence_progress<-shiny::renderTable({x<-packet();shiny::req(x);do.call(rbind,lapply(names(x$tables),function(t){rows<-x$tables[[t]];data.frame(Section=unname(names_table[t]) %||b2% t,Rows=length(rows),Human_entries=sum(vapply(rows,function(a)!is.null(a$.actor),logical(1))),Verified=sum(vapply(rows,function(a)identical(a$verification_status,"Verified"),logical(1))))}))})
 shiny::observe({x<-packet();shiny::req(x)
  tables<-names(x$tables)
  if(is.null(tables))tables<-character()
  labs<-unname(names_table[tables])
  if(length(labs)){miss<-is.na(labs);labs[miss]<-tables[miss]}
  shiny::updateSelectInput(session,"evidence_table",
    choices=if(length(tables))stats::setNames(tables,labs) else character(),
    selected=shiny::isolate(input$evidence_table))
  ops<-names(web_b2_operations())[web_b2_operations()==x$stage]
  if(is.null(ops))ops<-character()
  op_labs<-unname(labels[ops])
  shiny::updateSelectInput(session,"evidence_operation",
    choices=if(length(ops))stats::setNames(ops,op_labs) else character())
 })
 shiny::observe({shiny::invalidateLater(2000,session);shiny::req(packet(),input$project,input$version);x<-safe(request("view",list(project_id=input$project)));shiny::req(x);j<-x$jobs[x$jobs$version_id==input$version&x$jobs$operation %in% names(web_b2_operations())&x$jobs$state=="COMPLETE",,drop=FALSE];shiny::updateSelectInput(session,"evidence_job",choices=if(nrow(j))stats::setNames(j$id,paste(unname(labels[j$operation]),j$created_at)) else character(),selected=shiny::isolate(input$evidence_job))})
 latest_evidence_job<-shiny::reactive({
  shiny::invalidateLater(1000,session)
  shiny::req(input$project,input$version,input$evidence_operation)
  if(!nzchar(input$evidence_operation))return(NULL)
  x<-safe(request("view",list(project_id=input$project)))
  if(is.null(x)||is.null(x$jobs)||!nrow(x$jobs))return(NULL)
  j<-x$jobs[x$jobs$version_id==input$version&x$jobs$operation==input$evidence_operation,,drop=FALSE]
  if(!nrow(j))return(NULL)
  j[1,,drop=FALSE]
 })
 output$evidence_job_status<-shiny::renderUI({
  j<-latest_evidence_job()
  if(is.null(j))return(shiny::div(
    style="border-left:4px solid #6b7280;background:#f9fafb;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",
    shiny::strong("Not started"),shiny::span(" — Queue the selected processing step when ready.")
  ))
  state<-as.character(j$state[[1L]])
  msg<-as.character(j$message[[1L]] %||b2% "")
  progress<-as.integer(j$progress[[1L]] %||b2% 0L)
  is_screening<-identical(as.character(j$operation[[1L]]),"recommend_screening")
  cp<-if(is_screening)screening_checkpoint_progress(j) else NULL
  long_note<-if(is_screening)shiny::p(style="margin-top:8px;","AI title/abstract screening is usually the longest processing step. Large reviews may take several hours. You may leave this page; completed decisions are checkpointed and preserved.") else NULL
  if(state=="QUEUED")return(shiny::div(style="border-left:4px solid #2563eb;background:#eff6ff;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",shiny::strong("Queued — waiting for worker"),shiny::p(msg),long_note))
  if(state=="RUNNING") {
   headline<-"Processing…"
   detail<-msg
   if(!is.null(cp)&&is.finite(cp$total)&&cp$total>0){
    headline<-paste0("AI screening in progress — ",cp$completed," / ",cp$total," records (",format(cp$pct,nsmall=1),"%)")
    if(!is.na(cp$mtime))detail<-paste0("Last checkpoint: ",format(cp$mtime,"%H:%M:%S"))
   }
   return(shiny::div(style="border-left:4px solid #2563eb;background:#eff6ff;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",shiny::strong(headline),shiny::p(detail),long_note))
  }
  if(state=="COMPLETE")return(shiny::div(style="border-left:4px solid #15803d;background:#f0fdf4;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",shiny::strong("✓ Processing completed"),shiny::p("Select the completed result below and save it as a new evidence version to continue.")))
  if(state=="USER_ACTION_REQUIRED")return(shiny::div(style="border-left:4px solid #d97706;background:#fff7ed;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",shiny::strong("! Action required"),shiny::p(msg)))
  if(state=="FAILED") {
   preserved<-if(!is.null(cp)&&cp$completed>0)shiny::p(paste0("Preserved checkpoint: ",cp$completed,if(is.finite(cp$total))paste0(" / ",cp$total) else ""," records. The next retry will resume from these valid decisions.")) else NULL
   return(shiny::div(style="border-left:4px solid #b91c1c;background:#fef2f2;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",shiny::strong("✕ Processing failed"),shiny::p(msg),preserved,shiny::p("Correct the reported problem, then queue this step again.")))
  }
  shiny::div(style="border-left:4px solid #6b7280;background:#f9fafb;padding:9px 12px;margin:8px 0 12px;border-radius:6px;",shiny::strong(state),shiny::p(msg))
 })
 shiny::observe({
  j<-latest_evidence_job()
  if(is.null(j)){set_run_button(FALSE,"Queue processing");return()}
  state<-as.character(j$state[[1L]])
  if(state=="QUEUED")set_run_button(TRUE,"Queued…")
  else if(state=="RUNNING")set_run_button(TRUE,"Processing…")
  else if(state=="COMPLETE"){
   set_run_button(TRUE,"Completed — save result")
   shiny::updateSelectInput(session,"evidence_job",selected=as.character(j$id[[1L]]))
  } else if(state=="FAILED")set_run_button(FALSE,"Retry processing")
  else if(state=="USER_ACTION_REQUIRED")set_run_button(FALSE,"Queue processing")
  else set_run_button(FALSE,"Queue processing")
 })
 shiny::observe({x<-packet();shiny::req(x,input$evidence_table);rows<-x$tables[[input$evidence_table]];queue<-input$evidence_queue %||b2% "all"
  if(queue=="pending")rows<-Filter(function(a)is.null(a$.actor),rows)
  if(queue=="uncertain")rows<-Filter(function(a){i<-which(vapply(x$tables$Reviewer_1,function(r)identical(r$screening_id,a$screening_id),logical(1)));disagree<-FALSE
   if(length(i)==1L){aa<-x$tables$Reviewer_1[[i]];bb<-x$tables$Reviewer_2[[i]];disagree<-!is.null(aa$.actor)&&!is.null(bb$.actor)&&(!identical(aa$decision,bb$decision)||!identical(aa$exclusion_reason,bb$exclusion_reason))}
   disagree||grepl("uncertain|unclear|awaiting|clarification",paste(unlist(a),collapse=" "),ignore.case=TRUE)
  },rows)
  q<-tolower(input$evidence_filter %||b2% "");if(nzchar(q))rows<-Filter(function(a)grepl(q,tolower(paste(unlist(a),collapse=" ")),fixed=TRUE),rows);shiny::updateSelectInput(session,"evidence_row",choices=if(length(rows))stats::setNames(vapply(rows,`[[`,character(1),".row_id"),vapply(rows,function(a)paste(a$screening_id %||b2% "",a$title %||b2% a$outcome_name %||b2% a$domain %||b2% a$question_id %||b2% ""),character(1))) else character(),selected=shiny::isolate(input$evidence_row))})
 output$evidence_details<-shiny::renderTable({a<-selected();keys<-names(a)[!startsWith(names(a),".")];data.frame(Field=gsub("_"," ",keys),Value=vapply(a[keys],function(v)paste(unlist(v),collapse="; "),character(1)))})
 output$evidence_context<-shiny::renderUI({a<-selected();shiny::tagList(shiny::strong(if(identical(a$.source,"AI RECOMMENDATION"))"AI RECOMMENDATION — HUMAN REVIEW REQUIRED / UNVERIFIED" else a$.source %||b2% "SYSTEM OUTPUT"),shiny::p(paste("Report:",a$screening_id %||b2% ""," | Source location:",a$source_location %||b2% "")),shiny::p(a$title %||b2% ""),shiny::p(a$abstract %||b2% ""),shiny::p(a$agent_rationale %||b2% a$source_evidence %||b2% a$evidence_summary %||b2% ""),shiny::p(paste("Bias instrument:",a$rob_tool %||b2% "Not applicable")),shiny::p(if(!is.null(a$.human_review))paste("Proposal review:",a$.human_review$decision,a$.human_review$rationale) else ""),shiny::p("Registered PDFs and canonical outputs are available under Results / Artifacts."))})
 field_names<-shiny::reactive({x<-packet();shiny::req(x);spec<-web_b2_edit_spec()[[input$evidence_table]];if(is.null(spec))return(character());spec$fields %||b2% unlist(x$editable[[input$evidence_table]])})
 output$evidence_fields<-shiny::renderUI({x<-packet();a<-selected();fields<-field_names();shiny::tagList(lapply(seq_along(fields),function(i){k<-fields[i];val<-a[[k]] %||b2% "";choices<-NULL
  if(k=="decision")choices<-unlist(x$choices$decisions)
  if(k %in% c("author_final_decision","final_decision"))choices<-if(k=="final_decision")c("Include","Exclude","Awaiting full-text clarification") else c("Include","Exclude")
  if(grepl("exclusion_reason$",k))choices<-unlist(x$choices$reasons)
  if(k=="human_include_in_planned_analysis")choices<-c("No","Yes")
  if(k=="value_status")choices<-c("REPORTED","DERIVED","CALCULATED","IMPUTED","MISSING","NOT APPLICABLE")
  if(k=="retrieval_status")choices<-c("Missing","Available","Not retrieved")
  if(k=="human_response")choices<-c("Y","PY","PN","N","NI","NA")
  if(!is.null(choices))shiny::selectInput(paste0("evf_",i),gsub("_"," ",k),c("Choose explicitly"="",choices),selected=val)
  else shiny::textAreaInput(paste0("evf_",i),gsub("_"," ",k),val,rows=2)
 }))})
 shiny::observeEvent(input$proposal_review,saved(safe(request("evidence_review_proposal",list(project_id=input$project,version_id=input$version,table=input$evidence_table,row_id=input$evidence_row,decision=input$proposal_decision,rationale=input$proposal_rationale)))))
 shiny::observeEvent(input$evidence_add_row,saved(safe(request("evidence_add_row",list(project_id=input$project,version_id=input$version,row_id=input$evidence_row)))))
 shiny::observeEvent(input$evidence_save,{f<-field_names();values<-stats::setNames(lapply(seq_along(f),function(i)input[[paste0("evf_",i)]] %||b2% ""),f);saved(safe(request("evidence_edit",list(project_id=input$project,version_id=input$version,table=input$evidence_table,row_id=input$evidence_row,values=values))))})
 shiny::observeEvent(input$evidence_verify,saved(safe(request("evidence_verify",list(project_id=input$project,version_id=input$version,table=input$evidence_table,row_id=input$evidence_row,approved=isTRUE(input$evidence_confirm),rationale=input$evidence_rationale)))))
 shiny::observeEvent(input$evidence_upload,saved(safe(request("evidence_upload",list(project_id=input$project,version_id=input$version,row_id=input$evidence_row,upload=input$evidence_pdf)))))
 shiny::observeEvent(input$evidence_run,{
  shiny::req(input$project,input$version,input$evidence_operation)
  id<-safe(request("submit",list(project_id=input$project,version_id=input$version,operation=input$evidence_operation)))
  if(is.null(id))set_run_button(FALSE,"Queue processing")
  else if(identical(input$evidence_operation,"recommend_screening")) notice("AI title/abstract screening queued. This is usually the longest processing step and may take several hours for large reviews. You may leave this page; checkpointed decisions are preserved automatically.")
  else notice("Processing queued. You may leave this page; save the completed result as a new evidence version when it finishes.")
 })
 shiny::observeEvent(input$evidence_accept,saved(safe(request("evidence_accept",list(project_id=input$project,version_id=input$version,job_id=input$evidence_job)))))
}
