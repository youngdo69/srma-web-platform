# Human review UI; labels describe decisions, never engine script numbers.
web_b2_ui <- function() shiny::tabPanel("Screening / Evidence",shiny::p("Review the selected saved version. Each edit creates a new version; approval is never copied."),
 shiny::actionButton("evidence_start","Start screening from checked database inputs"),
 shiny::actionButton("evidence_refresh","Load selected evidence version"),shiny::textOutput("evidence_stage"),
 shiny::tableOutput("evidence_progress"),
 shiny::selectInput("evidence_operation","Next processing step",character()),shiny::actionButton("evidence_run","Queue processing"),
 shiny::selectInput("evidence_job","Completed processing result",character()),shiny::actionButton("evidence_accept","Save result as new evidence version"),
 shiny::p("Before finalizing a stage, explicitly approve its pending gate under Human Review Required. Automated outputs and AI recommendations are not human approvals."),
 shiny::selectInput("evidence_table","Review section",character()),shiny::textInput("evidence_filter","Find title, ID or review status"),
 shiny::selectInput("evidence_row","Record / result",character()),shiny::uiOutput("evidence_context"),shiny::tableOutput("evidence_details"),shiny::uiOutput("evidence_fields"),
 shiny::actionButton("evidence_add_row","Add another outcome/timepoint for selected report"),
 shiny::actionButton("evidence_save","Save human entry as new version"),
 shiny::checkboxInput("evidence_confirm","I independently verified these extracted values against the source",FALSE),
 shiny::textInput("evidence_rationale","Independent verification rationale"),shiny::actionButton("evidence_verify","Verify extraction as a second reviewer"),
 shiny::fileInput("evidence_pdf","Full-text PDF for selected report",accept=".pdf"),shiny::actionButton("evidence_upload","Save PDF in a new version"))
web_b2_server <- function(input,output,session,request,safe,notice,refresh) {
 packet<-shiny::reactiveVal(NULL)
 labels<-c(recommend_screening="Request AI screening recommendations",recommend_eligibility="Request AI full-text recommendations",recommend_extraction="Request AI extraction proposals",recommend_risk_of_bias="Request AI bias-assessment proposals",prepare_screening="Prepare deduplicated screening set",finalize_screening="Finalize title/abstract review and prepare full text",register_fulltext="Register uploaded full-text PDFs",finalize_included_studies="Finalize Human inclusion decisions",prepare_extraction="Prepare structured human extraction",finalize_extraction="Validate Human-verified extraction",prepare_risk_of_bias="Prepare design-specific bias review",finalize_evidence="Finalize Human-approved evidence package")
 names_table<-c(Reviewer_1="Independent reviewer 1",Reviewer_2="Independent reviewer 2",Conflict_Resolution="Conflict resolution",Author_Final_Review="Full-text eligibility and study mapping",Target_Study_Verification="Study characteristics",Target_Outcome_Verification="Outcome values",Signalling_Questions="RoB signalling questions",Domain_Review="RoB domain judgments",Overall_Review="Overall RoB judgments")
 load<-function(){x<-safe(request("evidence_view",list(project_id=input$project,version_id=input$version)));packet(x);x}
 selected<-shiny::reactive({x<-packet();shiny::req(x,input$evidence_table,input$evidence_row);rows<-x$tables[[input$evidence_table]];hits<-Filter(function(a)identical(a$.row_id,input$evidence_row),rows);shiny::req(length(hits)==1);hits[[1]]})
 saved<-function(v){if(!is.null(v)){packet(NULL);refresh(refresh()+1L);all<-safe(request("view",list(project_id=v$project_id)));shiny::updateSelectInput(session,"version",choices=stats::setNames(all$versions$id,paste("Version",all$versions$version)),selected=v$version_id);notice("Saved a new evidence version. Load it to continue; pending approvals must be decided explicitly.")}}
 shiny::observeEvent(list(input$project,input$version),packet(NULL))
 shiny::observeEvent(input$evidence_refresh,{load()})
 shiny::observeEvent(input$evidence_start,saved(safe(request("evidence_start",list(project_id=input$project,version_id=input$version)))))
 output$evidence_stage<-shiny::renderText({x<-packet();if(is.null(x))return("Load this version to review its evidence. Start a new screening set only from checked database inputs.");paste("Selected stage:",x$stage,"— review the source, save your decision, then request the next processing step.")})
 output$evidence_progress<-shiny::renderTable({x<-packet();shiny::req(x);do.call(rbind,lapply(names(x$tables),function(t){rows<-x$tables[[t]];data.frame(Section=unname(names_table[t]) %||b2% t,Rows=length(rows),Human_entries=sum(vapply(rows,function(a)!is.null(a$.actor),logical(1))),Verified=sum(vapply(rows,function(a)identical(a$verification_status,"Verified"),logical(1))))}))})
 shiny::observe({x<-packet();shiny::req(x);tables<-names(x$tables);labs<-unname(names_table[tables]);labs[is.na(labs)]<-tables[is.na(labs)];shiny::updateSelectInput(session,"evidence_table",choices=stats::setNames(tables,labs),selected=shiny::isolate(input$evidence_table));ops<-names(web_b2_operations())[web_b2_operations()==x$stage];shiny::updateSelectInput(session,"evidence_operation",choices=stats::setNames(ops,unname(labels[ops])))})
 shiny::observe({shiny::invalidateLater(2000,session);shiny::req(packet(),input$project,input$version);x<-safe(request("view",list(project_id=input$project)));shiny::req(x);j<-x$jobs[x$jobs$version_id==input$version&x$jobs$operation %in% names(web_b2_operations())&x$jobs$state=="COMPLETE",,drop=FALSE];shiny::updateSelectInput(session,"evidence_job",choices=if(nrow(j))stats::setNames(j$id,paste(unname(labels[j$operation]),j$created_at)) else character(),selected=shiny::isolate(input$evidence_job))})
 shiny::observe({x<-packet();shiny::req(x,input$evidence_table);rows<-x$tables[[input$evidence_table]];q<-tolower(input$evidence_filter %||b2% "");if(nzchar(q))rows<-Filter(function(a)grepl(q,tolower(paste(unlist(a),collapse=" ")),fixed=TRUE),rows);shiny::updateSelectInput(session,"evidence_row",choices=if(length(rows))stats::setNames(vapply(rows,`[[`,character(1),".row_id"),vapply(rows,function(a)paste(a$screening_id %||b2% "",a$title %||b2% a$outcome_name %||b2% a$domain %||b2% a$question_id %||b2% ""),character(1))) else character(),selected=shiny::isolate(input$evidence_row))})
 output$evidence_details<-shiny::renderTable({a<-selected();keys<-names(a)[!startsWith(names(a),".")];data.frame(Field=gsub("_"," ",keys),Value=vapply(a[keys],function(v)paste(unlist(v),collapse="; "),character(1)))})
 output$evidence_context<-shiny::renderUI({a<-selected();shiny::tagList(shiny::strong(if(identical(a$.source,"AI RECOMMENDATION"))"AI RECOMMENDATION — HUMAN REVIEW REQUIRED / UNVERIFIED" else a$.source %||b2% "SYSTEM OUTPUT"),shiny::p(paste("Report:",a$screening_id %||b2% ""," | Source location:",a$source_location %||b2% "")),shiny::p(a$title %||b2% ""),shiny::p(a$abstract %||b2% ""),shiny::p(a$agent_rationale %||b2% a$source_evidence %||b2% a$evidence_summary %||b2% ""),shiny::p(paste("Bias instrument:",a$rob_tool %||b2% "Not applicable")),shiny::p("Registered PDFs and canonical outputs are available under Results / Artifacts."))})
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
 shiny::observeEvent(input$evidence_add_row,saved(safe(request("evidence_add_row",list(project_id=input$project,version_id=input$version,row_id=input$evidence_row)))))
 shiny::observeEvent(input$evidence_save,{f<-field_names();values<-stats::setNames(lapply(seq_along(f),function(i)input[[paste0("evf_",i)]] %||b2% ""),f);saved(safe(request("evidence_edit",list(project_id=input$project,version_id=input$version,table=input$evidence_table,row_id=input$evidence_row,values=values))))})
 shiny::observeEvent(input$evidence_verify,saved(safe(request("evidence_verify",list(project_id=input$project,version_id=input$version,table=input$evidence_table,row_id=input$evidence_row,approved=isTRUE(input$evidence_confirm),rationale=input$evidence_rationale)))))
 shiny::observeEvent(input$evidence_upload,saved(safe(request("evidence_upload",list(project_id=input$project,version_id=input$version,row_id=input$evidence_row,upload=input$evidence_pdf)))))
 shiny::observeEvent(input$evidence_run,{id<-safe(request("submit",list(project_id=input$project,version_id=input$version,operation=input$evidence_operation)));if(!is.null(id))notice("Processing queued. You may leave this page; save the completed result as a new evidence version when it finishes.")})
 shiny::observeEvent(input$evidence_accept,saved(safe(request("evidence_accept",list(project_id=input$project,version_id=input$version,job_id=input$evidence_job)))))
}
