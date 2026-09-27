web_c2_ui <- function() shiny::tabPanel("Manuscript",
 shiny::p("Journal-neutral author-reviewed draft. This is not Final QC or a publication release. Start from Human-approved evidence and analyses."),
 shiny::actionButton("manuscript_catalog","Load sources for selected evidence version"),shiny::tableOutput("manuscript_sources"),
 shiny::textInput("manuscript_title","Manuscript title"),shiny::selectInput("manuscript_language","Language",c("English","Korean")),
 shiny::selectInput("manuscript_analyses","Approved analyses to report",character(),multiple=TRUE),shiny::selectInput("manuscript_artifacts","Registered tables and figures",character(),multiple=TRUE),
 shiny::textAreaInput("manuscript_pmids","Reference PubMed IDs (comma separated; verified before plan save)"),shiny::textAreaInput("manuscript_rationale","Plan rationale / evidence selection"),shiny::actionButton("manuscript_plan","Verify references and save new plan"),
 shiny::actionButton("manuscript_load","Load selected manuscript version"),shiny::textOutput("manuscript_status"),shiny::verbatimTextOutput("manuscript_plan_summary"),shiny::tableOutput("manuscript_references"),
 shiny::p("Review the displayed sources, plan and references. Approve manuscript_plan under Human Review Required, then prepare sections."),shiny::actionButton("manuscript_begin","Prepare sections from approved plan"),
 shiny::selectInput("manuscript_section","Section",web_c2_sections()),shiny::verbatimTextOutput("manuscript_section_status"),shiny::textAreaInput("manuscript_text","Human prose (numeric claims must use protected facts)",rows=10),
 shiny::selectInput("manuscript_fact","Protected source fact",character()),shiny::actionButton("manuscript_insert_fact","Insert selected fact"),
 shiny::selectInput("manuscript_insert_artifact","Registered artifact reference",character()),shiny::actionButton("manuscript_artifact_token","Insert artifact reference"),
 shiny::textAreaInput("manuscript_edit_reason","Author review/edit rationale"),shiny::selectInput("manuscript_decision","Section action",c("Save draft"="SAVE","Approve section"="APPROVE","Reject draft"="REJECT")),shiny::actionButton("manuscript_edit","Save section as new version"),
 shiny::actionButton("manuscript_ai","Request AI regeneration for this section"),shiny::p("AI requires operator credentials. Manual entry is always available. Every edit creates a version and clears Word approval."),
 shiny::verbatimTextOutput("manuscript_precheck"),shiny::verbatimTextOutput("manuscript_preview"),shiny::actionButton("manuscript_docx","Queue editable Word document"),
 shiny::selectInput("manuscript_job","Completed job for selected version",character()),shiny::actionButton("manuscript_accept","Accept completed output as new version"),shiny::downloadButton("manuscript_download","Download reviewed Word draft"),
 shiny::checkboxInput("manuscript_confirm","I reviewed text, analyses, citations, warnings and the matching Word document",FALSE),shiny::textAreaInput("manuscript_review_reason","Manuscript review rationale"),shiny::actionButton("manuscript_approve","Approve author-reviewed manuscript"))
web_c2_server <- function(input,output,session,request,safe,notice,refresh) {
 packet<-shiny::reactiveVal(NULL);catalog<-shiny::reactiveVal(NULL)
 args<-function()list(project_id=input$project,version_id=input$version)
 saved<-function(v){if(!is.null(v)){packet(NULL);refresh(refresh()+1L);view<-request("view",list(project_id=v$project_id));shiny::updateSelectInput(session,"version",choices=setNames(view$versions$id,paste("Version",view$versions$version)),selected=v$version_id);notice("New immutable manuscript version saved. Load it to review.")}}
 shiny::observeEvent(list(input$project,input$version),{packet(NULL);catalog(NULL)})
 shiny::observeEvent(input$manuscript_catalog,{x<-safe(request("manuscript_catalog",args()));catalog(x);if(!is.null(x)){shiny::updateSelectInput(session,"manuscript_analyses",choices=setNames(x$analyses$version_id,x$analyses$label));shiny::updateSelectInput(session,"manuscript_artifacts",choices=setNames(x$artifacts$id,paste(x$artifacts$logical_name,x$artifacts$analysis_version)))}})
 output$manuscript_sources<-shiny::renderTable({x<-catalog();shiny::req(x);x$analyses[,c("version","label"),drop=FALSE]})
 shiny::observeEvent(input$manuscript_plan,safe({ids<-trimws(strsplit(input$manuscript_pmids,",",fixed=TRUE)[[1]]);saved(request("manuscript_plan",c(args(),list(spec=list(title=input$manuscript_title,language=input$manuscript_language,analyses=as.list(input$manuscript_analyses),artifacts=as.list(input$manuscript_artifacts),pmids=as.list(ids),rationale=input$manuscript_rationale)))))}))
 shiny::observeEvent(input$manuscript_load,packet(safe(request("manuscript_view",args()))))
 output$manuscript_status<-shiny::renderText({x<-packet();if(is.null(x))return("Load a saved manuscript version.");paste(x$stage,if(isTRUE(x$current))"Current sources" else "HISTORICAL — upstream changed","; author review is not final release")})
 output$manuscript_plan_summary<-shiny::renderText({x<-packet();shiny::req(x);as.character(jsonlite::toJSON(list(spec=x$spec,evidence=x$evidence_version,sections=web_c2_sections(),source_hash=x$source_sha256),auto_unbox=TRUE,pretty=TRUE))})
 output$manuscript_references<-shiny::renderTable({x<-packet();shiny::req(x);web_b2_table(lapply(x$source$references$records,function(r)r[c("pmid","title","year","doi")]))})
 shiny::observeEvent(input$manuscript_begin,saved(safe(request("manuscript_begin",args()))))
 shiny::observe({x<-packet();shiny::req(x);k<-input$manuscript_section;shiny::updateTextAreaInput(session,"manuscript_text",value=x$sections[[k]]$text %||b2% "");shiny::updateSelectInput(session,"manuscript_insert_artifact",choices=setNames(names(x$source$artifacts),web_c2_artifact_labels(x)));f<-x$source$facts;shiny::updateSelectInput(session,"manuscript_fact",choices=setNames(names(f),vapply(names(f),function(n)paste(n,f[[n]]$rendered),character(1))))})
 output$manuscript_section_status<-shiny::renderText({x<-packet();shiny::req(x);s<-x$sections[[input$manuscript_section]];paste(s$status,s$source,s$actor,s$at)})
 shiny::observeEvent(input$manuscript_insert_fact,{shiny::req(input$manuscript_fact);shiny::updateTextAreaInput(session,"manuscript_text",value=paste(input$manuscript_text,paste0("{{FACT:",input$manuscript_fact,"}}")))})
 shiny::observeEvent(input$manuscript_artifact_token,{shiny::req(input$manuscript_insert_artifact);shiny::updateTextAreaInput(session,"manuscript_text",value=paste(input$manuscript_text,paste0("{{ARTIFACT:",input$manuscript_insert_artifact,"}}")))})
 shiny::observeEvent(input$manuscript_edit,saved(safe(request("manuscript_edit",c(args(),list(section=input$manuscript_section,text=input$manuscript_text,decision=input$manuscript_decision,rationale=input$manuscript_edit_reason))))))
 shiny::observeEvent(input$manuscript_ai,safe({v<-request("manuscript_prepare_ai",c(args(),list(section=input$manuscript_section)));request("submit",c(v,list(operation="generate_manuscript_section")));saved(v)}))
 output$manuscript_precheck<-shiny::renderText({x<-packet();shiny::req(x);paste(c(x$precheck$label,x$precheck$status,unlist(x$precheck$issues)),collapse="\n")})
 output$manuscript_preview<-shiny::renderText({x<-packet();shiny::req(x);x$preview})
 shiny::observeEvent(input$manuscript_docx,safe({request("submit",c(args(),list(operation="generate_manuscript_docx")));notice("Word generation queued. The worker uses the exact selected version.")}))
 shiny::observe({shiny::invalidateLater(2000,session);shiny::req(packet());x<-safe(request("view",list(project_id=input$project)));shiny::req(x);j<-x$jobs[x$jobs$version_id==input$version&x$jobs$operation %in% names(web_c2_operations())&x$jobs$state=="COMPLETE",,drop=FALSE];shiny::updateSelectInput(session,"manuscript_job",choices=setNames(j$id,paste(j$operation,j$created_at)),selected=shiny::isolate(input$manuscript_job))})
 shiny::observeEvent(input$manuscript_accept,saved(safe(request("manuscript_accept",c(args(),list(job_id=input$manuscript_job))))))
 output$manuscript_download<-shiny::downloadHandler(filename=function()"manuscript.docx",content=function(file){x<-packet();shiny::req(x$docx$artifact_id);shiny::req(!is.null(safe(request("download",list(project_id=input$project,artifact_id=x$docx$artifact_id,destination=file)))))})
 shiny::observeEvent(input$manuscript_approve,saved(safe(request("manuscript_approve",c(args(),list(confirmed=isTRUE(input$manuscript_confirm),rationale=input$manuscript_review_reason))))))
}
