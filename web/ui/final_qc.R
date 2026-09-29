web_c3_ui <- function() shiny::tabPanel("Final QC",
 shiny::p("Inspect the exact author-approved manuscript. Human QC approval and scientific release eligibility are separate."),
 shiny::actionButton("qc_start","Inspect selected approved manuscript"),shiny::actionButton("qc_refresh","Refresh QC history"),shiny::selectInput("qc_id","QC decision / history",character()),shiny::actionButton("qc_load","Load QC decision"),
 shiny::textOutput("qc_status"),shiny::verbatimTextOutput("qc_source"),shiny::tableOutput("qc_checks"),shiny::tableOutput("qc_findings"),
 shiny::actionButton("qc_accept","Register completed worker inspection"),shiny::selectInput("qc_finding","Human scientific / citation / visual review",character()),shiny::selectInput("qc_decision","Disposition",c("Correction required"="CORRECTION_REQUIRED","Confirmed against source"="CONFIRMED")),shiny::textAreaInput("qc_reason","Evidence and review rationale"),shiny::actionButton("qc_resolve","Save immutable review disposition"),
 shiny::checkboxInput("qc_confirm","I reviewed numbers, tables, figures, citation support, Methods, visual output and mandatory external gates; no blocking findings remain",FALSE),shiny::actionButton("qc_approve","Approve Human Final QC"),
 shiny::verbatimTextOutput("qc_external"),shiny::verbatimTextOutput("qc_eligibility"),shiny::downloadButton("qc_download","Download authorized QC evidence"))
web_c3_server <- function(input,output,session,request,safe,notice,refresh) {
 packet<-shiny::reactiveVal(NULL)
 args<-function()list(project_id=input$project,qc_id=input$qc_id)
 load_ids<-function(selected=NULL){x<-request("qc_list",list(project_id=input$project));shiny::updateSelectInput(session,"qc_id",choices=setNames(x$id,paste(x$status,x$created_at)),selected=selected)}
 saved<-function(v){if(!is.null(v)){load_ids(v$qc_id);packet(request("qc_view",list(project_id=v$project_id,qc_id=v$qc_id)));refresh(refresh()+1L)}}
 shiny::observeEvent(input$project,{packet(NULL)})
 shiny::observeEvent(input$qc_refresh,safe(load_ids()))
 shiny::observeEvent(input$qc_start,saved(safe(request("qc_start",list(project_id=input$project,version_id=input$version)))))
 shiny::observeEvent(input$qc_load,packet(safe(request("qc_view",args()))))
 shiny::observeEvent(input$qc_accept,saved(safe(request("qc_accept",args()))))
 shiny::observeEvent(input$qc_resolve,saved(safe(request("qc_resolve",c(args(),list(finding_id=input$qc_finding,decision=input$qc_decision,rationale=input$qc_reason))))))
 shiny::observeEvent(input$qc_approve,saved(safe(request("qc_approve",c(args(),list(confirmed=isTRUE(input$qc_confirm),rationale=input$qc_reason))))))
 shiny::observe({x<-packet();shiny::req(x);f<-Filter(function(a)a$mode=="HUMAN",x$result$findings);shiny::updateSelectInput(session,"qc_finding",choices=setNames(vapply(f,`[[`,character(1),"id"),vapply(f,function(a)paste(a$affected,a$message),character(1))))})
 output$qc_status<-shiny::renderText({x<-packet();if(is.null(x))return("Select approved manuscript or historical QC.");paste(x$effective_status,if(isTRUE(x$eligibility$RELEASE_ELIGIBLE))"Release eligibility verified" else "Release blocked / external acceptance pending")})
 output$qc_source<-shiny::renderText({x<-packet();shiny::req(x);as.character(jsonlite::toJSON(list(manuscript=x$proof$manuscript_version,evidence=x$proof$evidence_version,analyses=names(x$proof$analyses),source=x$proof$source_sha256,reference=x$proof$reference_sha256,docx=x$proof$docx$docx_sha256),pretty=TRUE,auto_unbox=TRUE))})
 output$qc_checks<-shiny::renderTable({x<-packet();shiny::req(x$result);data.frame(Check=names(x$result$checks),Result=unlist(x$result$checks))})
 output$qc_findings<-shiny::renderTable({x<-packet();shiny::req(x$result);web_b2_table(lapply(x$result$findings,function(f){d<-x$resolutions[[f$id]];c(f,list(resolution=d$decision %||b2% "Unresolved",reviewer=d$reviewer %||b2% "",reviewed_at=d$at %||b2% ""))}))})
 output$qc_external<-shiny::renderText({x<-packet();shiny::req(x);paste(unlist(x$external),collapse="\n")})
 output$qc_eligibility<-shiny::renderText({x<-packet();shiny::req(x);as.character(jsonlite::toJSON(x$eligibility,pretty=TRUE,auto_unbox=TRUE))})
 output$qc_download<-shiny::downloadHandler(filename=function()"final_qc.json",content=function(file){x<-packet();shiny::req(x$result_artifact);shiny::req(!is.null(safe(request("download",list(project_id=input$project,artifact_id=x$result_artifact,destination=file)))))})
}
