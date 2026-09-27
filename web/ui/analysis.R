web_c1_ui <- function() shiny::tabPanel("Meta-analysis",
 shiny::p("Start from Human-approved analysis-ready evidence. Every saved plan/result is a new immutable version. Exact scientific runtime acceptance is required before scientific release."),
 shiny::actionButton("analysis_catalog","Load evidence groups for selected version"),shiny::tableOutput("analysis_groups"),
 shiny::selectInput("analysis_group","Evidence group",character()),shiny::selectInput("analysis_rows","Included extraction rows (select explicitly)",character(),multiple=TRUE),
 shiny::selectInput("analysis_role","Analysis role",c("Choose explicitly"="","PRIMARY","SECONDARY","SENSITIVITY","SUBGROUP","EXPLORATORY")),
 shiny::selectInput("analysis_parent","Approved parent result (linked analyses only)",c("Choose an approved parent"="")),
 shiny::textInput("analysis_outcome","Outcome"),shiny::textInput("analysis_definition","Outcome / subgroup definition"),
 shiny::textInput("analysis_timepoint","Time point"),shiny::textInput("analysis_design","Study design"),shiny::textInput("analysis_population","Population / subset"),
 shiny::selectInput("analysis_basis","Estimate policy",c("Choose explicitly"="","Raw counts/measurements"="RAW","Adjusted estimates"="ADJUSTED","Unadjusted estimates"="UNADJUSTED")),
 shiny::verbatimTextOutput("analysis_methods"),shiny::checkboxInput("analysis_methods_confirm","I reviewed and accept these canonical methods and transformations",FALSE),
 shiny::textAreaInput("analysis_exclusion_reason","Reason for excluded rows (applied to each unselected row)"),shiny::textAreaInput("analysis_rationale","Protocol reference, compatibility and reason for this analysis"),
 shiny::selectInput("analysis_diagnostics","Leave-one-out / influence diagnostics",c("Choose explicitly"="","No"="no","Yes"="yes")),
 shiny::selectInput("analysis_exploratory","Exploratory funnel, Egger and configured moderator diagnostics (may be underpowered)",c("Choose explicitly"="","No"="no","Yes, explicitly requested"="yes")),
 shiny::textInput("analysis_moderators","Explicit moderator column names, comma-separated; blank means none. Thresholds come from project config (canonical default 10)"),
 shiny::actionButton("analysis_save","Save analysis plan as new version"),
 shiny::p("Load the saved plan, review it, and explicitly approve its analysis_plan gate under Human Review Required before running."),
 shiny::actionButton("analysis_load","Load selected analysis version"),shiny::textOutput("analysis_status"),shiny::verbatimTextOutput("analysis_plan_summary"),
 shiny::actionButton("analysis_run","Queue approved analysis and outputs"),shiny::selectInput("analysis_job","Completed analysis job",character()),shiny::actionButton("analysis_accept","Save completed result as new version"),
 shiny::tableOutput("analysis_results"),shiny::textOutput("analysis_warnings"),shiny::uiOutput("analysis_forest"),shiny::tableOutput("analysis_linked"),
 shiny::selectInput("analysis_artifact","Canonical result / editable table / figure",character()),shiny::downloadButton("analysis_download","Download verified output"),
 shiny::checkboxInput("analysis_result_confirm","I reviewed intended studies, outcome/time point, success, warnings and analysis role",FALSE),shiny::textAreaInput("analysis_result_rationale","Result review rationale"),shiny::actionButton("analysis_approve","Approve and freeze result as a new version"))
web_c1_server <- function(input,output,session,request,safe,notice,refresh) {
 catalog<-shiny::reactiveVal(NULL);packet<-shiny::reactiveVal(NULL)
 args<-function()list(project_id=input$project,version_id=input$version)
 saved<-function(v){if(!is.null(v)){packet(NULL);catalog(NULL);refresh(refresh()+1L);all<-safe(request("view",list(project_id=v$project_id)));shiny::updateSelectInput(session,"version",choices=stats::setNames(all$versions$id,paste("Version",all$versions$version)),selected=v$version_id);notice("New immutable analysis version saved. Load it to review; approvals are never copied.")}}
 shiny::observeEvent(list(input$project,input$version),{catalog(NULL);packet(NULL)})
 shiny::observeEvent(input$analysis_catalog,{x<-safe(request("analysis_catalog",args()));catalog(x);if(!is.null(x)){shiny::updateSelectInput(session,"analysis_group",choices=c("Choose explicitly"="",x$groups$analysis_group));shiny::updateSelectInput(session,"analysis_parent",choices=c("Choose an approved parent"="",stats::setNames(x$parents$version_id,x$parents$label)))}})
 output$analysis_groups<-shiny::renderTable({x<-catalog();shiny::req(x);x$groups[,intersect(c("analysis_group","meta_analysis_role","study_count","study_design","outcome","timepoint","effect_measure","next_action"),names(x$groups)),drop=FALSE]})
 shiny::observeEvent(input$analysis_group,{x<-catalog();shiny::req(x,nzchar(input$analysis_group));m<-x$groups[x$groups$analysis_group==input$analysis_group,,drop=FALSE];shiny::req(nrow(m)==1);rows<-x$rows[x$rows$outcome_row_id %in% unlist(x$group_rows[[input$analysis_group]]),,drop=FALSE];shiny::updateSelectInput(session,"analysis_rows",choices=stats::setNames(rows$outcome_row_id,paste(rows$screening_id,rows$outcome_name,rows$timepoint)),selected=character());for(k in c("outcome","timepoint"))shiny::updateTextInput(session,paste0("analysis_",k),value=m[[k]]);shiny::updateTextInput(session,"analysis_design",value=m$study_design)})
 output$analysis_methods<-shiny::renderText(paste(names(web_c1_methods()),unlist(web_c1_methods()),sep=": ",collapse="\n"))
 shiny::observeEvent(input$analysis_save,{
  x<-catalog();shiny::req(x)
  safe({web_assert(isTRUE(input$analysis_methods_confirm)&&input$analysis_diagnostics %in% c("yes","no")&&input$analysis_exploratory %in% c("yes","no"),"Confirm canonical methods and choose diagnostic options explicitly.")
   group_rows<-x$rows[x$rows$outcome_row_id %in% unlist(x$group_rows[[input$analysis_group]]),,drop=FALSE]
   excluded<-setdiff(group_rows$outcome_row_id,input$analysis_rows);mods<-trimws(strsplit(input$analysis_moderators %||b2% "",",",fixed=TRUE)[[1]]);mods<-mods[nzchar(mods)]
   spec<-list(group=input$analysis_group,row_ids=as.list(input$analysis_rows),role=input$analysis_role,outcome=input$analysis_outcome,definition=input$analysis_definition,timepoint=input$analysis_timepoint,design=input$analysis_design,population=input$analysis_population,estimate_basis=input$analysis_basis,methods=web_c1_methods(),exclusions=stats::setNames(as.list(rep(input$analysis_exclusion_reason,length(excluded))),excluded),rationale=input$analysis_rationale,parent_version=if(nzchar(input$analysis_parent))input$analysis_parent else NULL,diagnostics=input$analysis_diagnostics=="yes",exploratory=input$analysis_exploratory=="yes",moderators=as.list(mods))
   saved(request("analysis_plan",c(args(),list(spec=spec))))
  })
 })
 shiny::observeEvent(input$analysis_load,packet(safe(request("analysis_view",args()))))
 output$analysis_status<-shiny::renderText({x<-packet();if(is.null(x))return("Load a saved analysis plan/result.");paste(x$plan$role,x$stage,if(isTRUE(x$current))"— current evidence" else "— HISTORICAL: newer evidence invalidates downstream use")})
 output$analysis_plan_summary<-shiny::renderText({x<-packet();shiny::req(x);as.character(jsonlite::toJSON(x$plan,auto_unbox=TRUE,pretty=TRUE))})
 shiny::observeEvent(input$analysis_run,{j<-safe(request("submit",c(args(),list(operation="run_meta_analysis"))));if(!is.null(j))notice("Analysis queued. The independent worker will produce canonical results and outputs.")})
 shiny::observe({shiny::invalidateLater(2000,session);shiny::req(packet());x<-safe(request("view",list(project_id=input$project)));shiny::req(x);j<-x$jobs[x$jobs$version_id==input$version&x$jobs$operation=="run_meta_analysis"&x$jobs$state=="COMPLETE",,drop=FALSE];shiny::updateSelectInput(session,"analysis_job",choices=stats::setNames(j$id,j$created_at),selected=shiny::isolate(input$analysis_job))})
 shiny::observeEvent(input$analysis_accept,saved(safe(request("analysis_accept",c(args(),list(job_id=input$analysis_job))))))
 output$analysis_results<-shiny::renderTable({x<-packet();shiny::req(x$result);keys<-intersect(c("studies","effect_measure","model_name","tau2_method","inference_test","pooled_effect","lower_ci","upper_ci","p_value","tau2","i2","q","q_df","q_p_value","prediction_lower","prediction_upper"),names(x$result$summary));data.frame(Statistic=gsub("_"," ",keys),Value=vapply(x$result$summary[keys],function(a)paste(unlist(a),collapse="; "),character(1)))},digits=8)
 output$analysis_warnings<-shiny::renderText({x<-packet();shiny::req(x);paste(c(unlist(x$result$warnings),"Scientific release requires operator acceptance of the exact canonical runtime."),collapse="\n")})
 output$analysis_linked<-shiny::renderTable({x<-packet();shiny::req(x);x$linked})
 shiny::observe({x<-packet();shiny::req(x);a<-x$artifacts;shiny::updateSelectInput(session,"analysis_artifact",choices=if(length(a$id))stats::setNames(a$id,a$logical_name) else character())})
 output$analysis_download<-shiny::downloadHandler(filename=function(){x<-packet();shiny::req(x);x$artifacts$logical_name[match(input$analysis_artifact,x$artifacts$id)]},content=function(file){shiny::req(!is.null(safe(request("download",list(project_id=input$project,artifact_id=input$analysis_artifact,destination=file)))))})
 output$analysis_forest<-shiny::renderUI({x<-packet();shiny::req(x$result$forest_path);a<-x$artifacts;hit<-which(a$logical_name==basename(x$result$forest_path));shiny::req(length(hit)==1);f<-tempfile(fileext=".pdf");on.exit(unlink(f));shiny::req(!is.null(safe(request("download",list(project_id=input$project,artifact_id=a$id[hit],destination=f)))));shiny::tags$iframe(title="Canonical forest plot",src=paste0("data:application/pdf;base64,",base64enc::base64encode(f)),width="100%",height="650")})
 shiny::observeEvent(input$analysis_approve,saved(safe(request("analysis_approve",c(args(),list(confirmed=isTRUE(input$analysis_result_confirm),rationale=input$analysis_result_rationale))))))
}
