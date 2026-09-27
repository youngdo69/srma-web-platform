source("web/ui/manuscript.R")
shiny::testServer(function(input,output,session){
 notice<-shiny::reactiveVal("");refresh<-shiny::reactiveVal(0L)
 safe<-function(expr)tryCatch(expr,error=function(e){notice(conditionMessage(e));NULL})
 request<-function(action,args=list())req(action,args)
 web_c2_server(input,output,session,request,safe,notice,refresh)
 output$notice<-shiny::renderText(notice())
},{
 session$setInputs(project=approved_ms$project_id,version=approved_ms$version_id,manuscript_section="Results");session$flushReact();session$setInputs(manuscript_load=1);session$flushReact()
 stopifnot(grepl("APPROVED",output$manuscript_status),grepl("PRISMA",output$manuscript_preview),grepl("Pre-check",output$manuscript_precheck))
 session$setInputs(manuscript_edit=1,manuscript_text="changed",manuscript_decision="SAVE",manuscript_edit_reason="try");session$flushReact();stopifnot(grepl("editable",output$notice))
})
cat("PASS: authenticated Shiny manuscript preview/status/precheck; approved manuscript edit rejected\n")
