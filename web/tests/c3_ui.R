source("web/ui/final_qc.R")
shiny::testServer(function(input,output,session){
 notice<-shiny::reactiveVal("");refresh<-shiny::reactiveVal(0L)
 safe<-function(expr)tryCatch(expr,error=function(e){notice(conditionMessage(e));NULL})
 request<-function(action,args=list())req(action,args)
 web_c3_server(input,output,session,request,safe,notice,refresh)
 output$notice<-shiny::renderText(notice())
},{session$setInputs(project=qc$project_id,version=qc$version_id,qc_id=qc$qc_id);session$flushReact();session$setInputs(qc_load=1);session$flushReact();stopifnot(grepl("Release blocked",output$qc_status),grepl("CANONICAL_RUNTIME_ACCEPTANCE_PENDING",output$qc_external));session$setInputs(qc_confirm=TRUE,qc_reason="Cannot waive",qc_approve=1);session$flushReact();stopifnot(grepl("BLOCKER",output$notice))})
cat("PASS: Final QC UI separates external pending and blocked release; approval cannot bypass findings\n")
