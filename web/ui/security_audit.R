web_c5_ui <- function() shiny::tabPanel("Security & Audit",
  shiny::h3("Security & Audit"),
  shiny::p("Review access activity and project actions. Sensitive values are automatically hidden from this view."),
  shiny::uiOutput("security_session_status"),
  shiny::actionButton("security_logout_all", "Sign out all sessions", class = "btn-outline-danger"),
  shiny::hr(),
  shiny::h4("Recent project activity"),
  shiny::tableOutput("security_audit_table"),
  shiny::downloadButton("security_audit_download", "Download audit snapshot"))

web_c5_server <- function(input, output, session, request, safe, notice, refresh, token) {
  auth <- shiny::reactive({ shiny::req(token()); safe(request("security_summary")) })
  audit <- shiny::reactive({ shiny::req(token(), input$project); refresh(); safe(request("audit_list", list(project_id = input$project))) })
  output$security_session_status <- shiny::renderUI({
    x <- auth(); shiny::req(x)
    shiny::tagList(shiny::p(sprintf("Active sessions: %d", nrow(x$active_sessions))),
      shiny::p("Use 'Sign out all sessions' if you used a shared computer or suspect that another session remains open."))
  })
  output$security_audit_table <- shiny::renderTable({
    x <- audit(); shiny::req(x)
    if (!nrow(x)) return(data.frame(Message = "No project activity recorded yet."))
    x[, intersect(c("created_at", "event_type", "actor_id", "object_id", "detail"), names(x)), drop = FALSE]
  }, sanitize.text.function = identity)
  shiny::observeEvent(input$security_logout_all, {
    ok <- safe({ request("logout_all"); TRUE })
    if (isTRUE(ok)) { token(NULL); notice("All active sessions were signed out.") }
  })
  output$security_audit_download <- shiny::downloadHandler(
    filename = function() paste0("SRMA_Audit_", input$project %||% "project", ".json"),
    content = function(file) {
      shiny::req(input$project)
      if (is.null(safe(request("audit_export", list(project_id = input$project, destination = file))))) stop("Audit export unavailable.", call. = FALSE)
    })
}
