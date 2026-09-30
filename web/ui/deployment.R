web_c6_ui <- function() shiny::tabPanel("Service Status",
  shiny::h3("Service Status"),
  shiny::p("This page describes the Web service environment. Scientific release eligibility remains a separate Final QC decision for each review."),
  shiny::uiOutput("service_status_cards"))

web_c6_server <- function(input, output, session, request, safe, notice, refresh) {
  status <- shiny::reactive({ shiny::req(input$project); refresh(); safe(request("service_status", list(project_id = input$project))) })
  output$service_status_cards <- shiny::renderUI({
    x <- status(); shiny::req(x)
    badge <- function(ok, good, bad) shiny::span(class = if (ok) "badge bg-success" else "badge bg-warning text-dark", if (ok) good else bad)
    shiny::tagList(
      shiny::p(shiny::strong("Environment: "), toupper(x$mode)),
      shiny::p(shiny::strong("Operational database: "), x$database, " ", badge(x$database_ready, "ready", "operator action required")),
      shiny::p(shiny::strong("Organization sign-in: "), x$organization_sign_in, " ", badge(x$organization_sign_in == "Configured", "ready", "operator action required")),
      shiny::p(shiny::strong("Persistent storage: "), x$persistent_storage),
      shiny::p(shiny::strong("Database schema: "), x$schema),
      if (x$mode == "development") shiny::div(class="alert alert-secondary", "Development mode is for local use only.")
      else shiny::div(class="alert alert-info", "The beta service is configured for organization access. Project scientific release still depends on its own Final QC and external acceptance gates."))
  })
}
