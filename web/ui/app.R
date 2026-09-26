web_app <- function(settings, actor) {
  ui <- bslib::page_fluid(
    title = "SR-MA Workspace", theme = bslib::bs_theme(version = 5),
    shiny::h2("Research workspace"),
    shiny::p("Select a saved project, check its search strategies, and download the results."),
    shiny::selectInput("project", "Project", choices = character()),
    shiny::selectInput("version", "Saved version", choices = character()),
    shiny::actionButton("run", "Check search strategies"),
    shiny::p("Checks continue when you close this page. Your scientific review decisions remain required."),
    shiny::textOutput("notice"), shiny::h3("Progress"), shiny::tableOutput("jobs"),
    shiny::h3("Results"), shiny::selectInput("artifact", "Available output", choices = character()),
    shiny::downloadButton("download", "Download selected output"))
  server <- function(input, output, session) {
    con <- web_db(settings)
    session$onSessionEnded(function() DBI::dbDisconnect(con))
    notice <- shiny::reactiveVal("")
    safe <- function(expr) tryCatch(expr, error = function(e) {
      notice(if (inherits(e, "srma_web_error")) conditionMessage(e) else "The request could not finish. Please ask the operator to check it.")
      NULL
    })
    shiny::observe({
      projects <- web_query(con, "SELECT p.id,p.title FROM projects p JOIN workspace_members m ON p.workspace_id=m.workspace_id WHERE m.user_id=?u ORDER BY p.created_at", u = actor)
      shiny::updateSelectInput(session, "project", choices = setNames(projects$id, projects$title))
    })
    state <- shiny::reactive({
      shiny::invalidateLater(1000, session)
      shiny::req(input$project)
      safe(web_view(con, actor, input$project))
    })
    shiny::observeEvent(input$project, {
      x <- safe(web_view(con, actor, input$project))
      if (!is.null(x)) shiny::updateSelectInput(session, "version", choices = setNames(x$versions$id, paste("Version", x$versions$version)))
    })
    shiny::observeEvent(input$run, {
      shiny::req(input$project, input$version)
      id <- safe(web_submit(con, settings, actor, input$project, input$version))
      if (!is.null(id)) notice("Request saved. You can leave this page and return later.")
    })
    output$notice <- shiny::renderText(notice())
    output$jobs <- shiny::renderTable({
      x <- state(); shiny::req(x)
      x$jobs[, c("state", "message", "progress"), drop = FALSE]
    })
    shiny::observe({
      x <- state(); shiny::req(x)
      shiny::updateSelectInput(session, "artifact", choices = setNames(x$artifacts$id, paste(x$artifacts$logical_name, "— version", x$artifacts$version)), selected = shiny::isolate(input$artifact))
    })
    output$download <- shiny::downloadHandler(
      filename = function() {
        x <- state(); names <- x$artifacts$logical_name[x$artifacts$id == input$artifact]
        if (length(names)) names[[1L]] else "result.txt"
      }, content = function(file) {
        shiny::req(input$project, input$artifact)
        if (is.null(safe(web_download(con, settings, actor, input$project, input$artifact, file))))
          stop("This download is unavailable.", call. = FALSE)
      })
  }
  shiny::shinyApp(ui, server)
}
