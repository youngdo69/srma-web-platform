web_c4_ui <- function() shiny::tabPanel("Download Center",
  shiny::p("Download the outputs that belong to the selected reviewed version. A final release package becomes available only after every mandatory release check is complete."),
  shiny::actionButton("release_refresh", "Refresh downloads"),
  shiny::selectInput("release_qc", "Reviewed version", character()),
  shiny::textOutput("release_status"),
  shiny::uiOutput("release_next_step"),
  shiny::tableOutput("release_items"),
  shiny::selectInput("release_item", "Available file", character()),
  shiny::downloadButton("release_item_download", "Download selected file"),
  shiny::hr(),
  shiny::uiOutput("release_build_area"),
  shiny::selectInput("release_package", "Final package history", character()),
  shiny::downloadButton("release_package_download", "Download selected package"))

web_c4_server <- function(input, output, session, request, safe, notice, refresh) {
  catalog <- shiny::reactiveVal(NULL)
  packages <- shiny::reactiveVal(NULL)

  gate_label <- function(name) switch(name,
    POSTGRESQL = "Production database check",
    CANONICAL_RUNTIME = "Canonical analysis environment",
    SCIENTIFIC_PROVIDER = "Scientific provider check",
    AI = "AI provider check",
    MANUSCRIPT_PROVIDER = "Manuscript provider check",
    name)

  load_qc <- function(selected = NULL) {
    x <- request("release_qc_list", list(project_id = input$project))
    choices <- if (nrow(x)) {
      labels <- paste(ifelse(seq_len(nrow(x)) == 1L, "Latest reviewed QC", "Earlier reviewed QC"), x$created_at)
      setNames(x$id, labels)
    } else character()
    chosen <- selected %||b2% shiny::isolate(input$release_qc)
    if ((!nzchar(chosen %||b2% "")) && length(choices)) chosen <- unname(choices[[1L]])
    shiny::updateSelectInput(session, "release_qc", choices = choices, selected = chosen)
    invisible(x)
  }

  load_packages <- function(selected = NULL) {
    x <- request("release_package_list", list(project_id = input$project)); packages(x)
    labels <- if (nrow(x)) paste(ifelse(x$current, "Current lineage", "Historical"), substr(x$package_checksum, 1, 12), x$created_at) else character()
    shiny::updateSelectInput(session, "release_package", choices = if (nrow(x)) setNames(x$id, labels) else character(), selected = selected %||b2% shiny::isolate(input$release_package))
    invisible(x)
  }

  load_catalog <- function() {
    shiny::req(input$release_qc)
    x <- request("release_catalog", list(project_id = input$project, qc_id = input$release_qc)); catalog(x)
    items <- x$entries
    labels <- if (length(items)) vapply(items, function(a) paste(toupper(a$category), "—", a$logical_name), character(1)) else character()
    ids <- if (length(items)) vapply(items, `[[`, character(1), "item_id") else character()
    shiny::updateSelectInput(session, "release_item", choices = setNames(ids, labels), selected = shiny::isolate(input$release_item))
    invisible(x)
  }

  refresh_center <- function() {
    load_qc()
    load_packages()
    if (nzchar(input$release_qc %||b2% "")) load_catalog()
  }

  shiny::observeEvent(input$project, safe({catalog(NULL); packages(NULL); refresh_center()}), ignoreInit = TRUE)
  shiny::observeEvent(input$release_refresh, safe(refresh_center()))
  shiny::observeEvent(input$release_qc, safe(load_catalog()), ignoreInit = TRUE)
  shiny::observeEvent(input$release_build, safe({
    shiny::req(input$release_qc)
    built <- request("release_build", list(project_id = input$project, qc_id = input$release_qc))
    if (!is.null(built)) {
      load_packages(built$package_id)
      notice(if (isTRUE(built$reused)) "The identical final package already exists and has been selected." else "Final release package created from the exact approved lineage.")
    }
  }))

  output$release_status <- shiny::renderText({
    x <- catalog()
    if (is.null(x)) return("Select a reviewed Final QC version to see its downloads.")
    if (!isTRUE(x$current)) return("This is a historical reviewed version. Its files remain available for audit, but it cannot be used for a new final package.")
    if (isTRUE(x$release_eligible)) return("All mandatory release checks are complete. The final package can be created.")
    "Your reviewed manuscript and outputs are available below. Final-package creation is still blocked by one or more operating-environment checks."
  })

  output$release_next_step <- shiny::renderUI({
    x <- catalog(); shiny::req(x)
    mandatory <- intersect(c("POSTGRESQL", "CANONICAL_RUNTIME"), names(x$external))
    blocking <- mandatory[unlist(x$external[mandatory]) != "ACCEPTED"]
    optional <- setdiff(names(x$external)[unlist(x$external) != "ACCEPTED"], mandatory)
    blocks <- list()
    if (length(blocking)) blocks <- c(blocks, list(shiny::div(class = "alert alert-warning",
      shiny::strong("Before a final release package can be created:"),
      shiny::tags$ul(lapply(blocking, function(n) shiny::tags$li(gate_label(n)))))))
    if (!length(blocking) && isTRUE(x$release_eligible)) blocks <- c(blocks, list(shiny::div(class = "alert alert-success", "All mandatory release checks are complete.")))
    if (length(optional)) blocks <- c(blocks, list(shiny::div(class = "alert alert-light",
      "Additional provider checks remain pending for workflows that use those services: ",
      paste(vapply(optional, gate_label, character(1)), collapse = ", "), ".")))
    shiny::tagList(blocks)
  })

  output$release_items <- shiny::renderTable({
    x <- catalog(); shiny::req(x)
    if (!length(x$entries)) return(data.frame())
    data.frame(
      Type = vapply(x$entries, function(a) tools::toTitleCase(a$category), character(1)),
      File = vapply(x$entries, `[[`, character(1), "logical_name"),
      Size = vapply(x$entries, function(a) {
        b <- as.numeric(a$size)
        if (b >= 1024^2) sprintf("%.1f MB", b / 1024^2) else if (b >= 1024) sprintf("%.1f KB", b / 1024) else paste0(b, " B")
      }, character(1)),
      stringsAsFactors = FALSE)
  })

  output$release_build_area <- shiny::renderUI({
    x <- catalog()
    if (is.null(x)) return(NULL)
    if (!isTRUE(x$current)) return(shiny::div(class = "alert alert-secondary", "Historical versions cannot create a new final package."))
    if (!isTRUE(x$release_eligible)) return(shiny::div(class = "alert alert-secondary", "Final package unavailable until the checks listed above are completed. Individual approved files can still be downloaded."))
    shiny::tagList(
      shiny::actionButton("release_build", "Create final release package", class = "btn-primary"),
      shiny::p("The package contains only registered, checksum-verified files from this exact approved lineage."))
  })

  output$release_item_download <- shiny::downloadHandler(
    filename = function() {
      x <- catalog(); shiny::req(x, input$release_item)
      hit <- Filter(function(a) a$item_id == input$release_item, x$entries)
      if (length(hit)) web_c4_safe_component(hit[[1]]$logical_name) else "output.bin"
    },
    content = function(file) {
      shiny::req(input$release_qc, input$release_item)
      if (is.null(safe(request("release_download_item", list(project_id = input$project, qc_id = input$release_qc, item_id = input$release_item, destination = file))))) stop("This download is unavailable.", call. = FALSE)
    })

  output$release_package_download <- shiny::downloadHandler(
    filename = function() if (nzchar(input$release_package %||b2% "")) paste0("SRMA_FINAL_PACKAGE_", substr(input$release_package, 1, 8), ".zip") else "SRMA_FINAL_PACKAGE.zip",
    content = function(file) {
      shiny::req(input$release_package)
      if (is.null(safe(request("release_download_package", list(project_id = input$project, package_id = input$release_package, destination = file))))) stop("This package is unavailable.", call. = FALSE)
    })

  shiny::observe({
    shiny::req(input$project)
    refresh()
    if (is.null(packages())) safe(refresh_center())
  })
}
