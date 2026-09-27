web_app <- function(settings) {
  ui <- shiny::fluidPage(theme = bslib::bs_theme(version = 5),
    shiny::titlePanel("Systematic Review Workspace"),
    shiny::uiOutput("identity_ui"), shiny::textOutput("notice"), shiny::uiOutput("workspace_ui"))
  server <- function(input, output, session) {
    con <- web_db(settings)
    session$onSessionEnded(function() DBI::dbDisconnect(con))
    token <- shiny::reactiveVal(NULL); notice <- shiny::reactiveVal("")
    refresh <- shiny::reactiveVal(0L); editing <- shiny::reactiveVal(NULL)
    draft_review <- shiny::reactiveVal(NULL)
    csrf <- web_secret()
    upload_root <- NULL
    safe <- function(expr) tryCatch(expr, error = function(e) {
      notice(if (inherits(e, "srma_web_error")) conditionMessage(e) else "The request could not finish. Ask the operator to check it.")
      if (inherits(e, "srma_web_error") && e$code == "UNAUTHENTICATED") token(NULL)
      NULL
    })
    request <- function(action, args = list()) web_request(con, token(), action, args,
      input$csrf, csrf, settings = settings, upload_root = upload_root)
    output$notice <- shiny::renderText(notice())
    output$identity_ui <- shiny::renderUI({
      if (is.null(token()) && !identical(Sys.getenv("SRMA_WEB_TEST_IDP"), "1"))
        shiny::p("Workspace sign-in is not configured yet. Contact your administrator.")
      else if (is.null(token())) shiny::tagList(
        shiny::p("Automated testing workspace. Enter the session credential supplied by the test operator."),
        shiny::passwordInput("credential", "Test session credential"), shiny::actionButton("login", "Sign in"))
      else shiny::actionButton("logout", "Sign out")
    })
    shiny::observeEvent(input$login, {
      identity <- safe(web_session(con, input$credential))
      if (!is.null(identity)) {
        if (inherits(session, "ShinySession")) upload_root <- safe(web_bind_upload_root(session, settings))
        token(input$credential); notice(""); shiny::updateTextInput(session, "credential", value = "")}
    })
    shiny::observeEvent(input$logout, {
      if (!is.null(safe(request("logout")))) {token(NULL); notice("Signed out. This session can no longer be used.")}
    })
    output$workspace_ui <- shiny::renderUI({
      shiny::req(token())
      shiny::tagList(shiny::div(style = "display:none", shiny::textInput("csrf", NULL, value = csrf)),
        shiny::tabsetPanel(id = "page",
          shiny::tabPanel("My Projects", shiny::selectInput("project", "Your reviews", character()),
            shiny::p("Choose a review to resume its saved workflow.")),
          shiny::tabPanel("New Systematic Review", shiny::uiOutput("wizard")),
          shiny::tabPanel("Project Overview", shiny::textOutput("overview"),
            shiny::selectInput("version", "Saved version", character()), shiny::actionButton("edit", "Edit as a new version")),
          shiny::tabPanel("Workflow Progress", shiny::tableOutput("workflow")),
          shiny::tabPanel("Current Action", shiny::textOutput("next_action"),
            shiny::actionButton("run", "Check existing search strategies"), shiny::tableOutput("jobs")),
          shiny::tabPanel("Search / Study Import", shiny::uiOutput("search_import")),
          shiny::tabPanel("Results / Artifacts", shiny::selectInput("artifact", "Available output", character()),
            shiny::downloadButton("download", "Download selected output")),
          shiny::tabPanel("Human Review Required",
            shiny::h3("Canonical configuration and concepts"),
            shiny::selectInput("draft_job", "Draft source for selected version", c("Human-provided saved draft"="")),
            shiny::actionButton("load_draft", "Load draft for review"), shiny::uiOutput("canonical_review"),
            shiny::tableOutput("gates"),
            shiny::selectInput("review_gate", "Pending review for selected version", character()),
            shiny::selectInput("review_decision", "Your decision", c("Choose a decision" = "", "Approve" = "APPROVED", "Reject" = "REJECTED")),
            shiny::textAreaInput("review_rationale", "Reason and supporting protocol registration reference, where applicable"),
            shiny::actionButton("record_review", "Record human review decision"),
            shiny::p("A saved draft or completed automated check does not approve scientific decisions. Review decisions remain required.")),
          shiny::tabPanel("Download", shiny::p("Individual verified outputs are available under Results / Artifacts. Final review packaging will be available after final quality control."))))
    })
    shiny::observe({
      shiny::req(token()); refresh()
      projects <- safe(request("projects")); shiny::req(projects)
      shiny::updateSelectInput(session, "project", choices = setNames(projects$id, projects$title), selected = shiny::isolate(input$project))
    })
    state <- shiny::reactive({
      shiny::invalidateLater(1500, session); shiny::req(token(), input$project)
      safe(request("view", list(project_id = input$project)))
    })
    shiny::observeEvent(list(input$project, refresh()), {
      shiny::req(token(), input$project)
      x <- safe(request("view", list(project_id = input$project))); shiny::req(x)
      shiny::updateSelectInput(session, "version", choices = if(nrow(x$versions))setNames(x$versions$id, paste("Version", x$versions$version)) else character())
    })
    output$overview <- shiny::renderText({x <- state(); shiny::req(x); x$project$title})
    output$workflow <- shiny::renderTable({
      x <- state(); shiny::req(x)
      data.frame(Step = c("Project Setup", "Protocol", "Search", "Study Import", "Screening", "Full-text Review", "Data Extraction", "Risk of Bias", "Meta-analysis", "Manuscript", "Final QC", "Download"),
        Status = c("Draft saved", "Human review required", "Search validation available", "See Search / Study Import", rep("Not enabled in this release", 7), "Individual artifacts available"))
    })
    output$next_action <- shiny::renderText({
      x <- state(); shiny::req(x)
      if (nrow(x$jobs) && x$jobs$state[1] %in% c("QUEUED", "RUNNING"))
        "Your validation check is waiting or running. You may leave this page. When it finishes, review the report under Results / Artifacts and follow any database input instructions."
      else "Review the project metadata and protocol decisions. Existing canonical search strategies can be checked here; a draft with no strategies needs search preparation first. Next, review the report and database-specific import requirements."
    })
    output$jobs <- shiny::renderTable({x <- state(); shiny::req(x); x$jobs[x$jobs$version_id==input$version, c("state", "message", "progress"), drop = FALSE]})
    output$gates <- shiny::renderTable({
      x <- state(); shiny::req(x, input$version)
      gates <- x$gates[x$gates$version_id == input$version, , drop = FALSE]
      labels <- c(protocol_registration = "Protocol registration confirmation", configuration_human_review = "Review configuration approval")
      label <- unname(labels[gates$gate_type]); label[is.na(label)] <- "Human review requirement"
      data.frame(Review = label, Status = gates$status)
    })
    shiny::observe({
      x <- state(); shiny::req(x, input$version)
      pending <- x$gates[x$gates$version_id == input$version & x$gates$status == "PENDING", , drop = FALSE]
      labels <- c(protocol_registration = "Protocol registration", configuration_human_review = "Review configuration")
      operations <- c(generate_search = "search generation", retrieve_pubmed = "PubMed retrieval")
      label <- unname(labels[pending$gate_type]); label[is.na(label)] <- "Human review"
      step <- unname(operations[pending$required_operation]); step[is.na(step)] <- "the next workflow step"
      shiny::updateSelectInput(session, "review_gate", choices = if(nrow(pending))setNames(pending$id, paste(label, "before", step)) else character(),
        selected = shiny::isolate(input$review_gate))
    })
    shiny::observeEvent(input$record_review, {
      shiny::req(input$project, input$version, input$review_gate)
      x <- state(); shiny::req(x)
      if (!input$review_gate %in% x$gates$id[x$gates$version_id == input$version & x$gates$status == "PENDING"] ||
          !input$review_decision %in% c("APPROVED", "REJECTED") ||
          is.null(input$review_rationale) || !nzchar(trimws(input$review_rationale))) {
        notice("Select a pending review, choose your decision, and provide its reason or registration reference.")
        return()
      }
      done <- safe({request("decide_gate", list(project_id = input$project, gate_id = input$review_gate,
        decision = input$review_decision, rationale = input$review_rationale)); TRUE})
      if (isTRUE(done)) {
        refresh(refresh() + 1L)
        shiny::updateSelectInput(session, "review_decision", selected = "")
        shiny::updateTextAreaInput(session, "review_rationale", value = "")
        notice("Human decision recorded for this saved version. It cannot be changed or carried to a new version automatically. Scientific runtime and search quality checks still apply.")
      }
    })
    shiny::observeEvent(input$run, {
      shiny::req(input$project, input$version)
      id <- safe(request("submit", list(project_id = input$project, version_id = input$version)))
      if (!is.null(id)) notice("Request saved. Processing continues when you leave this page.")
    })
    shiny::observe({x <- state(); shiny::req(x)
      shiny::updateSelectInput(session, "artifact", choices = if (nrow(x$artifacts)) setNames(x$artifacts$id, paste(x$artifacts$logical_name, "— version", x$artifacts$version)) else character(), selected = shiny::isolate(input$artifact))
    })
    output$download <- shiny::downloadHandler(filename = function() {
      x <- state(); shiny::req(x); name <- x$artifacts$logical_name[x$artifacts$id == input$artifact]
      if (length(name)) basename(name[1]) else "result.txt"
    }, content = function(file) {
      if (is.null(safe(request("download", list(project_id = input$project, artifact_id = input$artifact, destination = file)))))
        stop("This download is unavailable.", call. = FALSE)
    })
    output$wizard <- shiny::renderUI({
      shiny::req(token()); workspaces <- safe(request("workspaces")); shiny::req(workspaces)
      shiny::tagList(
        shiny::p("Describe your review. Scientific choices are yours to edit. Saving creates an immutable draft and does not approve the protocol or generate search terms."),
        shiny::selectInput("workspace", "Workspace", setNames(workspaces$id, workspaces$name)),
        shiny::textInput("title", "Review title"), shiny::textInput("canonical_id", "Project identifier", paste0("review-", substr(web_id(), 1, 12))),
        shiny::helpText("Use letters, numbers, hyphens or underscores. Keep this identifier when editing the same review."),
        shiny::textInput("investigator", "Principal investigator (optional)"),
        shiny::selectInput("framework", "Question framework", c("PICO", "PICOS")),
        shiny::textAreaInput("question", "Review question"),
        shiny::textAreaInput("population", "Population"), shiny::textAreaInput("intervention", "Intervention or exposure"),
        shiny::textAreaInput("comparator", "Comparator"), shiny::textAreaInput("primary", "Primary outcomes — one per line"),
        shiny::textAreaInput("secondary", "Secondary outcomes — optional, one per line"),
        shiny::textAreaInput("designs", "Eligible study designs — one per line"),
        shiny::textAreaInput("inclusion", "Inclusion criteria — one per line"),
        shiny::textAreaInput("exclusion", "Exclusion criteria — one per line"),
        shiny::textInput("reason", "Initial screening exclusion reason"),
        shiny::textInput("language", "Language restriction — state your decision"),
        shiny::checkboxGroupInput("databases", "Databases", c("PubMed" = "pubmed", "Embase" = "embase", "CENTRAL" = "central")),
        shiny::checkboxInput("inception", "Search from database inception", TRUE),
        shiny::textInput("start_date", "Search start date if not inception (YYYY-MM-DD)"),
        shiny::textInput("end_date", "Search end date (optional, YYYY-MM-DD)"),
        shiny::actionButton("save", "Save validated draft as new version"), shiny::actionButton("new_draft", "Start a separate review"))
    })
    lines <- function(value) as.list(Filter(nzchar, trimws(strsplit(if (is.null(value)) "" else value, "\n", fixed = TRUE)[[1]])))
    shiny::observeEvent(input$new_draft, {editing(NULL); shiny::updateTextInput(session, "canonical_id", value = paste0("review-", substr(web_id(), 1, 12))); notice("The next save will create a separate review. Review every field before saving.")})
    shiny::observeEvent(input$save, {
      if (!isTRUE(input$inception) && !nzchar(trimws(input$start_date))) {
        notice("Enter a search start date or choose database inception."); return()
      }
      values <- list("project.title" = input$title, "project.project_id" = input$canonical_id,
        "project.principal_investigator" = input$investigator, "research_question.framework" = input$framework,
        "research_question.question" = input$question, "research_question.population.description" = input$population,
        "research_question.intervention.description" = input$intervention, "research_question.comparator.description" = input$comparator,
        "research_question.outcomes.primary" = lines(input$primary), "research_question.outcomes.secondary" = lines(input$secondary),
        "eligibility.study_designs.include" = lines(input$designs), "eligibility.inclusion" = lines(input$inclusion), "eligibility.exclusion" = lines(input$exclusion),
        "eligibility.language_restriction" = input$language, "search.search_start_date" = if (isTRUE(input$inception)) "" else input$start_date,
        "search.search_end_date" = input$end_date, databases = as.list(input$databases), exclusion_reason = input$reason)
      edit <- editing()
      saved <- safe(request("save_configuration", list(workspace_id = input$workspace, values = values,
        project_id = if (!is.null(edit)) edit$project_id else NULL, version_id = if (!is.null(edit)) edit$version_id else NULL)))
      if (!is.null(saved)) {editing(saved); refresh(refresh() + 1L); notice("Validated draft saved as a new version. Protocol and scientific choices still require human review.")}
    })
    shiny::observeEvent(input$edit, {
      cfg <- safe(request("configuration", list(project_id = input$project, version_id = input$version))); shiny::req(cfg)
      editing(list(project_id = input$project, version_id = input$version))
      description <- function(x) if (is.list(x)) x$description else x
      fields <- list(title = cfg$project$title, canonical_id = cfg$project$project_id, investigator = cfg$project$principal_investigator,
        question = cfg$research_question$question, population = description(cfg$research_question$population),
        intervention = description(cfg$research_question$intervention), comparator = description(cfg$research_question$comparator),
        primary = if (is.list(cfg$research_question$outcomes)) cfg$research_question$outcomes$primary else cfg$research_question$outcomes,
        secondary = if (is.list(cfg$research_question$outcomes)) cfg$research_question$outcomes$secondary else NULL,
        designs = cfg$eligibility$study_designs$include, inclusion = cfg$eligibility$inclusion, exclusion = cfg$eligibility$exclusion,
        reason = cfg$screening$exclusion_reasons[[1]]$label, language = cfg$eligibility$language_restriction,
        start_date = cfg$search$search_start_date, end_date = cfg$search$search_end_date)
      for (name in names(fields)) shiny::updateTextInput(session, name, value = paste(unlist(fields[[name]]), collapse = "\n"))
      shiny::updateSelectInput(session, "framework", selected = cfg$research_question$framework)
      shiny::updateCheckboxGroupInput(session, "databases", selected = names(Filter(function(x) if (is.list(x)) isTRUE(x$enabled) else isTRUE(x), cfg$search$databases)))
      shiny::updateCheckboxInput(session, "inception", value = !nzchar(paste0(cfg$search$search_start_date)))
      projects <- safe(request("projects")); shiny::req(projects)
      shiny::updateSelectInput(session, "workspace", selected = projects$workspace_id[projects$id == input$project])
      shiny::updateTabsetPanel(session, "page", selected = "New Systematic Review")
    })
    database_info <- shiny::reactive({
      shiny::invalidateLater(3000,session)
      shiny::req(token(), input$project, input$version)
      safe(request("database_info", list(project_id = input$project, version_id = input$version)))
    })
    output$search_import <- shiny::renderUI({
      info <- database_info(); shiny::req(info)
      shiny::tagList(
        shiny::p("Prepare canonical concepts, review and activate a new version, record its required protocol decisions, then generate searches. PubMed retrieval requires canonical quality PASS for the exact saved query."),
        shiny::actionButton("initialize_project", "Prepare canonical draft"),
        shiny::p("If the operator has not configured the scientific runtime and provider credential, your saved work is retained. You may enter human-provided concepts below."),
        shiny::textAreaInput("manual_terms", "Human-provided canonical search concepts (YAML)", rows=10),
        shiny::p("Supply the canonical project, search concepts and validation fields. Terms and controlled vocabulary must be explicitly chosen by you; the application does not invent synonyms."),
        shiny::actionButton("save_terms", "Save human-provided draft as new version"),
        shiny::actionButton("generate_search", "Generate searches for approved version"),
        shiny::actionButton("retrieve_pubmed", "Retrieve quality-approved PubMed query"),
        shiny::textOutput("quality_status"),
        lapply(info, function(db) shiny::tagList(
          shiny::h3(db$label),
          shiny::p(if (db$mode == "automated") "Request PubMed retrieval after the exact query passes canonical quality checks."
            else paste0("Run the displayed ", db$label, " strategy in the database, export all results as ", db$format, ", and upload the export below.")),
          shiny::p(if (db$input_present) "Input saved. Canonical preflight must confirm validity." else "Input still required."),
          shiny::tags$label("Canonical saved search strategy — copy for database use"),
          shiny::tags$textarea(readonly = NA, rows = 6, style = "width:100%", db$strategy),
          if (!nzchar(db$strategy)) shiny::p("No canonical strategy is saved for this version. Complete search preparation before searching the database."))),
        shiny::selectInput("strategy_database", "Search strategy to download", setNames(
          vapply(info, function(x) x$database, character(1)), vapply(info, function(x) x$label, character(1)))),
        shiny::downloadButton("strategy_download", "Download canonical search strategy"),
        shiny::selectInput("upload_database", "Manual database", setNames(
          vapply(Filter(function(x) x$mode == "guided_manual", info), function(x) x$database, character(1)),
          vapply(Filter(function(x) x$mode == "guided_manual", info), function(x) x$label, character(1)))),
        if (!is.null(upload_root)) shiny::fileInput("export_file", "Database export (RIS; up to 20 MB)", accept = ".ris"),
        shiny::actionButton("upload_export", "Save export as new version"),
        shiny::actionButton("preflight", "Check database inputs"),
        shiny::tableOutput("database_validation"),
        shiny::p("Each upload creates a new immutable version. Preflight never starts merge, deduplication or screening."))
    })
    shiny::observeEvent(list(input$project,input$version), {draft_review(NULL)})
    shiny::observe({
      x<-state();shiny::req(x,input$version)
      jobs<-x$jobs[x$jobs$version_id==input$version & x$jobs$operation=="initialize_project" & x$jobs$state=="COMPLETE",,drop=FALSE]
      shiny::updateSelectInput(session,"draft_job",choices=c("Human-provided saved draft"="",if(nrow(jobs))setNames(jobs$id,paste("Prepared draft",jobs$created_at)) else character()),selected=shiny::isolate(input$draft_job))
    })
    shiny::observeEvent(input$load_draft, {
      shiny::req(input$project,input$version)
      review<-safe(request("initialization_review",list(project_id=input$project,version_id=input$version,
        job_id=if(is.null(input$draft_job)||!nzchar(input$draft_job))NULL else input$draft_job)))
      draft_review(review)
    })
    output$canonical_review<-shiny::renderUI({
      r<-draft_review();shiny::req(r)
      shiny::tagList(shiny::h4("Proposed configuration"),shiny::tags$pre(r$config),
        shiny::h4("Proposed search concepts"),shiny::tags$pre(r$terms),
        lapply(seq_along(r$decisions),function(i){d<-r$decisions[[i]];shiny::tagList(
          shiny::h4(d$question),shiny::p(d$why_it_matters),
          shiny::p(paste("Canonical recommendation:",d$recommended_option)),shiny::p(d$recommendation_rationale),
          shiny::selectInput(paste0("canonical_decision_",i),"Your explicit choice",c("Choose an option"="",unlist(d$suggested_options))))}),
        shiny::textAreaInput("activation_rationale","Review rationale"),
        shiny::checkboxInput("approve_canonical","I reviewed this configuration, these concepts and all listed decisions",FALSE),
        shiny::actionButton("activate_canonical","Approve and activate as a new saved version"))
    })
    shiny::observeEvent(input$activate_canonical, {
      r<-draft_review();shiny::req(r,input$project,input$version)
      if(!identical(r$project_id,input$project)||!identical(r$version_id,input$version)) {notice("Reload the draft for this selected version.");return()}
      selections<-setNames(lapply(seq_along(r$decisions),function(i)input[[paste0("canonical_decision_",i)]]),
        vapply(r$decisions,`[[`,character(1),"decision_id"))
      saved<-safe(request("activate_draft",list(project_id=r$project_id,version_id=r$version_id,job_id=r$job_id,
        selections=selections,rationale=input$activation_rationale,human_approved=isTRUE(input$approve_canonical))))
      if(!is.null(saved)){draft_review(NULL);refresh(refresh()+1L);notice("Approved canonical inputs saved as a new immutable version. Select it and record its protocol decisions before search generation.")}
    })
    shiny::observeEvent(input$save_terms, {
      shiny::req(input$project,input$version,input$manual_terms)
      saved<-safe(request("manual_concepts",list(project_id=input$project,version_id=input$version,terms_yaml=input$manual_terms)))
      if(!is.null(saved)){refresh(refresh()+1L);notice("Human-provided draft saved. Select the new version and load it under Human Review Required.")}
    })
    for(op in c("initialize_project","generate_search","retrieve_pubmed")) local({
      operation<-op
      shiny::observeEvent(input[[operation]],{
        shiny::req(input$project,input$version)
        id<-safe(request("submit",list(project_id=input$project,version_id=input$version,operation=operation)))
        if(!is.null(id))notice("Version-bound request saved. Review its status and registered outputs.")
      })
    })
    output$quality_status<-shiny::renderText({
      x<-state();shiny::req(x,input$version)
      j<-x$jobs[x$jobs$version_id==input$version & x$jobs$operation=="generate_search",,drop=FALSE]
      if(!nrow(j))"No generated search quality result for this version." else paste(j$state[1],j$message[1])
    })
    output$database_validation <- shiny::renderTable({
      shiny::invalidateLater(2000, session); shiny::req(token(), input$project, input$version)
      safe(request("database_status", list(project_id = input$project, version_id = input$version)))
    })
    output$strategy_download <- shiny::downloadHandler(
      filename = function() paste0(if (input$strategy_database %in% c("pubmed", "embase", "central")) input$strategy_database else "database", "_search.txt"),
      content = function(file) {
        if (is.null(safe(request("download_strategy", list(project_id = input$project, version_id = input$version,
          database = input$strategy_database, destination = file))))) stop("This download is unavailable.", call. = FALSE)
      })
    shiny::observeEvent(input$upload_export, {
      shiny::req(input$export_file, input$project, input$version, upload_root)
      saved <- safe(request("upload_database", list(project_id = input$project, version_id = input$version,
        database = input$upload_database, upload = input$export_file)))
      if (!is.null(saved)) {refresh(refresh() + 1L); notice("Export saved in a new version. Run canonical database preflight to check it.")}
    })
    shiny::observeEvent(input$preflight, {
      shiny::req(input$project, input$version)
      id <- safe(request("submit", list(project_id = input$project, version_id = input$version, operation = "validate_database_inputs")))
      if (!is.null(id)) notice("Database preflight queued. Review its report when processing finishes.")
    })
  }
  shiny::shinyApp(ui, server)
}
