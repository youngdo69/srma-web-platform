source(file.path(web_root,"web/ui/evidence.R"))
source(file.path(web_root,"web/ui/analysis.R"))
source(file.path(web_root,"web/ui/manuscript.R"))
source(file.path(web_root,"web/ui/final_qc.R"))
source(file.path(web_root,"web/ui/download_center.R"))
source(file.path(web_root,"web/ui/security_audit.R"))
source(file.path(web_root,"web/ui/deployment.R"))
srma_has_text <- function(x) {
  !is.null(x) && length(x) > 0L && nzchar(trimws(as.character(x[[1L]])))
}

empty_reviews_ui <- function() {
  shiny::div(
    class = "srma-empty-state",
    shiny::div(class = "srma-empty-icon", "JS"),
    shiny::h2("Welcome to JS SR-MA Toolkit", class = "srma-empty-title"),
    shiny::p(
      paste(
        "Create your first systematic review and manage the entire workflow in one place —",
        "from protocol setup and literature search to screening, data extraction, meta-analysis,",
        "manuscript preparation, and final quality control."
      ),
      class = "srma-empty-description"
    ),
    shiny::div(
      class = "srma-empty-message",
      "You don't have any reviews yet."
    ),
    shiny::actionButton(
      inputId = "start_first_review",
      label = "Start your first systematic review",
      class = "btn-primary srma-primary-action"
    ),
    shiny::p(
      "You can return to this dashboard anytime to continue an existing review.",
      class = "srma-empty-hint"
    )
  )
}


srma_required_label <- function(text) {
  shiny::tagList(
    text,
    shiny::span(" *", class = "srma-required", `aria-hidden` = "true")
  )
}

srma_pending_decision_count <- function(gates) {
  if (is.null(gates) || !nrow(gates)) return(0L)
  initial_types <- c("configuration_human_review", "protocol_registration")
  initial <- gates$gate_type %in% initial_types
  as.integer(sum(!initial) + length(unique(gates$gate_type[initial])))
}

srma_human_decision_card <- function(prefix, title, description, applies_to, rationale_label) {
  shiny::div(
    class = "srma-human-card",
    shiny::div(
      class = "srma-human-card-head",
      shiny::div(
        shiny::div(class = "srma-human-card-title", title),
        shiny::p(description, class = "srma-human-card-description")
      ),
      shiny::span("Required", class = "srma-human-badge")
    ),
    if (nzchar(applies_to))
      shiny::div(
        class = "srma-human-applies",
        shiny::strong("Applies before: "),
        applies_to
      ),
    shiny::radioButtons(
      inputId = paste0(prefix, "_decision"),
      label = "Your decision",
      choices = c("Approve" = "APPROVED", "Request changes" = "REJECTED"),
      selected = character(),
      inline = TRUE
    ),
    shiny::textAreaInput(
      inputId = paste0(prefix, "_rationale"),
      label = rationale_label,
      rows = 4,
      width = "100%"
    ),
    shiny::div(
      class = "srma-human-card-actions",
      shiny::actionButton(
        inputId = paste0("save_", prefix),
        label = "Save decision",
        class = "btn-primary"
      )
    )
  )
}


srma_protocol_registration_card <- function() {
  shiny::div(
    class = "srma-human-card srma-protocol-card",
    shiny::div(
      class = "srma-human-card-head",
      shiny::div(
        shiny::div(class = "srma-human-card-title", "Protocol registration"),
        shiny::p(
          "Select the current registration status. The registration gate remains open until a completed registration is confirmed.",
          class = "srma-human-card-description"
        )
      ),
      shiny::span("Required", class = "srma-human-badge")
    ),
    shiny::div(
      class = "srma-human-applies",
      shiny::strong("Required before: "),
      "literature search begins"
    ),
    shiny::radioButtons(
      inputId = "protocol_registration_status",
      label = "Registration status",
      choices = c(
        "Registered" = "REGISTERED",
        "Registration in progress" = "IN_PROGRESS",
        "Not registered" = "NOT_REGISTERED"
      ),
      selected = character(),
      inline = FALSE
    ),
    shiny::conditionalPanel(
      condition = "input.protocol_registration_status == 'REGISTERED'",
      shiny::div(
        class = "srma-protocol-panel",
        shiny::selectInput(
          "protocol_registry",
          "Registry *",
          choices = c("PROSPERO" = "PROSPERO", "Other accepted registry" = "OTHER"),
          selected = "PROSPERO"
        ),
        shiny::textInput(
          "protocol_registration_reference",
          "Registration ID or URL *",
          placeholder = "e.g., CRD420... or registry URL"
        ),
        shiny::textAreaInput(
          "protocol_registration_notes",
          "Notes (optional)",
          rows = 3,
          width = "100%",
          placeholder = "Optional note about the registered protocol."
        ),
        shiny::div(
          class = "srma-protocol-guidance srma-protocol-guidance-ok",
          "Confirming a completed registration will satisfy this Human Gate for the selected saved version."
        ),
        shiny::div(
          class = "srma-human-card-actions",
          shiny::actionButton(
            "confirm_protocol_registration",
            "Confirm registration",
            class = "btn-primary"
          )
        )
      )
    ),
    shiny::conditionalPanel(
      condition = "input.protocol_registration_status == 'IN_PROGRESS'",
      shiny::div(
        class = "srma-protocol-guidance srma-protocol-guidance-warn",
        shiny::strong("Registration remains pending."),
        shiny::br(),
        "Complete the registration and return here to enter the registry reference. No Human Gate is closed at this stage."
      )
    ),
    shiny::conditionalPanel(
      condition = "input.protocol_registration_status == 'NOT_REGISTERED'",
      shiny::div(
        class = "srma-protocol-guidance srma-protocol-guidance-block",
        shiny::strong("Registration is required before the literature-search workflow can continue."),
        shiny::br(),
        "The Human Gate will remain pending until a completed registration is confirmed."
      )
    )
  )
}

web_app <- function(settings) {
  auto_dev_session <- identical(settings$mode, "development") &&
    identical(Sys.getenv("SRMA_WEB_TEST_IDP"), "1")

  ui <- shiny::fluidPage(
    theme = bslib::bs_theme(version = 5),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        body { background: #f7f9fc; color: #1f2937; }
        .srma-shell { max-width: 1600px; margin: 0 auto; padding: 28px 34px 54px; }
        .srma-header { display:flex; align-items:flex-start; justify-content:space-between; gap:24px; margin-bottom:16px; }
        .srma-brand h1 { margin:0; font-size:38px; font-weight:800; letter-spacing:-0.025em; }
        .srma-brand p { margin:8px 0 0; color:#6b7280; font-size:18px; }
        .srma-identity { min-width:240px; text-align:right; font-size:17px; }
        .srma-notice { margin: 6px 0 14px; color:#9a3412; font-weight:700; font-size:17px; }
        .nav-tabs { gap: 3px; border-bottom-color:#cbd5e1; }
        .nav-tabs .nav-link {
          color:#1f5f8b;
          font-weight:700;
          border-radius:8px 8px 0 0;
          padding:10px 16px;
          transition:background-color .15s ease, color .15s ease, border-color .15s ease;
        }
        .nav-tabs .nav-link:hover {
          color:#124a70;
          background:#eaf4fb;
          border-color:#d7e8f5 #d7e8f5 transparent;
        }
        .nav-tabs .nav-link.active {
          color:#ffffff;
          background:#2474a8;
          border-color:#2474a8 #2474a8 #ffffff;
          font-weight:700;
        }
        .tab-content { background:#fff; border:1px solid #e5e7eb; border-top:0; padding:24px; min-height:420px; border-radius:0 0 12px 12px; }
        .srma-empty-state { max-width:1180px; min-height:calc(100vh - 365px); margin:28px auto 18px; padding:68px 76px; text-align:center; background:#fff; border:1px solid #e5e7eb; border-radius:22px; box-shadow:0 10px 34px rgba(15,23,42,.08); display:flex; flex-direction:column; align-items:center; justify-content:center; }
        .srma-empty-icon { width:86px; height:86px; margin:0 auto 28px; display:flex; align-items:center; justify-content:center; border-radius:22px; background:#eef4fb; color:#244a73; font-size:30px; font-weight:800; }
        .srma-empty-title { margin:0 0 20px; font-size:42px; font-weight:800; letter-spacing:-0.02em; }
        .srma-empty-description { max-width:900px; margin:0 auto 30px; font-size:22px; line-height:1.72; color:#4b5563; }
        .srma-empty-message { margin-bottom:26px; font-size:21px; font-weight:700; }
        .srma-primary-action { padding:16px 34px; font-size:20px; font-weight:800; border-radius:11px; }
        .srma-empty-hint { margin:24px 0 0; font-size:17px; color:#6b7280; }
        .srma-project-picker { max-width:1100px; margin:28px auto; font-size:18px; }
        .srma-wizard-wrap { max-width:1320px; margin:20px auto 40px; }
        .srma-wizard-heading { margin-bottom:26px; }
        .srma-wizard-heading h2 { margin:0 0 10px; font-size:38px; font-weight:800; letter-spacing:-0.02em; }
        .srma-wizard-heading p { margin:0; color:#64748b; font-size:20px; line-height:1.6; }
        .srma-wizard-progress { display:grid; grid-template-columns:repeat(5,1fr); gap:10px; margin:0 0 26px; }
        .srma-wizard-step { position:relative; min-width:0; padding:16px 14px; border-radius:12px; background:#f1f5f9; color:#64748b; font-size:19px; font-weight:800; text-align:center; }
        .srma-wizard-step.active { background:#2474a8; color:#fff; box-shadow:0 4px 14px rgba(36,116,168,.18); }
        .srma-wizard-step.done { background:#e6f2fa; color:#1f5f8b; }
        .srma-wizard-card { background:#fff; border:1px solid #e2e8f0; border-radius:20px; box-shadow:0 10px 32px rgba(15,23,42,.07); padding:46px 54px; min-height:690px; display:flex; flex-direction:column; }
        .srma-wizard-kicker { color:#2474a8; font-size:17px; font-weight:800; letter-spacing:.04em; text-transform:uppercase; margin-bottom:7px; }
        .srma-wizard-title { margin:0 0 10px; font-size:34px; font-weight:800; letter-spacing:-0.015em; }
        .srma-wizard-subtitle { margin:0 0 32px; color:#64748b; font-size:20px; line-height:1.65; }
        .srma-form-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:10px 28px; }
        .srma-form-grid .form-group, .srma-form-grid .shiny-input-container { width:100% !important; max-width:none !important; }
        .srma-form-full { grid-column:1 / -1; }
        .srma-wizard-card textarea.form-control { min-height:154px; line-height:1.6; }
        .srma-wizard-card label { font-weight:750; font-size:20px; color:#27364a; margin-bottom:10px; }
        .srma-wizard-card .form-control, .srma-wizard-card .form-select { border-color:#cbd5e1; border-radius:10px; font-size:19px; min-height:52px; padding:12px 15px; }
        .srma-required { color:#b42318; font-weight:800; }
        .srma-field-help { margin:0 0 16px; color:#64748b; font-size:17px; line-height:1.55; }
        .srma-field-error { margin:-2px 0 16px; color:#b42318; font-size:17px; font-weight:800; }
        .srma-wizard-card .form-control.srma-invalid, .srma-wizard-card .form-select.srma-invalid { border-color:#dc2626; }
        .srma-wizard-card details { margin-top:12px; border-top:1px solid #e5e7eb; padding-top:14px; }
        .srma-wizard-card details summary { cursor:pointer; color:#475569; font-weight:800; font-size:18px; }
        .srma-wizard-footer { display:flex; align-items:center; justify-content:space-between; gap:12px; margin-top:auto; padding-top:24px; border-top:1px solid #e5e7eb; }
        .srma-wizard-footer-right { display:flex; gap:10px; margin-left:auto; }
        .srma-wizard-footer .btn { min-width:150px; min-height:52px; padding:12px 24px; border-radius:10px; font-size:19px; font-weight:800; }
        .srma-summary-section { margin-top:22px; }
        .srma-summary-section:first-of-type { margin-top:8px; }
        .srma-summary-section h4 { margin:0 0 14px; font-size:25px; font-weight:800; color:#1f3b57; }
        .srma-summary-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:14px 18px; width:100%; }
        .srma-summary-item { padding:14px 16px; border:1px solid #e2e8f0; border-radius:10px; background:#f8fafc; }
        .srma-summary-item.full { grid-column:1 / -1; }
        .srma-summary-label { display:block; color:#64748b; font-size:15px; font-weight:800; text-transform:uppercase; letter-spacing:.04em; margin-bottom:7px; }
        .srma-summary-value { white-space:pre-wrap; color:#1f2937; line-height:1.65; font-size:19px; }
        .srma-review-note { margin-top:24px; padding:18px 20px; border-radius:12px; background:#eef6fb; color:#31556f; font-size:18px; line-height:1.65; }
        .srma-primary-nav { display:flex; flex-wrap:wrap; gap:6px; align-items:center; margin:4px 0 10px; padding:10px 0 14px; border-bottom:1px solid #dbe4ee; }
        .srma-nav-btn { appearance:none; border:1px solid transparent; background:transparent; color:#165d8c; font-weight:500; font-size:24px; line-height:1.15; padding:12px 13px; border-radius:11px; cursor:pointer; transition:all .15s ease; }
        .srma-nav-btn:hover:not(:disabled) { color:#0f4c74; background:#eaf4fb; border-color:#d5e8f5; }
        .srma-nav-btn.active { color:#fff; background:#2474a8; border-color:#2474a8; font-weight:700; box-shadow:0 4px 14px rgba(36,116,168,.18); }
        .srma-nav-btn:disabled { color:#9aa8b6; cursor:not-allowed; opacity:.72; }
        .srma-secondary-nav { display:flex; flex-wrap:wrap; gap:9px; align-items:center; margin:2px 0 16px; min-height:46px; }
        .srma-subnav-btn { appearance:none; border:1px solid #d7e5ef; background:#f7fbfe; color:#315f7d; font-weight:600; font-size:17px; padding:10px 15px; border-radius:999px; cursor:pointer; transition:all .15s ease; }
        .srma-subnav-btn:hover:not(:disabled) { background:#eaf4fb; color:#174f73; }
        .srma-subnav-btn.active { background:#dceef9; border-color:#a9d2ea; color:#114f78; font-weight:700; }
        .srma-subnav-btn:disabled { color:#9aa8b6; background:#f5f7f9; border-color:#e5e9ed; cursor:not-allowed; }
        .srma-internal-tabs > .tabbable > .nav-tabs { display:none !important; }
        .srma-internal-tabs > .tabbable > .tab-content { border:1px solid #e5e7eb; border-radius:14px; padding:30px; min-height:calc(100vh - 245px); background:#fff; }
        .srma-nav-hint { color:#7a8897; font-size:16px; margin:2px 0 14px; }
        .srma-dashboard { max-width:1320px; margin:0 auto; }
        .srma-dashboard-heading { display:flex; align-items:flex-end; justify-content:space-between; gap:20px; margin:2px 0 24px; }
        .srma-dashboard-heading h2 { margin:0 0 6px; font-size:34px; font-weight:750; letter-spacing:-0.02em; color:#172b3f; }
        .srma-dashboard-heading p { margin:0; color:#64748b; font-size:18px; line-height:1.55; }
        .srma-dashboard-selector { display:grid; grid-template-columns:minmax(0,1fr) 230px auto; gap:16px; align-items:end; padding:22px 24px; margin-bottom:22px; border:1px solid #dfe7ef; border-radius:16px; background:#f8fbfe; }
        .srma-dashboard-selector .form-group, .srma-dashboard-selector .shiny-input-container { width:100% !important; margin-bottom:0 !important; }
        .srma-dashboard-selector label { font-size:16px; font-weight:600; color:#3a5168; margin-bottom:7px; }
        .srma-dashboard-selector .form-control, .srma-dashboard-selector .form-select { min-height:48px; font-size:17px; border-radius:9px; }
        .srma-dashboard-selector .btn { min-height:48px; padding:10px 18px; border-radius:9px; font-size:16px; font-weight:600; white-space:nowrap; }
        .srma-dashboard-cards { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:18px; margin-bottom:22px; }
        .srma-dashboard-card { min-height:190px; padding:24px 26px; border:1px solid #e0e7ef; border-radius:16px; background:#fff; box-shadow:0 6px 22px rgba(15,23,42,.045); display:flex; flex-direction:column; }
        .srma-dashboard-card.accent { background:linear-gradient(145deg,#f7fbfe,#edf6fc); border-color:#cfe5f4; }
        .srma-card-eyebrow { color:#5b7188; font-size:14px; font-weight:700; text-transform:uppercase; letter-spacing:.055em; margin-bottom:10px; }
        .srma-card-value { color:#17344e; font-size:29px; line-height:1.15; font-weight:750; letter-spacing:-0.015em; margin-bottom:10px; }
        .srma-card-text { color:#617286; font-size:16px; line-height:1.55; margin:0 0 16px; }
        .srma-card-action { margin-top:auto; align-self:flex-start; min-height:44px; padding:9px 17px; border-radius:9px; font-size:16px; font-weight:650; }
        .srma-attention-number { font-size:42px; line-height:1; font-weight:750; color:#9a4a11; margin:2px 0 9px; }
        .srma-attention-number.clear { color:#28734f; }
        .srma-dashboard-panel { margin-top:20px; padding:26px; border:1px solid #e0e7ef; border-radius:16px; background:#fff; }
        .srma-dashboard-panel h3 { margin:0 0 8px; font-size:24px; font-weight:700; color:#203b54; }
        .srma-dashboard-panel > p { margin:0 0 22px; color:#6b7b8c; font-size:16px; }
        .srma-workflow-map { display:grid; grid-template-columns:repeat(7,minmax(0,1fr)); gap:8px; align-items:stretch; }
        .srma-workflow-node { position:relative; padding:18px 12px; min-height:104px; border:1px solid #e1e7ed; border-radius:12px; background:#f8fafc; text-align:center; display:flex; flex-direction:column; align-items:center; justify-content:center; }
        .srma-workflow-node .dot { width:14px; height:14px; border-radius:50%; margin-bottom:10px; background:#cbd5e1; }
        .srma-workflow-node .label { color:#506176; font-size:15px; line-height:1.3; font-weight:600; }
        .srma-workflow-node.complete { background:#f0f8f4; border-color:#cfe6d8; }
        .srma-workflow-node.complete .dot { background:#3c8b61; }
        .srma-workflow-node.complete .label { color:#326b4d; }
        .srma-workflow-node.current { background:#edf6fc; border-color:#acd4eb; box-shadow:0 4px 14px rgba(36,116,168,.08); }
        .srma-workflow-node.current .dot { background:#2474a8; }
        .srma-workflow-node.current .label { color:#165d8c; font-weight:700; }
        .srma-workflow-node.upcoming { opacity:.82; }
        .srma-activity-list { list-style:none; margin:0; padding:0; }
        .srma-activity-list li { padding:12px 0; border-bottom:1px solid #edf1f5; color:#495c70; font-size:16px; line-height:1.45; }
        .srma-activity-list li:last-child { border-bottom:0; }
        .srma-activity-state { display:inline-block; min-width:86px; margin-right:10px; color:#587087; font-weight:700; }
        .srma-human-wrap { max-width:1120px; margin:0 auto; }
        .srma-human-heading { margin:2px 0 26px; }
        .srma-human-heading h2 { margin:0 0 8px; font-size:34px; font-weight:750; color:#172b3f; letter-spacing:-0.02em; }
        .srma-human-heading p { margin:0; color:#64748b; font-size:18px; line-height:1.6; }
        .srma-human-summary { display:flex; align-items:center; gap:12px; margin:0 0 20px; padding:15px 18px; border-radius:12px; background:#eef6fb; color:#31556f; font-size:17px; }
        .srma-human-summary strong { font-size:20px; color:#164f75; }
        .srma-human-card { margin:0 0 20px; padding:26px 28px; border:1px solid #dfe7ef; border-radius:16px; background:#fff; box-shadow:0 6px 22px rgba(15,23,42,.045); }
        .srma-human-card-head { display:flex; align-items:flex-start; justify-content:space-between; gap:18px; margin-bottom:14px; }
        .srma-human-card-title { color:#17344e; font-size:25px; line-height:1.2; font-weight:750; margin-bottom:7px; }
        .srma-human-card-description { margin:0; color:#617286; font-size:17px; line-height:1.6; }
        .srma-human-badge { flex:0 0 auto; padding:7px 11px; border-radius:999px; background:#fff2df; color:#8a4b13; font-size:13px; font-weight:750; }
        .srma-human-applies { margin:0 0 20px; padding:11px 13px; border-radius:9px; background:#f7f9fc; color:#566a7d; font-size:15px; }
        .srma-human-card .control-label { font-size:17px; font-weight:650; color:#27364a; margin-bottom:8px; }
        .srma-human-card .radio-inline { font-size:17px; margin-right:24px; }
        .srma-human-card textarea.form-control { min-height:112px; font-size:16px; line-height:1.55; }
        .srma-human-card-actions { display:flex; justify-content:flex-end; padding-top:4px; }
        .srma-human-card-actions .btn { min-height:46px; padding:10px 20px; border-radius:9px; font-size:16px; font-weight:650; }
        .srma-protocol-card .shiny-options-group { margin-bottom:8px; }
        .srma-protocol-panel { margin-top:10px; }
        .srma-protocol-guidance { margin-top:14px; padding:14px 16px; border-radius:10px; line-height:1.55; font-size:15px; }
        .srma-protocol-guidance-ok { background:#eef8f1; border:1px solid #b9dfc4; color:#245c34; }
        .srma-protocol-guidance-warn { background:#fff8e8; border:1px solid #efd79a; color:#795600; }
        .srma-protocol-guidance-block { background:#fff1f1; border:1px solid #efb8b8; color:#8a2525; }
        .srma-human-details { margin-top:22px; border-top:1px solid #e5e7eb; padding-top:20px; }
        .srma-human-details summary { cursor:pointer; color:#315f7d; font-size:17px; font-weight:650; }
        .srma-human-details-body { margin-top:18px; padding:20px; border-radius:12px; background:#f8fafc; }
        .srma-human-complete { padding:30px; border:1px solid #cfe6d8; border-radius:16px; background:#f0f8f4; color:#326b4d; }
        .srma-human-complete h3 { margin:0 0 8px; font-size:24px; }
        .srma-human-fallback { margin-top:18px; padding:16px 18px; border:1px solid #e2e8f0; border-radius:12px; background:#f8fafc; }
        @media (max-width: 980px) {
          .srma-dashboard-selector { grid-template-columns:1fr 1fr; }
          .srma-dashboard-selector .btn { grid-column:1 / -1; justify-self:start; }
          .srma-dashboard-cards { grid-template-columns:1fr; }
          .srma-workflow-map { grid-template-columns:repeat(2,minmax(0,1fr)); }
        }
        @media (max-width: 768px) {
          .srma-shell { padding:16px 12px 32px; }
          .srma-header { display:block; }
          .srma-identity { text-align:left; margin-top:14px; }
          .srma-empty-state { margin:24px auto; padding:32px 22px; }
          .tab-content { padding:16px; }
          .srma-wizard-progress { grid-template-columns:1fr; }
          .srma-wizard-step { text-align:left; }
          .srma-wizard-card { padding:22px 18px; }
          .srma-form-grid, .srma-summary-grid { grid-template-columns:1fr; }
          .srma-form-full, .srma-summary-item.full { grid-column:1; }
          .srma-wizard-footer { align-items:stretch; flex-direction:column; }
          .srma-wizard-footer-right { width:100%; margin-left:0; }
          .srma-wizard-footer .btn { flex:1; }
          .srma-primary-nav { gap:6px; }
          .srma-nav-btn { flex:1 1 calc(50% - 6px); text-align:center; }
          .srma-secondary-nav { gap:6px; }
          .srma-subnav-btn { flex:1 1 auto; }
          .srma-internal-tabs > .tabbable > .tab-content { padding:16px; min-height:520px; }
          .srma-brand h1 { font-size:30px; }
          .srma-brand p { font-size:16px; }
          .srma-empty-state { min-height:520px; padding:38px 24px; }
          .srma-empty-title { font-size:32px; }
          .srma-empty-description { font-size:18px; }
          .srma-nav-btn { font-size:18px; padding:10px 9px; }
          .srma-subnav-btn { font-size:15px; }
          .srma-wizard-heading h2 { font-size:31px; }
          .srma-wizard-heading p { font-size:17px; }
          .srma-wizard-step { font-size:17px; }
          .srma-wizard-title { font-size:29px; }
          .srma-wizard-subtitle { font-size:17px; }
          .srma-wizard-card label { font-size:18px; }
          .srma-wizard-card .form-control, .srma-wizard-card .form-select { font-size:17px; }
          .srma-wizard-footer .btn { font-size:17px; }
          .srma-dashboard-heading { display:block; }
          .srma-dashboard-heading h2 { font-size:29px; }
          .srma-dashboard-selector { grid-template-columns:1fr; padding:18px; }
          .srma-dashboard-selector .btn { grid-column:auto; width:100%; }
          .srma-dashboard-card { min-height:160px; }
          .srma-workflow-map { grid-template-columns:1fr; }

        }
      "))
    ),
    shiny::div(
      class = "srma-shell",
      shiny::div(
        class = "srma-header",
        shiny::div(
          class = "srma-brand",
          shiny::h1("JS SR-MA Toolkit"),
          shiny::p("Systematic review and meta-analysis workspace")
        ),
        shiny::div(class = "srma-identity", shiny::uiOutput("identity_ui"))
      ),
      shiny::div(class = "srma-notice", shiny::textOutput("notice")),
      shiny::uiOutput("workspace_ui")
    )
  )
  server <- function(input, output, session) {
    con <- web_db(settings)
    session$onSessionEnded(function() DBI::dbDisconnect(con))
    token <- shiny::reactiveVal(NULL); notice <- shiny::reactiveVal("")
    refresh <- shiny::reactiveVal(0L); editing <- shiny::reactiveVal(NULL)
    draft_review <- shiny::reactiveVal(NULL)
    nav_section <- shiny::reactiveVal("dashboard")
    project_available <- shiny::reactiveVal(FALSE)
    pending_project_selection <- shiny::reactiveVal(NULL)
    wizard_step <- shiny::reactiveVal(1L)
    wizard_errors <- shiny::reactiveVal(character())
    wizard_values <- shiny::reactiveValues(
      workspace = NULL,
      title = "",
      canonical_id = paste0("review-", substr(web_id(), 1, 12)),
      investigator = "",
      framework = "PICO",
      question = "",
      population = "",
      intervention = "",
      comparator = "",
      primary = "",
      secondary = "",
      designs = "",
      inclusion = "",
      exclusion = "",
      reason = "",
      language = "",
      databases = character(),
      inception = TRUE,
      start_date = "",
      end_date = ""
    )
    csrf <- web_secret()
    upload_root <- NULL

    # Local development authentication is intentionally zero-touch. When both
    # development mode and the explicit test identity provider are enabled, the
    # server issues a local-only opaque session internally. No browser-visible
    # session key is required. Beta and production authentication are unchanged.
    issue_local_dev_session <- function() {
      web_assert(isTRUE(auto_dev_session), "Local development sign-in is disabled.", "UNAUTHENTICATED")
      web_local_identity(con)
      linked <- web_query(
        con,
        "SELECT user_id FROM external_identities WHERE provider='local-test' AND subject='fixture'"
      )
      if (!nrow(linked)) {
        DBI::dbExecute(
          con,
          "INSERT INTO external_identities VALUES('local-test','fixture','local-researcher')"
        )
      }
      provider <- web_test_provider("fixture")
      web_sign_in(con, provider$provider, provider$credential, ttl = 43200)
    }

    safe <- function(expr) tryCatch(expr, error = function(e) {

      if (inherits(e, "shiny.silent.error"))
        return(NULL)

      message(
        "[SRMA WEB ERROR] ",
        paste(class(e), collapse = "/"),
        " | ",
        conditionMessage(e)
      )

      if (inherits(e, "srma_web_error") &&
          !is.null(e$diagnostic) &&
          nzchar(e$diagnostic)) {
        message("[SRMA WEB DIAGNOSTIC] ", e$diagnostic)
      }

      notice(
        if (inherits(e, "srma_web_error"))
          conditionMessage(e)
        else
          "The request could not finish. Ask the operator to check it."
      )

      if (inherits(e, "srma_web_error") &&
          identical(e$code, "UNAUTHENTICATED"))
        token(NULL)

      NULL
    })
    request <- function(action, args = list()) {
      current_token <- token()
      tryCatch(
        web_request(con, current_token, action, args,
          input$csrf, csrf, settings = settings, upload_root = upload_root),
        error = function(e) {
          # If a local development session expires while the app is still open,
          # renew it transparently and retry the original request once.
          if (isTRUE(auto_dev_session) &&
              inherits(e, "srma_web_error") &&
              identical(e$code, "UNAUTHENTICATED")) {
            renewed <- issue_local_dev_session()
            token(renewed)
            return(web_request(con, renewed, action, args,
              input$csrf, csrf, settings = settings, upload_root = upload_root))
          }
          stop(e)
        }
      )
    }
    output$notice <- shiny::renderText(notice())
    shiny::observe({
      if (isTRUE(auto_dev_session) && is.null(token())) {
        issued <- safe(issue_local_dev_session())
        if (!is.null(issued)) {
          if (inherits(session, "ShinySession"))
            upload_root <- safe(web_bind_upload_root(session, settings))
          token(issued)
          notice("")
        }
      } else if (is.null(token()) && settings$mode %in% c("beta", "production") && isTRUE(settings$trusted_auth_proxy)) {
        issued <- safe(web_proxy_sign_in(con, settings, session$request))
        if (!is.null(issued)) {
          if (inherits(session, "ShinySession")) upload_root <- safe(web_bind_upload_root(session, settings))
          token(issued); notice("")
        }
      }
    })
    output$identity_ui <- shiny::renderUI({
      if (isTRUE(auto_dev_session)) {
        if (is.null(token()))
          shiny::p("Starting local development session…")
        else
          shiny::span("Local development session", style = "color:#64748b;font-weight:700;")
      } else if (is.null(token()) && settings$mode %in% c("beta", "production")) {
        shiny::p("Sign in through your organization's access portal. If access was granted recently, refresh this page.")
      } else if (is.null(token()) && !identical(Sys.getenv("SRMA_WEB_TEST_IDP"), "1")) {
        shiny::p("Workspace sign-in is not configured yet. Contact your administrator.")
      } else if (is.null(token())) {
        shiny::tagList(
          shiny::p("Automated testing workspace. Enter the session credential supplied by the test operator."),
          shiny::passwordInput("credential", "Test session credential"),
          shiny::actionButton("login", "Sign in")
        )
      } else {
        shiny::actionButton("logout", "Sign out")
      }
    })

    # Manual test-credential sign-in remains available for non-development test
    # environments. It is not rendered during the zero-touch local development flow.
    shiny::observeEvent(input$login, {
      credential <- if (is.null(input$credential)) "" else tolower(trimws(input$credential))
      identity <- safe(web_session(con, credential))
      if (!is.null(identity)) {
        if (inherits(session, "ShinySession")) upload_root <- safe(web_bind_upload_root(session, settings))
        token(credential)
        notice("")
        shiny::updateTextInput(session, "credential", value = "")
      }
    })
    shiny::observeEvent(input$logout, {
      if (!isTRUE(auto_dev_session) && !is.null(safe(request("logout")))) {
        token(NULL)
        notice("Signed out. This session can no longer be used.")
      }
    })
    output$workspace_ui <- shiny::renderUI({
      shiny::req(token())
      shiny::tagList(
        shiny::div(style = "display:none", shiny::textInput("csrf", NULL, value = csrf)),
        shiny::uiOutput("primary_nav"),
        shiny::uiOutput("secondary_nav"),
        shiny::div(
          class = "srma-internal-tabs",
          shiny::tabsetPanel(id = "page",
            shiny::tabPanel(
              title = "My Reviews",
              value = "my_projects",
              shiny::uiOutput("my_projects_content")
            ),
            shiny::tabPanel(
              title = "New Systematic Review",
              value = "new_review",
              shiny::uiOutput("wizard")
            ),
            shiny::tabPanel("Project Overview", shiny::textOutput("overview")),
            shiny::tabPanel("Workflow Progress", shiny::tableOutput("workflow")),
            shiny::tabPanel("Current Action", shiny::textOutput("next_action"),
              shiny::actionButton("run", "Check existing search strategies"), shiny::tableOutput("jobs")),
            shiny::tabPanel("Search / Study Import", shiny::uiOutput("search_import")),
            web_b2_ui(), web_c1_ui(), web_c2_ui(), web_c3_ui(), web_c4_ui(), web_c5_ui(), web_c6_ui(),
            shiny::tabPanel("Results / Artifacts", shiny::selectInput("artifact", "Available output", character()),
              shiny::downloadButton("download", "Download selected output")),
            shiny::tabPanel(
              "Human Review Required",
              shiny::uiOutput("human_decisions_ui"),
              shiny::div(
                class = "srma-human-wrap srma-human-details-static",
                shiny::tags$details(
                  class = "srma-human-details",
                  shiny::tags$summary("Review details and canonical concepts"),
                  shiny::div(
                    class = "srma-human-details-body",
                    shiny::selectInput("draft_job", "Review details source", c("Saved review configuration" = "")),
                    shiny::actionButton("load_draft", "Load review details"),
                    shiny::uiOutput("canonical_review")
                  )
                )
              )
            )
          )
        )
      )
    })



    nav_sections <- list(
      dashboard = c("my_projects"),
      review_setup = c("new_review", "Human Review Required"),
      studies = c("Search / Study Import", "Screening / Evidence"),
      analysis = c("Meta-analysis"),
      manuscript = c("Manuscript"),
      review_export = c("Final QC", "Download Center", "Results / Artifacts"),
      settings = c("Security & Audit", "Service Status")
    )

    nav_defaults <- c(
      dashboard = "my_projects",
      review_setup = "new_review",
      studies = "Search / Study Import",
      analysis = "Meta-analysis",
      manuscript = "Manuscript",
      review_export = "Final QC",
      settings = "Security & Audit"
    )

    nav_labels <- list(
      dashboard = character(),
      review_setup = c("new_review" = "New / Edit Review", "Human Review Required" = "Human Decisions"),
      studies = c("Search / Study Import" = "Search & Import", "Screening / Evidence" = "Screening & Evidence"),
      analysis = c("Meta-analysis" = "Meta-analysis"),
      manuscript = c("Manuscript" = "Manuscript"),
      review_export = c("Final QC" = "Final QC", "Download Center" = "Downloads", "Results / Artifacts" = "Results & Artifacts"),
      settings = c("Security & Audit" = "Security & Audit", "Service Status" = "Service Status")
    )

    nav_section_for_page <- function(page) {
      if (is.null(page) || !length(page)) return("dashboard")
      hit <- names(Filter(function(x) page %in% x, nav_sections))
      if (length(hit)) hit[[1L]] else "dashboard"
    }

    nav_requires_project <- function(page) {
      !page %in% c("my_projects", "new_review", "Security & Audit")
    }

    nav_button <- function(label, section, active = FALSE, disabled = FALSE) {
      attrs <- list(
        type = "button",
        class = paste("srma-nav-btn", if (isTRUE(active)) "active" else ""),
        label
      )
      if (isTRUE(disabled)) {
        attrs$disabled <- "disabled"
        attrs$title <- "Create or select a systematic review first."
      } else {
        js <- as.character(jsonlite::toJSON(section, auto_unbox = TRUE))
        attrs$onclick <- sprintf("Shiny.setInputValue('nav_section_target', %s, {priority: 'event'});", js)
      }
      do.call(shiny::tags$button, attrs)
    }

    subnav_button <- function(label, target, active = FALSE, disabled = FALSE) {
      attrs <- list(
        type = "button",
        class = paste("srma-subnav-btn", if (isTRUE(active)) "active" else ""),
        label
      )
      if (isTRUE(disabled)) {
        attrs$disabled <- "disabled"
        attrs$title <- "Create or select a systematic review first."
      } else {
        js <- as.character(jsonlite::toJSON(target, auto_unbox = TRUE))
        attrs$onclick <- sprintf("Shiny.setInputValue('nav_target', %s, {priority: 'event'});", js)
      }
      do.call(shiny::tags$button, attrs)
    }

    output$primary_nav <- shiny::renderUI({
      shiny::req(token())
      current <- nav_section()
      has_project <- isTRUE(project_available())
      sections <- c(
        dashboard = "Dashboard",
        review_setup = "Review Setup",
        studies = "Studies",
        analysis = "Analysis",
        manuscript = "Manuscript",
        review_export = "Review & Export",
        settings = "Settings"
      )
      disabled_sections <- if (has_project) character() else c("studies", "analysis", "manuscript", "review_export")
      shiny::div(
        class = "srma-primary-nav",
        lapply(names(sections), function(section)
          nav_button(
            unname(sections[[section]]),
            section,
            active = identical(current, section),
            disabled = section %in% disabled_sections
          )
        )
      )
    })

    output$secondary_nav <- shiny::renderUI({
      shiny::req(token())
      section <- nav_section()
      items <- nav_labels[[section]]
      if (is.null(items) || !length(items)) return(NULL)
      current_page <- if (srma_has_text(input$page)) as.character(input$page[[1L]]) else unname(nav_defaults[[section]])
      has_project <- isTRUE(project_available())
      buttons <- lapply(names(items), function(target)
        subnav_button(
          unname(items[[target]]),
          target,
          active = identical(current_page, target),
          disabled = !has_project && nav_requires_project(target)
        )
      )
      shiny::tagList(
        shiny::div(class = "srma-secondary-nav", buttons),
        if (!has_project && identical(section, "dashboard"))
          shiny::p("Create your first systematic review to unlock the full workflow.", class = "srma-nav-hint")
      )
    })

    shiny::observeEvent(input$nav_section_target, {
      section <- input$nav_section_target
      if (!section %in% names(nav_sections)) return()
      if (!isTRUE(project_available()) && section %in% c("studies", "analysis", "manuscript", "review_export")) return()
      nav_section(section)
      target <- unname(nav_defaults[[section]])
      shiny::updateTabsetPanel(session, "page", selected = target)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$nav_target, {
      target <- input$nav_target
      valid <- unlist(nav_sections, use.names = FALSE)
      if (!target %in% valid) return()
      if (!isTRUE(project_available()) && nav_requires_project(target)) return()
      nav_section(nav_section_for_page(target))
      shiny::updateTabsetPanel(session, "page", selected = target)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$page, {
      nav_section(nav_section_for_page(input$page))
    }, ignoreInit = FALSE)

    output$my_projects_content <- shiny::renderUI({
      shiny::req(token())
      refresh()

      projects <- safe(request("projects"))

      if (is.null(projects)) {
        return(NULL)
      }

      if (!nrow(projects)) {
        return(empty_reviews_ui())
      }

      current <- shiny::isolate(input$project)
      selected <- if (srma_has_text(current) && current %in% projects$id) current else projects$id[[1L]]

      shiny::div(
        class = "srma-dashboard",
        shiny::div(
          class = "srma-dashboard-heading",
          shiny::div(
            shiny::h2("Review dashboard"),
            shiny::p("See where this review stands, what needs attention, and the next action to take.")
          )
        ),
        shiny::div(
          class = "srma-dashboard-selector",
          shiny::selectInput(
            inputId = "project",
            label = "Current review",
            choices = stats::setNames(projects$id, projects$title),
            selected = selected,
            width = "100%"
          ),
          shiny::selectInput(
            inputId = "version",
            label = "Saved version",
            choices = character(),
            width = "100%"
          ),
          shiny::actionButton("edit", "Edit as new version", class = "btn-outline-secondary")
        ),
        shiny::uiOutput("dashboard_cards"),
        shiny::uiOutput("dashboard_workflow"),
        shiny::uiOutput("dashboard_activity")
      )
    })

    shiny::observeEvent(input$start_first_review, {
      nav_section("review_setup")
      shiny::updateTabsetPanel(session, "page", selected = "new_review")
    })

    shiny::observe({
      shiny::req(token())
      refresh()

      projects <- safe(request("projects"))
      shiny::req(!is.null(projects))

      if (!nrow(projects)) {
        project_available(FALSE)
        shiny::updateSelectInput(
          session,
          "project",
          choices = character(),
          selected = character()
        )

        shiny::updateSelectInput(
          session,
          "version",
          choices = character(),
          selected = character()
        )

        return()
      }

      project_available(TRUE)
      current <- shiny::isolate(input$project)
      pending <- pending_project_selection()

      selected <- if (
        srma_has_text(pending) && pending %in% projects$id
      ) {
        pending
      } else if (
        srma_has_text(current) &&
        current %in% projects$id
      ) {
        current
      } else {
        projects$id[[1L]]
      }

      if (srma_has_text(pending) && identical(selected, pending)) pending_project_selection(NULL)

      shiny::updateSelectInput(
        session,
        "project",
        choices = setNames(projects$id, projects$title),
        selected = selected
      )
    })
    state <- shiny::reactive({
      shiny::invalidateLater(1500, session)
      shiny::req(token(), srma_has_text(input$project))
      safe(request("view", list(project_id = input$project)))
    })
    shiny::observeEvent(list(input$project, refresh()), {
      shiny::req(token(), srma_has_text(input$project))
      x <- safe(request("view", list(project_id = input$project))); shiny::req(x)
      choices <- if (nrow(x$versions)) setNames(x$versions$id, paste("Version", x$versions$version)) else character()
      current_version <- shiny::isolate(input$version)
      selected_version <- if (srma_has_text(current_version) && current_version %in% x$versions$id) current_version else if (nrow(x$versions)) x$versions$id[[1L]] else character()
      shiny::updateSelectInput(session, "version", choices = choices, selected = selected_version)
    })

    dashboard_context <- shiny::reactive({
      x <- state(); shiny::req(x)
      version_id <- if (srma_has_text(input$version)) as.character(input$version[[1L]]) else if (nrow(x$versions)) as.character(x$versions$id[[1L]]) else ""
      gates <- if (nrow(x$gates) && nzchar(version_id)) x$gates[x$gates$version_id == version_id, , drop = FALSE] else x$gates[0, , drop = FALSE]
      pending <- if (nrow(gates)) gates[gates$status == "PENDING", , drop = FALSE] else gates
      pending_decisions <- srma_pending_decision_count(pending)
      jobs <- if (nrow(x$jobs) && nzchar(version_id)) x$jobs[x$jobs$version_id == version_id, , drop = FALSE] else x$jobs[0, , drop = FALSE]
      active_jobs <- if (nrow(jobs)) jobs[jobs$state %in% c("QUEUED", "RUNNING"), , drop = FALSE] else jobs
      version_row <- if (nrow(x$versions) && nzchar(version_id)) x$versions[x$versions$id == version_id, , drop = FALSE] else x$versions[0, , drop = FALSE]
      version_label <- if (nrow(version_row) && "version" %in% names(version_row)) paste("Version", version_row$version[[1L]]) else "Saved draft"

      next_action <- if (nrow(pending)) {
        list(
          stage = "Human review",
          title = "Complete required human decisions",
          description = paste0(pending_decisions, if (pending_decisions == 1L) " decision is" else " decisions are", " waiting before downstream workflow steps."),
          button = "Review decisions",
          target = "Human Review Required"
        )
      } else if (nrow(active_jobs)) {
        list(
          stage = "Processing",
          title = "A background task is running",
          description = "You can continue working while the queued task finishes. Review its result before moving forward.",
          button = "Open Studies",
          target = "Search / Study Import"
        )
      } else {
        list(
          stage = "Search preparation",
          title = "Prepare the search workflow",
          description = "Review the saved protocol, then prepare and validate database search strategies for this version.",
          button = "Continue to search setup",
          target = "Search / Study Import"
        )
      }

      list(
        x = x,
        version_id = version_id,
        version_label = version_label,
        pending = pending,
        pending_decisions = pending_decisions,
        jobs = jobs,
        active_jobs = active_jobs,
        next_action = next_action
      )
    })

    output$dashboard_cards <- shiny::renderUI({
      ctx <- dashboard_context()
      attention <- ctx$pending_decisions + nrow(ctx$active_jobs)
      attention_text <- if (attention == 0L) "Nothing urgent" else paste(attention, if (attention == 1L) "item" else "items")
      shiny::div(
        class = "srma-dashboard-cards",
        shiny::div(
          class = "srma-dashboard-card",
          shiny::div(class = "srma-card-eyebrow", "Current stage"),
          shiny::div(class = "srma-card-value", ctx$next_action$stage),
          shiny::p(paste(ctx$version_label, "is the active immutable review version."), class = "srma-card-text")
        ),
        shiny::div(
          class = "srma-dashboard-card accent",
          shiny::div(class = "srma-card-eyebrow", "Next action"),
          shiny::div(class = "srma-card-value", ctx$next_action$title),
          shiny::p(ctx$next_action$description, class = "srma-card-text"),
          shiny::actionButton("dashboard_continue", ctx$next_action$button, class = "btn-primary srma-card-action")
        ),
        shiny::div(
          class = "srma-dashboard-card",
          shiny::div(class = "srma-card-eyebrow", "Needs attention"),
          shiny::div(class = paste("srma-attention-number", if (attention == 0L) "clear" else ""), if (attention == 0L) "✓" else attention),
          shiny::div(class = "srma-card-value", attention_text),
          shiny::p(
            if (ctx$pending_decisions > 0L) paste(ctx$pending_decisions, "human review decision(s) pending.") else if (nrow(ctx$active_jobs)) paste(nrow(ctx$active_jobs), "background task(s) active.") else "No pending human decisions or active background jobs.",
            class = "srma-card-text"
          )
        )
      )
    })

    output$dashboard_workflow <- shiny::renderUI({
      ctx <- dashboard_context()
      human_pending <- nrow(ctx$pending) > 0L
      processing <- nrow(ctx$active_jobs) > 0L
      labels <- c("Review setup", "Human review", "Studies", "Analysis", "Manuscript", "Final QC", "Export")
      classes <- if (human_pending) {
        c("complete", "current", rep("upcoming", 5L))
      } else if (processing) {
        c("complete", "complete", "current", rep("upcoming", 4L))
      } else {
        c("complete", "complete", "current", rep("upcoming", 4L))
      }
      shiny::div(
        class = "srma-dashboard-panel",
        shiny::h3("Workflow map"),
        shiny::p("A simple view of the recommended review sequence. Scientific gates still control when downstream work can proceed."),
        shiny::div(
          class = "srma-workflow-map",
          lapply(seq_along(labels), function(i)
            shiny::div(
              class = paste("srma-workflow-node", classes[[i]]),
              shiny::span(class = "dot"),
              shiny::span(labels[[i]], class = "label")
            )
          )
        )
      )
    })

    output$dashboard_activity <- shiny::renderUI({
      ctx <- dashboard_context()
      jobs <- ctx$jobs
      items <- list(
        shiny::tags$li(
          shiny::span("SAVED", class = "srma-activity-state"),
          paste(ctx$version_label, "is available and bound to this review.")
        )
      )
      if (nrow(jobs)) {
        take <- seq_len(min(4L, nrow(jobs)))
        job_items <- lapply(take, function(i) {
          state_value <- if ("state" %in% names(jobs)) as.character(jobs$state[[i]]) else "JOB"
          operation <- if ("operation" %in% names(jobs)) gsub("_", " ", as.character(jobs$operation[[i]])) else "Background task"
          raw_message <- if ("message" %in% names(jobs)) as.character(jobs$message[[i]]) else ""
          message_value <- if (length(raw_message) && !is.na(raw_message) && nzchar(trimws(raw_message))) raw_message else operation
          shiny::tags$li(
            shiny::span(state_value, class = "srma-activity-state"),
            message_value
          )
        })
        items <- c(items, job_items)
      } else if (nrow(ctx$pending)) {
        items <- c(items, list(
          shiny::tags$li(
            shiny::span("ACTION", class = "srma-activity-state"),
            "Human review is required before the next version-bound scientific workflow step."
          )
        ))
      }
      shiny::div(
        class = "srma-dashboard-panel",
        shiny::h3("Recent activity"),
        shiny::p("Review the latest saved state and background activity for the selected version."),
        do.call(shiny::tags$ul, c(list(class = "srma-activity-list"), items))
      )
    })

    shiny::observeEvent(input$dashboard_continue, {
      ctx <- dashboard_context()
      target <- ctx$next_action$target
      nav_section(nav_section_for_page(target))
      shiny::updateTabsetPanel(session, "page", selected = target)
    })

    web_b2_server(input,output,session,request,safe,notice,refresh)
    web_c1_server(input,output,session,request,safe,notice,refresh)
    web_c2_server(input,output,session,request,safe,notice,refresh)
    web_c3_server(input,output,session,request,safe,notice,refresh)
    web_c4_server(input,output,session,request,safe,notice,refresh)
    web_c5_server(input,output,session,request,safe,notice,refresh,token)
    web_c6_server(input,output,session,request,safe,notice,refresh)
    output$overview <- shiny::renderText({x <- state(); shiny::req(x); x$project$title})
    output$workflow <- shiny::renderTable({
      x <- state(); shiny::req(x)
      data.frame(Step = c("Project Setup", "Protocol", "Search", "Study Import", "Screening", "Full-text Review", "Data Extraction", "Risk of Bias", "Meta-analysis", "Manuscript", "Final QC", "Download"),
        Status = c("Draft saved", "Human review required", "Search validation available", "See Search / Study Import", c(rep("See Screening / Evidence",4),"See Meta-analysis","See Manuscript","See Final QC"), "See Download Center"))
    })
    output$next_action <- shiny::renderText({
      x <- state(); shiny::req(x)
      if (nrow(x$jobs) && x$jobs$state[1] %in% c("QUEUED", "RUNNING"))
        "Your validation check is waiting or running. You may leave this page. When it finishes, review the report under Results / Artifacts and follow any database input instructions."
      else "Review the project metadata and protocol decisions. Existing canonical search strategies can be checked here; a draft with no strategies needs search preparation first. Next, review the report and database-specific import requirements."
    })
    output$jobs <- shiny::renderTable({x <- state(); shiny::req(x); x$jobs[x$jobs$version_id==input$version, c("state", "message", "progress"), drop = FALSE]})
    human_gate_applies <- function(gates) {
      if (is.null(gates) || !nrow(gates) || !"required_operation" %in% names(gates)) return("")
      labels <- c(
        generate_search = "search generation",
        retrieve_pubmed = "PubMed retrieval"
      )
      operations <- unique(as.character(gates$required_operation))
      text <- unname(labels[operations])
      text[is.na(text)] <- gsub("_", " ", operations[is.na(text)])
      paste(text[nzchar(text)], collapse = " · ")
    }

    output$human_decisions_ui <- shiny::renderUI({
      # Human decision controls must remain stable while the user is typing.
      # The general project state polls in the background every 1.5 seconds,
      # so do not make this editable UI depend directly on that polling timer.
      # Rebuild it only when the selected project/version changes or after an
      # explicit workflow refresh (for example, after a saved decision).
      refresh()
      shiny::req(input$project, input$version)
      # Fetch a fresh backend snapshot when this editable UI is explicitly
      # rebuilt. Using isolate(state()) here can return the cached pre-save
      # snapshot, because state() polls independently every 1.5 seconds.
      # A direct view request keeps typing stable between refreshes while
      # making a saved decision visible immediately.
      x <- safe(request("view", list(project_id = input$project))); shiny::req(x)
      pending <- x$gates[x$gates$version_id == input$version & x$gates$status == "PENDING", , drop = FALSE]
      config_gates <- pending[pending$gate_type == "configuration_human_review", , drop = FALSE]
      protocol_gates <- pending[pending$gate_type == "protocol_registration", , drop = FALSE]
      known <- pending$gate_type %in% c("configuration_human_review", "protocol_registration")
      other <- pending[!known, , drop = FALSE]
      count <- srma_pending_decision_count(pending)

      shiny::div(
        class = "srma-human-wrap",
        shiny::div(
          class = "srma-human-heading",
          shiny::h2("Human Review"),
          shiny::p("Scientific decisions remain under your control. Complete the items below before the review can continue.")
        ),
        if (count == 0L) {
          shiny::div(
            class = "srma-human-complete",
            shiny::h3("No human decisions are pending"),
            shiny::p("This saved version has no pending initial review decisions. Return to the Dashboard to continue the workflow.")
          )
        } else {
          shiny::tagList(
            shiny::div(
              class = "srma-human-summary",
              shiny::strong(count),
              if (count == 1L) "decision needs your attention." else "decisions need your attention."
            ),
            if (nrow(config_gates)) srma_human_decision_card(
              "configuration_review",
              "Review configuration",
              "Confirm that the review question, eligibility criteria, outcomes, and selected databases represent the systematic review you intend to conduct.",
              "literature search begins",
              "Reason / notes *"
            ),
            if (nrow(protocol_gates)) srma_protocol_registration_card(),
            if (nrow(other)) shiny::div(
              class = "srma-human-fallback",
              shiny::strong("Additional human review items"),
              shiny::p("Additional workflow-specific review items are pending and will be handled in their dedicated workflow screens."),
              shiny::tableOutput("other_human_gates")
            )
          )
        }
      )
    })

    output$other_human_gates <- shiny::renderTable({
      x <- state(); shiny::req(x, input$version)
      pending <- x$gates[x$gates$version_id == input$version & x$gates$status == "PENDING", , drop = FALSE]
      other <- pending[!pending$gate_type %in% c("configuration_human_review", "protocol_registration"), , drop = FALSE]
      if (!nrow(other)) return(NULL)
      data.frame(Review = gsub("_", " ", other$gate_type), Status = other$status)
    })

    record_gate_group <- function(gate_type, decision, rationale, success_message) {
      shiny::req(input$project, input$version)
      decision <- if (is.null(decision)) "" else as.character(decision)
      rationale <- if (is.null(rationale)) "" else trimws(as.character(rationale))
      if (!decision %in% c("APPROVED", "REJECTED") || !nzchar(rationale)) {
        notice("Choose a decision and provide the required reason or registration reference.")
        return(invisible(FALSE))
      }
      x <- state(); shiny::req(x)
      gates <- x$gates[
        x$gates$version_id == input$version &
          x$gates$status == "PENDING" &
          x$gates$gate_type == gate_type,
        , drop = FALSE
      ]
      if (!nrow(gates)) {
        notice("This human review decision has already been recorded for the selected version.")
        return(invisible(FALSE))
      }
      for (gate_id in gates$id) {
        done <- safe({
          request("decide_gate", list(
            project_id = input$project,
            gate_id = gate_id,
            decision = decision,
            rationale = rationale
          ))
          TRUE
        })
        if (!isTRUE(done)) return(invisible(FALSE))
      }
      refresh(refresh() + 1L)
      notice(success_message)
      invisible(TRUE)
    }

    shiny::observeEvent(input$save_configuration_review, {
      if (isTRUE(record_gate_group(
        "configuration_human_review",
        input$configuration_review_decision,
        input$configuration_review_rationale,
        "Configuration review recorded for this saved version."
      ))) {
        shiny::updateRadioButtons(session, "configuration_review_decision", selected = character())
        shiny::updateTextAreaInput(session, "configuration_review_rationale", value = "")
      }
    })

    shiny::observeEvent(input$confirm_protocol_registration, {
      status <- if (is.null(input$protocol_registration_status)) "" else as.character(input$protocol_registration_status)
      if (!identical(status, "REGISTERED")) {
        notice("The protocol registration gate remains pending until a completed registration is confirmed.")
        return()
      }

      registry <- if (is.null(input$protocol_registry)) "" else trimws(as.character(input$protocol_registry))
      reference <- if (is.null(input$protocol_registration_reference)) "" else trimws(as.character(input$protocol_registration_reference))
      notes <- if (is.null(input$protocol_registration_notes)) "" else trimws(as.character(input$protocol_registration_notes))

      if (!nzchar(registry) || !nzchar(reference)) {
        notice("Choose the registry and enter the registration ID or URL before confirming registration.")
        return()
      }

      registry_label <- if (identical(registry, "PROSPERO")) "PROSPERO" else "Other accepted registry"
      rationale <- paste0(
        "Registration status: Registered\n",
        "Registry: ", registry_label, "\n",
        "Registration ID or URL: ", reference,
        if (nzchar(notes)) paste0("\nNotes: ", notes) else ""
      )

      if (isTRUE(record_gate_group(
        "protocol_registration",
        "APPROVED",
        rationale,
        "Protocol registration confirmed for this saved version."
      ))) {
        shiny::updateRadioButtons(session, "protocol_registration_status", selected = character())
        shiny::updateSelectInput(session, "protocol_registry", selected = "PROSPERO")
        shiny::updateTextInput(session, "protocol_registration_reference", value = "")
        shiny::updateTextAreaInput(session, "protocol_registration_notes", value = "")
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
    wizard_text <- function(x, fallback = "Not specified") {
      if (is.null(x) || !length(x)) return(fallback)
      value <- paste(as.character(x), collapse = "\n")
      if (!nzchar(trimws(value))) fallback else value
    }

    wizard_multiline <- function(x, fallback = "Not specified") {
      value <- wizard_text(x, "")
      if (!nzchar(value)) return(fallback)
      parts <- trimws(strsplit(value, "\n", fixed = TRUE)[[1L]])
      parts <- parts[nzchar(parts)]
      if (!length(parts)) fallback else paste(parts, collapse = "\n")
    }

    wizard_capture <- function(step = wizard_step()) {
      capture <- function(name, current) {
        value <- input[[name]]
        if (is.null(value)) current else value
      }

      if (identical(step, 1L)) {
        wizard_values$workspace <- capture("workspace", wizard_values$workspace)
        wizard_values$title <- capture("title", wizard_values$title)
        wizard_values$canonical_id <- capture("canonical_id", wizard_values$canonical_id)
        wizard_values$investigator <- capture("investigator", wizard_values$investigator)
        wizard_values$framework <- capture("framework", wizard_values$framework)
        wizard_values$question <- capture("question", wizard_values$question)
      } else if (identical(step, 2L)) {
        wizard_values$population <- capture("population", wizard_values$population)
        wizard_values$intervention <- capture("intervention", wizard_values$intervention)
        wizard_values$comparator <- capture("comparator", wizard_values$comparator)
        wizard_values$primary <- capture("primary", wizard_values$primary)
        wizard_values$secondary <- capture("secondary", wizard_values$secondary)
      } else if (identical(step, 3L)) {
        wizard_values$designs <- capture("designs", wizard_values$designs)
        wizard_values$inclusion <- capture("inclusion", wizard_values$inclusion)
        wizard_values$exclusion <- capture("exclusion", wizard_values$exclusion)
        wizard_values$reason <- capture("reason", wizard_values$reason)
        wizard_values$language <- capture("language", wizard_values$language)
      } else if (identical(step, 4L)) {
        wizard_values$databases <- capture("databases", wizard_values$databases)
        wizard_values$inception <- isTRUE(capture("inception", wizard_values$inception))
        wizard_values$start_date <- capture("start_date", wizard_values$start_date)
        wizard_values$end_date <- capture("end_date", wizard_values$end_date)
      }
      invisible(TRUE)
    }

    reset_wizard <- function(keep_workspace = TRUE) {
      workspace <- if (isTRUE(keep_workspace)) wizard_values$workspace else NULL
      wizard_values$workspace <- workspace
      wizard_values$title <- ""
      wizard_values$canonical_id <- paste0("review-", substr(web_id(), 1, 12))
      wizard_values$investigator <- ""
      wizard_values$framework <- "PICO"
      wizard_values$question <- ""
      wizard_values$population <- ""
      wizard_values$intervention <- ""
      wizard_values$comparator <- ""
      wizard_values$primary <- ""
      wizard_values$secondary <- ""
      wizard_values$designs <- ""
      wizard_values$inclusion <- ""
      wizard_values$exclusion <- ""
      wizard_values$reason <- ""
      wizard_values$language <- ""
      wizard_values$databases <- character()
      wizard_values$inception <- TRUE
      wizard_values$start_date <- ""
      wizard_values$end_date <- ""
      wizard_step(1L)
      wizard_errors(character())
      invisible(TRUE)
    }

    wizard_progress_ui <- function(step) {
      labels <- c("Review basics", "Research question", "Eligibility", "Search sources", "Review & create")
      shiny::div(
        class = "srma-wizard-progress",
        lapply(seq_along(labels), function(i) {
          status <- if (i < step) "done" else if (i == step) "active" else ""
          shiny::div(
            class = paste("srma-wizard-step", status),
            paste0(i, ". ", labels[[i]])
          )
        })
      )
    }

    wizard_footer_ui <- function(step) {
      if (identical(step, 1L)) {
        return(shiny::div(
          class = "srma-wizard-footer",
          shiny::actionButton("wizard_cancel", "Cancel", class = "btn-outline-secondary"),
          shiny::div(
            class = "srma-wizard-footer-right",
            shiny::actionButton("wizard_next", "Continue", class = "btn-primary")
          )
        ))
      }

      if (step < 5L) {
        return(shiny::div(
          class = "srma-wizard-footer",
          shiny::actionButton("wizard_back", "Back", class = "btn-outline-secondary"),
          shiny::div(
            class = "srma-wizard-footer-right",
            shiny::actionButton("wizard_next", "Continue", class = "btn-primary")
          )
        ))
      }

      save_label <- if (is.null(editing())) "Create systematic review" else "Save as new version"
      shiny::div(
        class = "srma-wizard-footer",
        shiny::actionButton("wizard_back", "Back", class = "btn-outline-secondary"),
        shiny::div(
          class = "srma-wizard-footer-right",
          if (!is.null(editing())) shiny::actionButton("new_draft", "Start as a separate review", class = "btn-outline-secondary"),
          shiny::actionButton("save", save_label, class = "btn-primary")
        )
      )
    }

    summary_item <- function(label, value, full = FALSE) {
      shiny::div(
        class = paste("srma-summary-item", if (isTRUE(full)) "full" else ""),
        shiny::span(class = "srma-summary-label", label),
        shiny::div(class = "srma-summary-value", value)
      )
    }

    summary_section <- function(title, ...) {
      shiny::div(
        class = "srma-summary-section",
        shiny::h4(title),
        shiny::div(class = "srma-summary-grid", ...)
      )
    }

    field_error <- function(key, message) {
      if (!key %in% wizard_errors()) return(NULL)
      shiny::div(class = "srma-field-error", message)
    }

    output$wizard <- shiny::renderUI({
      shiny::req(token())
      workspaces <- safe(request("workspaces"))
      shiny::req(!is.null(workspaces), nrow(workspaces) > 0L)

      if (is.null(wizard_values$workspace) || !wizard_values$workspace %in% workspaces$id) {
        wizard_values$workspace <- workspaces$id[[1L]]
      }

      step <- wizard_step()
      workspace_ui <- if (nrow(workspaces) > 1L) {
        shiny::selectInput(
          "workspace", "Workspace",
          choices = setNames(workspaces$id, workspaces$name),
          selected = wizard_values$workspace,
          width = "100%"
        )
      } else {
        shiny::div(
          style = "display:none",
          shiny::selectInput(
            "workspace", NULL,
            choices = setNames(workspaces$id, workspaces$name),
            selected = wizard_values$workspace
          )
        )
      }

      step_body <- switch(
        as.character(step),
        `1` = shiny::tagList(
          shiny::div(class = "srma-wizard-kicker", "Step 1 of 5"),
          shiny::h3("Review basics", class = "srma-wizard-title"),
          shiny::p(
            "Start with the identity and core question of your review. You can refine scientific details before saving.",
            class = "srma-wizard-subtitle"
          ),
          shiny::div(
            class = "srma-form-grid",
            shiny::div(class = "srma-form-full", workspace_ui),
            shiny::div(
              class = "srma-form-full",
              shiny::textInput("title", srma_required_label("Review title"), value = wizard_values$title, width = "100%"),
              field_error("title", "Enter a review title to continue.")
            ),
            shiny::textInput(
              "investigator", "Principal investigator (optional)",
              value = wizard_values$investigator, width = "100%",
              placeholder = "e.g., J. Smith"
            ),
            shiny::selectInput(
              "framework", "Question framework",
              choices = c("PICO", "PICOS"), selected = wizard_values$framework, width = "100%"
            ),
            shiny::div(
              class = "srma-form-full",
              shiny::textAreaInput(
                "question", srma_required_label("Review question"),
                value = wizard_values$question, rows = 4, width = "100%",
                placeholder = "State the clinical or research question this review will answer."
              ),
              shiny::div(
                class = "srma-field-help",
                "Example: In adults with condition X, does intervention A improve outcome B compared with standard care?"
              ),
              field_error("question", "Enter the review question to continue.")
            ),
            shiny::div(
              class = "srma-form-full",
              shiny::tags$details(
                shiny::tags$summary("Advanced options"),
                shiny::textInput(
                  "canonical_id", "Project identifier",
                  value = wizard_values$canonical_id, width = "100%"
                ),
                shiny::helpText(
                  "This identifier is generated automatically and usually does not need to be changed. ",
                  "Use only letters, numbers, hyphens or underscores if you edit it."
                )
              )
            )
          )
        ),
        `2` = shiny::tagList(
          shiny::div(class = "srma-wizard-kicker", "Step 2 of 5"),
          shiny::h3("Research question", class = "srma-wizard-title"),
          shiny::p(
            "Describe the key elements of the question. Keep each field concise; detailed eligibility rules come next.",
            class = "srma-wizard-subtitle"
          ),
          shiny::div(
            class = "srma-form-grid",
            shiny::div(
              shiny::textAreaInput("population", srma_required_label("Population"), value = wizard_values$population, rows = 4, width = "100%"),
              field_error("population", "Describe the review population.")
            ),
            shiny::div(
              shiny::textAreaInput("intervention", srma_required_label("Intervention or exposure"), value = wizard_values$intervention, rows = 4, width = "100%"),
              field_error("intervention", "Describe the intervention or exposure.")
            ),
            shiny::div(
              shiny::textAreaInput("comparator", srma_required_label("Comparator"), value = wizard_values$comparator, rows = 4, width = "100%"),
              shiny::div(class = "srma-field-help", "If the review has no active comparator, state the intended non-comparative reference explicitly (for example, 'No comparator / single-arm evidence')."),
              field_error("comparator", "Describe the comparator or explicitly state that no comparator applies.")
            ),
            shiny::div(
              shiny::textAreaInput("primary", srma_required_label("Primary outcomes — one per line"), value = wizard_values$primary, rows = 4, width = "100%"),
              field_error("primary", "Enter at least one primary outcome.")
            ),
            shiny::div(
              class = "srma-form-full",
              shiny::textAreaInput(
                "secondary", "Secondary outcomes — optional, one per line",
                value = wizard_values$secondary, rows = 4, width = "100%"
              )
            )
          )
        ),
        `3` = shiny::tagList(
          shiny::div(class = "srma-wizard-kicker", "Step 3 of 5"),
          shiny::h3("Eligibility criteria", class = "srma-wizard-title"),
          shiny::p(
            "Define which studies belong in the review. Enter one study design or criterion per line where requested.",
            class = "srma-wizard-subtitle"
          ),
          shiny::div(
            class = "srma-form-grid",
            shiny::div(
              shiny::textAreaInput("designs", srma_required_label("Eligible study designs — one per line"), value = wizard_values$designs, rows = 4, width = "100%"),
              field_error("designs", "Enter at least one eligible study design.")
            ),
            shiny::textInput("language", "Language restriction (optional)", value = wizard_values$language, width = "100%", placeholder = "e.g., No language restriction"),
            shiny::div(
              shiny::textAreaInput("inclusion", srma_required_label("Inclusion criteria — one per line"), value = wizard_values$inclusion, rows = 5, width = "100%"),
              field_error("inclusion", "Enter the inclusion criteria.")
            ),
            shiny::div(
              shiny::textAreaInput("exclusion", srma_required_label("Exclusion criteria — one per line"), value = wizard_values$exclusion, rows = 5, width = "100%"),
              field_error("exclusion", "Enter the exclusion criteria.")
            ),
            shiny::div(
              class = "srma-form-full",
              shiny::textInput(
                "reason", srma_required_label("Default screening exclusion reason"),
                value = wizard_values$reason, width = "100%",
                placeholder = "e.g., Does not meet eligibility criteria"
              ),
              shiny::div(class = "srma-field-help", "This becomes the first screening exclusion reason. Additional reasons can be added later during screening."),
              field_error("reason", "Enter one default screening exclusion reason.")
            )
          )
        ),
        `4` = shiny::tagList(
          shiny::div(class = "srma-wizard-kicker", "Step 4 of 5"),
          shiny::h3("Search sources", class = "srma-wizard-title"),
          shiny::p(
            "Choose the databases and search date range. Search strategies are generated and reviewed later in the workflow.",
            class = "srma-wizard-subtitle"
          ),
          shiny::div(
            class = "srma-form-grid",
            shiny::div(
              class = "srma-form-full",
              shiny::checkboxGroupInput(
                "databases", srma_required_label("Databases"),
                choices = c("PubMed" = "pubmed", "Embase" = "embase", "CENTRAL" = "central"),
                selected = wizard_values$databases,
                inline = TRUE
              ),
              shiny::div(class = "srma-field-help", "Select at least one database."),
              field_error("databases", "Select at least one database to continue.")
            ),
            shiny::div(
              class = "srma-form-full",
              shiny::checkboxInput("inception", "Search from database inception", value = isTRUE(wizard_values$inception))
            ),
            shiny::conditionalPanel(
              condition = "!input.inception",
              shiny::div(
                shiny::textInput("start_date", srma_required_label("Search start date (YYYY-MM-DD)"), value = wizard_values$start_date, width = "100%"),
                field_error("start_date", "Enter a start date or select database inception.")
              )
            ),
            shiny::textInput("end_date", "Search end date (optional, YYYY-MM-DD)", value = wizard_values$end_date, width = "100%")
          )
        ),
        `5` = {
          db_labels <- c(pubmed = "PubMed", embase = "Embase", central = "CENTRAL")
          selected_db <- unname(db_labels[wizard_values$databases])
          selected_db <- selected_db[!is.na(selected_db)]
          date_range <- if (isTRUE(wizard_values$inception)) {
            if (nzchar(trimws(wizard_values$end_date))) paste("Database inception to", wizard_values$end_date) else "Database inception to current search date"
          } else {
            start_value <- wizard_text(wizard_values$start_date)
            end_value <- if (nzchar(trimws(wizard_values$end_date))) wizard_values$end_date else "current search date"
            paste(start_value, "to", end_value)
          }

          shiny::tagList(
            shiny::div(class = "srma-wizard-kicker", "Step 5 of 5"),
            shiny::h3("Review & create", class = "srma-wizard-title"),
            shiny::p(
              "Review the setup below. Creating the review saves an immutable draft; it does not approve scientific decisions or generate search terms.",
              class = "srma-wizard-subtitle"
            ),
            summary_section(
              "Review basics",
              summary_item("Review title", wizard_text(wizard_values$title), TRUE),
              summary_item("Principal investigator", wizard_text(wizard_values$investigator)),
              summary_item("Framework", wizard_text(wizard_values$framework)),
              summary_item("Review question", wizard_text(wizard_values$question), TRUE)
            ),
            summary_section(
              "Research question",
              summary_item("Population", wizard_multiline(wizard_values$population)),
              summary_item("Intervention / exposure", wizard_multiline(wizard_values$intervention)),
              summary_item("Comparator", wizard_multiline(wizard_values$comparator)),
              summary_item("Primary outcomes", wizard_multiline(wizard_values$primary)),
              summary_item("Secondary outcomes", wizard_multiline(wizard_values$secondary), TRUE)
            ),
            summary_section(
              "Eligibility",
              summary_item("Eligible study designs", wizard_multiline(wizard_values$designs)),
              summary_item("Language restriction", wizard_text(wizard_values$language)),
              summary_item("Inclusion criteria", wizard_multiline(wizard_values$inclusion), TRUE),
              summary_item("Exclusion criteria", wizard_multiline(wizard_values$exclusion), TRUE),
              summary_item("Default screening exclusion reason", wizard_text(wizard_values$reason), TRUE)
            ),
            summary_section(
              "Search",
              summary_item("Databases", if (length(selected_db)) paste(selected_db, collapse = ", ") else "Not selected"),
              summary_item("Search period", date_range)
            ),
            shiny::div(
              class = "srma-review-note",
              "Scientific choices remain subject to the toolkit's Human Review gates. Search generation, screening, analysis, and manuscript steps remain version-bound after creation."
            ),
            shiny::tags$details(
              shiny::tags$summary("Technical details"),
              shiny::p(paste("Project identifier:", wizard_values$canonical_id))
            )
          )
        }
      )

      shiny::div(
        class = "srma-wizard-wrap",
        shiny::div(
          class = "srma-wizard-heading",
          shiny::h2(if (is.null(editing())) "New Systematic Review" else "Edit Review as a New Version"),
          shiny::p("Complete the setup in five short steps. Your entries are kept while you move between steps.")
        ),
        wizard_progress_ui(step),
        shiny::div(
          class = "srma-wizard-card",
          step_body,
          wizard_footer_ui(step)
        )
      )
    })

    shiny::observeEvent(input$wizard_cancel, {
      wizard_capture(wizard_step())
      wizard_errors(character())
      shiny::updateTabsetPanel(session, "page", selected = "my_projects")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$wizard_back, {
      step <- wizard_step()
      wizard_capture(step)
      wizard_errors(character())
      wizard_step(max(1L, step - 1L))
      notice("")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$wizard_next, {
      step <- wizard_step()
      wizard_capture(step)

      errors <- character()

      if (identical(step, 1L)) {
        if (!nzchar(trimws(wizard_values$title))) errors <- c(errors, "title")
        if (!nzchar(trimws(wizard_values$question))) errors <- c(errors, "question")
        if (!nzchar(trimws(wizard_values$canonical_id))) errors <- c(errors, "canonical_id")
      }

      if (identical(step, 2L)) {
        if (!nzchar(trimws(wizard_values$population))) errors <- c(errors, "population")
        if (!nzchar(trimws(wizard_values$intervention))) errors <- c(errors, "intervention")
        if (!nzchar(trimws(wizard_values$comparator))) errors <- c(errors, "comparator")
        if (!nzchar(trimws(wizard_values$primary))) errors <- c(errors, "primary")
      }

      if (identical(step, 3L)) {
        if (!nzchar(trimws(wizard_values$designs))) errors <- c(errors, "designs")
        if (!nzchar(trimws(wizard_values$inclusion))) errors <- c(errors, "inclusion")
        if (!nzchar(trimws(wizard_values$exclusion))) errors <- c(errors, "exclusion")
        if (!nzchar(trimws(wizard_values$reason))) errors <- c(errors, "reason")
      }

      if (identical(step, 4L)) {
        if (!length(wizard_values$databases)) errors <- c(errors, "databases")
        if (!isTRUE(wizard_values$inception) && !nzchar(trimws(wizard_values$start_date))) {
          errors <- c(errors, "start_date")
        }
      }

      wizard_errors(unique(errors))
      if (length(errors)) {
        notice("Complete the required fields marked below before continuing.")
        return()
      }

      wizard_errors(character())
      wizard_step(min(5L, step + 1L))
      notice("")
    }, ignoreInit = TRUE)

    lines <- function(value) as.list(Filter(nzchar, trimws(strsplit(if (is.null(value)) "" else value, "\n", fixed = TRUE)[[1]])))

    shiny::observeEvent(input$new_draft, {
      editing(NULL)
      reset_wizard(keep_workspace = TRUE)
      notice("A separate review is ready to set up. Review every field before creating it.")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$save, {
      required_values <- c(
        title = wizard_values$title,
        question = wizard_values$question,
        population = wizard_values$population,
        intervention = wizard_values$intervention,
        comparator = wizard_values$comparator,
        primary = wizard_values$primary,
        designs = wizard_values$designs,
        inclusion = wizard_values$inclusion,
        exclusion = wizard_values$exclusion,
        reason = wizard_values$reason
      )
      missing <- names(required_values)[!nzchar(trimws(required_values))]
      if (length(missing)) {
        if (any(missing %in% c("population", "intervention", "comparator", "primary"))) {
          wizard_errors(unique(missing[missing %in% c("population", "intervention", "comparator", "primary")]))
          wizard_step(2L)
        } else if (any(missing %in% c("designs", "inclusion", "exclusion", "reason"))) {
          wizard_errors(unique(missing[missing %in% c("designs", "inclusion", "exclusion", "reason")]))
          wizard_step(3L)
        } else {
          wizard_errors(unique(missing[missing %in% c("title", "question")]))
          wizard_step(1L)
        }
        notice("Complete the required fields marked below before creating the review.")
        return()
      }
      if (!isTRUE(wizard_values$inception) && !nzchar(trimws(wizard_values$start_date))) {
        notice("Enter a search start date or choose database inception.")
        return()
      }
      if (!length(wizard_values$databases)) {
        notice("Select at least one database before creating the review.")
        return()
      }

      values <- list(
        "project.title" = wizard_values$title,
        "project.project_id" = wizard_values$canonical_id,
        "project.principal_investigator" = wizard_values$investigator,
        "research_question.framework" = wizard_values$framework,
        "research_question.question" = wizard_values$question,
        "research_question.population.description" = wizard_values$population,
        "research_question.intervention.description" = wizard_values$intervention,
        "research_question.comparator.description" = wizard_values$comparator,
        "research_question.outcomes.primary" = lines(wizard_values$primary),
        "research_question.outcomes.secondary" = lines(wizard_values$secondary),
        "eligibility.study_designs.include" = lines(wizard_values$designs),
        "eligibility.inclusion" = lines(wizard_values$inclusion),
        "eligibility.exclusion" = lines(wizard_values$exclusion),
        "eligibility.language_restriction" = wizard_values$language,
        "search.search_start_date" = if (isTRUE(wizard_values$inception)) "" else wizard_values$start_date,
        "search.search_end_date" = wizard_values$end_date,
        databases = as.list(wizard_values$databases),
        exclusion_reason = wizard_values$reason
      )

      wizard_errors(character())
      notice("Creating the systematic review…")

      edit <- editing()
      saved <- safe(request(
        "save_configuration",
        list(
          workspace_id = wizard_values$workspace,
          values = values,
          project_id = if (!is.null(edit)) edit$project_id else NULL,
          version_id = if (!is.null(edit)) edit$version_id else NULL
        )
      ))

      if (!is.null(saved)) {
        was_new <- is.null(edit)
        editing(saved)
        if (!is.null(saved$project_id) && srma_has_text(saved$project_id)) pending_project_selection(as.character(saved$project_id[[1L]]))
        refresh(refresh() + 1L)
        wizard_step(1L)
        notice(if (was_new) {
          "Systematic review created as an immutable draft. Scientific choices still require Human Review before downstream workflow steps."
        } else {
          "A new immutable version was saved. Scientific choices still require Human Review before downstream workflow steps."
        })
        nav_section("dashboard")
        shiny::updateTabsetPanel(session, "page", selected = "my_projects")
      }
    })

    shiny::observeEvent(input$edit, {
      cfg <- safe(request("configuration", list(project_id = input$project, version_id = input$version)))
      shiny::req(cfg)
      editing(list(project_id = input$project, version_id = input$version))

      description <- function(x) if (is.list(x)) x$description else x
      collapse_value <- function(x) {
        if (is.null(x) || !length(x)) "" else paste(unlist(x), collapse = "\n")
      }

      wizard_values$title <- collapse_value(cfg$project$title)
      wizard_values$canonical_id <- collapse_value(cfg$project$project_id)
      wizard_values$investigator <- collapse_value(cfg$project$principal_investigator)
      wizard_values$framework <- collapse_value(cfg$research_question$framework)
      wizard_values$question <- collapse_value(cfg$research_question$question)
      wizard_values$population <- collapse_value(description(cfg$research_question$population))
      wizard_values$intervention <- collapse_value(description(cfg$research_question$intervention))
      wizard_values$comparator <- collapse_value(description(cfg$research_question$comparator))
      wizard_values$primary <- collapse_value(if (is.list(cfg$research_question$outcomes)) cfg$research_question$outcomes$primary else cfg$research_question$outcomes)
      wizard_values$secondary <- collapse_value(if (is.list(cfg$research_question$outcomes)) cfg$research_question$outcomes$secondary else NULL)
      wizard_values$designs <- collapse_value(cfg$eligibility$study_designs$include)
      wizard_values$inclusion <- collapse_value(cfg$eligibility$inclusion)
      wizard_values$exclusion <- collapse_value(cfg$eligibility$exclusion)
      wizard_values$language <- collapse_value(cfg$eligibility$language_restriction)
      wizard_values$start_date <- collapse_value(cfg$search$search_start_date)
      wizard_values$end_date <- collapse_value(cfg$search$search_end_date)
      wizard_values$inception <- !nzchar(trimws(wizard_values$start_date))

      reasons <- cfg$screening$exclusion_reasons
      wizard_values$reason <- if (!is.null(reasons) && length(reasons) && !is.null(reasons[[1L]]$label)) {
        collapse_value(reasons[[1L]]$label)
      } else {
        ""
      }

      wizard_values$databases <- names(Filter(
        function(x) if (is.list(x)) isTRUE(x$enabled) else isTRUE(x),
        cfg$search$databases
      ))

      projects <- safe(request("projects"))
      if (!is.null(projects) && nrow(projects)) {
        match_idx <- match(input$project, projects$id)
        if (!is.na(match_idx)) wizard_values$workspace <- projects$workspace_id[[match_idx]]
      }

      wizard_step(1L)
      notice("")
      nav_section("review_setup")
      shiny::updateTabsetPanel(session, "page", selected = "new_review")
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
      shiny::updateSelectInput(session,"draft_job",choices=c("Saved review configuration"="",if(nrow(jobs))setNames(jobs$id,paste("Prepared canonical draft",jobs$created_at)) else character()),selected=shiny::isolate(input$draft_job))
    })
    shiny::observeEvent(input$load_draft, {
      shiny::req(input$project,input$version)
      shiny::withProgress(message = "Loading review details...", value = 0.2, {
        cfg <- safe(request("configuration", list(project_id = input$project, version_id = input$version)))
        if (is.null(cfg)) {
          draft_review(NULL)
          shiny::showNotification("Review details could not be loaded.", type = "error", duration = 5)
          return()
        }
        shiny::incProgress(0.45)
        job_id <- if (is.null(input$draft_job) || !nzchar(input$draft_job)) NULL else input$draft_job
        canonical <- NULL
        if (!is.null(job_id)) {
          canonical <- safe(request("initialization_review", list(
            project_id = input$project, version_id = input$version, job_id = job_id
          )))
        }
        draft_review(list(
          project_id = input$project,
          version_id = input$version,
          job_id = job_id,
          configuration = cfg,
          canonical = canonical
        ))
        shiny::incProgress(0.35)
      })
    })
    output$canonical_review<-shiny::renderUI({
      r<-draft_review();shiny::req(r)
      cfg <- r$configuration
      value_text <- function(x) {
        if (is.null(x) || !length(x)) return("Not specified")
        if (is.list(x) && !is.null(x$description)) x <- x$description
        y <- trimws(paste(unlist(x), collapse = "; "))
        if (nzchar(y)) y else "Not specified"
      }
      enabled_db <- names(Filter(function(x) if (is.list(x)) isTRUE(x$enabled) else isTRUE(x), cfg$search$databases))
      search_start <- value_text(cfg$search$search_start_date)
      search_end <- value_text(cfg$search$search_end_date)
      if (identical(search_start, "Not specified")) search_start <- "Database inception"
      if (identical(search_end, "Not specified")) search_end <- "Current / not specified"

      canonical_block <- if (is.null(r$canonical)) {
        shiny::div(
          class = "srma-review-note",
          shiny::strong("Canonical search concepts have not been prepared for this saved version yet."),
          shiny::br(),
          "That is expected for a review created in the setup wizard. Search concepts will be prepared and explicitly reviewed before literature search generation."
        )
      } else {
        c <- r$canonical
        shiny::tagList(
          shiny::div(class = "srma-summary-section",
            shiny::h4("Prepared canonical search concepts"),
            shiny::tags$pre(c$terms)
          ),
          lapply(seq_along(c$decisions), function(i) {
            d <- c$decisions[[i]]
            shiny::div(class = "srma-summary-section",
              shiny::h4(d$question),
              shiny::p(d$why_it_matters),
              shiny::p(paste("Canonical recommendation:", d$recommended_option)),
              shiny::p(d$recommendation_rationale),
              shiny::selectInput(
                paste0("canonical_decision_", i), "Your explicit choice",
                c("Choose an option" = "", unlist(d$suggested_options))
              )
            )
          }),
          shiny::textAreaInput("activation_rationale", "Review rationale"),
          shiny::checkboxInput(
            "approve_canonical",
            "I reviewed this configuration, these concepts and all listed decisions",
            FALSE
          ),
          shiny::actionButton("activate_canonical", "Approve and activate as a new saved version")
        )
      }

      shiny::tagList(
        shiny::div(class = "srma-summary-section",
          shiny::h4("Saved review configuration"),
          shiny::div(class = "srma-summary-grid",
            summary_item("Review title", value_text(cfg$project$title), TRUE),
            summary_item("Framework", value_text(cfg$research_question$framework)),
            summary_item("Project identifier", value_text(cfg$project$project_id)),
            summary_item("Review question", value_text(cfg$research_question$question), TRUE),
            summary_item("Population", value_text(cfg$research_question$population), TRUE),
            summary_item("Intervention / exposure", value_text(cfg$research_question$intervention), TRUE),
            summary_item("Comparator", value_text(cfg$research_question$comparator), TRUE),
            summary_item("Primary outcomes", value_text(cfg$research_question$outcomes$primary), TRUE),
            summary_item("Secondary outcomes", value_text(cfg$research_question$outcomes$secondary), TRUE)
          )
        ),
        shiny::div(class = "srma-summary-section",
          shiny::h4("Eligibility"),
          shiny::div(class = "srma-summary-grid",
            summary_item("Study designs", value_text(cfg$eligibility$study_designs$include), TRUE),
            summary_item("Inclusion criteria", value_text(cfg$eligibility$inclusion), TRUE),
            summary_item("Exclusion criteria", value_text(cfg$eligibility$exclusion), TRUE),
            summary_item("Language", value_text(cfg$eligibility$language_restriction), TRUE)
          )
        ),
        shiny::div(class = "srma-summary-section",
          shiny::h4("Search scope"),
          shiny::div(class = "srma-summary-grid",
            summary_item("Databases", if (length(enabled_db)) paste(enabled_db, collapse = ", ") else "Not specified"),
            summary_item("Search period", paste(search_start, "to", search_end))
          )
        ),
        canonical_block
      )
    })
    shiny::observeEvent(input$activate_canonical, {
      r<-draft_review();shiny::req(r,input$project,input$version,r$canonical)
      c <- r$canonical
      if(!identical(r$project_id,input$project)||!identical(r$version_id,input$version)) {notice("Reload the draft for this selected version.");return()}
      selections<-setNames(lapply(seq_along(c$decisions),function(i)input[[paste0("canonical_decision_",i)]]),
        vapply(c$decisions,`[[`,character(1),"decision_id"))
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
