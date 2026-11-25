web_c2_ui <- function() shiny::tabPanel("Manuscript",
  # RC28.25.8A: presentation-only simplification. No claims of production acceptance.
  shiny::tags$style(shiny::HTML("
.srma-manuscript-production { max-width: 880px; padding: 14px 0 18px; }
.srma-manuscript-production h3 { margin-top: 5px; margin-bottom: 7px; font-size: 22px; font-weight: 650; }
.srma-manuscript-production .srma-production-lead { color: #596878; margin-bottom: 16px; }
.srma-production-steps { display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 17px; }
.srma-production-steps span { padding: 6px 10px; border: 1px solid #dde3e9; border-radius: 6px; background: #f6f8fa; font-size: 12px; }
.srma-production-status { min-height: 44px; margin: 5px 0 14px; padding: 10px 12px; border-left: 3px solid #657f93; background: #f5f7f9; }
.srma-production-actions { display: flex; flex-wrap: wrap; align-items: center; gap: 12px; }
.srma-production-actions button[disabled] { cursor: not-allowed; opacity: .58; }
.srma-production-note { font-size: 12px; color: #6f7880; margin-top: 12px; }
.srma-manuscript-advanced { max-width: 920px; border-top: 1px solid #e3e7eb; padding-top: 11px; margin-bottom: 26px; }
.srma-manuscript-advanced summary { font-size: 12px; cursor: pointer; color: #64748b; }
.srma-manuscript-advanced-body { margin-top: 16px; padding: 15px; border: 1px solid #eceff3; border-radius: 6px; }
")),
  shiny::tags$div(class="srma-manuscript-production",
    shiny::tags$h3("Manuscript Building & Production"),
    shiny::tags$p(class="srma-production-lead",
      "From finalized evidence to a journal-neutral, editable Word manuscript."),
    shiny::tags$div(class="srma-production-steps",
      shiny::tags$span("1  Source preflight"),
      shiny::tags$span("2  PRISMA / tables / figures"),
      shiny::tags$span("3  Draft & citation QC"),
      shiny::tags$span("4  Editable DOCX")),
    shiny::tags$div(class="srma-production-status",role="status",`aria-live`="polite",
      shiny::textOutput("manuscript_production_status"),
      shiny::textOutput("manuscript_publication_qc_status")),
    shiny::tags$div(class="srma-production-actions",
      shiny::actionButton("manuscript_generate","Generate manuscript draft",
        class="btn-primary"),
      shiny::uiOutput("manuscript_production_download")),
    shiny::uiOutput("manuscript_revision_editor"),
    shiny::uiOutput("manuscript_revision_validation"),
    shiny::uiOutput("manuscript_author_review"),
    shiny::tags$div(id="srma-manuscript-production-progress",hidden="hidden",
      role="status",`aria-live`="polite",
      shiny::tags$div(class="srma-ref-progress-track",role="progressbar",
        `aria-label`="Manuscript production in progress",
        shiny::tags$div(class="srma-ref-progress-indicator")),
      shiny::tags$span(id="srma-manuscript-production-progress-label","")),
    shiny::tags$p(class="srma-production-note","Use Revise manuscript to edit the saved draft and keep its approved evidence and references."),
    shiny::tags$style(shiny::HTML("
#srma-manuscript-production-progress { max-width: 520px; margin: 12px 0; font-size: 12px; }
#srma-manuscript-production-progress[hidden] {display:none !important;}
#srma-manuscript-production-progress[data-state='running'] .srma-ref-progress-indicator {animation:srma-ref-sweep 1.5s ease-in-out infinite;}
#srma-manuscript-production-progress[data-state='success'] .srma-ref-progress-indicator {animation:none;transform:none;width:100%;background:#637b6d;}
#srma-manuscript-production-progress[data-state='error'] .srma-ref-progress-indicator {animation:none;transform:none;width:100%;background:#9b6262;}
")),
    shiny::tags$script(shiny::HTML("
(function(){'use strict';
  if(window.__srmaManuscriptProductionInit)return;
  window.__srmaManuscriptProductionInit=true;
  var bound=false;
  function show(state,message){
    var root=document.getElementById('srma-manuscript-production-progress');
    var button=document.getElementById('manuscript_generate');
    if(!root)return;root.hidden=false;root.setAttribute('data-state',state);
    var label=document.getElementById('srma-manuscript-production-progress-label');
    if(label)label.textContent=message||'';
    if(button){button.disabled=state==='running';}
  }
  function register(){if(bound||!window.Shiny||!window.Shiny.addCustomMessageHandler)return;
    window.Shiny.addCustomMessageHandler('srma-manuscript-production',function(data){
      if(data&&['running','success','error'].indexOf(data.state)>-1)show(data.state,data.message);
    });bound=true;
  }
  document.addEventListener('click',function(e){if(e.target&&e.target.closest&&e.target.closest('#manuscript_generate')){
    register();show('running','Preparing verified sources and queuing manuscript production…');
  }});
  document.addEventListener('DOMContentLoaded',register);
  document.addEventListener('shiny:connected',register);
  document.addEventListener('shiny:disconnected',function(){
    var root=document.getElementById('srma-manuscript-production-progress');
    if(root&&root.getAttribute('data-state')==='running')show('error','Connection lost. Check job status before retrying.');
  });
  if(document.readyState!=='loading')register();
})();
"))),
  shiny::tags$details(class="srma-manuscript-advanced",id="srma-manuscript-advanced",
    shiny::tags$summary("Operator tools (temporary legacy workflow)"),
    shiny::tags$div(class="srma-manuscript-advanced-body",
 shiny::p("Journal-neutral working draft. Sources auto-load from finalized Human-approved Evidence; final author and RoB approval remain required."),
 shiny::textOutput("manuscript_handoff_status"),
 shiny::actionButton("manuscript_catalog","Load sources for selected evidence version"),shiny::tableOutput("manuscript_sources"),
 shiny::textInput("manuscript_title","Manuscript title"),shiny::selectInput("manuscript_language","Language",c("English","Korean")),
 shiny::selectInput("manuscript_analyses","Approved analyses to report",character(),multiple=TRUE),shiny::selectInput("manuscript_artifacts","Registered tables and figures",character(),multiple=TRUE),
 shiny::textAreaInput("manuscript_pmids","Reference PubMed IDs (included-study candidates auto-filled; verified via R/39 before plan save)"),shiny::textAreaInput("manuscript_rationale","Plan rationale / evidence selection"),shiny::actionButton("manuscript_plan","Find & verify missing PubMed references, then save plan"),
 # RC28.25.6: a truthful indeterminate progress bar. Discovery is synchronous,
 # so elapsed time cannot be converted into a reliable percentage.
 shiny::tags$div(id="srma-manuscript-reference-progress",class="srma-ref-progress",
   hidden="hidden",role="status",`aria-live`="polite",
   shiny::tags$div(class="srma-ref-progress-track",role="progressbar",
     `aria-label`="Reference discovery in progress",
     shiny::tags$div(class="srma-ref-progress-indicator")),
   shiny::tags$span(class="srma-ref-progress-label","")),
 shiny::tags$style(shiny::HTML("
.srma-ref-progress { max-width: 490px; margin: 8px 0 12px; font-size: 12px; line-height: 1.5; }
.srma-ref-progress[hidden] { display: none !important; }
.srma-ref-progress-track { height: 5px; border-radius: 4px; overflow: hidden; background: #d5dce4; margin-bottom: 5px; }
.srma-ref-progress-indicator { height: 100%; width: 38%; background: #436982; transform: translateX(-110%); }
.srma-ref-progress[data-state='running'] .srma-ref-progress-indicator { animation: srma-ref-sweep 1.5s ease-in-out infinite; }
.srma-ref-progress[data-state='success'] .srma-ref-progress-indicator,
.srma-ref-progress[data-state='error'] .srma-ref-progress-indicator { animation: none; transform: none; width: 100%; }
.srma-ref-progress[data-state='success'] .srma-ref-progress-indicator { background: #637b6d; }
.srma-ref-progress[data-state='error'] .srma-ref-progress-indicator { background: #9b6262; }
@keyframes srma-ref-sweep { from { transform: translateX(-110%); } to { transform: translateX(270%); } }
@media (prefers-reduced-motion: reduce) { .srma-ref-progress[data-state='running'] .srma-ref-progress-indicator { animation: none; transform: none; width: 100%; } }
")),
 shiny::tags$script(shiny::HTML("
(function () {
  'use strict';
  if (window.__srmaReferenceProgressInit) return;
  window.__srmaReferenceProgressInit = true;
  var type = 'srma-manuscript-reference-progress';
  var handlerReady = false;
  function render(state, message) {
    var box = document.getElementById('srma-manuscript-reference-progress');
    var button = document.getElementById('manuscript_plan');
    if (!box || !button) return;
    box.hidden = false;
    box.setAttribute('data-state', state);
    var label = box.querySelector('.srma-ref-progress-label');
    if (label) label.textContent = message || '';
    button.disabled = (state === 'running');
    if (state === 'running') button.setAttribute('aria-busy', 'true');
    else button.removeAttribute('aria-busy');
  }
  function register() {
    if (handlerReady || !window.Shiny || !window.Shiny.addCustomMessageHandler) return;
    window.Shiny.addCustomMessageHandler(type, function (payload) {
      if (!payload) return;
      if (payload.state !== 'running' && payload.state !== 'success' && payload.state !== 'error') return;
      render(payload.state, payload.message);
    });
    handlerReady = true;
  }
  document.addEventListener('click', function (event) {
    if (!event.target || !event.target.closest) return;
    if (!event.target.closest('#manuscript_plan')) return;
    register();
    render('running', 'Searching PubMed and verifying references. The plan will be saved only after verification.');
  });
  document.addEventListener('DOMContentLoaded', register);
  document.addEventListener('shiny:connected', register);
  document.addEventListener('shiny:disconnected', function () {
    var box = document.getElementById('srma-manuscript-reference-progress');
    if (box && box.getAttribute('data-state') === 'running')
      render('error', 'Connection lost; check the App status before retrying.');
  });
  if (document.readyState !== 'loading') register();
})();
")),
 shiny::p("Topic-anchored PubMed discovery runs only when references are insufficient. Metadata verification is not citation-suitability approval. Select contextual references before approving the Human manuscript plan."),
 shiny::textOutput("manuscript_reference_discovery_status"),
 shiny::p("Previously verified PMIDs and topic-filtered literature are reused automatically. The author reviews citations and scientific interpretation in the finished DOCX; no separate reference-selection decision is required."),
 shiny::actionButton("manuscript_load","Load selected manuscript version"),
 shiny::p("Version selector: use an Evidence Version for 'Load sources'; use a saved Manuscript PLAN Version for 'Load selected manuscript version'. No new Evidence review is needed after saving references."),
 shiny::textOutput("manuscript_status"),shiny::verbatimTextOutput("manuscript_plan_summary"),shiny::tableOutput("manuscript_references"),
 shiny::p("To revise Statistical Analysis before Word generation, select Methods and save a draft. For a change to the analysis itself, use the approved Analysis workflow."),shiny::actionButton("manuscript_begin","Prepare section-editing draft"),
 shiny::selectInput("manuscript_section","Section",web_c2_sections()),shiny::verbatimTextOutput("manuscript_section_status"),shiny::textAreaInput("manuscript_text","Human prose (numeric claims must use protected facts)",rows=10),
 shiny::selectInput("manuscript_fact","Protected source fact",character()),shiny::actionButton("manuscript_insert_fact","Insert selected fact"),
 shiny::selectInput("manuscript_insert_artifact","Registered artifact reference",character()),shiny::actionButton("manuscript_artifact_token","Insert artifact reference"),
 shiny::textAreaInput("manuscript_edit_reason","Author review/edit rationale"),shiny::selectInput("manuscript_decision","Section action",c("Save draft"="SAVE","Approve section"="APPROVE","Reject draft"="REJECT")),shiny::actionButton("manuscript_edit","Save section as new version"),
 shiny::actionButton("manuscript_ai","Request AI regeneration for this section"),shiny::p("AI requires operator credentials. Manual entry is always available. Every edit creates a version and clears Word approval."),
 shiny::verbatimTextOutput("manuscript_precheck"),shiny::verbatimTextOutput("manuscript_preview"),shiny::actionButton("manuscript_docx","Queue editable Word document"),
 shiny::selectInput("manuscript_job","Completed job for selected version",character()),shiny::actionButton("manuscript_accept","Accept completed output as new version"))))
web_c2_server <- function(input,output,session,request,safe,notice,refresh) {
 packet<-shiny::reactiveVal(NULL);catalog<-shiny::reactiveVal(NULL)
 production<-shiny::reactiveVal(NULL)
 # RC28.25.9BCD.5.3: a registered DOCX artifact must survive asynchronous
 # version-selector updates. The old packet() cache alone is not authoritative.
 packet_version<-shiny::reactiveVal(NULL)
 validation_finished<-shiny::reactiveVal(NULL)
 output$manuscript_author_review<-shiny::renderUI({
   if(!identical(input$page,"Manuscript") || !identical(packet()$stage,"REVIEW_REQUIRED") ||
      !identical(packet_version(),input$version))return(NULL)
   validation_finished()
   ready<-tryCatch(request("manuscript_author_ready",list(project_id=input$project,version_id=input$version)),
     error=function(e)FALSE)
   if(!isTRUE(ready))return(shiny::p("Final author approval opens after the required scientific checks pass."))
   shiny::tagList(shiny::checkboxInput("manuscript_confirm",
     "I reviewed text, analyses, citations, warnings and the matching Word document",FALSE),
     shiny::textAreaInput("manuscript_review_reason","Manuscript review rationale"),
     shiny::actionButton("manuscript_approve","Approve author-reviewed manuscript"))
 })
 completed_docx<-shiny::reactiveVal(NULL)
 docx_id<-function(x) {
   if(!is.list(x) || !is.list(x$docx))return("")
   id<-x$docx$artifact_id
   if(is.null(id) || length(id)!=1L)return("")
   id<-as.character(id[[1L]])
   if(length(id)!=1L || is.na(id) || !nzchar(trimws(id)))return("")
   trimws(id)
 }
 # Prefer the currently selected immutable DOCX. During an automatic
 # version transition, use the just-accepted artifact as a safe fallback.
 # This fallback is labelled with its exact version, so it cannot masquerade
 # as another selected version. It performs no Worker or AI requests.
 download_source<-shiny::reactive({
   selected<-packet()
   selected_id<-docx_id(selected)
   pv<-packet_version()
   if(nzchar(selected_id) && !is.null(pv) &&
      identical(as.character(input$version),as.character(pv)))
     return(list(project_id=as.character(input$project),
       version_id=as.character(pv),artifact_id=selected_id,
       approved=identical(selected$stage,"APPROVED"),
       label=if(!is.null(completed_docx()) &&
         identical(as.character(completed_docx()$version_id),as.character(pv)))
           completed_docx()$label else ""))
   recent<-completed_docx()
   if(is.list(recent) && isTRUE(recent$awaiting_selection) &&
      identical(as.character(input$version),recent$from_version) &&
      identical(as.character(input$project),as.character(recent$project_id)) &&
      is.character(recent$artifact_id) && length(recent$artifact_id)==1L &&
      !is.na(recent$artifact_id) && nzchar(recent$artifact_id))
     return(recent)
   NULL
 })
 args<-function()list(project_id=input$project,version_id=input$version)
 # Safe for intentionally empty Narrative-only artifacts, PLAN sections, and facts.
 # base::setNames(NULL, ...) raises "attempt to set an attribute on NULL".
 named_choices <- function(values, labels=values) {
   if(is.null(values) || length(values)==0L)return(character())
   values<-as.character(values)
   labels<-if(is.null(labels))character() else as.character(labels)
   if(length(labels)!=length(values))labels<-values
   stats::setNames(values,labels)
 }
 # 9BCD.5: after Worker acceptance, reload the saved immutable DOCX version
 # in this Shiny session. Do not ask the author to choose a version or press F5.
 saved<-function(v,autoload=FALSE){
   if(is.null(v))return(invisible(FALSE))
   packet(NULL)
   packet_version(NULL)
   refresh(refresh()+1L)
   view<-request("view",list(project_id=v$project_id))
   shiny::updateSelectInput(session,"version",
     choices=named_choices(view$versions$id,paste("Version",view$versions$version)),
     selected=v$version_id)
   if(isTRUE(autoload)) {
     docx<-tryCatch(request("manuscript_view",list(project_id=v$project_id,
       version_id=v$version_id)),error=function(err)NULL)
     if(!is.list(docx) || is.null(docx$docx$artifact_id) ||
        length(docx$docx$artifact_id)==0L ||
        !nzchar(as.character(docx$docx$artifact_id[[1L]]))) {
       notice("DOCX registration completed, but automatic loading failed. The saved version is preserved; check the App log, not the Generate button.")
       return(invisible(FALSE))
     }
     idx<-match(v$version_id,view$versions$id)
     ver<-if(!is.na(idx))paste("Version",view$versions$version[[idx]]) else
       paste("saved manuscript",substr(v$version_id,1L,12L))
     # Persist the exact server-registered artifact independently of packet().
     # observeEvent(input$version) may run after updateSelectInput and reset
     # packet; this reactive value is the download fallback until selection
     # catches up. Never infer an artifact ID from a filename or job message.
     completed_docx(list(project_id=as.character(v$project_id),
       version_id=as.character(v$version_id),artifact_id=docx_id(docx),
       label=ver,packet=docx,approved=identical(docx$stage,"APPROVED"),
       from_version=as.character(input$version),awaiting_selection=TRUE))
     packet_version(v$version_id)
     packet(docx)
     notice(paste("Editable DOCX ready:",ver,"is selected and downloadable."))
   } else notice("New immutable manuscript version saved. Load it to review.")
   invisible(TRUE)
 }
 handoff_set <- function(x) {
   catalog(x)
   if(is.null(x))return(invisible(NULL))
   shiny::updateSelectInput(session,"manuscript_analyses",
     choices=named_choices(x$analyses$version_id,x$analyses$label),
     selected=if(identical(x$route,"NARRATIVE"))x$analyses$version_id else character())
   shiny::updateSelectInput(session,"manuscript_artifacts",
     choices=named_choices(x$artifacts$id,paste(x$artifacts$logical_name,x$artifacts$analysis_version)),
     selected=character())
   if(identical(x$route,"NARRATIVE")) {
     if(nzchar(x$title))shiny::updateTextInput(session,"manuscript_title",value=x$title)
     if(x$language %in% c("English","Korean"))
       shiny::updateSelectInput(session,"manuscript_language",selected=x$language)
     shiny::updateTextAreaInput(session,"manuscript_pmids",value=paste(x$pmids,collapse=","))
     shiny::updateTextAreaInput(session,"manuscript_rationale",value=x$rationale)
   }
 }
  # The same action button always operates on an explicit, immutable version.
  # Its label makes regeneration semantics visible without changing input IDs.
  shiny::observe({
    x<-packet()
    exists<-nzchar(docx_id(x)) || !is.null(download_source())
    shiny::updateActionButton(session,"manuscript_generate",
      label=if(exists) "Generate new AI draft" else "Generate manuscript draft")
  })
  output$manuscript_publication_qc_status<-shiny::renderText({
    x<-packet()
    if(is.null(x) || is.null(x$docx$artifact_id))return("")
    q<-x$production$qc$publication_9cd
    if(is.null(q))return("Existing DOCX has no recorded 9C/9D acceptance result; it is preserved until a new version passes verification.")
    paste0("9C/9D production QC recorded. PRISMA: ",
      as.character(q$prisma_status %||b2% "UNRECORDED"),
      " | R/34: ",as.character(q$r34_status %||b2% "UNRECORDED"),
      ". Final author/R40 review remains separate.")
  })
  # Status comes only from persisted job state and the selected version.
  output$manuscript_production_status<-shiny::renderText({
   run<-production()
   if(!is.null(run) && run$state %in% c("PREPARING","QUEUED","RUNNING","ACCEPTING"))
     return(paste0("Manuscript production: ",run$state,
       " — ",run$message %||b2% "Worker is processing the verified source package."))
   if(!is.null(run) && identical(run$state,"FAILED"))
     return(paste("Manuscript production FAILED:",run$message))
   if(!is.null(run) && identical(run$state,"COMPLETE"))
     return(paste("DOCX Ready —",run$message,
       ". Download Editable DOCX is available on this screen."))
   x<-packet()
   if(!is.null(x)) {
     id<-x$docx$artifact_id
     ready<-!is.null(id) && length(id)>0L && nzchar(as.character(id[[1L]]))
     if(ready)return("Editable Word document available for this version.")
     stage<-as.character(x$stage %||b2% "UNKNOWN")
     return(paste0("Selected manuscript: ",stage,
       ". Generate a draft from a verified plan."))
   }
   x<-catalog()
   if(!is.null(x))return("Finalized Evidence identified. Generate will create a versioned plan and queue the author-editable manuscript.")
   "Select a finalized Evidence Version or a saved Manuscript Version above."
 })
 output$manuscript_production_download<-shiny::renderUI({
   d<-download_source()
   if(!is.null(d)) {
     current<-identical(as.character(input$version),as.character(d$version_id))
     kind<-if(isTRUE(d$approved))"author-approved DOCX" else "review DOCX"
     text<-if(current || !nzchar(as.character(d$label)))paste("Download",kind) else paste("Download",d$label,kind)
     return(shiny::downloadButton("manuscript_download",text))
   }
   shiny::tags$button(type="button",class="btn btn-default",disabled="disabled",
     "Download Editable DOCX")
 })
 output$manuscript_revision_editor<-shiny::renderUI({
   x<-packet(); source<-x$revision_editor
   if(!isTRUE(x$current) || !is.list(source))return(NULL)
   shiny::tags$details(shiny::tags$summary("Revise manuscript"),
     shiny::selectInput("manuscript_revision_section","Section",names(source$sections),selected=source$default),
     shiny::p(if(isTRUE(source$canonical))"Edit plain prose. Keep existing citation numbers."
       else "This archive supports plain Statistical Analysis edits."),
     shiny::selectInput("manuscript_revision_type","Revision type",c("Wording"="EDITORIAL",
       "Methods description"="METHODS_DESCRIPTION","Change the analysis"="SCIENTIFIC_ANALYSIS")),
     shiny::conditionalPanel("input.manuscript_revision_type != 'SCIENTIFIC_ANALYSIS'",
       shiny::uiOutput("manuscript_revision_prose"),
       shiny::textAreaInput("manuscript_revision_reason","Reason for the change",rows=2),
       shiny::conditionalPanel("input.manuscript_revision_section == 'Statistical Analysis'",
         shiny::tags$details(shiny::tags$summary("Analysis evidence"),lapply(source$methods,function(r)shiny::p(r$text)))),
       shiny::actionButton("manuscript_revision_preview","Compare and save")),
     shiny::conditionalPanel("input.manuscript_revision_type == 'SCIENTIFIC_ANALYSIS'",
       shiny::p("Use the Analysis approval workflow to change scientific settings."),
       shiny::actionButton("manuscript_revision_analysis","Open Analysis")))
 })
 revision_section<-shiny::reactive({
   source<-packet()$revision_editor
   key<-input$manuscript_revision_section
   if(!isTRUE(key %in% names(source$sections)))key<-source$default
   if(length(key))c(list(id=key),source$sections[[key]])
 })
 output$manuscript_revision_prose<-shiny::renderUI({
   section<-revision_section();shiny::req(section)
   shiny::textAreaInput(revision_text_id(section$id),section$id,section$text,rows=9)
 })
 revision_text_id<-function(section)paste0("manuscript_revision_text_",gsub(" ","_",section,fixed=TRUE))
 revision_preview<-shiny::reactiveVal(NULL)
 revision_status<-shiny::reactiveVal(NULL)
 output$manuscript_revision_validation<-shiny::renderUI({
   x<-packet()
   if(!nzchar(docx_id(x)) || !identical(packet_version(),input$version))return(NULL)
   key<-args()
   previous<-revision_status()
   status<-if(identical(previous$key,key))previous$status
   if(is.null(status) || isTRUE(status$bound_worker$execution %in% c("QUEUED","RUNNING"))) {
     status<-tryCatch(request("r40_status",key),error=function(e)NULL)
     if(isTRUE(status$bound_worker$execution %in% c("QUEUED","RUNNING")))shiny::invalidateLater(3500,session)
   }
   rules<-Filter(function(r)identical(r$applicability,"REQUIRED"),status$rule_matrix)
   checks<-Filter(function(r)!identical(r$id,"INPUT_HUMAN_CONFIRMATION"),rules)
   failed<-sum(vapply(checks,function(r)identical(r$result,"FAIL"),logical(1)))
   pending<-sum(vapply(checks,function(r)identical(r$result,"NOT_ASSESSED"),logical(1)))
   content_ready<-isTRUE(status$integration$R40_VALIDATION_COMPLETE)
   latest_check<-status$bound_worker$execution
   validation_finished(list(key=key,ready=content_ready,execution=latest_check))
   open<-Filter(function(r)r$result %in% c("FAIL","NOT_ASSESSED"),checks)
   shiny::tagList(shiny::p(if(content_ready)
     "Required content checks passed." else
     "Review copy. Submission is blocked until the required checks pass."),
     if(length(rules))shiny::p(paste(failed,"failed;",pending,"pending.")),
     if(length(open))shiny::tags$details(shiny::tags$summary("Checks that need attention"),
       shiny::tags$ul(lapply(open,function(r)shiny::tags$li(r$basis)))),
     if(identical(x$stage,"REVIEW_REQUIRED") && isTRUE(status$bound_ready) && !content_ready &&
        !isTRUE(latest_check %in% c("QUEUED","RUNNING")))shiny::actionButton("manuscript_revision_verify","Check manuscript"))
 })
 shiny::observeEvent(input$manuscript_revision_verify,{
   key<-args()
   shiny::req(identical(packet_version(),key$version_id),identical(packet()$stage,"REVIEW_REQUIRED"))
   status<-safe(request("r40_bound_start",key))
   if(is.list(status))revision_status(list(key=key,status=status))
 })
 shiny::observeEvent(input$manuscript_revision_analysis,{
   x<-packet();shiny::req(x$evidence_version)
   shiny::updateSelectInput(session,"version",selected=x$evidence_version)
   shiny::updateTabsetPanel(session,"page",selected="Analysis")
 })
 shiny::observeEvent(input$manuscript_revision_preview,{
   source<-revision_section()
   shiny::req(source,identical(packet_version(),input$version))
   proposed<-input[[revision_text_id(source$id)]]
   shiny::req(is.character(proposed),nzchar(trimws(proposed)))
   if(identical(proposed,source$text)){notice("Enter a change before saving.");return()}
   type<-if(identical(source$id,"Statistical Analysis"))input$manuscript_revision_type else "EDITORIAL"
   revision_preview(c(args(),list(section=source$id,expected_section_sha256=source$sha256,type=type,
     text=proposed,rationale=input$manuscript_revision_reason)))
   shiny::showModal(shiny::modalDialog(title=paste("Compare",source$id),
     shiny::h5("Original"),shiny::tags$pre(source$text),shiny::h5("Revised"),shiny::tags$pre(proposed),
     shiny::p("The revised manuscript will need verification before final approval."),
     footer=shiny::tagList(shiny::modalButton("Back"),shiny::actionButton("manuscript_revision_confirm","Save and generate Word"))))
 })
 shiny::observeEvent(input$manuscript_revision_confirm,{
   pending<-revision_preview();revision_preview(NULL);shiny::removeModal()
   shiny::req(pending,identical(pending$project_id,input$project),identical(pending$version_id,input$version))
   v<-safe(request("manuscript_revision_save",pending))
   if(is.null(v))return()
   job<-safe(request("submit",c(v,list(operation="generate_manuscript_docx",
     idempotency_key=paste0("word-revision-",v$version_id)))))
   if(is.null(job)) {
     saved(v);notice("Revision saved. Word could not be queued; use Queue editable Word document to retry.");return()
   }
   production(list(state="QUEUED",project_id=v$project_id,version_id=v$version_id,job_id=job,
     operation="generate_manuscript_docx",message="Rendering the revised Word document"))
 })
 # Silent on non-evidence/manuscript versions. Never call AI during prefill.
 shiny::observeEvent(list(input$project,input$version),{
   if(is.null(input$project)||is.null(input$version)||
      !nzchar(input$project)||!nzchar(input$version)) {
     packet(NULL);packet_version(NULL);catalog(NULL)
     return()
   }
   pinned<-completed_docx()
   if(isTRUE(pinned$awaiting_selection) &&
      (!identical(as.character(input$project),pinned$project_id) ||
       !identical(as.character(input$version),pinned$from_version))) {
     pinned$awaiting_selection<-FALSE
     completed_docx(pinned)
   }
   # The accepted DOCX was already verified and loaded in saved(). On the
   # ensuing selector notification, restore it instead of clearing it.
   if(is.list(pinned) &&
      identical(as.character(input$project),as.character(pinned$project_id)) &&
      identical(as.character(input$version),as.character(pinned$version_id)) &&
      nzchar(docx_id(pinned$packet))) {
     packet(pinned$packet);packet_version(pinned$version_id);catalog(NULL)
     return(invisible(NULL))
   }
   # Only clear the old selected packet when a replacement has been resolved.
   # A transient selector/request failure must not erase a completed DOCX.
   mv<-tryCatch(request("manuscript_view",args()),error=function(e)NULL)
   if(is.list(mv) && !is.null(mv$evidence_version) && !is.null(mv$stage)) {
     packet(mv);packet_version(as.character(input$version));catalog(NULL)
     return(invisible(NULL))
   }
   x<-tryCatch(request("manuscript_catalog",args()),error=function(e)NULL)
   if(!is.null(x)) {
     packet(NULL);packet_version(NULL);handoff_set(x)
   }
 },ignoreInit=FALSE)
 shiny::observeEvent(input$manuscript_catalog,{
   # This button accepts an Evidence Version. Newly saved Manuscript PLANs
   # are also listed in the shared Version selector, but are not evidence rows.
   # Detect that harmless mix-up without starting a new Evidence review.
   mv<-tryCatch(request("manuscript_view",args()),error=function(e)NULL)
   if(is.list(mv) && !is.null(mv$evidence_version) && !is.null(mv$stage)) {
     notice(paste0("Selected Version is a Manuscript ",mv$stage,
       ". Use 'Load selected manuscript version' to view saved references. ",
       "To reload Evidence sources, select the original Evidence Version first."))
   } else {
     x<-safe(request("manuscript_catalog",args()))
     if(!is.null(x))handoff_set(x)
   }
 })
 # RC28.25.8B: the only routine action on the production panel.
 # It uses the same authenticated Web request API and durable Worker queue;
 # no provider or file operations occur in browser JavaScript.
 shiny::observeEvent(input$manuscript_generate,{
   previous<-production()
   if(!is.null(previous) && previous$state %in% c("PREPARING","QUEUED","RUNNING","ACCEPTING"))return()
   fail<-function(message){
     production(list(state="FAILED",message=message))
     session$sendCustomMessage("srma-manuscript-production",list(state="error",message=message))
   }
   production(list(state="PREPARING",message="Validating source and reference library"))
   plan_id<-NULL
   x<-packet()
   if(!is.null(x) && identical(x$stage,"PLAN")) {
     if(!isTRUE(x$current)) {fail("The selected Plan uses stale evidence. Select a current Evidence Version.");return()}
     plan_id<-input$version
   } else if(!is.null(x) && !is.null(x$docx$artifact_id)) {
      # Versioned regeneration reuses frozen Evidence, approved analysis results,
      # verified references and provenance. No re-entry or PubMed call in PLAN.
      if(!isTRUE(x$current)) {
        fail("This manuscript has stale upstream sources. Select a current Evidence Version; historical results remain preserved.")
        return()
      }
      if(is.null(x$spec) || !is.list(x$spec)) {
        fail("Saved manuscript specification is unavailable. Do not overwrite the existing DOCX.")
        return()
      }
      v<-safe(request("manuscript_plan",list(project_id=input$project,
        version_id=input$version,spec=x$spec)))
      if(is.null(v) || is.null(v$version_id)) {
        fail("Could not create a new immutable manuscript PLAN. Existing DOCX is unchanged; review the App notice.")
        return()
      }
      plan_id<-v$version_id
      refresh(refresh()+1L)
    } else {
     c<-catalog()
     if(is.null(c)) {fail("Select a finalized Evidence Version or an existing Manuscript PLAN.");return()}
     pmids<-unique(as.character(unlist(c$pmids %||b2% list(),use.names=FALSE)))
     pmids<-pmids[!is.na(pmids) & grepl("^[0-9]{1,9}$",pmids)]
     if(!length(pmids)) {
       fail("No included-study PMID candidates are available. A source-verified Manuscript PLAN is required for this Evidence Version.")
       return()
     }
     title<-trimws(as.character(c$title %||b2% input$manuscript_title %||b2% ""))
     if(!nzchar(title)) {fail("The selected Evidence Version does not contain a manuscript title.");return()}
     spec<-list(title=title,language=c$language %||b2% "English",
       analyses=as.list(as.character(c$analyses$version_id)),
       artifacts=as.list(as.character(c$artifacts$id)),
       pmids=as.list(pmids),
       rationale=as.character(c$rationale %||b2%
         "Report source-bound Human-approved scientific evidence. Preserve study-specific endpoints and actual timepoints."))
     v<-safe(request("manuscript_plan",c(args(),list(spec=spec))))
     if(is.null(v) || is.null(v$version_id)) {
       fail("The source plan could not be prepared. See the App notice and Terminal 2 log.")
       return()
     }
     plan_id<-v$version_id
     refresh(refresh()+1L)
   }
   queued<-safe(request("submit",list(project_id=input$project,
     version_id=plan_id,operation="generate_final_manuscript")))
   if(is.null(queued)) {fail("Unable to queue manuscript generation. Review the App notice.");return()}
   jobid<-if(is.character(queued) && length(queued)==1L)queued else
     if(is.list(queued) && !is.null(queued$id))queued$id else
     if(is.list(queued) && !is.null(queued$job_id))queued$job_id else ""
   production(list(state="QUEUED",project_id=input$project,version_id=plan_id,
     job_id=as.character(jobid),message="Waiting for scientific Worker"))
   session$sendCustomMessage("srma-manuscript-production",list(state="running",
      message="New version queued. Source preflight, PRISMA, R/34 assets, drafting, citation QC and DOCX assembly will run in the Worker."))
 })
 # Complete/failed outcomes are based on actual persisted job state, not
 # guessed percentages or a browser timer. Accept once, on the exact version.
 shiny::observe({
   shiny::invalidateLater(2500,session)
   run<-shiny::isolate(production())
   if(is.null(run) || !run$state %in% c("QUEUED","RUNNING"))return()
   p<-tryCatch(request("view",list(project_id=run$project_id)),error=function(e)NULL)
   if(is.null(p) || !is.data.frame(p$jobs) || !nrow(p$jobs))return()
   j<-p$jobs
   j<-j[j$version_id==run$version_id & j$operation==(run$operation %||b2% "generate_final_manuscript"),,drop=FALSE]
   if(nzchar(run$job_id))j<-j[j$id==run$job_id,,drop=FALSE]
   if(!nrow(j))return()
   j<-j[order(j$created_at,decreasing=TRUE),,drop=FALSE]
   state<-as.character(j$state[[1L]])
   if(state %in% c("FAILED","USER_ACTION_REQUIRED","CANCELLED")) {
     phase<-tryCatch(web_c2_8g_progress_read(as.character(j$id[[1L]])),
       error=function(e)NULL)
     # Only the explicit, source-coverage exhausted stage may replace a generic
     # failure message; stale intermediate stages could hide other errors.
     detail<-if(!is.null(phase) &&
       identical(phase$phase,"EVIDENCE_REVIEW_REQUIRED"))
         as.character(phase$message) else as.character(j$message[[1L]])
     msg<-paste("Worker",state,":",detail)
     production(list(state="FAILED",message=msg))
     session$sendCustomMessage("srma-manuscript-production",list(state="error",message=msg))
     return()
   }
   if(identical(state,"COMPLETE")) {
     if(!identical(input$project,run$project_id))return()
     production(list(state="ACCEPTING",message="Registering verified DOCX output"))
     v<-safe(request("manuscript_accept",list(project_id=run$project_id,
       version_id=run$version_id,job_id=as.character(j$id[[1L]]))))
     if(is.null(v)) {
       production(list(state="FAILED",message="Worker completed, but DOCX registration needs operator review."))
       session$sendCustomMessage("srma-manuscript-production",list(state="error",
         message="Worker completed; DOCX registration failed. Do not repeat generation until the job is inspected."))
       return()
     }
     loaded<-saved(v,autoload=TRUE)
     if(!isTRUE(loaded)) {
       production(list(state="FAILED",message=paste0(
         "DOCX was saved as immutable version ",v$version_id,
         ", but could not be loaded automatically. Do not regenerate.")))
       session$sendCustomMessage("srma-manuscript-production",list(state="error",
         message="DOCX saved; automatic version reload failed. Do not restart the AI job."))
       return()
     }
     production(list(state="COMPLETE",version_id=v$version_id,
       project_id=v$project_id,message=paste("New saved Version",v$version_id)))
     session$sendCustomMessage("srma-manuscript-production",list(state="success",
       message="Author-editable DOCX is ready. Download it here without changing versions or refreshing."))
   } else {
     # The Worker writes phase/count snapshots in a private status sidecar.
     # Polling does NOT start new AI requests or create extra Jobs.
     phase<-tryCatch(web_c2_8g_progress_read(as.character(j$id[[1L]])),
       error=function(e)NULL)
     msg<-if(!is.null(phase))as.character(phase$message) else
       as.character(j$message[[1L]])
     production(list(state="RUNNING",project_id=run$project_id,
       version_id=run$version_id,job_id=as.character(j$id[[1L]]),message=msg))
     if(!identical(as.character(run$message),msg))
       session$sendCustomMessage("srma-manuscript-production",
         list(state="running",message=msg))
   }
 })
 output$manuscript_handoff_status<-shiny::renderText({
   x<-catalog()
   if(is.null(x))return("Choose a finalized Evidence Version; approved sources will load automatically.")
   if(identical(x$route,"NARRATIVE"))paste0("Narrative source: ",x$evidence_version,
     " | Human-verified results: ",x$selected_results,
     if(isTRUE(x$unresolved_rob))" | Risk-of-Bias pending" else " | RoB recorded",
     paste0(" | Verified reference metadata: ",length(x$verified_pmids)),
     " | Included-study PMID candidates: ",length(x$candidate_pmids),
     " (verify via R/39 on plan save)",
     if(length(x$conflicting_source_pmids))
       paste0(" | PMID conflicts requiring source check: ",length(x$conflicting_source_pmids))
     else "",
     if(!length(x$pmids))" | No source PMIDs found; reference discovery is still required" else "") else
     "Canonical quantitative sources (existing READY/ROB guards retained)."
 })
 output$manuscript_reference_discovery_status<-shiny::renderText({
   x<-packet()
   if(is.null(x))return("Load a saved manuscript plan to inspect reference discovery and actual citations.")
   d<-x$source$reference_discovery
   if(is.null(d))return("Reference discovery has not been recorded for this manuscript.")
   n<-length(x$source$references$records)
   candidate_n<-length(d$candidates %||b2% list())
   counts<-web_c2_ref_citation_summary(x)
   limits<-x$source$reference_policy
   paste0("Verified library: ",n," | topic-matched candidates: ",candidate_n,
     " | reference handling: automatic PMID/metadata/citation verification (final author review)",
     " | actual unique body citations: ",counts$total,"/",limits$total,
     " (Introduction ",counts$introduction,"/",limits$introduction,
     "; Discussion ",counts$discussion,"/",limits$discussion,
     "). Repeating the same PMID in both sections is permitted; it counts only once overall.")
 })
 shiny::observe({
   x<-packet();shiny::req(x)
   d<-x$source$reference_discovery
   if(is.null(d) || !identical(d$review_status,"HUMAN_SELECTION_REQUIRED")) {
     shiny::updateSelectInput(session,"manuscript_reference_keep",choices=character(),selected=character())
     return()
   }
   candidates<-d$candidates
   ids<-vapply(candidates,function(r)web_c2_handoff_one(r$pmid),character(1))
   labels<-vapply(candidates,function(r)paste0("PMID ",web_c2_handoff_one(r$pmid),
     " | ",web_c2_handoff_one(r$year)," | ",
     substr(web_c2_handoff_one(r$title),1L,110L)),character(1))
   selected<-vapply(x$source$references$records,function(r)
     web_c2_handoff_one(r$pmid),character(1))
   shiny::updateSelectInput(session,"manuscript_reference_keep",
     choices=named_choices(ids,labels),selected=selected)
 })
 output$manuscript_reference_candidates<-shiny::renderTable({
   x<-packet();shiny::req(x)
   d<-x$source$reference_discovery
   if(is.null(d) || !identical(d$review_status,"HUMAN_SELECTION_REQUIRED"))return(NULL)
   rows<-lapply(d$candidates,function(r)list(
     PMID=web_c2_handoff_one(r$pmid),Year=web_c2_handoff_one(r$year),
     Title=web_c2_handoff_one(r$title),
     Score=web_c2_handoff_one(r$web_relevance_score),
     Source=if(web_c2_handoff_one(r$pmid) %in%
       unlist(d$protected_pmids,use.names=FALSE))"Source-study / mandatory" else "Context / selectable"))
   web_b2_table(rows)
 })
 # Separate reference-selection UI is intentionally retired; source and
 # citation validation runs automatically, with author review after DOCX.
 output$manuscript_sources<-shiny::renderTable({x<-catalog();shiny::req(x);x$analyses[,c("version","label"),drop=FALSE]})
 # The browser starts the bar immediately on click, without waiting for R.
 # This handler always sends a terminal state, even when `safe` catches an error.
 # No percentage is shown: R/39 runs synchronously and exposes no measured ETA.
 shiny::observeEvent(input$manuscript_plan,{
   completed<-FALSE
   session$sendCustomMessage("srma-manuscript-reference-progress",list(
     state="running",message="PubMed search, bibliographic verification and manuscript plan save are running."))
   on.exit(session$sendCustomMessage("srma-manuscript-reference-progress",list(
     state=if(isTRUE(completed))"success" else "error",
     message=if(isTRUE(completed))
       "Reference plan saved. Select the new version and load it to review."
     else "Plan was not saved. Check the error notice and Terminal 2 log.")),add=TRUE)
   version<-safe({
     ids<-trimws(strsplit(input$manuscript_pmids,",",fixed=TRUE)[[1]])
     ids<-ids[nzchar(ids)]
     request("manuscript_plan",c(args(),list(spec=list(
       title=input$manuscript_title,language=input$manuscript_language,
       analyses=as.list(input$manuscript_analyses),artifacts=as.list(input$manuscript_artifacts),
       pmids=as.list(ids),rationale=input$manuscript_rationale))))
   })
   if(!is.null(version)) {
     completed<-isTRUE(safe({ saved(version); TRUE }))
   }
 })
 shiny::observeEvent(input$manuscript_load,{
   x<-safe(request("manuscript_view",args()))
   if(is.list(x)){
     packet(x);packet_version(as.character(input$version));catalog(NULL)
   }
 })
 output$manuscript_status<-shiny::renderText({x<-packet();if(is.null(x))return("Load a saved manuscript version.");paste(x$stage,if(isTRUE(x$current))"Current sources" else "HISTORICAL — upstream changed","; author review is not final release")})
 output$manuscript_plan_summary<-shiny::renderText({x<-packet();shiny::req(x);as.character(jsonlite::toJSON(list(spec=x$spec,evidence=x$evidence_version,sections=web_c2_sections(),source_hash=x$source_sha256),auto_unbox=TRUE,pretty=TRUE))})
 output$manuscript_references<-shiny::renderTable({x<-packet();shiny::req(x);web_b2_table(lapply(x$source$references$records,function(r)r[c("pmid","title","year","doi")]))})
 shiny::observeEvent(input$manuscript_begin,saved(safe(request("manuscript_begin",args()))))
 shiny::observe({x<-packet();k<-input$manuscript_section;shiny::req(x,k)
   text<-if(identical(k,"Methods") && identical(x$stage,"PLAN"))web_stat_methods_editor_text(x) else x$sections[[k]]$text %||b2% ""
   shiny::updateTextAreaInput(session,"manuscript_text",value=text)
   shiny::updateSelectInput(session,"manuscript_insert_artifact",choices=named_choices(names(x$source$artifacts),web_c2_artifact_labels(x)))
   f<-x$source$facts
   shiny::updateSelectInput(session,"manuscript_fact",choices=named_choices(names(f),vapply(names(f),function(n)paste(n,f[[n]]$rendered),character(1))))
 })
 output$manuscript_section_status<-shiny::renderText({
   x<-packet();k<-input$manuscript_section;shiny::req(x,k)
   section<-x$sections[[k]]
   if(is.null(section) && identical(x$stage,"PLAN") && identical(input$manuscript_section,"Methods"))
     return(if(identical(x$source$statistical_methods$status,"PASS"))"Statistical methods are ready for wording edits." else
       "Statistical methods evidence is incomplete. Review the approved protocol and analysis records.")
   if(is.null(section))return("No editable sections yet. Generate the complete Word draft for final author review.")
   paste(section$status,section$source,section$actor,section$at)
 })
 shiny::observeEvent(input$manuscript_insert_fact,{shiny::req(input$manuscript_fact);shiny::updateTextAreaInput(session,"manuscript_text",value=paste(input$manuscript_text,paste0("{{FACT:",input$manuscript_fact,"}}")))})
 shiny::observeEvent(input$manuscript_artifact_token,{shiny::req(input$manuscript_insert_artifact);shiny::updateTextAreaInput(session,"manuscript_text",value=paste(input$manuscript_text,paste0("{{ARTIFACT:",input$manuscript_insert_artifact,"}}")))})
 shiny::observeEvent(input$manuscript_edit,saved(safe(request("manuscript_edit",c(args(),list(section=input$manuscript_section,text=input$manuscript_text,decision=input$manuscript_decision,rationale=input$manuscript_edit_reason))))))
 shiny::observeEvent(input$manuscript_ai,safe({v<-request("manuscript_prepare_ai",c(args(),list(section=input$manuscript_section)));request("submit",c(v,list(operation="generate_manuscript_section")));saved(v)}))
 output$manuscript_precheck<-shiny::renderText({x<-packet();shiny::req(x);paste(c(x$precheck$label,x$precheck$status,unlist(x$precheck$issues)),collapse="\n")})
 output$manuscript_preview<-shiny::renderText({x<-packet();shiny::req(x);x$preview})
 shiny::observeEvent(input$manuscript_docx,safe({request("submit",c(args(),list(operation="generate_manuscript_docx")));notice("Word generation queued. The worker uses the exact selected version.")}))
 shiny::observe({shiny::invalidateLater(2000,session);shiny::req(packet());x<-safe(request("view",list(project_id=input$project)));shiny::req(x);j<-x$jobs[x$jobs$version_id==input$version&x$jobs$operation %in% names(web_c2_operations())&x$jobs$state=="COMPLETE",,drop=FALSE];shiny::updateSelectInput(session,"manuscript_job",choices=named_choices(j$id,paste(j$operation,j$created_at)),selected=shiny::isolate(input$manuscript_job))})
 shiny::observeEvent(input$manuscript_accept,saved(safe(request("manuscript_accept",c(args(),list(job_id=input$manuscript_job))))))
 output$manuscript_download<-shiny::downloadHandler(
   filename=function(){
     d<-shiny::isolate(download_source())
     if(!is.null(d) && grepl("^Version [0-9]+$",d$label))
       paste0("manuscript_",gsub(" ","_",d$label),".docx") else "manuscript.docx"
   },
   content=function(file){
     d<-shiny::isolate(download_source())
     shiny::req(is.list(d),nzchar(d$artifact_id))
     # Download the verified stored artifact, not the last generated PLAN.
     # Some request implementations return NULL after successfully writing a
     # destination, so validate the file rather than the return value.
     safe(request("download",list(project_id=d$project_id,
       artifact_id=d$artifact_id,destination=file)))
     if(!file.exists(file) || is.na(file.info(file)$size) ||
        file.info(file)$size<=0L)
       stop("Registered DOCX download did not produce a file; check App log.",
         call.=FALSE)
   })
 shiny::observeEvent(input$manuscript_approve,{
   v<-safe(request("manuscript_approve",c(args(),list(confirmed=isTRUE(input$manuscript_confirm),rationale=input$manuscript_review_reason))))
   if(is.list(v))saved(v,autoload=TRUE)
 })
}
