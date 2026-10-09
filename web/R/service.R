web_register_engine <- function(con, settings) {
  baseline <- jsonlite::read_json(file.path(settings$engine_root, "web/docs/engine-baseline.json"))
  manifest <- jsonlite::read_json(file.path(settings$engine_root, "web/docs/engine-source-manifest.json"))
  for (p in names(manifest)) web_assert(identical(web_sha(file.path(settings$engine_root, p)), manifest[[p]]),
      "The scientific source differs from the registered baseline.", "SYSTEM_FAILED")
  hash <- digest::digest(as.character(web_json(manifest)), algo = "sha256", serialize = FALSE)
  id <- paste0("snapshot-", substr(hash, 1, 16))
  if (!nrow(web_query(con, "SELECT id FROM engine_releases WHERE id=?id", id = id)))
    web_exec(con, "INSERT INTO engine_releases VALUES(?id,?commit,?hash,?manifest,?now)",
      id = id, commit = baseline$commit, hash = hash, manifest = as.character(web_json(manifest)), now = web_now())
  id
}
web_local_identity <- function(con) {
  # Explicit development bootstrap only. Never called by a browser request.
  web_tx(con, function() {
    if (!nrow(DBI::dbGetQuery(con, "SELECT id FROM app_users WHERE id='local-researcher'")))
      DBI::dbExecute(con, "INSERT INTO app_users VALUES('local-researcher','Local researcher')")
    if (!nrow(DBI::dbGetQuery(con, "SELECT id FROM workspaces WHERE id='local-workspace'")))
      DBI::dbExecute(con, "INSERT INTO workspaces VALUES('local-workspace','Local workspace')")
    if (!nrow(DBI::dbGetQuery(con, "SELECT user_id FROM workspace_members WHERE workspace_id='local-workspace' AND user_id='local-researcher'")))
      DBI::dbExecute(con, "INSERT INTO workspace_members VALUES('local-workspace','local-researcher')")
  })
}

web_create_seed_review <- function(con, settings, actor, workspace_id, topic, title = "", canonical_id) {
  web_assert(
    nrow(web_query(
      con,
      "SELECT user_id FROM workspace_members WHERE workspace_id=?w AND user_id=?u",
      w = workspace_id,
      u = actor
    )) == 1L,
    "This workspace is not accessible."
  )

  web_assert(
    is.character(topic) && length(topic) == 1L && !is.na(topic) &&
      nzchar(trimws(topic)) && nchar(topic) <= 10000L,
    "Enter the research topic or question before creating the review."
  )
  web_assert(
    is.character(title) && length(title) == 1L && !is.na(title) &&
      nchar(title) <= 1000L,
    "The optional review title is too long."
  )
  web_assert(
    is.character(canonical_id) && length(canonical_id) == 1L &&
      !is.na(canonical_id) && grepl("^[A-Za-z0-9_-]+$", canonical_id) &&
      nchar(canonical_id) <= 128L,
    "Use only letters, numbers, hyphens or underscores for the project identifier."
  )

  topic <- trimws(topic)
  title <- trimws(title)
  canonical_id <- trimws(canonical_id)
  display_title <- if (nzchar(title)) title else topic
  engine <- web_register_engine(con, settings)

  existing <- web_query(
    con,
    "SELECT id,deleted_at FROM projects WHERE workspace_id=?w AND canonical_id=?c",
    w = workspace_id,
    c = canonical_id
  )
  web_assert(
    !nrow(existing),
    "This project identifier is already in use. Use a different identifier."
  )

  project_id <- web_id()
  version_id <- web_id()
  relative <- file.path("snapshots", version_id)
  target <- file.path(settings$data_root, relative)
  seed_file <- file.path(target, "config", "project_seed.yml")

  dir.create(dirname(seed_file), recursive = TRUE, showWarnings = FALSE, mode = "0700")
  seed_fields <- list(
    topic = topic,
    project_id = canonical_id
  )
  if (nzchar(title)) seed_fields$title <- title
  yaml::write_yaml(list(project_seed = seed_fields), seed_file)
  web_no_file_secrets(seed_file, settings$engine_root)
  seed_hash <- web_sha(seed_file)
  manifest <- setNames(list(seed_hash), "config/project_seed.yml")

  web_tx(con, function() {
    web_exec(
      con,
      "INSERT INTO projects(id,workspace_id,canonical_id,title,created_at) VALUES(?id,?w,?c,?t,?now)",
      id = project_id,
      w = workspace_id,
      c = canonical_id,
      t = display_title,
      now = web_now()
    )
    web_exec(
      con,
      "INSERT INTO project_versions VALUES(?id,?p,1,?hash,?locator,?manifest,?now)",
      id = version_id,
      p = project_id,
      hash = seed_hash,
      locator = relative,
      manifest = as.character(web_json(manifest)),
      now = web_now()
    )
    project <- web_authorize(con, actor, project_id)
    web_audit(
      con,
      project,
      "PROJECT_SEED_CREATED",
      version_id,
      actor,
      list(
        engine_release = engine,
        source = "CANONICAL_PROJECT_SEED",
        topic_sha256 = digest::digest(topic, algo = "sha256", serialize = FALSE)
      )
    )
    list(project_id = project_id, version_id = version_id)
  })
}
web_import_project <- function(con, settings, actor, workspace_id, source_root, configuration_only = FALSE, draft_gates = FALSE, transaction = web_tx) {
  web_assert(nrow(web_query(con, "SELECT user_id FROM workspace_members WHERE workspace_id=?w AND user_id=?u", w = workspace_id, u = actor)) == 1L,
    "This workspace is not accessible.")
  engine <- web_register_engine(con, settings)
  info <- web_engine_inspect(settings, source_root, configuration_only)
  version_id <- web_id()
  relative <- file.path("snapshots", version_id)
  target <- file.path(settings$data_root, relative)
  dir.create(target, recursive = TRUE, mode = "0700")
  # Only canonical config + declared inputs enter an immutable version. Never copy .env.
  for (p in unlist(info$inputs, use.names = FALSE)) {
    source <- web_inside(file.path(source_root, p), source_root)
    web_no_file_secrets(source,settings$engine_root)
    dest <- web_inside(file.path(target, p), target, exists = FALSE)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    web_assert(file.copy(source, dest), "Project inputs could not be saved.", "SYSTEM_FAILED")
    web_assert(identical(web_sha(dest), info$manifest[[p]]), "Project input changed during import; import it again.")
  }
  transaction(con, function() {
    existing <- web_query(con, "SELECT * FROM projects WHERE workspace_id=?w AND canonical_id=?c", w = workspace_id, c = info$project_id)
    if (nrow(existing) && !is.na(existing$deleted_at[[1L]]) && nzchar(existing$deleted_at[[1L]])) {
      web_error("VALIDATION_FAILED", "This project identifier belongs to a deleted review. Use a new project identifier for a new review.")
    }
    if (!nrow(existing)) {
      project_id <- web_id()
      web_exec(con, "INSERT INTO projects(id,workspace_id,canonical_id,title,created_at) VALUES(?id,?w,?c,?t,?now)", id = project_id,
        w = workspace_id, c = info$project_id, t = info$title, now = web_now())
    } else project_id <- existing$id
    # Serialize version allocation on PostgreSQL as well as SQLite BEGIN IMMEDIATE.
    web_exec(con, "UPDATE projects SET title=?t WHERE id=?p", t = info$title, p = project_id)
    n <- web_query(con, "SELECT COALESCE(MAX(version),0)+1 AS n FROM project_versions WHERE project_id=?p", p = project_id)$n
    web_exec(con, "INSERT INTO project_versions VALUES(?id,?p,?n,?hash,?locator,?manifest,?now)",
      id = version_id, p = project_id, n = n, hash = info$manifest[["config/project_config.yml"]],
      locator = relative, manifest = as.character(web_json(info$manifest)), now = web_now())
    project <- web_authorize(con, actor, project_id)
    web_audit(con, project, "PROJECT_VERSION_IMPORTED", version_id, actor, list(version = n, engine_release = engine))
    if (draft_gates) for (op in c("generate_search", "retrieve_pubmed"))
      for (kind in c("protocol_registration", "configuration_human_review")) {
        gate <- web_id()
        web_exec(con, "INSERT INTO gates(id,project_id,workspace_id,version_id,gate_type,required_operation,status,created_at) VALUES(?id,?p,?w,?v,?k,?o,'PENDING',?now)",
          id = gate, p = project_id, w = workspace_id, v = version_id, k = kind, o = op, now = web_now())
        web_audit(con, project, "GATE_REQUIRED", gate, actor)
      }
    list(project_id = project_id, version_id = version_id)
  })
}
web_submit <- function(con, settings, actor, project_id, version_id, operation = "validate_search", idempotency_key = web_id(), payload = list()) {
  project <- web_authorize(con, actor, project_id)
  reg <- web_registry()[[operation]]
  web_b2_guard(con,settings,actor,project_id,version_id,operation)
  web_c1_guard(con,settings,actor,project_id,version_id,operation)
  web_c2_guard(con,settings,actor,project_id,version_id,operation)
  web_c3_guard(con,settings,actor,project_id,version_id,operation)
  web_pipeline_prerequisites(settings, operation)
  if (identical(operation, "prepare_prospero")) {
    pending_config <- web_query(con,
      "SELECT id FROM gates WHERE project_id=?p AND version_id=?v AND gate_type='configuration_human_review' AND status<>'APPROVED'",
      p = project_id, v = version_id)
    web_assert(!nrow(pending_config),
      "Complete the review-configuration Human Gate before preparing the PROSPERO registration.",
      "USER_ACTION_REQUIRED")
  }
  web_require_activated(con,project_id,version_id,operation)
  web_assert(!is.null(reg) && length(payload) == 0L, "This operation is not enabled in this release.")
  web_assert(isTRUE(reg$enabled), if (!is.null(reg$blocked_reason)) reg$blocked_reason else "This operation is not enabled in this release.")
  web_assert(is.character(idempotency_key) && length(idempotency_key) == 1L && nzchar(idempotency_key) && nchar(idempotency_key) <= 128L, "Invalid request identifier.")
  if (operation == "validate_database_inputs") web_science_settings(settings)
  engine <- web_register_engine(con, settings)
  web_tx(con, function() {
    web_exec(con, "UPDATE projects SET title=title WHERE id=?p", p = project_id)
    web_assert(nrow(web_query(con, "SELECT id FROM project_versions WHERE id=?v AND project_id=?p", v = version_id, p = project_id)) == 1L,
      "This project version is not available.")
    existing <- web_query(con, "SELECT id FROM jobs WHERE project_id=?p AND version_id=?v AND operation=?o AND (idempotency_key=?k OR state IN ('QUEUED','RUNNING')) ORDER BY created_at LIMIT 1",
      p = project_id, v = version_id, o = operation, k = idempotency_key)
    if (nrow(existing)) return(existing$id)
    sources <- web_pipeline_sources(con,project_id,version_id,operation)
    blocked <- web_gate_open(con, project_id, version_id, operation)
    state <- if (blocked) "USER_ACTION_REQUIRED" else "QUEUED"
    id <- web_id()
    web_exec(con, "INSERT INTO jobs(id,project_id,version_id,engine_id,operation,idempotency_key,state,message,progress,created_at,updated_at) VALUES(?id,?p,?v,?e,?o,?k,?s,?m,0,?now,?now)",
      id = id, p = project_id, v = version_id, e = engine, o = operation, k = idempotency_key, s = state,
      m = if (blocked) "Human review is required before this step can continue." else paste0("Waiting: ", reg$label, "…"), now = web_now())
    for(source in sources) web_exec(con,"INSERT INTO job_sources VALUES(?j,?p,?v,?s)",j=id,p=project_id,v=version_id,s=source)
    web_audit(con, project, paste0("JOB_", state), id, actor)
    id
  })
}
web_require_gate <- function(con, actor, project_id, version_id, gate_type, operation, artifact_id = NA_character_) {
  project <- web_authorize(con, actor, project_id)
  web_tx(con, function() {
    web_exec(con, "UPDATE projects SET title=title WHERE id=?p", p = project_id)
    web_assert(!nrow(web_query(con, "SELECT id FROM jobs WHERE project_id=?p AND state='RUNNING'", p = project_id)),
      "Wait for the current check to finish before recording a new review requirement.")
    id <- web_id()
    web_exec(con, "INSERT INTO gates(id,project_id,workspace_id,version_id,gate_type,required_operation,status,artifact_id,created_at) VALUES(?id,?p,?w,?v,?g,?o,'PENDING',?a,?now)",
      id = id, p = project_id, w = project$workspace_id, v = version_id, g = gate_type, o = operation, a = artifact_id, now = web_now())
    web_audit(con, project, "GATE_REQUIRED", id, actor)
    id
  })
}
web_decide_gate <- function(con, actor, project_id, gate_id, decision, rationale = "") {
  project <- web_authorize(con, actor, project_id)
  web_assert(decision %in% c("APPROVED", "REJECTED"), "A human review decision is required.")
  gate<-web_query(con,"SELECT gate_type FROM gates WHERE id=?g AND project_id=?p",g=gate_id,p=project_id)
  if(nrow(gate)&&startsWith(gate$gate_type,"b2_"))web_assert(is.character(rationale)&&length(rationale)==1L&&!is.na(rationale)&&nzchar(trimws(rationale)),"Provide a rationale for this explicit evidence approval.")
  if(nrow(gate)&&gate$gate_type %in% c("analysis_plan","analysis_results")) {
    a<-web_query(con,"SELECT a.* FROM analysis_versions a JOIN gates g ON g.version_id=a.version_id WHERE g.id=?g AND a.project_id=?p",g=gate_id,p=project_id)
    web_assert(nrow(a)==1L&&nzchar(trimws(rationale)),"Explicit analysis review rationale is required.")
    web_c1_current(con,project_id,a$evidence_version_id)
    web_assert(a$stage==if(gate$gate_type=="analysis_plan")"PLAN" else "RESULT","This gate does not match the analysis stage.")
  }
  if(nrow(gate)&&gate$gate_type %in% c("manuscript_plan","manuscript_review")) {
    m<-web_query(con,"SELECT m.* FROM manuscript_versions m JOIN gates g ON g.version_id=m.version_id WHERE g.id=?g AND m.project_id=?p",g=gate_id,p=project_id)
    web_assert(nrow(m)==1&&nzchar(trimws(rationale)),"Explicit manuscript review rationale required.")
    # Full source guards run in prepare/approve; direct gate action never creates APPROVED manuscripts.
    web_c1_current(con,project_id,m$evidence_version_id)
  }
  web_assert(gate$gate_type!="final_qc","Use the Final QC review screen; generic gate approval cannot bypass checks.")
  web_tx(con, function() {
    count <- web_exec(con, "UPDATE gates SET status=?d,decision=?d,reviewer_id=?u,decided_at=?now,rationale=?r WHERE id=?g AND project_id=?p AND status='PENDING'",
      d = decision, u = actor, now = web_now(), r = rationale, g = gate_id, p = project_id)
    web_assert(count == 1L, "This review is unavailable or already decided.")
    web_audit(con, project, "GATE_DECIDED", gate_id, actor, list(decision = decision, rationale = rationale))
  })
}
web_retry <- function(con, settings, actor, project_id, job_id) {
  project <- web_authorize(con, actor, project_id)
  web_tx(con, function() {
    web_exec(con, "UPDATE projects SET title=title WHERE id=?p", p = project_id)
    j <- web_query(con, "SELECT * FROM jobs WHERE id=?id AND project_id=?p", id = job_id, p = project_id)
    web_assert(nrow(j) == 1L && j$state %in% c("FAILED", "USER_ACTION_REQUIRED"), "This request cannot be retried.")
    web_assert(isTRUE(web_registry()[[j$operation]]$replay_safe) && j$attempts < settings$max_attempts,
      "Import a corrected version or ask the operator to inspect this request.")
    if (j$state == "FAILED") {
      result <- web_parse(j$result_json)
      web_assert(result$error_code %in% c("SYSTEM_FAILED", "DEPENDENCY_FAILED"), "Import corrected inputs as a new version before running this check again.")
    }
    web_assert(!web_gate_open(con, project_id, j$version_id, j$operation), "Human review is still required.", "USER_ACTION_REQUIRED")
    web_exec(con, "UPDATE jobs SET state='QUEUED',progress=0,message='Waiting to retry search checks…',worker_token=NULL,updated_at=?now WHERE id=?id", now = web_now(), id = job_id)
    web_audit(con, project, "JOB_RETRY_REQUESTED", job_id, actor)
  })
}
web_cancel <- function(con, actor, project_id, job_id) {
  project <- web_authorize(con, actor, project_id)
  web_tx(con, function() {
    count <- web_exec(con, "UPDATE jobs SET state='CANCELLED',message='Request cancelled.',updated_at=?now WHERE id=?id AND project_id=?p AND state='QUEUED'", now = web_now(), id = job_id, p = project_id)
    web_assert(count == 1L, "Only a waiting request can be cancelled.")
    web_audit(con, project, "JOB_CANCELLED", job_id, actor)
  })
}
web_prospero_status <- function(con, actor, project_id, version_id = NULL) {
  web_authorize(con, actor, project_id)
  if (is.null(version_id) || !is.character(version_id) || length(version_id) != 1L || !nzchar(version_id)) {
    return(list(state = "NOT_STARTED", message = "PROSPERO preparation has not been started for this saved version."))
  }
  job <- web_query(con,
    "SELECT id,state,message,progress,result_json,created_at FROM jobs WHERE project_id=?p AND version_id=?v AND operation='prepare_prospero' ORDER BY created_at DESC LIMIT 1",
    p = project_id, v = version_id)
  if (!nrow(job)) return(list(state = "NOT_STARTED", message = "PROSPERO preparation has not been started for this saved version."))
  result <- if (!is.na(job$result_json[[1]]) && nzchar(job$result_json[[1]])) tryCatch(web_parse(job$result_json[[1]]), error = function(e) NULL) else NULL
  artifacts <- web_query(con,
    "SELECT id,logical_name,artifact_type,created_at FROM artifacts WHERE project_id=?p AND version_id=?v AND job_id=?j AND status='AVAILABLE' ORDER BY created_at",
    p = project_id, v = version_id, j = job$id[[1]])
  draft <- artifacts[artifacts$artifact_type == "prospero_draft", , drop = FALSE]
  provenance <- if (is.list(result) && is.list(result$provenance)) result$provenance$prospero else NULL
  list(
    job_id = job$id[[1]], state = job$state[[1]], message = job$message[[1]],
    progress = job$progress[[1]], created_at = job$created_at[[1]],
    counts = if (is.list(provenance)) provenance$counts else NULL,
    user_required = if (is.list(provenance)) provenance$user_required else NULL,
    llm_status = if (is.list(provenance)) provenance$llm_status else NULL,
    draft_artifact_id = if (nrow(draft)) draft$id[[1]] else NULL,
    draft_name = if (nrow(draft)) draft$logical_name[[1]] else NULL
  )
}

web_delete_project <- function(con, actor, project_id, confirmation) {
  project <- web_authorize(con, actor, project_id)
  web_assert(is.character(confirmation) && length(confirmation) == 1L && identical(trimws(confirmation), "DELETE"),
    "Type DELETE exactly to confirm deletion of this review.")
  active <- web_query(con,
    "SELECT id,state FROM jobs WHERE project_id=?p AND state IN ('QUEUED','RUNNING') LIMIT 1",
    p = project_id)
  web_assert(!nrow(active),
    "Wait for the current background task to finish before deleting this review.",
    "USER_ACTION_REQUIRED")
  web_tx(con, function() {
    current <- web_query(con, "SELECT id,title,canonical_id,deleted_at FROM projects WHERE id=?p", p = project_id)
    web_assert(nrow(current) == 1L && (is.na(current$deleted_at[[1L]]) || !nzchar(current$deleted_at[[1L]])),
      "This review is already deleted or unavailable.")
    web_audit(con, project, "PROJECT_DELETED", project_id, actor,
      list(title = current$title[[1L]], canonical_id = current$canonical_id[[1L]], retention = "audit_preserved"))
    count <- web_exec(con,
      "UPDATE projects SET deleted_at=?now,deleted_by=?u WHERE id=?p AND deleted_at IS NULL",
      now = web_now(), u = actor, p = project_id)
    web_assert(count == 1L, "This review is already deleted or unavailable.")
    list(project_id = project_id, title = current$title[[1L]], deleted = TRUE)
  })
}

web_view <- function(con, actor, project_id) {
  p <- web_authorize(con, actor, project_id)
  versions <- web_query(con, paste0(
    "SELECT pv.id,pv.version,pv.created_at,",
    "CASE WHEN pv.manifest_json LIKE '%config/project_seed.yml%' ",
    "AND pv.manifest_json NOT LIKE '%config/project_config.yml%' THEN 'SEED' ELSE 'CANONICAL' END AS kind,",
    "COALESCE(vo.kind,'') AS origin_kind ",
    "FROM project_versions pv LEFT JOIN version_origins vo ON vo.version_id=pv.id ",
    "WHERE pv.project_id=?p ORDER BY pv.version DESC"
  ), p = project_id)
  jobs <- web_query(con, "SELECT id,version_id,operation,state,message,progress,created_at FROM jobs WHERE project_id=?p ORDER BY created_at DESC", p = project_id)
  prep_versions <- unique(jobs$version_id[jobs$operation == "prepare_prospero"])
  prep_versions <- prep_versions[!is.na(prep_versions) & nzchar(prep_versions)]
  prospero <- if (length(prep_versions)) setNames(
    lapply(prep_versions, function(v) web_prospero_status(con, actor, project_id, v)),
    prep_versions
  ) else list()
  list(project = p[, c("id", "title"), drop = FALSE],
    gates = web_query(con, paste0(
      "SELECT g.id,g.gate_type,g.required_operation,g.status,g.decision,g.version_id,",
      "g.reviewer_id,u.display_name AS reviewer_name,g.decided_at,g.rationale,g.created_at ",
      "FROM gates g LEFT JOIN app_users u ON u.id=g.reviewer_id ",
      "WHERE g.project_id=?p ORDER BY g.created_at DESC"), p = project_id),
    versions = versions,
    jobs = jobs,
    artifacts = web_query(con, "SELECT id,logical_name,version,approval_state,created_at FROM artifacts WHERE project_id=?p AND status='AVAILABLE' ORDER BY created_at DESC", p = project_id),
    prospero = prospero)
}
