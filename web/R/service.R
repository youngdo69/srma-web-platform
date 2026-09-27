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
web_import_project <- function(con, settings, actor, workspace_id, source_root) {
  web_assert(nrow(web_query(con, "SELECT user_id FROM workspace_members WHERE workspace_id=?w AND user_id=?u", w = workspace_id, u = actor)) == 1L,
    "This workspace is not accessible.")
  info <- web_engine_inspect(settings, source_root)
  version_id <- web_id()
  relative <- file.path("snapshots", version_id)
  target <- file.path(settings$data_root, relative)
  dir.create(target, recursive = TRUE, mode = "0700")
  # Only canonical config + declared inputs enter an immutable version. Never copy .env.
  for (p in unlist(info$inputs, use.names = FALSE)) {
    source <- web_inside(file.path(source_root, p), source_root)
    dest <- web_inside(file.path(target, p), target, exists = FALSE)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    web_assert(file.copy(source, dest), "Project inputs could not be saved.", "SYSTEM_FAILED")
    web_assert(identical(web_sha(dest), info$manifest[[p]]), "Project input changed during import; import it again.")
  }
  web_tx(con, function() {
    existing <- web_query(con, "SELECT * FROM projects WHERE workspace_id=?w AND canonical_id=?c", w = workspace_id, c = info$project_id)
    if (!nrow(existing)) {
      project_id <- web_id()
      web_exec(con, "INSERT INTO projects VALUES(?id,?w,?c,?t,?now)", id = project_id,
        w = workspace_id, c = info$project_id, t = info$title, now = web_now())
    } else project_id <- existing$id
    # Serialize version allocation on PostgreSQL as well as SQLite BEGIN IMMEDIATE.
    web_exec(con, "UPDATE projects SET title=title WHERE id=?p", p = project_id)
    n <- web_query(con, "SELECT COALESCE(MAX(version),0)+1 AS n FROM project_versions WHERE project_id=?p", p = project_id)$n
    web_exec(con, "INSERT INTO project_versions VALUES(?id,?p,?n,?hash,?locator,?manifest,?now)",
      id = version_id, p = project_id, n = n, hash = info$manifest[["config/project_config.yml"]],
      locator = relative, manifest = as.character(web_json(info$manifest)), now = web_now())
    project <- web_authorize(con, actor, project_id)
    web_audit(con, project, "PROJECT_VERSION_IMPORTED", version_id, actor, list(version = n))
    list(project_id = project_id, version_id = version_id)
  })
}
web_submit <- function(con, settings, actor, project_id, version_id, operation = "validate_search", idempotency_key = web_id(), payload = list()) {
  project <- web_authorize(con, actor, project_id)
  reg <- web_registry()[[operation]]
  web_assert(!is.null(reg) && isTRUE(reg$enabled) && length(payload) == 0L, "This operation is not enabled in this release.")
  web_assert(is.character(idempotency_key) && length(idempotency_key) == 1L && nzchar(idempotency_key) && nchar(idempotency_key) <= 128L, "Invalid request identifier.")
  engine <- web_register_engine(con, settings)
  web_tx(con, function() {
    web_exec(con, "UPDATE projects SET title=title WHERE id=?p", p = project_id)
    web_assert(nrow(web_query(con, "SELECT id FROM project_versions WHERE id=?v AND project_id=?p", v = version_id, p = project_id)) == 1L,
      "This project version is not available.")
    existing <- web_query(con, "SELECT id FROM jobs WHERE project_id=?p AND version_id=?v AND operation=?o AND (idempotency_key=?k OR state IN ('QUEUED','RUNNING')) ORDER BY created_at LIMIT 1",
      p = project_id, v = version_id, o = operation, k = idempotency_key)
    if (nrow(existing)) return(existing$id)
    blocked <- web_gate_open(con, project_id, version_id, operation)
    state <- if (blocked) "USER_ACTION_REQUIRED" else "QUEUED"
    id <- web_id()
    web_exec(con, "INSERT INTO jobs(id,project_id,version_id,engine_id,operation,idempotency_key,state,message,progress,created_at,updated_at) VALUES(?id,?p,?v,?e,?o,?k,?s,?m,0,?now,?now)",
      id = id, p = project_id, v = version_id, e = engine, o = operation, k = idempotency_key, s = state,
      m = if (blocked) "Human review is required before this step can continue." else "Waiting to check search strategies…", now = web_now())
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
web_view <- function(con, actor, project_id) {
  p <- web_authorize(con, actor, project_id)
  list(project = p[, c("id", "title"), drop = FALSE],
    versions = web_query(con, "SELECT id,version,created_at FROM project_versions WHERE project_id=?p ORDER BY version DESC", p = project_id),
    jobs = web_query(con, "SELECT id,state,message,progress,created_at FROM jobs WHERE project_id=?p ORDER BY created_at DESC", p = project_id),
    artifacts = web_query(con, "SELECT id,logical_name,version,approval_state,created_at FROM artifacts WHERE project_id=?p AND status='AVAILABLE' ORDER BY created_at DESC", p = project_id))
}
