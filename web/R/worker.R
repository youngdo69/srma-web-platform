web_claim <- function(con) {
  web_tx(con, function() {
    candidates <- DBI::dbGetQuery(con, "SELECT j.* FROM jobs j WHERE j.state='QUEUED' AND NOT EXISTS(SELECT 1 FROM jobs r WHERE r.project_id=j.project_id AND r.state='RUNNING') ORDER BY j.created_at LIMIT 1")
    if (!nrow(candidates)) return(NULL)
    j <- candidates[1, , drop = FALSE]
    web_exec(con, "UPDATE projects SET title=title WHERE id=?p", p = j$project_id)
    p <- web_query(con, "SELECT * FROM projects WHERE id=?p", p = j$project_id)
    if (web_gate_open(con, j$project_id, j$version_id, j$operation)) {
      changed <- web_exec(con, "UPDATE jobs SET state='USER_ACTION_REQUIRED',message='Human review is required before this step can continue.',updated_at=?now WHERE id=?id AND state='QUEUED'", now = web_now(), id = j$id)
      if (changed) web_audit(con, p, "JOB_USER_ACTION_REQUIRED", j$id)
      return(NULL)
    }
    token <- web_id()
    changed <- web_exec(con, "UPDATE jobs SET state='RUNNING',attempts=attempts+1,worker_token=?token,heartbeat=?time,progress=10,message='Preparing project inputs…',updated_at=?now WHERE id=?id AND state='QUEUED' AND NOT EXISTS(SELECT 1 FROM jobs r WHERE r.project_id=?p AND r.state='RUNNING')",
      token = token, time = as.numeric(Sys.time()), now = web_now(), id = j$id, p = j$project_id)
    if (changed != 1L) return(NULL)
    web_audit(con, p, "JOB_RUNNING", j$id, detail = list(worker_token = token))
    web_query(con, "SELECT * FROM jobs WHERE id=?id", id = j$id)
  })
}
web_worker_once <- function(con, settings) {
  j <- web_claim(con)
  if (is.null(j)) return(invisible(NULL))
  p <- web_query(con, "SELECT * FROM projects WHERE id=?p", p = j$project_id)
  relative <- file.path("jobs", j$id, j$worker_token)
  attempt <- file.path(settings$data_root, relative)
  work <- file.path(attempt, "project")
  dir.create(work, recursive = TRUE, mode = "0700")
  web_exec(con, "UPDATE jobs SET diagnostic_locator=?loc WHERE id=?id AND worker_token=?token", loc = relative, id = j$id, token = j$worker_token)
  sources <- data.frame(source_job_id=character())
  result <- tryCatch({
    web_assert(identical(web_register_engine(con, settings), j$engine_id), "The registered engine is unavailable.", "SYSTEM_FAILED")
    v <- web_query(con, "SELECT * FROM project_versions WHERE id=?v AND project_id=?p", v = j$version_id, p = j$project_id)
    manifest <- web_parse(v$manifest_json)
    snapshot <- web_inside(file.path(settings$data_root, v$snapshot_locator), settings$data_root)
    for (name in names(manifest)) {
      source <- web_inside(file.path(snapshot, name), snapshot)
      web_assert(identical(web_sha(source), manifest[[name]]), "Saved project inputs failed integrity verification.")
      dest <- web_inside(file.path(work, name), work, exists = FALSE)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      web_assert(file.copy(source, dest), "Could not prepare project inputs.", "SYSTEM_FAILED")
    }
    sources <- web_query(con,"SELECT source_job_id FROM job_sources WHERE job_id=?j AND project_id=?p AND version_id=?v",j=j$id,p=j$project_id,v=j$version_id)
    owner <- web_query(con,"SELECT user_id FROM workspace_members WHERE workspace_id=?w LIMIT 1",w=p$workspace_id)$user_id
    for(source in sources$source_job_id) web_overlay_job(con,settings,owner,j$project_id,j$version_id,source,work)
    web_b2_guard(con,settings,owner,j$project_id,j$version_id,j$operation)
    web_c1_guard(con,settings,owner,j$project_id,j$version_id,j$operation)
  web_c2_guard(con,settings,owner,j$project_id,j$version_id,j$operation)
    web_require_activated(con,j$project_id,j$version_id,j$operation)
    web_exec(con, "UPDATE jobs SET progress=30,message=?message,heartbeat=?time,updated_at=?now WHERE id=?id AND state='RUNNING' AND worker_token=?token",
      message = web_registry()[[j$operation]]$progress, time = as.numeric(Sys.time()), now = web_now(), id = j$id, token = j$worker_token)
    web_audit(con, p, "JOB_PROGRESS", j$id, detail = list(progress = 30))
    srma_run(settings, p$canonical_id, j$operation, list(), j$id, work, j$engine_id, file.path(attempt, "engine"),version_id=j$version_id)
  }, error = function(e) web_result(j$operation, p$canonical_id, j$id, "FAILED",
       if (inherits(e, "srma_web_error")) conditionMessage(e) else "Processing could not finish. The operator can inspect the diagnostic record.",
       j$engine_id, if (inherits(e, "srma_web_error")) e$code else "SYSTEM_FAILED", conditionMessage(e)))
  # Storage writes may leave unreferenced immutable objects after interruption; never publish partial registry rows.
  stored <- tryCatch({
    if (result$status %in% c("COMPLETE", "USER_ACTION_REQUIRED")) lapply(result$artifacts, function(a) {
      web_inside(a$path, work)
      web_no_file_secrets(a$path,settings$engine_root)
      id <- web_id()
      obj <- web_store(settings)$put(a$path, paste0(j$project_id, "/", id))
      c(list(id = id, logical_name = a$logical_name, type = a$type, relative_path = substring(normalizePath(a$path),nchar(normalizePath(work))+2L)), obj)
    }) else list()
  }, error = identity)
  if (inherits(stored, "error")) {
    result <- web_result(j$operation, p$canonical_id, j$id, "FAILED", "The output could not be stored. The operator can retry this check.", j$engine_id, "SYSTEM_FAILED", conditionMessage(stored))
    stored <- list()
  }
  web_tx(con, function() {
    # Fence late completions from a failed/recovered attempt before registry publication.
    current <- web_exec(con, "UPDATE jobs SET updated_at=updated_at WHERE id=?id AND state='RUNNING' AND worker_token=?token", id = j$id, token = j$worker_token)
    if (current != 1L) return(invisible(FALSE))
    web_exec(con, "UPDATE projects SET title=title WHERE id=?p", p = j$project_id)
    for (a in stored) {
      version <- web_query(con, "SELECT COALESCE(MAX(version),0)+1 AS n FROM artifacts WHERE project_id=?p AND logical_name=?n", p = j$project_id, n = a$logical_name)$n
      web_exec(con, "INSERT INTO artifacts VALUES(?id,?p,?v,?j,?name,?type,?n,'AVAILABLE',?sha,?loc,?e,'UNREVIEWED',?now)",
        id = a$id, p = j$project_id, v = j$version_id, j = j$id, name = a$logical_name, type = a$type,
        n = version, sha = a$checksum, loc = a$locator, e = j$engine_id, now = web_now())
    }
    result$artifacts <- lapply(stored, function(a) list(artifact_id = a$id, logical_name = a$logical_name, checksum = a$checksum, relative_path = a$relative_path))
    result$provenance$project_version_id <- j$version_id
    result$provenance$attempt <- j$attempts
    result$provenance$source_job_ids <- as.list(sources$source_job_id)
    web_exec(con, "UPDATE jobs SET state=?s,message=?m,progress=?progress,result_json=?result,heartbeat=?time,updated_at=?now WHERE id=?id AND state='RUNNING' AND worker_token=?token",
      s = result$status, m = result$user_message, progress = if (result$status == "COMPLETE") 100L else 30L,
      result = as.character(web_json(result)), time = as.numeric(Sys.time()), now = web_now(), id = j$id, token = j$worker_token)
    web_audit(con, p, paste0("JOB_", result$status), j$id, detail = list(artifacts = length(stored), attempt = j$attempts))
    TRUE
  })
  invisible(j$id)
}
web_recover_stale <- function(con, settings) {
  # Checks are replay-safe and isolated. Recovery marks FAILED; explicit retry remains bounded.
  web_tx(con, function() {
    jobs <- web_query(con, "SELECT * FROM jobs WHERE state='RUNNING' AND heartbeat<?before", before = as.numeric(Sys.time()) - settings$timeout - 60)
    for (i in seq_len(nrow(jobs))) {
      j <- jobs[i, , drop = FALSE]
      r <- web_result(j$operation, j$project_id, j$id, "FAILED", "Processing was interrupted. The operator can retry this check.", j$engine_id, "SYSTEM_FAILED", "Worker lease expired; late completion is fenced.")
      n <- web_exec(con, "UPDATE jobs SET state='FAILED',message=?m,result_json=?r,worker_token=NULL,updated_at=?now WHERE id=?id AND state='RUNNING' AND worker_token=?token AND heartbeat<?before",
        before = as.numeric(Sys.time()) - settings$timeout - 60, m = r$user_message, r = as.character(web_json(r)), now = web_now(), id = j$id, token = j$worker_token)
      if (n) web_audit(con, web_query(con, "SELECT * FROM projects WHERE id=?p", p = j$project_id), "WORKER_LEASE_EXPIRED", j$id)
    }
    nrow(jobs)
  })
}
