source("web/load.R")
source("web/tests/fixture.R")
passed <- 0L
check <- function(name, expr) {
  ok <- tryCatch({force(expr); TRUE}, error = function(e) {cat("FAIL:", name, "—", conditionMessage(e), "\n"); FALSE})
  if (!ok) quit(status = 1L)
  passed <<- passed + 1L; cat("PASS:", name, "\n")
}
reject <- function(expr, pattern = NULL) {
  e <- tryCatch({force(expr); NULL}, error = identity)
  stopifnot(inherits(e, "error"))
  if (!is.null(pattern)) stopifnot(grepl(pattern, conditionMessage(e)))
  invisible(e)
}
settings <- web_settings(data_root = tempfile("srma-tests-"))
con <- web_db(settings)
check("schema creation and migration idempotence", {web_migrate(con, settings); web_migrate(con, settings); stopifnot(length(DBI::dbListTables(con)) == 11L)})
web_local_identity(con)
actor <- "local-researcher"
fixture <- web_fixture(tempfile("input-"))
check("real canonical configuration import", {v <- web_import_project(con, settings, actor, "local-workspace", fixture); stopifnot(nchar(v$project_id) == 32)})
check("registry fails closed for unsupported operations and nonempty payload", {
  reject(web_submit(con, settings, actor, v$project_id, v$version_id, "run_meta_analysis"))
  reject(web_submit(con, settings, actor, v$project_id, v$version_id, payload = list(script = "anything")))
})
check("all five structured errors are distinct", {
 for (code in c("USER_ACTION_REQUIRED","VALIDATION_FAILED","ENGINE_FAILED","DEPENDENCY_FAILED","SYSTEM_FAILED")) {
   e <- reject(web_error(code, "Safe message", "Private diagnostics")); stopifnot(e$code == code, conditionMessage(e) == "Safe message")
 }
})
check("queue idempotency and active-request deduplication", {
  job <- web_submit(con, settings, actor, v$project_id, v$version_id, idempotency_key = "request-one")
  again <- web_submit(con, settings, actor, v$project_id, v$version_id, idempotency_key = "request-one")
  third <- web_submit(con, settings, actor, v$project_id, v$version_id, idempotency_key = "another-key")
  stopifnot(job == again, job == third, DBI::dbGetQuery(con,"SELECT COUNT(*) AS n FROM jobs")$n == 1L)
})
check("database rejects invalid transition", reject(web_exec(con, "UPDATE jobs SET state='COMPLETE' WHERE id=?id", id = job)))
check("unauthorized project and job access rejected", {
  reject(web_view(con, "unknown", v$project_id)); reject(web_cancel(con, "unknown", v$project_id, job))
})
check("real engine worker completes and publishes two artifacts", {
  web_worker_once(con, settings)
  j <- web_query(con, "SELECT * FROM jobs WHERE id=?id", id = job)
  if (j$state != "COMPLETE") print(web_parse(j$result_json))
  stopifnot(j$state == "COMPLETE", j$attempts == 1L, j$progress == 100L)
  a <- web_query(con, "SELECT * FROM artifacts WHERE job_id=?j", j = job)
  stopifnot(nrow(a) == 2L)
  r <- web_parse(j$result_json)
  stopifnot(all(c("operation","project_id","job_id","status","user_message","technical","warnings","engine_version","artifacts","started_at","finished_at","provenance") %in% names(r)))
})
check("authorized download checksum and safe application projection", {
  dest <- tempfile();web_download(con, settings, actor, v$project_id, a$id[1], dest)
  stopifnot(web_sha(dest) == a$checksum[1])
  view <- web_view(con, actor, v$project_id)
  stopifnot(!any(c("result_json","diagnostic_locator","snapshot_locator","storage_locator") %in% unlist(lapply(view,names))))
  reject(web_download(con, settings, "unknown", v$project_id, a$id[1], tempfile()))
})
check("database reopen resumes project and job", {
  DBI::dbDisconnect(con);con <- web_db(settings)
  stopifnot(web_view(con, actor, v$project_id)$jobs$state[1] == "COMPLETE")
})
check("terminal job cannot be rerun or cancelled", {
  reject(web_retry(con, settings, actor, v$project_id, job));reject(web_cancel(con, actor, v$project_id, job))
  stopifnot(is.null(web_worker_once(con,settings)))
})
check("human gate blocks before queue and preserves decision audit", {
  gate <- web_require_gate(con, actor, v$project_id, v$version_id, "local_review_requirement", "validate_search", a$id[1])
  blocked <- web_submit(con, settings, actor, v$project_id, v$version_id, idempotency_key = "gated-request")
  stopifnot(web_query(con,"SELECT state FROM jobs WHERE id=?id",id=blocked)$state == "USER_ACTION_REQUIRED")
  reject(web_retry(con, settings, actor, v$project_id, blocked))
  web_decide_gate(con, actor, v$project_id, gate, "APPROVED", "Explicit synthetic test decision")
  reject(web_decide_gate(con, actor, v$project_id, gate, "REJECTED"))
  web_retry(con, settings, actor, v$project_id, blocked)
  web_worker_once(con,settings)
  stopifnot(web_query(con,"SELECT state FROM jobs WHERE id=?id",id=blocked)$state == "COMPLETE")
  stopifnot(nrow(DBI::dbGetQuery(con,"SELECT * FROM audit_events WHERE event_type='GATE_DECIDED'")) == 1L)
})
check("artifact versions are attributable and monotonic", {
  versions <- DBI::dbGetQuery(con,"SELECT version FROM artifacts ORDER BY version")$version
  stopifnot(identical(as.integer(versions), c(1L,1L,2L,2L)))
})
check("gate created after queue is checked at worker pickup", {
  late <- web_submit(con,settings,actor,v$project_id,v$version_id,idempotency_key="late-gate")
  gate2 <- web_require_gate(con,actor,v$project_id,v$version_id,"later_review","validate_search")
  web_worker_once(con,settings)
  stopifnot(web_query(con,"SELECT state FROM jobs WHERE id=?id",id=late)$state == "USER_ACTION_REQUIRED")
  web_decide_gate(con,actor,v$project_id,gate2,"REJECTED")
  reject(web_retry(con,settings,actor,v$project_id,late))
})
check("new input version does not inherit old gate decisions", {
  v2 <- web_import_project(con,settings,actor,"local-workspace",fixture)
  stopifnot(v2$version_id != v$version_id)
  g <- web_require_gate(con,actor,v$project_id,v2$version_id,"version_review","validate_search")
  b <- web_submit(con,settings,actor,v$project_id,v2$version_id,idempotency_key="new-version")
  stopifnot(web_query(con,"SELECT state FROM jobs WHERE id=?id",id=b)$state == "USER_ACTION_REQUIRED")
})
check("cross-project version, artifact and gate references rejected", {
  other <- web_import_project(con,settings,actor,"local-workspace",web_fixture(tempfile("other-")))
  reject(web_submit(con,settings,actor,other$project_id,v$version_id))
  reject(web_download(con,settings,actor,other$project_id,a$id[1],tempfile()))
  reject(web_require_gate(con,actor,other$project_id,other$version_id,"bad_reference","validate_search",a$id[1]))
})
check("snapshot tampering is detected", {
  v3 <- web_import_project(con,settings,actor,"local-workspace",web_fixture(tempfile("tamper-")))
  j3 <- web_submit(con,settings,actor,v3$project_id,v3$version_id)
  locator <- web_query(con,"SELECT snapshot_locator FROM project_versions WHERE id=?v",v=v3$version_id)$snapshot_locator
  writeLines("changed",file.path(settings$data_root,locator,"config/search_terms.yml"))
  web_worker_once(con,settings)
  stopifnot(web_query(con,"SELECT state FROM jobs WHERE id=?id",id=j3)$state == "FAILED")
})
check("bad real search syntax yields validation failure without raw error leakage", {
  bad <- web_fixture(tempfile("bad-"));writeLines("BROKEN(",file.path(bad,"search_strategies/pubmed_search.txt"))
  bv <- web_import_project(con,settings,actor,"local-workspace",bad)
  bj <- web_submit(con,settings,actor,bv$project_id,bv$version_id);web_worker_once(con,settings)
  raw <- web_query(con,"SELECT result_json FROM jobs WHERE id=?id",id=bj)$result_json
  result <- web_parse(raw)
  stopifnot(result$status=="FAILED",result$error_code=="VALIDATION_FAILED",!grepl("R/|Error|/workspace", result$user_message))
  reject(web_retry(con,settings,actor,bv$project_id,bj))
})
check("path traversal and symlink escape rejected", {
  reject(web_inside(file.path(settings$data_root,".."),settings$data_root))
  escape <- file.path(settings$data_root,"escape");if (file.symlink(tempdir(),escape)) reject(web_inside(escape,settings$data_root))
  bad <- web_fixture(tempfile("path-"));cfg <- yaml::read_yaml(file.path(bad,"config/project_config.yml"));cfg$paths$logs_directory <- "../outside"
  yaml::write_yaml(cfg,file.path(bad,"config/project_config.yml"));reject(web_import_project(con,settings,actor,"local-workspace",bad))
})
check("lease recovery, bounded safe retry and queue cancellation", {
  rv <- web_import_project(con,settings,actor,"local-workspace",web_fixture(tempfile("recover-")))
  rj <- web_submit(con,settings,actor,rv$project_id,rv$version_id)
  claimed <- web_claim(con);stopifnot(claimed$id == rj)
  web_exec(con,"UPDATE jobs SET heartbeat=0 WHERE id=?id",id=rj)
  stopifnot(web_recover_stale(con,settings)==1L)
  web_retry(con,settings,actor,rv$project_id,rj)
  again <- web_claim(con);web_exec(con,"UPDATE jobs SET heartbeat=0 WHERE id=?id",id=rj);web_recover_stale(con,settings)
  reject(web_retry(con,settings,actor,rv$project_id,rj))
  cancel <- web_submit(con,settings,actor,rv$project_id,rv$version_id,idempotency_key="cancel-me")
  web_cancel(con,actor,rv$project_id,cancel)
  stopifnot(web_query(con,"SELECT state FROM jobs WHERE id=?id",id=cancel)$state=="CANCELLED")
})
check("artifact checksum tampering blocks downloads", {
  path <- file.path(settings$data_root,"objects",a$storage_locator[1]);writeLines("bad",path)
  reject(web_download(con,settings,actor,v$project_id,a$id[1],tempfile()))
})
check("audit update and deletion rejected", {
  reject(DBI::dbExecute(con,"UPDATE audit_events SET event_type='changed'"))
  reject(DBI::dbExecute(con,"DELETE FROM audit_events"))
})
check("Shiny bslib application can construct", {
  source("web/ui/app.R");app <- web_app(settings,actor);stopifnot(inherits(app,"shiny.appobj"))
})
check("Shiny server queues through the authorized service", {
  ui_v <- web_import_project(con,settings,actor,"local-workspace",web_fixture(tempfile("ui-")))
  shiny::testServer(web_app(settings,actor), {
    session$setInputs(project=ui_v$project_id,version=ui_v$version_id)
    session$flushReact()
    session$setInputs(run=1)
    session$flushReact()
    stopifnot(grepl("Request saved",output$notice))
  })
  ui_j <- web_query(con,"SELECT id FROM jobs WHERE project_id=?p AND state='QUEUED'",p=ui_v$project_id)$id
  stopifnot(length(ui_j)==1L)
  web_cancel(con,actor,ui_v$project_id,ui_j)
})
check("storage failure cannot publish partial registry entries", {
  sv <- web_import_project(con,settings,actor,"local-workspace",web_fixture(tempfile("storage-")))
  sj <- web_submit(con,settings,actor,sv$project_id,sv$version_id)
  real_store <- web_store
  web_store <- function(settings) list(put=function(...) stop("Injected storage outage"))
  tryCatch(web_worker_once(con,settings),finally={web_store <<- real_store})
  stopifnot(web_query(con,"SELECT state FROM jobs WHERE id=?id",id=sj)$state=="FAILED")
  stopifnot(nrow(web_query(con,"SELECT id FROM artifacts WHERE job_id=?id",id=sj))==0L)
  web_retry(con,settings,actor,sv$project_id,sj);web_worker_once(con,settings)
  stopifnot(web_query(con,"SELECT state FROM jobs WHERE id=?id",id=sj)$state=="COMPLETE")
})
check("actual second-workspace actor cannot access the first project", {
  DBI::dbExecute(con,"INSERT INTO app_users VALUES('second-actor','Second actor')")
  DBI::dbExecute(con,"INSERT INTO workspaces VALUES('second-workspace','Second workspace')")
  DBI::dbExecute(con,"INSERT INTO workspace_members VALUES('second-workspace','second-actor')")
  reject(web_view(con,"second-actor",v$project_id))
  reject(web_decide_gate(con,"second-actor",v$project_id,gate,"APPROVED"))
})
check("migration checksum mismatch fails closed", {
  hash <- DBI::dbGetQuery(con,"SELECT checksum FROM schema_migrations")$checksum
  DBI::dbExecute(con,"UPDATE schema_migrations SET checksum='changed'")
  reject(web_migrate(con,settings))
  web_exec(con,"UPDATE schema_migrations SET checksum=?hash",hash=hash)
})
DBI::dbDisconnect(con)
cat("RESULT:", passed, "test groups passed; 0 failed\n")
