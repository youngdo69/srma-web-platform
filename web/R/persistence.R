web_db <- function(settings) {
  if (settings$backend == "sqlite") {
    con <- DBI::dbConnect(RSQLite::SQLite(), settings$sqlite_file)
    DBI::dbExecute(con, "PRAGMA foreign_keys=ON")
    DBI::dbExecute(con, "PRAGMA busy_timeout=10000")
    DBI::dbGetQuery(con, "PRAGMA journal_mode=WAL")
  } else if (settings$backend == "postgres") {
    # libpq uses PGHOST/PGPORT/PGDATABASE/PGUSER and .pgpass; no secrets in source/config.
    con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), host = Sys.getenv("PGHOST", "localhost"),
      port = as.integer(Sys.getenv("PGPORT", "5432")), dbname = Sys.getenv("PGDATABASE", "srma_web"),
      user = Sys.getenv("PGUSER", "srma_web"), password = Sys.getenv("PGPASSWORD", ""))
  } else web_error("SYSTEM_FAILED", "Unsupported operational database.")
  con
}
web_query <- function(.connection, .sql, ...) DBI::dbGetQuery(.connection, DBI::sqlInterpolate(conn = .connection, sql = .sql, ...))
web_exec <- function(.connection, .sql, ...) DBI::dbExecute(.connection, DBI::sqlInterpolate(conn = .connection, sql = .sql, ...))
web_tx <- function(con, f) {
  if (inherits(con, "SQLiteConnection")) DBI::dbExecute(con, "BEGIN IMMEDIATE") else DBI::dbBegin(con)
  done <- FALSE
  on.exit(if (!done) try(DBI::dbRollback(con), silent = TRUE))
  value <- f()
  DBI::dbCommit(con); done <- TRUE
  value
}
web_migrate <- function(con, settings) {
  path <- file.path(settings$engine_root, "web/db/001_core.sql")
  hash <- web_sha(path)
  if (DBI::dbExistsTable(con, "schema_migrations")) {
    versions <- DBI::dbGetQuery(con, "SELECT * FROM schema_migrations")
    web_assert(nrow(versions) == 1L && versions$version == 1L && versions$checksum == hash,
      "Database migration history needs operator review.", "SYSTEM_FAILED")
    return(invisible(TRUE))
  }
  web_tx(con, function() {
    sql <- paste(readLines(path), collapse = "\n")
    for (statement in strsplit(sql, ";", fixed = TRUE)[[1L]])
      if (nzchar(trimws(statement))) DBI::dbExecute(con, statement)
    allowed <- "(OLD.state='QUEUED' AND NEW.state IN ('RUNNING','CANCELLED','USER_ACTION_REQUIRED','FAILED')) OR (OLD.state='RUNNING' AND NEW.state IN ('COMPLETE','FAILED','USER_ACTION_REQUIRED')) OR (OLD.state IN ('FAILED','USER_ACTION_REQUIRED') AND NEW.state='QUEUED')"
    if (inherits(con, "SQLiteConnection")) {
      DBI::dbExecute(con, paste0("CREATE TRIGGER job_transition BEFORE UPDATE OF state ON jobs WHEN NEW.state<>OLD.state AND NOT (", allowed, ") BEGIN SELECT RAISE(ABORT,'Invalid job transition'); END"))
      DBI::dbExecute(con, "CREATE TRIGGER gate_immutable BEFORE UPDATE ON gates WHEN OLD.status<>'PENDING' BEGIN SELECT RAISE(ABORT,'Gate decision is immutable'); END")
      DBI::dbExecute(con, "CREATE TRIGGER audit_immutable BEFORE UPDATE ON audit_events BEGIN SELECT RAISE(ABORT,'Audit events are immutable'); END")
      DBI::dbExecute(con, "CREATE TRIGGER audit_no_delete BEFORE DELETE ON audit_events BEGIN SELECT RAISE(ABORT,'Audit events are immutable'); END")
    } else {
      DBI::dbExecute(con, paste0("CREATE FUNCTION guard_job_transition() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.state<>OLD.state AND NOT (",allowed,") THEN RAISE EXCEPTION 'Invalid job transition'; END IF; RETURN NEW; END $$"))
      DBI::dbExecute(con, "CREATE TRIGGER job_transition BEFORE UPDATE ON jobs FOR EACH ROW EXECUTE FUNCTION guard_job_transition()")
      DBI::dbExecute(con, "CREATE FUNCTION guard_gate() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF OLD.status<>'PENDING' THEN RAISE EXCEPTION 'Gate decision is immutable'; END IF; RETURN NEW; END $$")
      DBI::dbExecute(con, "CREATE TRIGGER gate_immutable BEFORE UPDATE ON gates FOR EACH ROW EXECUTE FUNCTION guard_gate()")
      DBI::dbExecute(con, "CREATE FUNCTION guard_audit() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'Audit events are immutable'; END $$")
      DBI::dbExecute(con, "CREATE TRIGGER audit_immutable BEFORE UPDATE OR DELETE ON audit_events FOR EACH ROW EXECUTE FUNCTION guard_audit()")
    }
    web_exec(con, "INSERT INTO schema_migrations VALUES(1,?hash,?now)", hash = hash, now = web_now())
  })
}
web_authorize <- function(con, actor, project_id) {
  x <- web_query(con, "SELECT p.* FROM projects p JOIN workspace_members m ON p.workspace_id=m.workspace_id WHERE p.id=?p AND m.user_id=?u",
    p = project_id, u = actor)
  web_assert(nrow(x) == 1L, "This project is not accessible.")
  x
}
web_audit <- function(con, project, event, object, actor = NA_character_, detail = list()) {
  web_exec(con, "INSERT INTO audit_events VALUES(?id,?p,?w,?actor,?event,?object,?detail,?now)",
    id = web_id(), p = project$id, w = project$workspace_id, actor = actor, event = event,
    object = object, detail = as.character(web_json(detail)), now = web_now())
}
web_gate_open <- function(con, project_id, version_id, operation) {
  nrow(web_query(con, "SELECT id FROM gates WHERE project_id=?p AND version_id=?v AND required_operation=?o AND status<>'APPROVED'",
    p = project_id, v = version_id, o = operation)) > 0L
}
