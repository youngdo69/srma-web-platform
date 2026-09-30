web_c5_sensitive_name <- function(x) grepl("password|passwd|secret|token|credential|api[_-]?key|authorization|cookie", x, ignore.case = TRUE)
web_c5_redact <- function(x, key = "") {
  if (web_c5_sensitive_name(key)) return("[REDACTED]")
  if (is.list(x)) {
    n <- names(x)
    if (is.null(n)) return(lapply(x, web_c5_redact))
    out <- Map(function(value, name) web_c5_redact(value, name), x, n)
    names(out) <- n
    return(out)
  }
  if (is.character(x)) {
    # Bound detail size and remove common credential material without exposing operator secrets.
    x <- substr(x, 1L, 2000L)
    x <- gsub("(?i)(bearer\\s+)[A-Za-z0-9._~+/=-]+", "\\1[REDACTED]", x, perl = TRUE)
    x <- gsub("(?i)((?:api[_-]?key|token|secret|password|credential)\\s*[:=]\\s*)[^,;\\s]+", "\\1[REDACTED]", x, perl = TRUE)
  }
  x
}
web_c5_safe_detail <- function(detail_json) {
  value <- tryCatch(web_parse(detail_json), error = function(e) list(note = "Audit detail unavailable."))
  web_c5_redact(value)
}
web_auth_audit <- function(con, user_id, provider, event, detail = list()) {
  web_exec(con, "INSERT INTO auth_audit_events(id,user_id,provider,event_type,detail_json,created_at) VALUES(?i,?u,?p,?e,?d,?t)",
    i = web_id(), u = user_id, p = provider, e = event,
    d = as.character(web_json(web_c5_redact(detail))), t = web_now())
  invisible(TRUE)
}
web_c5_audit_list <- function(con, actor, project_id, limit = 200L) {
  web_authorize(con, actor, project_id)
  limit <- max(1L, min(500L, as.integer(limit)))
  rows <- web_query(con, paste0("SELECT id,actor_id,event_type,object_id,detail_json,created_at FROM audit_events WHERE project_id=?p ORDER BY created_at DESC LIMIT ", limit), p = project_id)
  if (nrow(rows)) rows$detail <- vapply(rows$detail_json, function(x) as.character(web_json(web_c5_safe_detail(x))), character(1))
  rows$detail_json <- NULL
  rows
}
web_c5_auth_summary <- function(con, actor) {
  sessions <- web_query(con, "SELECT provider,issued_at,expires_at FROM auth_sessions WHERE user_id=?u AND revoked=0 AND expires_at>?n ORDER BY issued_at DESC", u = actor, n = as.numeric(Sys.time()))
  auth <- web_query(con, "SELECT provider,event_type,created_at FROM auth_audit_events WHERE user_id=?u ORDER BY created_at DESC", u = actor)
  list(active_sessions = sessions, recent_auth_events = utils::head(auth, 25L))
}
web_c5_logout_all <- function(con, actor) {
  sessions <- web_query(con, "SELECT DISTINCT provider FROM auth_sessions WHERE user_id=?u AND revoked=0", u = actor)
  web_exec(con, "UPDATE auth_sessions SET revoked=1 WHERE user_id=?u AND revoked=0", u = actor)
  provider <- if (nrow(sessions)) paste(sessions$provider, collapse = ",") else "unknown"
  web_auth_audit(con, actor, provider, "SIGN_OUT_ALL")
  invisible(TRUE)
}
web_c5_audit_export <- function(con, actor, project_id, destination) {
  project <- web_authorize(con, actor, project_id)
  rows <- web_c5_audit_list(con, actor, project_id, 500L)
  jsonlite::write_json(list(schema = "srma-audit-export-v1", project_id = project_id,
    exported_at = web_now(), events = rows), destination, auto_unbox = TRUE, pretty = TRUE, null = "null")
  web_audit(con, project, "AUDIT_EXPORT_DOWNLOADED", project_id, actor, list(events = nrow(rows)))
  invisible(destination)
}
