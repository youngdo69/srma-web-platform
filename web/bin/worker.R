source("web/load.R")
settings <- web_settings()
con <- web_db(settings)
web_migrate(con, settings)
once <- "--once" %in% commandArgs(trailingOnly = TRUE)
repeat {
  tryCatch({web_recover_stale(con, settings); web_worker_once(con, settings)},
    error = function(e) cat(web_now(), "worker error:", conditionMessage(e), "\n", file = stderr()))
  if (once) break
  Sys.sleep(1)
}
DBI::dbDisconnect(con)
