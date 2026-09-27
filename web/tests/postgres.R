# Run ONLY against a disposable empty PostgreSQL database. No destructive reset.
source("web/load.R")
s <- web_settings(backend="postgres"); c <- web_db(s)
stopifnot(length(DBI::dbListTables(c)) == 0L)
cat("Actual server:", DBI::dbGetQuery(c,"SELECT version()")[[1]], "\n")
DBI::dbDisconnect(c)
Sys.setenv(SRMA_WEB_DB="postgres")
source("web/tests/run.R")
# Process competition is separately exercised with:
# SRMA_WEB_DB=postgres python3 web/tests/vertical.py
# Use a separate disposable database for b1_security.R and b1.R.
