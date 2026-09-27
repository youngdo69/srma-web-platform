# Exact runtime acceptance: no development-runtime seam is activated.
source("web/load.R")
s<-web_settings(data_root=tempfile("c1-runtime-acceptance-"))
web_science_settings(s) # checks the approved R version and locked package versions
Sys.setenv(C1_CANONICAL_ACCEPTANCE="1")
source("web/tests/c1_fixture.R")
cat("PASS: WEB-011 canonical scientific runtime/fixture acceptance; PostgreSQL and provider gates remain separate\n")
