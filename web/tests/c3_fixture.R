# Non-private synthetic input fixture; publication-facing labels are human-readable.
# Full canonical B2/C1/C2 execution is necessary for the positive release-QC lifecycle.
Sys.setenv(C2_TEST="1",C3_TEST="1",C3_VALID_FIXTURE="1")
source("web/tests/c1_fixture.R")
