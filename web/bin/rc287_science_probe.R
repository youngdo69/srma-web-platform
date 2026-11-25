# RC28.7 exact canonical scientific-runtime acceptance for Worker startup.
# Web and scientific locks remain independent. No scientific operation is run.
source("web/load.R")
result <- tryCatch(web_science_settings(web_settings(), verify = TRUE), error = identity)
if (inherits(result, "error")) {
  cat("SCIENTIFIC BLOCKED:", conditionMessage(result), "\n")
  quit(save = "no", status = 3L)
}
if (!identical(result$runtime_acceptance, "EXACT_CANONICAL_RUNTIME")) {
  cat("SCIENTIFIC BLOCKED: development-unverified runtime is not canonical acceptance.\n")
  quit(save = "no", status = 3L)
}
cat("PASS: scientific runtime matches canonical lock.\n")
quit(save = "no", status = 0L)
