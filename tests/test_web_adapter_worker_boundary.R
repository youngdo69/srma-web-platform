# Run from repo root: Rscript --vanilla tests/test_web_adapter_worker_boundary.R
# Deterministic Web-only test. No API calls; no scientific scripts are executed.
source("web/adapter/evidence.R")
web_assert <- function(ok,message,category="SYSTEM_FAILED") {
  if(!isTRUE(ok))stop(paste(category,message),call.=FALSE)
  invisible(TRUE)
}

run_boundary_tests <- function() {
  base <- tempfile("web-adapter-boundary-")
  dir.create(base,recursive=TRUE)
  wd <- getwd()
  on.exit({setwd(wd);unlink(base,recursive=TRUE)},add=TRUE)
  engine <- file.path(base,"golden_engine")
  job <- file.path(base,"private_job")
  for(d in c(file.path(engine,"R","utils","nested"),
             file.path(engine,"config"),file.path(job,"config")))
    dir.create(d,recursive=TRUE,showWarnings=FALSE)
  writeLines("approved utility",file.path(engine,"R","utils","nested","helper.R"))
  writeLines("approved task",file.path(engine,"R","TASK018_database_input_gate_hardening.R"))
  names <- c("agent_screening.yml","data_extraction_schema.yml","initialization_agent.yml")
  for(p in names)writeLines(paste0("engine-",p),file.path(engine,"config",p))
  pinned <- file.path(job,"config","data_extraction_schema.yml")
  writeLines("VERSION-PINNED-SCHEMA",pinned)
  # An existing saved model policy is also version-owned.
  policy <- file.path(job,"config","agent_screening.yml")
  writeLines("VERSION-PINNED-MODEL",policy)
  golden_files <- c(list.files(file.path(engine,"R"),recursive=TRUE,full.names=TRUE),
                    list.files(file.path(engine,"config"),full.names=TRUE))
  golden_files <- golden_files[!dir.exists(golden_files)]
  hashes_before <- unname(tools::md5sum(golden_files))
  setwd(job)
  stopifnot(isTRUE(web_b2_worker_stage_engine_files(engine)))
  stopifnot(identical(readLines(pinned),"VERSION-PINNED-SCHEMA"),
            identical(readLines(policy),"VERSION-PINNED-MODEL"),
            identical(readLines(file.path("config","initialization_agent.yml")),
                      "engine-initialization_agent.yml"),
            identical(readLines(file.path("R","utils","nested","helper.R")),
                      "approved utility"),
            identical(readLines(file.path("R","TASK018_database_input_gate_hardening.R")),
                      "approved task"),
            identical(hashes_before,unname(tools::md5sum(golden_files))))
  # Idempotent staging does not rewrite a saved policy on retries.
  stopifnot(isTRUE(web_b2_worker_stage_engine_files(engine)),
            identical(readLines(pinned),"VERSION-PINNED-SCHEMA"))
  # An empty compatibility file is temporary; a pre-existing dotenv is NEVER overwritten.
  f <- web_b2_worker_empty_env_bridge(job)
  stopifnot(file.exists(f),file.info(f)$size==0L)
  stopifnot(inherits(try(web_b2_worker_empty_env_bridge(job),silent=TRUE),"try-error"))
  unlink(f)
  writeLines("DUMMY_KEY=DO_NOT_USE",f)
  stopifnot(inherits(try(web_b2_worker_empty_env_bridge(job),silent=TRUE),"try-error"),
            identical(readLines(f),"DUMMY_KEY=DO_NOT_USE"))
  # Missing golden dependency must fail immediately and not proceed silently.
  unlink(file.path(engine,"R","TASK018_database_input_gate_hardening.R"))
  stopifnot(inherits(try(web_b2_worker_stage_engine_files(engine),silent=TRUE),"try-error"))
  cat("PASS: Web adapter preserves pinned policy, stages engine files, guards dotenv, fails closed\n")
}
run_boundary_tests()
