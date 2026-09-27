# Read-only current-environment evidence. Never restores dependencies or starts servers.
source("web/load.R")
lock<-jsonlite::read_json("renv.lock")
expressions<-parse("R/41_generate_journal_neutral_author_editable_docx.R")
assignment<-Filter(function(x)is.call(x)&&identical(x[[1]],as.name("<-"))&&identical(x[[2]],as.name("required_packages")),as.list(expressions))
env<-new.env(parent=baseenv());eval(assignment[[1]],env)
critical<-unique(c("renv","rentrez","dotenv","httr2","officer","flextable",env$required_packages))
available<-setNames(lapply(critical,function(p)list(namespace_available=isTRUE(requireNamespace(p,quietly=TRUE)),
 version=tryCatch(as.character(utils::packageVersion(p)),error=function(e)NULL))),critical)
provider<-yaml::read_yaml("config/initialization_agent.yml")$initialization_agent$provider$api_key_env
result<-list(current_R=as.character(getRversion()),required_R=lock$R$Version,locked_packages=length(lock$Packages),
 configured_scientific_runner_present=file.exists(Sys.getenv("SRMA_SCIENTIFIC_RSCRIPT","")),
 configured_scientific_library_present=dir.exists(Sys.getenv("SRMA_SCIENTIFIC_LIBRARY","")),
 provider_credential_present=nzchar(Sys.getenv(provider,"")),
 development_namespace_inventory=available,
 renv_synchronization="NOT_EXECUTED: canonical isolated runtime unavailable",
 START_HERE_environment_only="NOT_EXECUTED: canonical isolated runtime unavailable",
 scientific_DOCX_smoke="NOT_EXECUTED: canonical isolated runtime unavailable",
 manuscript_scientific_regression="NOT_EXECUTED; not implied by dependency testing",
 PostgreSQL="POSTGRESQL_EXTERNAL_ACCEPTANCE_PENDING; existing root-only infrastructure limitation, not retried",
 credentialed_path="CREDENTIALED_ACCEPTANCE_PENDING")
jsonlite::write_json(result,"web/evidence/completion-001/environment.json",auto_unbox=TRUE,pretty=TRUE,null="null")
cat("Current environment inventory recorded. No external/scientific acceptance PASS claimed.\n")
