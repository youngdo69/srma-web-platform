# Actual frozen R/11 and R/16 negative-path execution, without scientific approval.
# Explicit synthetic terms only. No study records are invented or downloaded.
source("web/load.R"); source("web/tests/fixture.R")
s <- web_science_settings(web_settings(data_root = tempfile("guard-data-")))
root <- web_fixture(tempfile("guard-project-"))
terms <- list(search = list(concepts = list(
  list(concept_id="synthetic_a", label="Synthetic parser concept A", role="Population", required=TRUE,
       free_text_terms=list("syntheticqaunfindablealpha")),
  list(concept_id="synthetic_b", label="Synthetic parser concept B", role="Intervention", required=TRUE,
       free_text_terms=list("syntheticqaunfindablebeta")))))
yaml::write_yaml(terms,file.path(root,"config/search_terms.yml"))
provider <- yaml::read_yaml("config/initialization_agent.yml")$initialization_agent$provider
key_available <- nzchar(Sys.getenv(provider$api_key_env, ""))
# This deliberately credential-free guard test never calls a paid LLM provider.
old <- Sys.getenv(provider$api_key_env, unset=NA_character_)
Sys.unsetenv(provider$api_key_env)
run <- function(operation) web_engine_call(s,list(action=operation,project_dir=root,
  project_id="synthetic-negative-test",job_id="private-guard-test",engine_version="frozen-baseline"))
generated <- run("generate_search")
if(generated$status!="USER_ACTION_REQUIRED") print(generated)
stopifnot(generated$status=="USER_ACTION_REQUIRED")
quality <- yaml::read_yaml(file.path(root,"search_strategies/search_quality_report.yml"))$search_quality
stopifnot(quality$status!="PASS",!isTRUE(quality$llm_qc_available))
retrieved <- run("retrieve_pubmed")
stopifnot(retrieved$status=="FAILED",grepl("blocked before EFetch",retrieved$technical$diagnostic,fixed=TRUE))
stopifnot(!length(list.files(file.path(root,"data/raw/pubmed"),recursive=TRUE)))
if (!is.na(old)) do.call(Sys.setenv,setNames(list(old),provider$api_key_env))
jsonlite::write_json(list(web_test_R=R.version.string,scientific_runtime=generated$provenance$R,
  synthetic_terms=TRUE,provider=provider$name,credential_available_in_operator_environment=key_available,
  test_credential_deliberately_absent=TRUE,generation_status=generated$status,canonical_quality=quality$status,
  pubmed_retrieval_status=retrieved$status,blocked_before_EFetch=TRUE,downloaded_records=0L,
  generation_provenance=generated$provenance),"web/evidence/b1/search-negative-path.json",auto_unbox=TRUE,pretty=TRUE)
cat("PASS: actual canonical search generation yields review required without provider credentials; actual R/16 blocks before EFetch; no scientific approval or records fabricated\n")
