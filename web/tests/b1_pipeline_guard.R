# Actual R/11 + R/16 negative path on development R; no provider calls or QC PASS.
source("web/load.R");source("web/tests/fixture.R")
s<-web_settings(data_root=tempfile("negative-"));s$timeout<-40
needed<-c("rentrez","dotenv","httr2")
missing<-needed[!vapply(needed,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)){cat("NOT EXECUTED: canonical R/11 and R/16 negative-path acceptance requires missing dependencies:",paste(missing,collapse=", "),"\n");quit(status=77L)}
root<-web_fixture(tempfile("negative-project-"))
terms<-list(search=list(concepts=lapply(c("alpha","beta"),function(x)list(concept_id=x,label=x,role=x,required=TRUE,free_text_terms=list(paste0("syntheticqaunfindable",x))))))
yaml::write_yaml(terms,file.path(root,"config/search_terms.yml"))
provider<-yaml::read_yaml("config/initialization_agent.yml")$initialization_agent$provider$api_key_env
old<-Sys.getenv(provider,unset=NA_character_);Sys.unsetenv(provider)
r<-web_engine_call(s,list(action="generate_search",project_dir=root))
if(r$status!="USER_ACTION_REQUIRED")print(r)
stopifnot(r$status=="USER_ACTION_REQUIRED",r$provenance$search_quality=="REVIEW_REQUIRED",nzchar(r$provenance$query_sha256),length(r$artifacts)>0)
q<-yaml::read_yaml(file.path(root,"search_strategies/search_quality_report.yml"))$search_quality
stopifnot(q$status=="REVIEW_REQUIRED",!isTRUE(q$llm_qc_available))
r2<-web_engine_call(s,list(action="retrieve_pubmed",project_dir=root))
stopifnot(r2$status=="FAILED",grepl("blocked before EFetch",r2$technical$diagnostic,fixed=TRUE),!length(list.files(file.path(root,"data/raw/pubmed"),recursive=TRUE)))
# Tamper only synthetic query: unchanged canonical engine must detect hash mismatch.
cat("\nchanged synthetic query",file=file.path(root,"search_strategies/pubmed_search.txt"),append=TRUE)
r3<-web_engine_call(s,list(action="retrieve_pubmed",project_dir=root))
stopifnot(r3$status=="FAILED",grepl("hash|changed|match",r3$technical$diagnostic,ignore.case=TRUE))
if(is.na(old))Sys.unsetenv(provider) else do.call(Sys.setenv,setNames(list(old),provider))
cat("PASS: actual canonical R/11 REVIEW_REQUIRED mapped without COMPLETE; actual R/16 blocks before EFetch and rejects modified query. Development runtime only; zero records retrieved.\n")
