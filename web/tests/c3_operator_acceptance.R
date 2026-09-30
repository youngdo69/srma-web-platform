# Operator-only, real acceptance. Never exposed through browser requests.
# Use disposable non-private test storage; PostgreSQL mode requires an EMPTY unprivileged database.
source("web/load.R")
kind<-commandArgs(trailingOnly=TRUE);stopifnot(length(kind)==1,kind %in% c("CANONICAL_RUNTIME","POSTGRESQL"))
s<-web_settings();engine_before<-web_c3_code_hash(s);lock<-web_sha(file.path(s$engine_root,"renv.lock"))
logs<-tempfile("c3-operator-evidence-");dir.create(logs,mode="0700")
if(kind=="CANONICAL_RUNTIME") {
 scientific<-web_science_settings(s);probe<-web_engine_call(scientific,list(action="runtime_probe"));stopifnot(probe$status=="COMPLETE")
 # Exact locked runtime, real canonical statistical reproduction + manual manuscript/DOCX pipeline.
 Sys.setenv(SRMA_RSCRIPT=scientific$rscript,SRMA_SCIENTIFIC_RSCRIPT=scientific$rscript,SRMA_SCIENTIFIC_LIBRARY=scientific$engine_library,C1_TEST_LIBRARY=scientific$engine_library,C1_CANONICAL_ACCEPTANCE="1",C1_TEST_ROOT=file.path(logs,"reproduction"),SRMA_WEB_DB="sqlite")
 log<-file.path(logs,"canonical-reproduction.txt")
 rc<-system2(scientific$rscript,shQuote(c("--vanilla","web/tests/c2_fixture.R")),stdout=log,stderr=log,timeout=1800)
 stopifnot(rc==0L)
 evidence<-list(runtime=probe$R,packages=probe$packages,reproduction_command="Rscript --vanilla web/tests/c2_fixture.R",exit_code=rc,result_sha256=web_sha(log),web_sha256=engine_before,lock_sha256=lock)
} else {
 stopifnot(s$backend=="postgres");db<-web_db(s);stopifnot(length(DBI::dbListTables(db))==0)
 priv<-DBI::dbGetQuery(db,"SELECT rolsuper,rolcreaterole FROM pg_roles WHERE rolname=current_user")
 stopifnot(!priv$rolsuper,!priv$rolcreaterole);DBI::dbDisconnect(db)
 log<-file.path(logs,"postgres.txt")
 rc<-system2(s$rscript,shQuote(c("--vanilla","web/tests/postgres.R")),stdout=log,stderr=log,timeout=600);stopifnot(rc==0L)
 db<-web_db(s);web_migrate(db,s)
 # Verify current QC/acceptance trigger protection as well as shared integration tests.
 triggers<-DBI::dbGetQuery(db,"SELECT tgname FROM pg_trigger WHERE tgname IN ('qc_no_mutation','acceptance_no_mutation')")
 stopifnot(nrow(triggers)==2)
 evidence<-list(server=DBI::dbGetQuery(db,"SELECT version()")[[1]],unprivileged=TRUE,command="Rscript --vanilla web/tests/postgres.R",exit_code=rc,result_sha256=web_sha(log),web_sha256=engine_before,lock_sha256=lock)
 DBI::dbDisconnect(db)
}
stopifnot(engine_before==web_c3_code_hash(s),lock==web_sha(file.path(s$engine_root,"renv.lock")))
export_path<-Sys.getenv("SRMA_ACCEPTANCE_EXPORT","")
if(nzchar(export_path)) {
 dir.create(dirname(export_path),recursive=TRUE,showWarnings=FALSE,mode="0700")
 jsonlite::write_json(list(kind=kind,lock_sha256=lock,evidence=evidence),export_path,auto_unbox=TRUE,null="null",digits=NA,pretty=TRUE)
}
record<-!identical(Sys.getenv("SRMA_ACCEPTANCE_RECORD","1"),"0")
if(record) {
 # Record only when the operator intentionally points settings at a database that may hold acceptance rows.
 db<-web_db(s);web_migrate(db,s);engine<-web_register_engine(db,s)
 web_exec(db,"INSERT INTO release_acceptances VALUES(?i,?e,?k,?h,?x,?t)",i=web_id(),e=engine,k=kind,h=lock,x=as.character(jsonlite::toJSON(evidence,auto_unbox=TRUE,null="null",digits=NA)),t=web_now());DBI::dbDisconnect(db)
}
cat("PASS: actual",kind,"acceptance evidence verified",if(nzchar(export_path))paste("and exported to",export_path) else "",if(record)"and recorded" else "without database recording","; project release still requires current successful QC and Human review\n")
