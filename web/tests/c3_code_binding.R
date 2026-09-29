# Acceptance must expire after any executable Web guard or migration changes.
source("web/load.R")
root<-tempfile("c3-code-binding-");dir.create(root);dir.create(file.path(root,"web"))
for(d in c("R","adapter","db","ui"))stopifnot(file.copy(file.path(web_root,"web",d),file.path(root,"web"),recursive=TRUE))
stopifnot(file.copy(file.path(web_root,"web/load.R"),file.path(root,"web/load.R")))
s<-list(engine_root=root);original<-web_c3_code_hash(s)
for(p in c("web/R/identity.R","web/R/service.R","web/R/worker.R","web/adapter/final_qc.R","web/db/012_final_qc.sql","web/ui/final_qc.R","web/load.R")) {
 path<-file.path(root,p);raw<-readBin(path,"raw",file.info(path)$size)
 writeBin(c(raw,charToRaw("\n")),path);stopifnot(web_c3_code_hash(s)!=original)
 writeBin(raw,path);stopifnot(web_c3_code_hash(s)==original)
}
unlink(root,recursive=TRUE)
cat("PASS: acceptance code binding detects auth/service/worker/adapter/migration/UI/loader changes\n")
