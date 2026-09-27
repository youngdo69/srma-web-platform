source("web/load.R")
lock<-jsonlite::read_json("web/renv.lock")
selected<-normalizePath(Sys.getenv("SRMA_WEB_LIBRARY"),mustWork=TRUE)
stopifnot(getRversion()==package_version(lock$R$Version))
records<-lapply(names(lock$Packages),function(p){
 path<-find.package(p);actual<-packageDescription(p,fields="Version")
 stopifnot(startsWith(normalizePath(path),paste0(selected,"/")),packageVersion(p)==package_version(lock$Packages[[p]]$Version),requireNamespace(p,quietly=TRUE))
 list(package=p,expected=lock$Packages[[p]]$Version,actual=actual,library=dirname(path))
})
stopifnot(!any(grepl("root/usr/lib/R/site-library",.libPaths(),fixed=TRUE)))
jsonlite::write_json(list(R=R.version.string,library_paths=as.list(.libPaths()),packages=records,old_Block_A_packages_used=FALSE),
 "web/evidence/b1/web-clean-runtime.json",auto_unbox=TRUE,pretty=TRUE)
cat("PASS: all",length(records),"locked Web packages load exclusively from the fresh Web library; no Block A package library\n")
