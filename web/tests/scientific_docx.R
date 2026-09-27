# Run with canonical R and the isolated maintenance library, never the Web R.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args) %in% c(2L,3L))
repo <- normalizePath(args[1],mustWork=TRUE)
library <- normalizePath(args[2],mustWork=TRUE)
.libPaths(c(library,.Library),include.site=FALSE)
lock <- jsonlite::read_json(file.path(repo,"renv.lock"))
stopifnot(getRversion()==package_version(lock$R$Version))
for(p in names(lock$Packages)) stopifnot(requireNamespace(p,quietly=TRUE),
  packageVersion(p)==package_version(lock$Packages[[p]]$Version))
proposed_count <- NULL
if(length(args)==3L) {
  proposed <- jsonlite::read_json(args[3])
  stopifnot(identical(proposed$R,lock$R),all(names(lock$Packages) %in% names(proposed$Packages)))
  for(p in names(lock$Packages)) stopifnot(identical(lock$Packages[[p]],proposed$Packages[[p]]))
  for(p in names(proposed$Packages)) stopifnot(requireNamespace(p,quietly=TRUE),
    packageVersion(p)==package_version(proposed$Packages[[p]]$Version))
  proposed_count <- length(proposed$Packages)
}
module <- file.path(repo,"R/41_generate_journal_neutral_author_editable_docx.R")
expressions <- parse(module)
assignment <- Filter(function(x) is.call(x) && identical(x[[1]],as.name("<-")) &&
  identical(x[[2]],as.name("required_packages")),as.list(expressions))
stopifnot(length(assignment)==1L)
env <- new.env(parent=baseenv());eval(assignment[[1]],env)
stopifnot(all(vapply(env$required_packages,requireNamespace,logical(1),quietly=TRUE)))
# Dependency/API smoke only. R/41's workflow and upstream approval gates are not run.
doc <- officer::read_docx()
doc <- officer::body_add_par(doc,"Synthetic dependency smoke; no scientific manuscript or study data.")
table <- flextable::flextable(data.frame(Test=c("Synthetic A","Synthetic B"),Value=c("Placeholder","Placeholder")))
doc <- flextable::body_add_flextable(doc,table)
path <- tempfile(fileext=".docx");print(doc,target=path)
parts <- unzip(path,list=TRUE)$Name
stopifnot(all(c("[Content_Types].xml","word/document.xml") %in% parts))
xml <- paste(readLines(unz(path,"word/document.xml"),warn=FALSE),collapse="")
parsed <- xml2::read_xml(xml)
stopifnot(length(xml2::xml_find_all(parsed,".//w:tbl",xml2::xml_ns(parsed)))==1L,
  grepl("Synthetic A",xml,fixed=TRUE))
jsonlite::write_json(list(R=R.version.string,library=library,
  original_locked_packages_verified=length(lock$Packages),R41_required_packages=as.list(env$required_packages),
  proposed_locked_packages_verified=proposed_count,
  officer_version=as.character(packageVersion("officer")),flextable_version=as.character(packageVersion("flextable")),
  editable_word_table=TRUE,synthetic_dependency_smoke_only=TRUE,canonical_R41_workflow_executed=FALSE),
  file.path(repo,"web/evidence/b1/scientific-docx-validation.json"),pretty=TRUE,auto_unbox=TRUE)
cat("PASS: original scientific lock packages unchanged; R/41 required namespaces load; actual DOCX and editable table generated. No R/41 workflow or human gate bypass.\n")
