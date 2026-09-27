# Available API smoke only; does not claim canonical lock synchronization.
source("web/load.R")
stopifnot(requireNamespace("officer",quietly=TRUE),requireNamespace("flextable",quietly=TRUE),requireNamespace("xml2",quietly=TRUE))
doc<-officer::read_docx();doc<-officer::body_add_par(doc,"Synthetic dependency test only. No scientific manuscript.")
doc<-flextable::body_add_flextable(doc,flextable::flextable(data.frame(Test="Synthetic",Value="Placeholder")))
path<-tempfile(fileext=".docx");print(doc,target=path)
parts<-unzip(path,list=TRUE)$Name;stopifnot("word/document.xml" %in% parts)
connection<-unz(path,"word/document.xml");xml<-paste(readLines(connection,warn=FALSE),collapse="");close(connection)
parsed<-xml2::read_xml(xml);stopifnot(length(xml2::xml_find_all(parsed,".//w:tbl",xml2::xml_ns(parsed)))==1L,grepl("Synthetic",xml,fixed=TRUE))
jsonlite::write_json(list(status="PASS",scope="Development-runtime editable Word table API smoke only",
 R=R.version.string,officer=as.character(packageVersion("officer")),flextable=as.character(packageVersion("flextable")),
 docx_sha256=web_sha(path),canonical_lock_acceptance=FALSE,R41_workflow_executed=FALSE,scientific_manuscript_regression=FALSE),
 "web/evidence/completion-001/development-docx.json",auto_unbox=TRUE,pretty=TRUE)
unlink(path)
cat("PASS: available development officer/flextable create an editable DOCX table; not canonical scientific-runtime or R/41 acceptance\n")
