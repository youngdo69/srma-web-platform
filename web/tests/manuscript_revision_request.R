source("web/load.R",encoding="UTF-8")

local({
  settings<-web_settings(data_root=tempfile("manuscript-request-"),backend="sqlite")
  on.exit(unlink(settings$data_root,recursive=TRUE),add=TRUE)
  con<-web_db(settings)
  on.exit(DBI::dbDisconnect(con),add=TRUE,after=FALSE)
  web_migrate(con,settings)
  web_local_identity(con)
  web_exec(con,"INSERT INTO external_identities VALUES('fixture-provider','fixture','local-researcher')")
  provider<-web_identity_provider("fixture-provider",function(credential)list(subject=credential))
  token<-web_sign_in(con,provider,"fixture")
  csrf<-"fixture-csrf"

  # Keep the test at the authenticated public request boundary.
  method<-web_request
  fixture<-new.env(parent=environment(method))
  environment(method)<-fixture
  fixture$web_revision_save<-function(con,settings,actor,project_id,version_id,expected_section_sha256,
                                      type,text,rationale,section="Statistical Analysis") {
    list(actor=actor,version=version_id,section=section)
  }
  args<-list(project_id="fixture-project",version_id="fixture-version",section="Discussion",
    expected_section_sha256=web_c2_hash("original"),type="EDITORIAL",text="Revised prose.",rationale="Clarify wording.")
  submit<-function(input=args,csrf_value=csrf,session=token)
    method(con,session,"manuscript_revision_save",input,csrf_value,csrf,settings)
  rejected<-function(expr)inherits(tryCatch(force(expr),error=identity),"srma_web_error")
  stopifnot(identical(submit(),list(actor="local-researcher",version="fixture-version",section="Discussion")),
    identical(submit(args[setdiff(names(args),"section")])$section,"Statistical Analysis"),
    rejected(submit(c(args,list(actor="other-user")))),rejected(submit(csrf_value="incorrect")),
    rejected(submit(session="invalid")))
  cat("Authenticated manuscript revision Section forwarding and request protections PASS\n")
})
