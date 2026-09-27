# Actual Shiny dispatch -> canonical manual preparation -> review -> activation.
source("web/tests/b1_ui.R")
con<-web_db(s)
projects<-web_query(con,"SELECT * FROM projects WHERE canonical_id='synthetic-ui'")
versions<-web_query(con,"SELECT id FROM project_versions WHERE project_id=?p ORDER BY version DESC",p=projects$id)
terms<-list(project=list(id="synthetic-ui",title="Synthetic UI review",short_name="Synthetic"),
 search=list(concepts=lapply(c("population","intervention"),function(x)list(concept_id=x,label=x,role=x,required=TRUE,free_text_terms=list(x),
 abbreviations=list(),spelling_variants=list(),controlled_vocabulary=list(pubmed_mesh_candidates=list(),embase_emtree_candidates=list()))),
 combination=list(operator_between_concepts="AND",notes="Synthetic explicit UI test"),outcome_terms_policy="Human provided"),
 validation=list(known_relevant_ids=list(),controlled_vocabulary_requires_verification=TRUE))
shiny::testServer(web_app(s),{
 session$setInputs(credential=login_token,login=1);session$flushReact()
 session$setInputs(csrf=csrf,project=projects$id,version=versions$id[1],manual_terms=yaml::as.yaml(terms),save_terms=1);session$flushReact()
 stopifnot(grepl("Human-provided draft saved",output$notice))
 draft<-web_query(con,"SELECT version_id FROM version_origins WHERE project_id=?p AND kind='MANUAL_DRAFT'",p=projects$id)$version_id
 session$setInputs(version=draft,draft_job="");session$flushReact()
 session$setInputs(load_draft=1);session$flushReact()
 stopifnot(grepl("Proposed configuration",output$canonical_review$html),grepl("synthetic-ui",output$canonical_review$html))
 session$setInputs(approve_canonical=FALSE,activation_rationale="Explicit synthetic human UI review",activate_canonical=1);session$flushReact()
 stopifnot(nrow(web_query(con,"SELECT version_id FROM version_origins WHERE project_id=?p AND kind='ACTIVATED'",p=projects$id))==0L)
 session$setInputs(approve_canonical=TRUE,activate_canonical=2);session$flushReact()
 if(!grepl("Approved canonical inputs saved",output$notice))print(output$notice)
 stopifnot(grepl("Approved canonical inputs saved",output$notice))
 active<-web_query(con,"SELECT version_id FROM version_origins WHERE project_id=?p AND kind='ACTIVATED'",p=projects$id)$version_id
 stopifnot(length(active)==1L,active!=draft)
 session$setInputs(version=active,generate_search=1);session$flushReact()
 stopifnot(grepl("provider credential",output$notice),nrow(web_query(con,"SELECT id FROM jobs"))==0L)
})
DBI::dbDisconnect(con)
cat("PASS: UI manual concept save, canonical review, explicit confirmation, new approved version, credential fail-closed dispatch\n")
