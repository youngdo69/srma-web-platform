# TEST INPUT CONSTRUCTION before verification/finalization, not repair of approved science.
# R27 already supports study_label. Populate it in this structured source fixture before
# the normal Human edits/independent verification and real R26/R27 transformations.
x<-context()
for(i in seq_along(x$tables$Target_Outcome_Verification))x$tables$Target_Outcome_Verification[[i]]$study_label<-paste("Illustrative study",LETTERS[i])
root<-tempfile("c3-source-labels-");dir.create(root);web_copy_version(con,s,actor,v$project_id,v$version_id,root);web_b2_write(root,x)
v<-web_b2_save_snapshot(con,s,actor,v$project_id,v$version_id,root,x$stage);unlink(root,recursive=TRUE)
