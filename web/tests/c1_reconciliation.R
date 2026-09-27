# Runs after real B2 fixture; no fabricated evidence database/gate state.
catlog<-web_c1_catalog(con,s,actor,v$project_id,v$version_id);print(catlog$groups)
ready<-v;m<-catlog$groups[1,,drop=FALSE]
x<-web_b2_context(con,s,actor,v$project_id,v$version_id)$packet
rows<-Filter(function(a)identical(a$human_include_in_planned_analysis,"Yes"),x$tables$Target_Outcome_Verification)
spec<-list(group=m$analysis_group,row_ids=as.list(vapply(rows,`[[`,character(1),"outcome_row_id")),role="PRIMARY",outcome=m$outcome,definition="Explicitly synthetic event at 30 days",timepoint=m$timepoint,design=m$study_design,population="Synthetic eligible randomized participants",estimate_basis="RAW",methods=web_c1_methods(),exclusions=list(),rationale="Prespecified synthetic canonical integration fixture",parent_version=NULL,diagnostics=TRUE,exploratory=FALSE,moderators=list())
plan<-web_c1_plan(con,s,actor,v$project_id,v$version_id,spec)
reject(web_submit(con,s,actor,plan$project_id,plan$version_id,"run_meta_analysis"))
g<-web_query(con,"SELECT id FROM gates WHERE version_id=?v AND gate_type='analysis_plan'",v=plan$version_id)$id
web_decide_gate(con,actor,plan$project_id,g,"APPROVED","Reviewed all canonical settings and eligible studies")
j<-web_submit(con,s,actor,plan$project_id,plan$version_id,"run_meta_analysis")
stopifnot(identical(j,web_submit(con,s,actor,plan$project_id,plan$version_id,"run_meta_analysis")))
web_worker_once(con,s);job<-web_query(con,"SELECT * FROM jobs WHERE id=?j",j=j)
if(job$state!="COMPLETE"){print(web_parse(job$result_json));stop("C1 engine failed")}
res<-web_c1_accept(con,s,actor,plan$project_id,plan$version_id,j)
x<-web_c1_view(con,s,actor,res$project_id,res$version_id);stopifnot(x$stage=="RESULT",x$result$consistency$status=="PASS")
ref<-jsonlite::read_json("web/tests/c1_reference.json");for(k in names(ref$expected))stopifnot(isTRUE(all.equal(as.numeric(x$result$summary[[k]]),as.numeric(ref$expected[[k]]),tolerance=1e-10)))
cat("PASS: frozen synthetic canonical-engine numeric reference\n")
approved<-web_c1_approve(con,s,actor,res$project_id,res$version_id,TRUE,"Confirmed study membership, outcome/timepoint, warnings and PRIMARY role")
stopifnot(web_c1_view(con,s,actor,approved$project_id,approved$version_id)$stage=="APPROVED")
saveRDS(list(settings=s,ready=ready,plan=plan,result=res,approved=approved,spec=spec,job=j),file.path(s$data_root,"c1-result.rds"))
cat("PASS: canonical primary model -> CSV/PDF/XLSX consistency -> Human approved immutable result\n")

source("web/tests/c1_extended.R")
