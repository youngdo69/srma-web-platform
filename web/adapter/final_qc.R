web_c3_canonical <- function(request,config,paths,root,input_manifest) {
 x<-web_c2_read(root);proof<-jsonlite::read_json(file.path(root,"config/generated/final_qc_input.json"),simplifyVector=FALSE)
 result<-web_c3_checks(x,proof,root,request$engine_root);result$job_id<-request$job_id;result$version_id<-request$version_id;result$engine<-request$engine_version;result$at<-web_now()
 out<-file.path(root,"results/web_final_qc");dir.create(out,recursive=TRUE,showWarnings=FALSE);dest<-file.path(out,"final_qc.json")
 jsonlite::write_json(result,dest,pretty=TRUE,auto_unbox=TRUE,null="null",digits=NA)
 web_result(request$action,request$project_id,request$job_id,"COMPLETE","Final QC inspection finished. Review findings and external gates; no release is implied.",request$engine_version,artifacts=list(list(path=dest,logical_name="final_qc.json",type="final_qc")),provenance=list(version_id=request$version_id,proof_sha256=result$proof_sha256,R=R.version.string))
}
