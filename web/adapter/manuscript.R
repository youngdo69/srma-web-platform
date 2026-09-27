# Canonical publication functions run in the existing isolated worker subprocess.
web_c2_canonical <- function(request,config,paths,root,input_manifest) {
 x<-web_c2_read(root);engine<-request$engine_root
 web_assert(web_c2_hash(x$source)==x$source_sha256&&web_c2_hash(x$source$references)==x$reference_sha256,"Manuscript source checksum mismatch.")
 for(a in x$source$artifacts)web_assert(web_sha(web_inside(file.path(root,a$path),root))==a$sha256,"Selected artifact checksum mismatch.")
 out<-file.path(root,"results/web_manuscript");unlink(out,recursive=TRUE);dir.create(out,recursive=TRUE)
 result<-list(version_id=request$version_id,manuscript_id=x$manuscript_id,source_sha256=x$source_sha256,reference_sha256=x$reference_sha256,evidence_version=x$evidence_version,analyses=as.list(names(x$source$analyses)),artifacts=x$source$artifacts,content_sha256=web_c2_hash(x$sections),job_id=request$job_id,engine=request$engine_version,created_at=web_now(),generator_sha256=web_sha(file.path(engine,"web/adapter/manuscript.R")))
 if(request$action=="generate_manuscript_section") {
  web_assert(nzchar(Sys.getenv("OPENAI_API_KEY")),"AI unavailable; use Human entry.","PROVIDER_UNAVAILABLE")
  e<-web_c2_defs(engine,39);e$model_name<-x$generation$model;e$openai_api_key<-Sys.getenv("OPENAI_API_KEY")
  # Canonical API helper writes raw responses only to private temporary space, destroyed on exit.
  private<-tempfile("manuscript-provider-");dir.create(private,mode="0700");on.exit(unlink(private,recursive=TRUE),add=TRUE);e$raw_root<-private
  k<-x$generation$section
  bounded<-list(section=k,title=x$spec$title,language=x$spec$language)
  if(k %in% c("Introduction","Discussion"))bounded$references<-lapply(x$source$references$records,function(r)r[intersect(names(r),c("pmid","title","abstract"))])
  if(k %in% c("Abstract","Methods","Results","Discussion","Limitations","Conclusions"))bounded$facts<-x$source$facts
  if(k=="Methods")bounded$methods<-lapply(x$source$analyses,function(a)list(role=a$plan$role,methods=a$plan$methods))
  web_assert(nchar(as.character(web_json(bounded)))<=100000,"Source package exceeds the bounded drafting budget. Use Human drafting.","USER_ACTION_REQUIRED")
  text<-e$call_openai("Draft only the requested section. Use only supplied evidence. Do not invent facts, references, author metadata or calculations. Use {{FACT:key}} for every numeric claim and [[PMID:id]] for supplied references. No raw numeric claims. Each literature paragraph must cite its supporting record. No hidden reasoning. Leave missing information [AUTHOR REQUIRED]. The author must review all text.",as.character(web_json(bounded)),"section",max_output_tokens=4000L)
  result$text<-web_c2_text(text,engine);result$section<-k;result$generation<-c(x$generation,list(at=web_now()))
 } else {
  rendered<-web_c2_render(x,engine);web_c2_text(rendered$text,engine);e<-web_c2_defs(engine,41)
  needed<-c("officer","flextable");web_assert(all(vapply(needed,requireNamespace,logical(1),quietly=TRUE)),"Editable Word dependencies unavailable.","DEPENDENCY_FAILED")
  doc<-officer::read_docx();styles<-e$available_paragraph_styles(doc);e$style_map<-list(title=e$pick_paragraph_style(styles,c("Title","graphic title","heading 1")),heading1=e$pick_paragraph_style(styles,c("heading 1")),heading2=e$pick_paragraph_style(styles,c("heading 2")),heading3=e$pick_paragraph_style(styles,c("heading 3")),normal=e$pick_paragraph_style(styles,"Normal"))
  for(block in e$parse_markdown_blocks(rendered$text))doc<-e$add_manuscript_block(doc,block)
  # Editable tables are a presentation view of approved registry values, never independent results.
  fmt<-web_c2_defs(engine,38)
  for(v in names(x$source$analyses)) {
   a<-x$source$analyses[[v]];tab<-data.frame(Statistic=c("Role","Outcome","Studies","Effect","Lower CI","Upper CI","P value","I²","Tau²"),Value=c(a$plan$role,a$plan$outcome,vapply(a$result$summary[c("studies","pooled_effect","lower_ci","upper_ci","p_value","i2","tau2")],fmt$format_num,character(1))))
   art<-Filter(function(t)t$type=="table"&&t$analysis_version==v,x$source$artifacts)[[1]];doc<-officer::body_add_par(doc,web_c2_artifact_labels(x)[[art$id]],style="heading 2");doc<-flextable::body_add_flextable(doc,e$make_word_table(tab))
  }
  # Selected canonical forest images are rendered from their verified PDF bytes, never redrawn from numbers.
  for(i in seq_along(x$source$artifacts)) {
   a<-x$source$artifacts[[i]];if(a$type!="figure")next
   web_assert(nzchar(Sys.which("pdftoppm")),"PDF renderer unavailable for the selected canonical figure.","DEPENDENCY_FAILED")
   png<-file.path(out,paste0("figure-",i,".png"));status<-system2(Sys.which("pdftoppm"),shQuote(c("-f","1","-singlefile","-scale-to","1600","-png",file.path(root,a$path),sub("[.]png$","",png))),stdout=FALSE,stderr=FALSE);web_assert(status==0L&&file.exists(png),"Canonical figure rendering failed.","DOCUMENT_GENERATION_FAILED")
   doc<-officer::body_add_par(doc,rendered$legends[i],style="heading 2");dims<-e$fit_word_image_dimensions(png,max_width=6,max_height=7);doc<-officer::body_add_img(doc,src=png,width=dims[["width"]],height=dims[["height"]])
  }
  dest<-file.path(out,"manuscript.docx");print(doc,target=dest);web_no_file_secrets(dest,engine)
  result$docx_sha256<-web_sha(dest);result$canonical_generator_sha256<-web_sha(list.files(file.path(engine,"R"),pattern="^41_",full.names=TRUE));result$precheck<-web_c2_check(x,engine)
  writeLines(rendered$text,file.path(out,"manuscript.md"));jsonlite::write_json(rendered$citations,file.path(out,"citation_map.json"),pretty=TRUE)
 }
 jsonlite::write_json(result,file.path(out,"result.json"),pretty=TRUE,auto_unbox=TRUE,null="null",digits=NA)
 outputs<-list.files(out,full.names=TRUE)
 web_result(request$action,request$project_id,request$job_id,"COMPLETE","Manuscript output generated. Explicit Human review is required; this is not final release.",request$engine_version,artifacts=lapply(outputs,function(p)list(path=p,logical_name=basename(p),type="manuscript")),provenance=list(version_id=request$version_id,source_sha256=x$source_sha256,reference_sha256=x$reference_sha256,R=R.version.string))
}
