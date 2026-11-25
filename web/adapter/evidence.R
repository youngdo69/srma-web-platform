# Canonical file-contract adapter. Never computes screening, dedup, effects or RoB scores.
web_b2_exports <- function(path,env,constants=character()) {
 for(expr in parse(path))if(is.call(expr)&&identical(expr[[1]],as.name("<-"))&&is.symbol(expr[[2]])) {
  key<-as.character(expr[[2]]);rhs<-expr[[3]]
  if(key %in% constants||(is.call(rhs)&&identical(rhs[[1]],as.name("function"))))eval(expr,env)
 }
 invisible(env)
}
web_b2_rows <- function(df) {
 if(!nrow(df))return(list())
 df[]<-lapply(df,function(v){v<-as.character(v);v[is.na(v)]<-"";v})
 lapply(seq_len(nrow(df)),function(i)c(as.list(df[i,,drop=FALSE]),list(.row_id=web_id(),.source="SYSTEM OUTPUT")))
}
web_b2_df <- function(rows) {
 if(!length(rows))return(data.frame())
 keys<-unique(unlist(lapply(rows,names)));keys<-keys[!startsWith(keys,".")]
 as.data.frame(setNames(lapply(keys,function(k)vapply(rows,function(x)as.character(x[[k]] %||b2% ""),character(1))),keys),stringsAsFactors=FALSE)
}
web_b2_book <- function(path,tables,start=3L) {
 dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE);wb<-openxlsx::createWorkbook()
 for(n in names(tables)){openxlsx::addWorksheet(wb,n);openxlsx::writeData(wb,n,web_b2_df(tables[[n]]),startRow=if(n %in% c("Reviewer_1","Reviewer_2"))4L else start)}
 openxlsx::saveWorkbook(wb,path,overwrite=TRUE)
}
# Compatibility adapter for R/21, which still validates every Exclude reason
# against config/project_config.yml. Register a TRANSPARENT policy-only marker
# in the current isolated Worker attempt, never in the user's project version
# or canonical source. The value denotes missing case-level reason, not proof
# that any particular study design is ineligible.
web_b2_worker_allow_policy_marker <- function(path,label) {
  web_assert(identical(label,web_b2_policy_exclusion_label()),
    "Unexpected bulk exclusion marker.","USER_ACTION_REQUIRED")
  cfg<-yaml::read_yaml(path)
  web_assert(is.list(cfg$screening) && length(cfg$screening$exclusion_reasons)>0L,
    "Project screening reasons are unavailable.","USER_ACTION_REQUIRED")
  reasons<-cfg$screening$exclusion_reasons
  structured<-is.list(reasons)&&all(vapply(reasons,is.list,logical(1)))
  if(structured){
    labels<-vapply(reasons,function(x)as.character(x$label %||b2% ""),character(1))
    if(label %in% labels)return(invisible(FALSE))
    codes<-vapply(reasons,function(x)as.character(x$code %||b2% ""),character(1))
    suffix<-1L
    while(paste0("WEBPOL",suffix) %in% codes)suffix<-suffix+1L
    reasons[[length(reasons)+1L]]<-list(code=paste0("WEBPOL",suffix),label=label,
      description="Researcher-approved screening policy; individual eligibility reason not assessed")
  } else if(is.character(reasons)) {
    if(label %in% reasons)return(invisible(FALSE))
    reasons<-c(reasons,label)
  } else {
    web_assert(FALSE,"Unsupported exclusion-reason config format.","USER_ACTION_REQUIRED")
  }
  cfg$screening$exclusion_reasons<-reasons
  yaml::write_yaml(cfg,path)
  verified<-yaml::read_yaml(path)$screening$exclusion_reasons
  labels<-if(structured)vapply(verified,function(x)as.character(x$label),character(1)) else as.character(verified)
  web_assert(sum(labels==label)==1L,"Private worker policy marker could not be validated.")
  invisible(TRUE)
}

# Stage the approved engine's file dependencies in the PRIVATE job attempt.
# A version-pinned project config takes precedence over the engine default:
# silently replacing it would make the scientific run differ from its input version.
web_b2_worker_stage_engine_files <- function(engine) {
  utils_dir <- file.path(engine,"R","utils")
  web_assert(dir.exists(utils_dir),
    "Approved engine utilities directory is missing.","SYSTEM_FAILED")
  utility_files <- list.files(utils_dir,recursive=TRUE,full.names=FALSE,
    include.dirs=FALSE)
  for(p in utility_files) {
    from <- file.path(utils_dir,p)
    to <- file.path("R","utils",p)
    dir.create(dirname(to),recursive=TRUE,showWarnings=FALSE)
    web_assert(isTRUE(file.copy(from,to,overwrite=TRUE)),
      paste("Unable to stage approved engine utility:",p),"SYSTEM_FAILED")
  }
  for(p in c("agent_screening.yml","data_extraction_schema.yml",
              "initialization_agent.yml")) {
    to <- file.path("config",p)
    if(file.exists(to))next # Keep the saved, versioned project-specific policy.
    from <- file.path(engine,"config",p)
    dir.create(dirname(to),recursive=TRUE,showWarnings=FALSE)
    web_assert(file.exists(from) && isTRUE(file.copy(from,to,overwrite=FALSE)),
      paste("Unable to stage approved scientific config:",p),"SYSTEM_FAILED")
  }
  task <- "TASK018_database_input_gate_hardening.R"
  from <- file.path(engine,"R",task)
  to <- file.path("R",task)
  dir.create(dirname(to),recursive=TRUE,showWarnings=FALSE)
  web_assert(file.exists(from) && isTRUE(file.copy(from,to,overwrite=TRUE)),
    "Unable to stage approved database-input gate dependency.","SYSTEM_FAILED")
  invisible(TRUE)
}

# The canonical dotenv loader expects a file, but the Web worker must obtain
# credentials exclusively from its inherited environment. Never read, overwrite,
# or delete an .env file that was already present in a job attempt.
web_b2_worker_empty_env_bridge <- function(root) {
  path <- file.path(root,".env")
  link <- Sys.readlink(path)
  web_assert(!file.exists(path) && !dir.exists(path) &&
    (length(link)!=1L || is.na(link) || !nzchar(link)),
    "Private Worker attempt contains a .env file. Keep secrets out of project snapshots; configure the Worker environment instead.",
    "USER_ACTION_REQUIRED")
  web_assert(isTRUE(file.create(path)),
    "Could not create the private provider environment bridge.","SYSTEM_FAILED")
  try(Sys.chmod(path,mode="0600"),silent=TRUE)
  path
}

web_b2_canonical <- function(request,config,paths,root,input_manifest) {
 source(file.path(request$engine_root,"web/R/evidence.R"))
 engine<-request$engine_root;x<-web_b2_packet(root);op<-request$action
 web_assert(op %in% names(web_b2_operations())&&x$stage==unname(web_b2_operations()[op]),"Evidence stage does not match operation.")
 needed<-c("openxlsx","readr","dplyr","tibble","stringr","tidyr","purrr")
 web_assert(all(vapply(needed,requireNamespace,logical(1),quietly=TRUE)),"Canonical workflow packages are unavailable.","DEPENDENCY_FAILED")
 # Preserve version-pinned project policies; stage only missing engine defaults.
 web_b2_worker_stage_engine_files(engine)
 run<-function(name){env<-new.env(parent=globalenv());sys.source(file.path(engine,"R",name),env);env}
 strip<-function(p)sub(paste0(root,"/"),"",p,fixed=TRUE)
 csv<-function(p)readr::read_csv(p,col_types=readr::cols(.default=readr::col_character()),show_col_types=FALSE)
 if(identical(op,"discover_pdf_results")) {
  source(file.path(engine,"web/adapter/pdf_results.R"),local=TRUE)
  x<-web_b2_pdf_discover(engine,root,x)
 } else if(op %in% names(web_b2_ai_operations())) {
  # Canonical AI scripts call load_project_env(".env"). In Web execution the
  # operator credential is already inherited through the process environment;
  # secrets must never be copied into an immutable snapshot or job workspace.
  # Create an empty private compatibility file so the canonical loader keeps
  # using the inherited environment variable without persisting the secret.
  env_bridge<-web_b2_worker_empty_env_bridge(root)
  on.exit(unlink(env_bridge),add=TRUE) # Remove ONLY the bridge created above.
  source(file.path(engine,"web/adapter/evidence_ai.R"))
   x<-web_b2_ai(op,engine,root,paths,x,run)
   if (identical(op,"recommend_screening"))
     x<-web_b2_bind_agent_reviewer1(x,root)
 } else if(op=="prepare_screening") {
  run("17_import_merge_deduplicate.R");env<-run("18_prepare_screening_workbook.R")
  x$tables<-list(Reviewer_1=web_b2_rows(env$reviewer_1_data),Reviewer_2=web_b2_rows(env$reviewer_2_data),Conflict_Resolution=web_b2_rows(env$conflict_data),Source_Summary=web_b2_rows(env$source_summary))
  x$choices<-list(decisions=as.list(env$screening_decisions %||b2% config$screening$decisions),reasons=as.list(env$exclusion_reason_table$label %||b2% vapply(config$screening$exclusion_reasons,`[[`,character(1),"label")))
  x$stage<-"SCREENING"
  } else if(op=="finalize_screening") {
   x<-web_b2_bind_agent_reviewer1(x,root)
   a<-x$tables$Reviewer_1;b<-x$tables$Reviewer_2
   z<-web_b2_selective_review_targets(x)
   imported<-x$screening_excel_review
   policy_ids<-as.character(unlist(imported$policy_excluded_ids %||b2% list(),use.names=FALSE))
   policy_count<-as.integer(imported$policy_auto_exclude_count %||b2% 0L)
    # RC28.6: validate saved AI Include-only design recommendations against the
    # frozen source. Neither keyword guesses nor an AI Unclear classification
    # may qualify as a non-comparative exclusion at finalization.
    needs_design <- as.character(imported$comparative_option %||b2% "PASS") %in%
      c("SINGLE_ARM","BOTH")
    design <- web_b2_study_design_validate(x,required=needs_design)
    authenticated_noncomparative <- design$ids[design$classes=="NON_COMPARATIVE"]
    if(needs_design) web_assert(
      identical(as.character(imported$design_result_sha256 %||b2% ""),
        digest::digest(design$rows,algo="sha256",serialize=TRUE)),
      "The imported Human Review is not bound to this design-classification version.",
      "USER_ACTION_REQUIRED")
   web_assert(!is.null(imported) && identical(imported$workflow,"SELECTIVE_HUMAN_FINAL_RC28") &&
     identical(as.integer(imported$target_count)+policy_count,as.integer(length(z$review_ids))) &&
     identical(as.integer(imported$ai_only_exclude_count),as.integer(sum(z$groups=="EXCLUDE"))) &&
     length(policy_ids)==policy_count && !anyDuplicated(policy_ids) &&
     all(policy_ids %in% z$review_ids),
     "Import and save a complete Human Reviewer 2 Excel with consistent comparative option provenance.","USER_ACTION_REQUIRED")
   hids<-vapply(b,function(r)as.character(r$screening_id %||b2% ""),character(1))
   for(id in z$ids) {
     ai<-a[[match(id,z$ids)]];hu<-b[[match(id,hids)]]
     group<-z$groups[[match(id,z$ids)]]
     web_assert(identical(ai$.actor,"AI_AGENT_REVIEWER_1"),"Unauthenticated AI decision.")
     if(group=="EXCLUDE") {
       web_assert(identical(hu$decision,"Exclude") && is.null(hu$.actor) &&
         identical(hu$.source,"AI-ONLY AUTO EXCLUSION") &&
         identical(as.character(hu$exclusion_reason %||b2% ""),as.character(ai$exclusion_reason %||b2% "")),
         "AI-only Exclude record has inconsistent provenance.","USER_ACTION_REQUIRED")
     } else if(id %in% policy_ids) {
       basis<-as.character(hu$.policy_basis %||b2% "")
       web_assert(is.null(hu$.actor) && identical(hu$.source,"POLICY-ONLY AUTO EXCLUSION") &&
         identical(hu$decision,"Exclude") &&
         identical(hu$.policy,imported$comparative_option) &&
          ((identical(basis,"AI_UNCLEAR") && identical(group,"UNCERTAIN")) ||
           (identical(basis,"SINGLE_ARM") && identical(group,"INCLUDE") &&
            id %in% authenticated_noncomparative)),
         "Policy exclusion lacks authenticated AI classification and policy provenance.","USER_ACTION_REQUIRED")
     } else web_assert(!is.null(hu$.actor) && nzchar(as.character(hu$.actor)) &&
       identical(hu$.source,"HUMAN DECISION — SELECTIVE EXCEL") &&
       as.character(hu$decision %||b2% "") %in% c("Include","Exclude"),
       "A required Human Reviewer 2 final decision is missing.","USER_ACTION_REQUIRED")
   }
   # Compatibility-only: R/21 recomputes disagreement itself. The Human
   # Reviewer 2 final judgment supplies legacy conflict cells automatically.
   # There is NO extra adjudicator UI/workbook or separate Human action.
   old<-x$tables$Conflict_Resolution
   if(is.null(old)||length(old)!=length(a)) {
     old<-lapply(a,function(r)list(screening_id=r$screening_id,title=r$title,
       year=r$year,journal=r$journal,reviewer_1_decision="",
       reviewer_2_decision="",agreement_status="",conflict_status="",
       final_decision="",final_exclusion_reason="",adjudicator="",
       resolution_notes="",resolved_at=""))
   }
   for(i in seq_along(a)) {
     ai<-a[[i]];hu<-b[[match(as.character(ai$screening_id),hids)]]
     mismatch<-!identical(as.character(ai$decision %||b2% ""),as.character(hu$decision %||b2% "")) ||
       (identical(hu$decision,"Exclude") &&
        !identical(as.character(ai$exclusion_reason %||b2% ""),as.character(hu$exclusion_reason %||b2% "")))
     old[[i]]$screening_id<-ai$screening_id
     old[[i]]$final_decision<-if(mismatch)hu$decision else ""
     old[[i]]$final_exclusion_reason<-if(mismatch && identical(hu$decision,"Exclude"))hu$exclusion_reason else ""
     policy_auto<-as.character(ai$screening_id) %in% policy_ids
     old[[i]]$adjudicator<-if(mismatch) {
       if(policy_auto)"RESEARCHER_SELECTED_BULK_POLICY" else
         as.character(hu$.actor %||b2% "AI_ONLY_AGREEMENT")
     } else ""
     old[[i]]$resolution_notes<-if(mismatch) {
       if(policy_auto)paste0("Researcher-approved bulk exclusion; individual reason not assessed. Justification: ",
         as.character(imported$comparative_rationale %||b2% "")) else
         "Final Human Reviewer 2 selective Excel decision; no separate adjudication"
     } else ""
     old[[i]]$resolved_at<-if(mismatch)as.character(hu$.timestamp %||b2% imported$human_timestamp) else ""
   }
   x$tables$Conflict_Resolution<-old
   # Only a private Worker job workspace is augmented; immutable snapshots and
   # the canonical project_config.yml in the user's repository are untouched.
   uses_marker<-any(vapply(policy_ids,function(id) {
     hu<-b[[match(id,hids)]]
     identical(as.character(hu$exclusion_reason %||b2% ""),
               web_b2_policy_exclusion_label())
   },logical(1)))
   if(uses_marker)web_b2_worker_allow_policy_marker(
     "config/project_config.yml",web_b2_policy_exclusion_label())
  ac<-yaml::read_yaml("config/agent_screening.yml")$agent_screening
  book<-file.path(paths$screening_results_directory,paste0(ac$output$workbook_basename,".xlsx"))
  tables<-x$tables[c("Reviewer_1","Reviewer_2","Conflict_Resolution")]
  tables$Reviewer_2<-b[match(z$ids,hids)] # R/21 canonical order alignment by immutable ID.
  tables$Source_Summary<-x$tables$Source_Summary
  wb<-openxlsx::loadWorkbook(paths$screening_workbook)
  for(t in names(tables))openxlsx::writeData(wb,t,web_b2_df(tables[[t]]),startRow=if(t %in% c("Reviewer_1","Reviewer_2"))4L else 3L)
  openxlsx::saveWorkbook(wb,book,overwrite=TRUE)
  screened<-run("21_finalize_title_abstract_screening.R")
  readr::write_csv(data.frame(screening_id=z$ids,
    review_route=ifelse(z$groups=="EXCLUDE","AI_ONLY_EXCLUDE",
      ifelse(z$ids %in% policy_ids,"RESEARCHER_OPTION_AUTO_EXCLUDE","HUMAN_REVIEWER_2_FINAL")),
    option_basis=vapply(z$ids,function(id){j<-match(id,hids);
      as.character(b[[j]]$.policy_basis %||b2% "")},character(1)),
    comparative_option=rep(as.character(imported$comparative_option %||b2% "PASS"),length(z$ids)),
    policy_reason_status=vapply(z$ids,function(id){j<-match(id,hids);
      as.character(b[[j]]$.policy_reason_status %||b2% "")},character(1)),
    researcher_justification=vapply(z$ids,function(id){j<-match(id,hids);
      as.character(b[[j]]$.policy_justification %||b2% "")},character(1)),
    ai_model=vapply(a,function(r)as.character(r$.model %||b2% ""),character(1)),
    ai_decision=vapply(a,function(r)as.character(r$decision %||b2% ""),character(1)),
    human_actor=vapply(a,function(r){j<-match(as.character(r$screening_id),hids);
      as.character(b[[j]]$.actor %||b2% "")},character(1)),
    final_decision=as.character(screened$final_screening$final_decision[match(z$ids,
      as.character(screened$final_screening$screening_id))]),stringsAsFactors=FALSE),
    file.path(paths$processed_screening_directory,"reviewer2_selective_provenance.csv"))
  web_assert(all(screened$final_screening$final_decision %in% c("Include","Exclude")),"Resolve uncertain title/abstract decisions before finalizing the screening set.","USER_ACTION_REQUIRED")
  env<-run("22_prepare_full_text_registry.R")
  handoff<-csv(file.path(paths$processed_screening_directory,"included_for_full_text_review.csv"))
  defs<-web_b2_exports(file.path(engine,"R/24_finalize_included_studies.R"),new.env(parent=globalenv()),"required_author_columns")
  author<-as.data.frame(setNames(lapply(defs$required_author_columns,function(n)if(n %in% names(handoff))handoff[[n]] else rep("",nrow(handoff))),defs$required_author_columns))
  x$tables$Author_Final_Review<-web_b2_rows(author)
  for(i in seq_along(x$tables$Author_Final_Review)) {
   row<-x$tables$Author_Final_Review[[i]];row$study_id<-"";row$retrieval_status<-"Missing";row$pdf_status<-"Missing"
   row$.pdf_relative<-file.path("data/raw/full_text/pdfs",paste0(env$sanitize_screening_id_for_filename(row$screening_id),".pdf"))
   x$tables$Author_Final_Review[[i]]<-row
  }
  x$stage<-"FULLTEXT"
 } else if(op=="register_fulltext") {
  run("22_prepare_full_text_registry.R")
 } else if(op=="finalize_included_studies") {
  rows<-x$tables$Author_Final_Review
  web_assert(length(rows)>0&&all(vapply(rows,function(a)!is.null(a$.actor)&&nzchar(a$author_final_decision)&&nzchar(a$study_id),logical(1))),"Record explicit eligibility decisions and report-to-study links for every report.")
  for(i in seq_along(rows))if(nzchar(rows[[i]]$pdf_path))rows[[i]]$pdf_path<-web_inside(file.path(root,rows[[i]]$.pdf_relative),root)
  book<-"results/full_text/full_text_eligibility_agent.xlsx"
  web_b2_book(book,list(Instructions=list(list(note="Human-entered review; blank agent fields are not AI output")),Agent_Review=list(list(note="No AI recommendation requested")),Author_Final_Review=rows,Validation_Lists=list(list(note="Canonical configuration defines exclusion reasons"))))
  run("24_finalize_included_studies.R")
  mapping<-web_b2_df(rows)[,c("screening_id","study_id"),drop=FALSE];readr::write_csv(mapping,"data/processed/full_text/report_study_crosswalk.csv")
  x$included_reports<-as.list(csv("data/processed/full_text/included_studies.csv")$screening_id)
  x$stage<-"INCLUDED"
 } else if(op=="prepare_extraction") {
  schema<-yaml::read_yaml("config/data_extraction_schema.yml");inc<-csv("data/processed/full_text/included_studies.csv")
  fields<-unlist(schema$study_characteristics$fields);outfields<-unlist(schema$outcome_data$fields)
  study<-lapply(seq_len(nrow(inc)),function(i)c(list(screening_id=inc$screening_id[i]),setNames(as.list(rep("",length(fields))),fields),list(source_location="",source_evidence="",verification_status="Needs clarification",verifier="",verified_at="",.row_id=web_id(),.source="HUMAN ENTRY REQUIRED")))
  names_outcomes<-unique(as.character(unlist(config$research_question$outcomes,use.names=FALSE)));web_assert(length(names_outcomes)>0,"Define project outcomes first.")
  outcomes<-list()
  for(id in inc$screening_id)for(out in names_outcomes){row<-setNames(as.list(rep("",length(outfields))),outfields);row$outcome_name<-out
   row<-c(list(screening_id=id,outcome_row_id=paste0(id,"_",length(outcomes)+1L)),row,list(analysis_group="",meta_analysis_role="",parent_analysis_group="",human_include_in_planned_analysis="No",verification_status="Needs clarification",verifier="",verified_at="",value_status="MISSING",transformation_rule="",raw_value="",.row_id=web_id(),.source="HUMAN ENTRY REQUIRED"));outcomes[[length(outcomes)+1L]]<-row}
  x$tables$Target_Study_Verification<-study;x$tables$Target_Outcome_Verification<-outcomes
  x$editable<-list(Target_Study_Verification=as.list(c(fields,"source_location","source_evidence")),Target_Outcome_Verification=as.list(c(outfields,"analysis_group","meta_analysis_role","parent_analysis_group","human_include_in_planned_analysis","value_status","transformation_rule","raw_value")))
  for(t in c("Target_Study_Verification","Target_Outcome_Verification"))for(i in seq_along(x$tables[[t]])) {
   r<-x$tables[[t]][[i]];src<-Filter(function(a)identical(a$screening_id,r$screening_id),x$tables$Author_Final_Review)[[1]]
   r$.pdf_sha256<-src$.pdf_sha256;r$study_id<-src$study_id;x$tables[[t]][[i]]<-r
  }
  x$stage<-"EXTRACTION"
 } else if(op=="prepare_targeted_verification") {
  # Run the already-audited Golden Scientific Engine, never duplicate its
  # study-set selection, endpoint mapping or verification-plan algorithms.
  advisory<-"results/extraction/research_scope_advisory/research_scope_advisory.json"
  web_assert(file.exists(advisory),
    "Run and save R/25A Advisory before preparing R/25B targeted verification.","USER_ACTION_REQUIRED")
  web_assert(file.exists("data/processed/extraction/study_characteristics_agent.csv") &&
    file.exists("data/processed/extraction/outcome_data_agent.csv"),
    "Saved R/25 extraction outputs are required.","USER_ACTION_REQUIRED")
  existing<-unlist(lapply(x$tables[c("Target_Study_Verification","Target_Outcome_Verification")],
    function(rows)vapply(rows %||b2% list(),function(a)!is.null(a$.actor)||!is.null(a$.verified_by),logical(1))),use.names=FALSE)
  web_assert(!any(existing),"R/25B must not replace an existing Human entry.","USER_ACTION_REQUIRED")
  run("25B_prepare_targeted_human_verification.R")
  directory<-"results/extraction/targeted_human_verification"
  files<-list(Target_Study_Verification="target_study_data_agent.csv",
    Target_Outcome_Verification="target_outcome_data_agent.csv",
    Verification_Plan="targeted_verification_plan.csv",
    Audit_Checks="target_audit_checks.csv",
    Included_Not_Targeted="included_not_targeted_for_verification.csv")
  for(t in names(files)) {
    p<-file.path(directory,files[[t]])
    web_assert(file.exists(p),paste("R/25B did not generate",files[[t]]),"SYSTEM_FAILED")
    df<-readr::read_csv(p,show_col_types=FALSE,
      col_types=readr::cols(.default=readr::col_character()))
    x$tables[[t]]<-web_b2_rows(df)
  }
  for(t in c("Target_Study_Verification","Target_Outcome_Verification")) {
    for(i in seq_along(x$tables[[t]])) {
      r<-x$tables[[t]][[i]]
      matches<-Filter(function(a)identical(a$screening_id,r$screening_id),x$tables$Author_Final_Review)
      web_assert(length(matches)==1L,"Targeted report has no unique source PDF mapping.","USER_ACTION_REQUIRED")
      r$.pdf_sha256<-matches[[1L]]$.pdf_sha256
      r$study_id<-matches[[1L]]$study_id
      # PDF evidence location is a Human check, not an inferred AI citation.
      if(is.null(r$source_location))r$source_location<-""
      if(is.null(r$source_evidence))r$source_evidence<-""
      if(t=="Target_Outcome_Verification")for(k in c("analysis_group","meta_analysis_role","parent_analysis_group"))
        if(is.null(r[[k]]))r[[k]]<-""
      r$.source<-"AI RECOMMENDATION";r$.verification<-"UNVERIFIED"
      x$tables[[t]][[i]]<-r
    }
  }
  study_fields<-c("source_location","source_evidence",
    "human_verified_study_design","human_verified_analysis_population",
    "human_verified_surgery_or_procedure","human_verified_intervention_or_exposure",
    "human_verified_exposure_timing","human_verified_comparator","human_study_notes")
  outcome_fields<-c("human_include_in_planned_analysis","analysis_group","meta_analysis_role",
    "parent_analysis_group","source_location","source_evidence",
    "human_verified_outcome_definition","human_verified_timepoint","human_verified_data_type",
    "human_verified_intervention_events","human_verified_intervention_total",
    "human_verified_comparator_events","human_verified_comparator_total",
    "human_verified_effect_measure","human_verified_effect_estimate",
    "human_verified_lower_ci","human_verified_upper_ci","human_verified_adjusted",
    "human_verified_adjustment_variables","human_verified_source_location","human_outcome_notes")
  # Editable human-only overlays; raw Agent scientific values remain read-only.
  x$editable<-list(Target_Study_Verification=as.list(study_fields),
    Target_Outcome_Verification=as.list(outcome_fields))
  x$targeted_verification_source="CANONICAL_R25B"
  x$history<-c(x$history,list(list(event="R25B_TARGETED_VERIFICATION_PREPARED",
    at=web_now(),source="Golden Baseline R/25B",human_approval=FALSE)))
 } else if(op=="finalize_extraction") {
  if(!is.null(x$result_inventory_selection)) {
    web_assert(identical(x$human_outcome_harmonization$status,"HUMAN_CONFIRMED"),
      "Human Outcome Harmonization is required before R/26.","USER_ACTION_REQUIRED")
    # Exact source bindings must remain intact in this immutable Evidence Version.
    selected<-web_b2_result_selected_groups(x)
    mapped<-x$human_outcome_harmonization$mappings %||b2% list()
    web_assert(length(mapped)==length(selected)&&
      setequal(vapply(mapped,`[[`,character(1),"group_id"),names(selected)),
      "Harmonized analysis groups are stale or incomplete.","USER_ACTION_REQUIRED")
    mapped_source_ids<-vapply(mapped,`[[`,character(1),"source_row_id")
    result_rows<-Filter(function(r)isTRUE(r$.web_harmonized_result),
      x$tables$Target_Outcome_Verification %||b2% list())
    bound_source_ids<-vapply(result_rows,function(r)as.character(r$.source_ai_row_id %||b2% ""),character(1))
    web_assert(length(result_rows)==length(mapped)&&setequal(mapped_source_ids,bound_source_ids),
      "The source-to-Human-result bindings do not match the confirmed harmonization.",
      "USER_ACTION_REQUIRED")
  }
  # This runs in a PRIVATE Worker attempt. Original saved AI and Human rows stay
  # untouched in the immutable Evidence packet and its parent versions.
  ids<-web_b2_final_active_reports(x,TRUE)
  studies<-web_b2_final_filter_rows(x,x$tables$Target_Study_Verification)
  outcomes<-web_b2_final_filter_rows(x,x$tables$Target_Outcome_Verification)
  # RC28.19 Step4: apply Human synthesis choices ONLY in private Worker copy.
  # Narrative and unresolved groups are not quantitative R/26 inputs, yet their
  # immutable source and Human verification records remain preserved.
  if(identical(x$synthesis_plan$status,"HUMAN_CONFIRMED")) {
    plan<-x$synthesis_plan$decisions %||b2% list()
    keys<-vapply(plan,function(z)as.character(z$analysis_group),character(1))
    web_assert(!anyDuplicated(keys),"Duplicate synthesis decision.","USER_ACTION_REQUIRED")
    selected<-web_b2_result_selected_groups(x)
    mapped<-x$human_outcome_harmonization$mappings %||b2% list()
    web_assert(length(mapped)==length(selected)&&
      setequal(keys,unique(vapply(mapped,function(m)as.character(m$analysis_group),character(1)))),
      "Synthesis plan does not cover all harmonized source groups.","USER_ACTION_REQUIRED")
    modes<-stats::setNames(vapply(plan,function(z)as.character(z$mode),character(1)),keys)
    outcomes<-lapply(outcomes,function(r){
      if(isTRUE(r$.web_harmonized_result)) {
        group<-as.character(r$analysis_group %||b2% "")
        web_assert(group %in% keys,"Harmonized row lacks synthesis plan.","USER_ACTION_REQUIRED")
        if(!identical(modes[[group]],"QUANTITATIVE_CANDIDATE")) {
          r$human_include_in_planned_analysis<-"No"
          r$meta_analysis_role<-"Narrative only"
        }
      }
      r
    })
  }
  web_assert(length(studies)>0L && length(outcomes)>0L,
    "Final Study Selection has no extraction rows to finalize.","USER_ACTION_REQUIRED")
  # Constrain canonical full-text cohort inputs in this private job copy.
  inc_path<-"data/processed/full_text/included_studies.csv"
  web_assert(file.exists(inc_path),"Canonical included-studies input is missing.","USER_ACTION_REQUIRED")
  inc<-readr::read_csv(inc_path,show_col_types=FALSE)
  web_assert("screening_id" %in% names(inc),"Invalid included-studies input.","USER_ACTION_REQUIRED")
  inc<-inc[as.character(inc$screening_id) %in% ids,,drop=FALSE]
  web_assert(length(unique(as.character(inc$screening_id)))==length(ids) &&
    !anyDuplicated(as.character(inc$screening_id)),
    "Final study selection does not match canonical included-studies rows.","USER_ACTION_REQUIRED")
  readr::write_csv(inc,inc_path)
  cross_path<-"data/processed/full_text/report_study_crosswalk.csv"
  if(file.exists(cross_path)) {
    cross<-readr::read_csv(cross_path,show_col_types=FALSE)
    web_assert("screening_id" %in% names(cross),"Invalid report-study crosswalk.","USER_ACTION_REQUIRED")
    readr::write_csv(cross[as.character(cross$screening_id) %in% ids,,drop=FALSE],cross_path)
  }
  # No unselected row may be written to the R/26 input workbook.
  web_assert(all(vapply(c(studies,outcomes),function(a)
    as.character(a$screening_id %||b2% "") %in% ids,logical(1))),
    "An excluded report reached canonical finalization.","SYSTEM_FAILED")
  web_assert(all(vapply(c(studies,outcomes),function(a)a$screening_id %in% ids,logical(1))),"Extraction includes an unapproved report.")
  for(row in c(studies,outcomes)) {
   needed<-!is.null(row$study_design)||identical(row$human_include_in_planned_analysis,"Yes")
   if(needed)web_assert(!is.null(row$.verified_by)&&row$verification_status %in% c("Verified","Corrected"),"Independent Human verification is incomplete.")
   if(!is.null(row$value_status)){
    web_assert(row$value_status %in% c("REPORTED","DERIVED","CALCULATED","IMPUTED","MISSING","NOT APPLICABLE"),"Select an explicit extracted-value status.")
    if(row$value_status %in% c("DERIVED","CALCULATED","IMPUTED"))web_assert(nzchar(row$raw_value)&&nzchar(row$transformation_rule),"Preserve the raw value and transformation rule.")
   }
  }
  # Human-selected analysis roles feed the unchanged canonical validator. No model is fitted.
  book<-"results/extraction/targeted_human_verification/targeted_human_verification.xlsx"
  if(identical(x$targeted_verification_source,"CANONICAL_R25B"))
    web_assert(file.exists(book),"R/25B source workbook is missing from this Evidence Version.","USER_ACTION_REQUIRED")
  # REBUILD the private finalization workbook, never overwrite only the first N
  # rows of a larger R/25B sheet (stale excluded rows could survive otherwise).
  work_tables<-list(Target_Study_Verification=studies,Target_Outcome_Verification=outcomes)
  checks<-x$tables$Audit_Checks %||b2% list()
  if(length(checks) && all(vapply(checks,function(r)
       is.list(r) && !is.null(r$screening_id),logical(1))))
    work_tables$Audit_Checks<-web_b2_final_filter_rows(x,checks)
  web_b2_book(book,work_tables)
  run("26_finalize_human_verified_extraction.R")
  # Private Worker output for the NEXT stage uses the same final study set.
  # Parent immutable snapshots retain every original AI/Human row for audit.
  x$included_reports<-as.list(ids)
  scoped_tables<-c("Target_Study_Verification","Target_Outcome_Verification",
    "AI_outcome_data","AI_study_characteristics","AI_group_characteristics",
    "Verification_Plan","Audit_Checks","Included_Not_Targeted",
    "Advisory_Meta_Analysis_Sets","Advisory_Outcome_Harmonization","Advisory_Study_Roles")
  for(t in intersect(scoped_tables,names(x$tables))) {
    records<-x$tables[[t]]
    if(length(records) && all(vapply(records,function(r)
      is.list(r) && !is.null(r$screening_id),logical(1))))
      x$tables[[t]]<-Filter(function(r)r$screening_id %in% ids,records)
  }
  x$stage<-"VERIFIED"
 } else if(op=="prepare_risk_of_bias") {
  Sys.setenv(SRMA_ROB_AGENT_RUN="false")
  run("36_generate_risk_of_bias.R")
  book<-"results/risk_of_bias/risk_of_bias_author_editable.xlsx"
  for(t in c("Study_Result_Manifest","Signalling_Questions","Domain_Review","Overall_Review"))x$tables[[t]]<-web_b2_rows(openxlsx::read.xlsx(book,sheet=t,check.names=FALSE))
  for(i in seq_along(x$tables$Signalling_Questions))for(n in c("human_response","human_evidence_location","human_rationale"))x$tables$Signalling_Questions[[i]][[n]]<-""
  x$stage<-"ROB"
 } else if(op=="finalize_evidence") {
  for(t in c("Domain_Review","Overall_Review"))web_assert(all(vapply(x$tables[[t]],function(a)!is.null(a$.actor),logical(1))),"Every RoB judgment requires explicit Human review.")
  for(row in x$tables$Domain_Review)web_assert(nzchar(row$author_domain_rationale),"Record a rationale for every RoB domain.")
  for(row in x$tables$Overall_Review)web_assert(nzchar(row$author_final_overall_rationale),"Record the overall RoB rationale.")
  book<-"results/risk_of_bias/risk_of_bias_author_editable.xlsx";web_b2_book(book,x$tables[c("Study_Result_Manifest","Signalling_Questions","Domain_Review","Overall_Review")],1L)
  e<-web_b2_exports(file.path(engine,"R/37_finalize_risk_of_bias_and_generate_figures.R"),new.env(parent=globalenv()),c("rob2_domains","robins_domains"))
  final<-e$import_and_finalize_author_workbook(book);e$validate_tool_domains(final,"RoB 2",e$rob2_domains);e$validate_tool_domains(final,"ROBINS-I",e$robins_domains)
  readr::write_csv(final,"results/risk_of_bias/rob_final_combined.csv")
  x$stage<-"READY"
  jsonlite::write_json(list(status="human_verified_dataset",project_version=request$version_id,project=request$project_id,input_sha256=input_manifest,analysis_ready_manifest="data/processed/meta_analysis_ready/meta_analysis_manifest.csv",included="data/processed/full_text/included_studies.csv",crosswalk="data/processed/full_text/report_study_crosswalk.csv",rob="results/risk_of_bias/rob_final_combined.csv",engine=request$engine_version,source_job=request$job_id,unresolved_blocking=list(),not_selected=Filter(function(a)!identical(a$human_include_in_planned_analysis,"Yes"),x$tables$Target_Outcome_Verification)),"results/analysis_ready_evidence.json",auto_unbox=TRUE,pretty=TRUE)
 }
 # Absolute canonical PDF locations are ephemeral; retain canonical relative identity in the packet.
 for(t in names(x$tables))for(i in seq_along(x$tables[[t]]))for(n in names(x$tables[[t]][[i]]))if(is.character(x$tables[[t]][[i]][[n]]))x$tables[[t]][[i]][[n]]<-strip(x$tables[[t]][[i]][[n]])
 web_b2_write(root,x)
 outputs<-list.files(root,recursive=TRUE,full.names=TRUE)
 outputs<-outputs[!dir.exists(outputs)];rel<-vapply(outputs,strip,character(1))
 # Canonical scripts copied into jobs and provider diagnostic/prompt material are never published.
 keep<-grepl("^(data/raw/full_text/pdfs/|data/processed/|data/screening/|results/|config/generated/web_evidence[.]json$)",rel)&!grepl("(prompt|jsonl|raw_response|checkpoint)",rel,ignore.case=TRUE)
 outputs<-outputs[keep]
 web_result(op,request$project_id,request$job_id,"COMPLETE","Canonical processing finished. Save its results as a new evidence version, then review the next Human decision.",request$engine_version,
  artifacts=lapply(outputs,function(p)list(path=web_inside(p,root),logical_name=basename(p),type="evidence")),provenance=list(stage=x$stage,R=R.version.string,source_kind="SYSTEM OUTPUT",input_sha256=input_manifest,project_version=request$version_id))
}
