# Version-bound orchestration. Scientific preparation/decisions remain canonical.
web_pipeline_operations <- function() c("initialize_project","generate_search","retrieve_pubmed")
web_provider_ready <- function(settings) {
  p <- yaml::read_yaml(file.path(settings$engine_root,"config/initialization_agent.yml"))$initialization_agent$provider
  key_name <- if (is.character(p$api_key_env) && length(p$api_key_env) == 1L) trimws(p$api_key_env) else ""
  if (!nzchar(key_name) || !grepl("^[A-Za-z_][A-Za-z0-9_]*$", key_name)) return(FALSE)

  # Match the canonical START_HERE/operator behavior without copying .env into
  # project versions or job workspaces.  Prefer an already configured process
  # environment; if absent, load the operator-controlled engine-root .env.
  # Secret values are never returned, logged, written to manifests, or exposed
  # through the UI.
  if (!nzchar(Sys.getenv(key_name, unset = ""))) {
    env_file <- file.path(settings$engine_root, ".env")
    if (file.exists(env_file)) {
      if (requireNamespace("dotenv", quietly = TRUE)) {
        try(dotenv::load_dot_env(file = env_file), silent = TRUE)
      } else {
        lines <- tryCatch(readLines(env_file, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
        key_re <- paste0("^[[:space:]]*(?:export[[:space:]]+)?", key_name, "[[:space:]]*=")
        hit <- grep(key_re, lines, perl = TRUE)
        if (length(hit)) {
          value <- sub(key_re, "", lines[[hit[[length(hit)]]]], perl = TRUE)
          value <- trimws(value)
          if (nchar(value) >= 2L && ((startsWith(value, '"') && endsWith(value, '"')) || (startsWith(value, "'") && endsWith(value, "'")))) {
            value <- substr(value, 2L, nchar(value) - 1L)
          }
          if (nzchar(value)) do.call(Sys.setenv, setNames(list(value), key_name))
        }
      }
    }
  }

  nzchar(Sys.getenv(key_name, unset = ""))
}
web_pipeline_prerequisites <- function(settings, operation) {
  if (identical(operation, "prepare_prospero")) {
    web_science_settings(settings)
  }
  if (operation %in% web_pipeline_operations()) {
    web_assert(web_provider_ready(settings),
      "This step requires an operator-configured provider credential. Your work is saved; manual concept review is available.","USER_ACTION_REQUIRED")
    web_science_settings(settings)
  }
  invisible(TRUE)
}
web_copy_version <- function(con,settings,actor,project_id,version_id,root,core_only=FALSE) {
  v <- web_version_root(con,settings,actor,project_id,version_id)
  names <- names(v$manifest)
  if(core_only) {
    config_file <- file.path(v$root, "config/project_config.yml")
    seed_file <- file.path(v$root, "config/project_seed.yml")
    if (file.exists(config_file)) {
      cfg <- yaml::read_yaml(config_file)
      names <- intersect(names, c("config/project_config.yml", cfg$paths$search_terms_file))
    } else if (file.exists(seed_file)) {
      names <- intersect(names, "config/project_seed.yml")
    } else {
      web_error("VALIDATION_FAILED", "This saved version has neither a canonical configuration nor a project seed.")
    }
  }
  for(p in names) {dest<-web_inside(file.path(root,p),root,exists=FALSE);dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
    web_assert(file.copy(file.path(v$root,p),dest,overwrite=TRUE),"Could not prepare saved inputs.","SYSTEM_FAILED")}
  invisible(v)
}
web_scoped_job <- function(con,actor,project_id,version_id,job_id,operation=NULL) {
  web_authorize(con,actor,project_id)
  j <- web_query(con,"SELECT * FROM jobs WHERE id=?j AND project_id=?p AND version_id=?v",j=job_id,p=project_id,v=version_id)
  web_assert(nrow(j)==1L && (is.null(operation)||j$operation %in% operation),"This processing result does not belong to the selected version.")
  j
}
web_overlay_job <- function(con,settings,actor,project_id,version_id,job_id,root) {
  j <- web_scoped_job(con,actor,project_id,version_id,job_id)
  web_assert(j$state %in% c("COMPLETE","USER_ACTION_REQUIRED"),"This processing result is not ready.")
  r <- web_parse(j$result_json)
  a <- web_query(con,"SELECT * FROM artifacts WHERE job_id=?j AND project_id=?p AND version_id=?v AND status='AVAILABLE'",j=job_id,p=project_id,v=version_id)
  for(item in r$artifacts) {
    row <- a[a$id==item$artifact_id,,drop=FALSE]
    web_assert(nrow(row)==1L && identical(row$checksum,item$checksum),"Saved output integrity needs operator review.","SYSTEM_FAILED")
    rel <- item$relative_path
    web_assert(is.character(rel)&&length(rel)==1L&&!grepl("(^[/~]|^[A-Za-z]:|(^|/)\\.\\.(/|$)|^R/|^web/|(^|/)\\.)",rel),"Invalid generated output location.")
    path <- web_inside(file.path(root,rel),root,exists=FALSE);dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
    web_assert(file.copy(web_store(settings)$get(row$storage_locator,row$checksum),path,overwrite=TRUE),"Output could not be prepared.")
  }
  invisible(r)
}
web_latest_search <- function(con,project_id,version_id) {
  j <- web_query(con,"SELECT * FROM jobs WHERE project_id=?p AND version_id=?v AND operation='generate_search' ORDER BY created_at DESC LIMIT 1",p=project_id,v=version_id)
  web_assert(nrow(j)==1L && j$state=="COMPLETE", "Generate and review a passing search for this exact version first.","USER_ACTION_REQUIRED")
  r <- web_parse(j$result_json)
  web_assert(identical(r$provenance$search_quality,"PASS"),"Canonical search quality has not passed for this version.","USER_ACTION_REQUIRED")
  j$id
}
web_pipeline_sources <- function(con,project_id,version_id,operation) {
  if(operation=="retrieve_pubmed") return(web_latest_search(con,project_id,version_id))
  if(operation=="validate_database_inputs") {
    # Only selected-version generated inputs; failed/review-required search is never promoted.
    search <- tryCatch(web_latest_search(con,project_id,version_id),error=function(e)NULL)
    raw <- web_query(con,"SELECT j.id FROM jobs j JOIN job_sources s ON s.job_id=j.id WHERE j.project_id=?p AND j.version_id=?v AND j.operation='retrieve_pubmed' AND j.state='COMPLETE' AND s.source_job_id=?search ORDER BY j.created_at DESC LIMIT 1",p=project_id,v=version_id,search=if(is.null(search))"" else search)
    return(c(search,raw$id))
  }
  character()
}
web_require_activated <- function(con,project_id,version_id,operation) {
  if(operation %in% c("generate_search","retrieve_pubmed"))
    web_assert(nrow(web_query(con,"SELECT version_id FROM version_origins WHERE project_id=?p AND version_id=?v AND kind='ACTIVATED'",p=project_id,v=version_id))==1L,
      "Review and activate canonical concepts for this exact version first.","USER_ACTION_REQUIRED")
}
web_manual_concepts <- function(con,settings,actor,project_id,version_id,terms_yaml) {
  p <- web_authorize(con,actor,project_id)
  web_assert(is.character(terms_yaml)&&length(terms_yaml)==1L&&nchar(terms_yaml)>0&&nchar(terms_yaml)<=100000,
    "Enter a canonical search-concept document within the size limit.")
  root<-tempfile("concepts-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE),add=TRUE)
  web_copy_version(con,settings,actor,project_id,version_id,root,core_only=TRUE)
  r<-web_engine_call(settings,list(action="prepare_manual_concepts",project_dir=root,terms_yaml=terms_yaml))
  if(r$status!="COMPLETE")web_error(r$error_code,r$user_message,r$technical$diagnostic)
  source_key<-digest::digest(terms_yaml,algo="sha256",serialize=FALSE)
  web_tx(con,function(){
    web_exec(con,"UPDATE projects SET title=title WHERE id=?p",p=project_id)
    prior<-web_query(con,"SELECT version_id FROM version_origins WHERE project_id=?p AND parent_version_id=?v AND kind='MANUAL_DRAFT' AND source_key=?k",p=project_id,v=version_id,k=source_key)
    if(nrow(prior))return(list(project_id=project_id,version_id=prior$version_id))
    saved<-web_import_project(con,settings,actor,p$workspace_id,root,configuration_only=TRUE,draft_gates=TRUE,transaction=function(con,f)f())
    web_exec(con,"INSERT INTO version_origins VALUES(?v,?p,?parent,'MANUAL_DRAFT',?key,NULL,?actor,?now)",v=saved$version_id,p=project_id,parent=version_id,key=source_key,actor=actor,now=web_now())
    web_audit(con,p,"HUMAN_CONCEPT_DRAFT_SAVED",saved$version_id,actor,list(parent_version=version_id,source="HUMAN_PROVIDED",sha256=source_key))
    saved
  })
}
web_initialization_review <- function(con,settings,actor,project_id,version_id,job_id=NULL) {
  root<-tempfile("draft-review-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE),add=TRUE)
  web_copy_version(con,settings,actor,project_id,version_id,root)
  if(!is.null(job_id)&&nzchar(job_id)) {
    web_scoped_job(con,actor,project_id,version_id,job_id,"initialize_project")
    web_overlay_job(con,settings,actor,project_id,version_id,job_id,root)
  }
  names<-c("project_config_draft.yml","search_terms_draft.yml","initialization_decisions_required.yml")
  files<-file.path(root,"config/generated",names)
  web_assert(all(file.exists(files)),"Prepare canonical initialization or save human-provided concepts first.","USER_ACTION_REQUIRED")
  list(project_id=project_id,version_id=version_id,job_id=job_id,
    config=paste(readLines(files[1],warn=FALSE),collapse="\n"),terms=paste(readLines(files[2],warn=FALSE),collapse="\n"),
    decisions=yaml::read_yaml(files[3])$human_decisions_required)
}
web_initialization_edit_lines <- function(value, required = FALSE) {
  values <- trimws(as.character(unlist(value, use.names = FALSE)))
  values <- values[!is.na(values) & nzchar(values)]
  if (isTRUE(required)) web_assert(length(values) > 0L, "Edited review fields cannot be blank.")
  as.list(values)
}

web_initialization_edit_scalar <- function(value, required = TRUE, max_chars = 20000L) {
  web_assert(is.character(value) && length(value) == 1L && !is.na(value),
    "Edited review fields must contain text.")
  value <- trimws(value)
  if (isTRUE(required)) web_assert(nzchar(value), "Edited review fields cannot be blank.")
  web_assert(nchar(value) <= max_chars, "An edited review field is too long.")
  value
}

web_initialization_set_description <- function(current, value) {
  if (is.list(current)) {
    current$description <- value
    current
  } else {
    value
  }
}

web_apply_initialization_edits <- function(config, terms, edits) {
  if (is.null(edits) || !length(edits)) {
    return(list(config = config, terms = terms, edited_fields = character()))
  }

  web_assert(is.list(edits) && !is.null(names(edits)) &&
      all(!is.na(names(edits)) & nzchar(names(edits))) && !anyDuplicated(names(edits)),
    "Human edits are invalid.")

  allowed <- c(
    "review_title", "framework", "review_question", "population",
    "intervention", "comparator", "primary_outcomes", "secondary_outcomes",
    "study_designs", "inclusion_criteria", "exclusion_criteria",
    "language_restriction", "databases", "search_period", "search_terms"
  )
  web_assert(all(names(edits) %in% allowed), "This generated field cannot be edited here.")
  edit_json <- as.character(web_json(edits))
  web_assert(length(edit_json) == 1L && nchar(edit_json) <= 200000L,
    "Human edits exceed the allowed size.")

  if ("review_title" %in% names(edits))
    config$project$title <- web_initialization_edit_scalar(edits$review_title)

  if ("framework" %in% names(edits))
    config$research_question$framework <- web_initialization_edit_scalar(edits$framework, max_chars = 100L)

  if ("review_question" %in% names(edits))
    config$research_question$question <- web_initialization_edit_scalar(edits$review_question)

  if ("population" %in% names(edits))
    config$research_question$population <- web_initialization_set_description(
      config$research_question$population,
      web_initialization_edit_scalar(edits$population)
    )

  if ("intervention" %in% names(edits))
    config$research_question$intervention <- web_initialization_set_description(
      config$research_question$intervention,
      web_initialization_edit_scalar(edits$intervention)
    )

  if ("comparator" %in% names(edits))
    config$research_question$comparator <- web_initialization_set_description(
      config$research_question$comparator,
      web_initialization_edit_scalar(edits$comparator)
    )

  if ("primary_outcomes" %in% names(edits))
    config$research_question$outcomes$primary <- web_initialization_edit_lines(edits$primary_outcomes, required = TRUE)

  if ("secondary_outcomes" %in% names(edits))
    config$research_question$outcomes$secondary <- web_initialization_edit_lines(edits$secondary_outcomes, required = FALSE)

  if ("study_designs" %in% names(edits))
    config$eligibility$study_designs$include <- web_initialization_edit_lines(edits$study_designs, required = TRUE)

  if ("inclusion_criteria" %in% names(edits))
    config$eligibility$inclusion <- web_initialization_edit_lines(edits$inclusion_criteria, required = TRUE)

  if ("exclusion_criteria" %in% names(edits))
    config$eligibility$exclusion <- web_initialization_edit_lines(edits$exclusion_criteria, required = TRUE)

  if ("language_restriction" %in% names(edits))
    config$eligibility$language_restriction <- web_initialization_edit_scalar(
      edits$language_restriction, required = FALSE, max_chars = 1000L
    )

  if ("databases" %in% names(edits)) {
    selected <- unique(trimws(as.character(unlist(edits$databases, use.names = FALSE))))
    selected <- selected[!is.na(selected) & nzchar(selected)]
    available <- names(config$search$databases)
    web_assert(length(selected) > 0L && length(available) > 0L && all(selected %in% available),
      "Select at least one available database.")
    for (db in available) {
      current <- config$search$databases[[db]]
      if (is.list(current)) {
        current$enabled <- db %in% selected
        config$search$databases[[db]] <- current
      } else {
        config$search$databases[[db]] <- db %in% selected
      }
    }
  }

  if ("search_period" %in% names(edits)) {
    period <- edits$search_period
    web_assert(is.list(period), "The edited search period is invalid.")
    start <- if (is.null(period$start_date)) "" else web_initialization_edit_scalar(
      as.character(period$start_date), required = FALSE, max_chars = 100L
    )
    end <- if (is.null(period$end_date)) "" else web_initialization_edit_scalar(
      as.character(period$end_date), required = FALSE, max_chars = 100L
    )
    config$search$search_start_date <- start
    config$search$search_end_date <- end
  }

  if ("search_terms" %in% names(edits)) {
    raw_terms <- web_initialization_edit_scalar(edits$search_terms, required = TRUE, max_chars = 100000L)
    parsed_terms <- tryCatch(yaml::yaml.load(raw_terms, eval.expr = FALSE), error = function(e) NULL)
    web_assert(is.list(parsed_terms), "Edited search concepts must be valid YAML.")
    terms <- parsed_terms
  }

  project_id <- config$project$project_id
  web_assert(is.character(project_id) && length(project_id) == 1L && nzchar(project_id),
    "The edited configuration lost its project identifier.")
  if (!is.null(terms$project$id)) {
    web_assert(identical(as.character(terms$project$id), as.character(project_id)),
      "Edited search concepts must retain this project identifier.")
  }

  list(config = config, terms = terms, edited_fields = names(edits))
}

web_initialization_edit_paths <- function(edit_name) {
  switch(
    edit_name,
    review_title = list(target = "project_config", paths = "project.title"),
    framework = list(target = "project_config", paths = "research_question.framework"),
    review_question = list(target = "project_config", paths = "research_question.question"),
    population = list(target = "project_config", paths = "research_question.population"),
    intervention = list(target = "project_config", paths = "research_question.intervention"),
    comparator = list(target = "project_config", paths = "research_question.comparator"),
    primary_outcomes = list(target = "project_config", paths = "research_question.outcomes.primary"),
    secondary_outcomes = list(target = "project_config", paths = "research_question.outcomes.secondary"),
    study_designs = list(target = "project_config", paths = "eligibility.study_designs.include"),
    inclusion_criteria = list(target = "project_config", paths = "eligibility.inclusion"),
    exclusion_criteria = list(target = "project_config", paths = "eligibility.exclusion"),
    language_restriction = list(target = "project_config", paths = "eligibility.language_restriction"),
    databases = list(target = "project_config", paths = "search.databases"),
    search_period = list(target = "project_config", paths = c("search.search_start_date", "search.search_end_date")),
    search_terms = list(target = "search_terms", paths = ""),
    NULL
  )
}

web_normalize_patch_path <- function(path) {
  # Canonical decision patches may store a path either as one dotted string
  # or as multiple YAML path segments. Normalize to one scalar dotted path
  # before overlap checks.
  parts <- as.character(unlist(path, use.names = FALSE))
  parts <- parts[!is.na(parts) & nzchar(trimws(parts))]
  if (!length(parts)) return("")
  parts <- tolower(trimws(parts))
  parts <- gsub("[^a-z0-9_]+", ".", parts)
  parts <- gsub("^\\.+|\\.+$", "", parts)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) "" else paste(parts, collapse = ".")
}

web_paths_overlap <- function(a, b) {
  a <- web_normalize_patch_path(a)
  b <- web_normalize_patch_path(b)
  if (!nzchar(a) || !nzchar(b)) return(TRUE)
  identical(a, b) || startsWith(a, paste0(b, ".")) || startsWith(b, paste0(a, "."))
}

web_assert_no_edit_decision_conflicts <- function(decisions, selections, edit_names) {
  if (!length(edit_names) || !length(decisions) || !length(selections)) return(invisible(TRUE))
  conflicts <- character()

  for (decision in decisions) {
    decision_id <- as.character(unlist(decision$decision_id, use.names = FALSE))
    if (length(decision_id) != 1L || is.na(decision_id) || !nzchar(decision_id) ||
        is.null(selections[[decision_id]])) next
    chosen <- as.character(unlist(selections[[decision_id]], use.names = FALSE))
    if (length(chosen) != 1L || is.na(chosen) || !nzchar(chosen)) next

    patches <- decision$option_patches
    if (is.null(patches) || !length(patches)) next
    selected_patch <- Filter(function(x) {
      !is.null(x$option) && identical(as.character(x$option), chosen)
    }, patches)
    if (!length(selected_patch)) next

    operations <- selected_patch[[1L]]$operations
    if (is.null(operations) || !length(operations)) next

    for (edit_name in edit_names) {
      edit_scope <- web_initialization_edit_paths(edit_name)
      if (is.null(edit_scope)) next
      for (operation in operations) {
        target_parts <- if (is.null(operation$target)) character() else
          as.character(unlist(operation$target, use.names = FALSE))
        target_parts <- target_parts[!is.na(target_parts) & nzchar(trimws(target_parts))]
        target <- if (!length(target_parts)) "" else paste(target_parts, collapse = ".")
        path <- if (is.null(operation$path)) "" else operation$path
        if (!identical(target, edit_scope$target)) next
        if (identical(target, "search_terms") ||
            any(vapply(edit_scope$paths, web_paths_overlap, logical(1), b = path))) {
          conflicts <- c(conflicts, edit_name)
        }
      }
    }
  }

  conflicts <- unique(conflicts)
  web_assert(
    !length(conflicts),
    paste0(
      "Use the listed Human Decision instead of direct Edit for: ",
      paste(conflicts, collapse = ", "),
      "."
    )
  )
  invisible(TRUE)
}

web_activate_draft <- function(con,settings,actor,project_id,version_id,selections,human_approved,edits=list(),job_id=NULL) {
  p<-web_authorize(con,actor,project_id)
  web_assert(isTRUE(human_approved), "Explicit human approval is required.")
  web_assert(is.list(selections)&&length(selections)<=100L&&sum(nchar(unlist(selections)))<=100000,"Invalid decision selections.")

  review<-web_initialization_review(con,settings,actor,project_id,version_id,job_id)
  original_config <- tryCatch(yaml::yaml.load(review$config, eval.expr = FALSE), error = function(e) NULL)
  original_terms <- tryCatch(yaml::yaml.load(review$terms, eval.expr = FALSE), error = function(e) NULL)
  web_assert(is.list(original_config) && is.list(original_terms),
    "The generated review plan could not be prepared for editing.", "SYSTEM_FAILED")

  # Validate direct edits and detect any overlap with the selected canonical
  # Human Decisions before activation.  Do not feed the edited draft into the
  # canonical decision verifier: canonical verification must evaluate the
  # canonical draft plus the selected canonical patches only.
  edit_preview <- web_apply_initialization_edits(original_config, original_terms, edits)
  edited_fields <- edit_preview$edited_fields
  web_assert_no_edit_decision_conflicts(review$decisions, selections, edited_fields)

  rationale <- if (length(edited_fields)) {
    paste0("Human review completed in Web UI. Edited generated fields: ", paste(edited_fields, collapse = ", "), ".")
  } else {
    "Human review completed in Web UI without changes to generated fields."
  }

  source_key<-if(is.null(job_id)||!nzchar(job_id))"manual" else job_id
  approval_hash<-digest::digest(
    as.character(web_json(list(selections=selections,edits=edits,human_approved=TRUE))),
    algo="sha256",serialize=FALSE
  )

  root<-tempfile("activation-",settings$data_root);dir.create(root);on.exit(unlink(root,recursive=TRUE),add=TRUE)
  web_copy_version(con,settings,actor,project_id,version_id,root,core_only=TRUE)
  dir.create(file.path(root,"config/generated"),recursive=TRUE,showWarnings=FALSE)

  # Phase 1: canonical Human Decisions are applied and verified against the
  # unchanged toolkit-generated draft.
  writeLines(review$config,file.path(root,"config/generated/project_config_draft.yml"))
  writeLines(review$terms,file.path(root,"config/generated/search_terms_draft.yml"))
  yaml::write_yaml(list(human_decisions_required=review$decisions),file.path(root,"config/generated/initialization_decisions_required.yml"))

  r<-web_engine_call(settings,list(action="activate_initialization",project_dir=root,selections=selections,
    human_approved=TRUE,reviewer=actor,rationale=rationale))
  if(r$status!="COMPLETE")web_error(r$error_code,r$user_message,r$technical$diagnostic)

  activated_config<-yaml::read_yaml(file.path(root,"config/project_config.yml"))
  active_terms_path <- activated_config$paths$search_terms_file
  activated_terms <- yaml::read_yaml(file.path(root,active_terms_path))

  # Phase 2: apply non-conflicting direct Human Review edits only after the
  # canonical decision verification has passed.
  if (length(edited_fields)) {
    final_edited <- web_apply_initialization_edits(activated_config, activated_terms, edits)
    activated_config <- final_edited$config
    activated_terms <- final_edited$terms

    yaml::write_yaml(activated_config,file.path(root,"config/project_config.yml"))
    yaml::write_yaml(activated_terms,file.path(root,active_terms_path))

    # Re-run the canonical project-config validator after Human Review edits.
    inspect <- web_engine_inspect(settings, root, configuration_only = TRUE)
    web_assert(identical(inspect$status, "COMPLETE"),
      "The edited review configuration did not pass canonical validation.")

    # Search-concept YAML has its own canonical validator. Run it only when
    # the user directly edited the canonical search concepts.
    if ("search_terms" %in% edited_fields) {
      terms_yaml <- paste(readLines(file.path(root,active_terms_path), warn = FALSE), collapse = "\n")
      concept_check <- web_engine_call(settings,list(
        action="prepare_manual_concepts",
        project_dir=root,
        terms_yaml=terms_yaml
      ))
      if (concept_check$status!="COMPLETE")
        web_error(concept_check$error_code,concept_check$user_message,concept_check$technical$diagnostic)
    }
  }

  # Keep a concise machine-generated audit note; no free-text rationale is
  # required from the user.
  review_record_path <- file.path(root,"config/generated/web_human_review.yml")
  if (file.exists(review_record_path)) {
    review_record <- yaml::read_yaml(review_record_path)
    review_record$web_direct_edits <- list(
      edited_fields = as.list(edited_fields),
      rationale = rationale
    )
    yaml::write_yaml(review_record, review_record_path)
  }

  # Copy only approved canonical inputs and explicit review record into activation snapshot.
  keep<-c("config/project_config.yml",active_terms_path,"config/generated/web_human_review.yml")
  clean<-tempfile("approved-",settings$data_root);dir.create(clean);on.exit(unlink(clean,recursive=TRUE),add=TRUE)
  for(f in keep){dir.create(dirname(file.path(clean,f)),recursive=TRUE,showWarnings=FALSE);file.copy(file.path(root,f),file.path(clean,f))}

  web_tx(con,function(){
    if ("review_title" %in% edited_fields) {
      web_exec(con,"UPDATE projects SET title=?t WHERE id=?p",t=activated_config$project$title,p=project_id)
    } else {
      web_exec(con,"UPDATE projects SET title=title WHERE id=?p",p=project_id)
    }
    prior<-web_query(con,"SELECT version_id,approval_hash FROM version_origins WHERE project_id=?p AND parent_version_id=?v AND kind='ACTIVATED' AND source_key=?k",p=project_id,v=version_id,k=source_key)
    if(nrow(prior)){web_assert(identical(prior$approval_hash,approval_hash),"This draft was already activated with a different decision. Create a new draft.");return(list(project_id=project_id,version_id=prior$version_id))}
    saved<-web_import_project(con,settings,actor,p$workspace_id,clean,configuration_only=TRUE,draft_gates=TRUE,transaction=function(con,f)f())
    web_exec(con,"INSERT INTO version_origins VALUES(?v,?p,?parent,'ACTIVATED',?key,?hash,?actor,?now)",v=saved$version_id,p=project_id,parent=version_id,key=source_key,hash=approval_hash,actor=actor,now=web_now())
    gates<-web_query(con,"SELECT id FROM gates WHERE project_id=?p AND version_id=?v AND gate_type='configuration_human_review'",p=project_id,v=saved$version_id)
    for(g in gates$id){web_exec(con,"UPDATE gates SET status='APPROVED',decision='APPROVED',reviewer_id=?u,decided_at=?n,rationale=?r WHERE id=?g",u=actor,n=web_now(),r=rationale,g=g)
      web_audit(con,p,"GATE_DECIDED",g,actor,list(decision="APPROVED",rationale=rationale,parent_version=version_id,edited_fields=edited_fields))}
    web_audit(con,p,"CANONICAL_DRAFT_ACTIVATED",saved$version_id,actor,list(parent_version=version_id,source_key=source_key,approval_hash=approval_hash,edited_fields=edited_fields))
    saved
  })
}
