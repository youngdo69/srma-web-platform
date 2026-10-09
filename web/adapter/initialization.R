# Private canonical initialization/activation boundary. No PICO-to-term heuristics.
web_copy_initialization <- function(engine_root) {
  files <- c("R/utils/llm_provider_utils.R", "R/utils/initialization_prompt_utils.R",
    "R/utils/human_decision_patch_utils.R", "R/internal/approve_human_decisions.R",
    "R/internal/apply_human_decision_patches.R", "R/internal/verify_human_decision_application.R",
    "config/initialization_agent.yml", "config/human_decision_policy.yml",
    "prompts/project_initialization_prompt.md")
  for (f in files) {dir.create(dirname(f), recursive=TRUE, showWarnings=FALSE)
    web_assert(file.copy(file.path(engine_root,f),f,overwrite=TRUE),"Initialization resources are unavailable.","SYSTEM_FAILED")}
}
web_canonical_functions <- function(file, env) {
  # Reuse actual canonical function bodies without executing its console workflow.
  for (expr in parse(file)) if (is.call(expr) && identical(expr[[1]],as.name("<-")) &&
    is.call(expr[[3]]) && identical(expr[[3]][[1]],as.name("function"))) eval(expr,env)
}
web_validate_concepts <- function(engine_root, terms, config) {
  e <- new.env(parent=globalenv())
  web_canonical_functions(file.path(engine_root,"R/01_validate_project_initialization.R"),e)
  e$`%||%` <- function(x,y) if(is.null(x)||!length(x)) y else x
  concepts <- e$normalize_search_concepts(terms$search$concepts)
  web_assert(length(concepts)>=2L,"Enter at least two explicit canonical search concepts.")
  ids <- vapply(concepts,`[[`,character(1),"concept_id")
  web_assert(all(nzchar(ids))&&!anyDuplicated(ids),"Search concepts need distinct identifiers.")
  web_assert(all(vapply(concepts,function(x) nzchar(x$label)&&nzchar(x$role)&&length(x$free_text_terms)>0L,logical(1))),
    "Each concept needs a label, role and explicit search terms.")
  web_assert(identical(terms$project$id,config$project$project_id),"Search concepts must belong to this project.")
  web_assert(identical(terms$search$combination$operator_between_concepts,"AND") &&
    isTRUE(terms$validation$controlled_vocabulary_requires_verification),
    "Keep canonical concept combination and controlled-vocabulary verification enabled.")
  # Validation only: never replace, infer, or expand human-provided terms.
  invisible(TRUE)
}
web_initialization_paths <- function() {
  a <- yaml::read_yaml("config/initialization_agent.yml")$initialization_agent
  c(project=file.path(a$output$generated_directory,a$output$project_config_draft),
    search=file.path(a$output$generated_directory,a$output$search_terms_draft),
    decisions=file.path(a$output$generated_directory,a$output$decisions_required))
}

# Normalize a narrow, schema-deterministic LLM patch defect before canonical
# patch planning. For remove_value, the removable value belongs in value_json;
# match_value_json is only meaningful for remove_record_by_key.
web_normalize_hd_patch_semantics <- function(decisions) {
  repairs <- list()
  if (!length(decisions)) return(list(decisions=decisions, repairs=repairs))

  for (i in seq_along(decisions)) {
    patches <- decisions[[i]]$option_patches
    if (is.null(patches) || !length(patches)) next

    for (j in seq_along(patches)) {
      ops <- patches[[j]]$operations
      if (is.null(ops) || !length(ops)) next

      for (k in seq_along(ops)) {
        op <- ops[[k]]
        op_name <- if (is.null(op$operation)) "" else as.character(op$operation[[1L]])
        value_json <- if (is.null(op$value_json)) "" else as.character(op$value_json[[1L]])
        match_json <- if (is.null(op$match_value_json)) "" else as.character(op$match_value_json[[1L]])

        if (identical(op_name, "remove_value") &&
            !nzchar(value_json) && nzchar(match_json)) {
          op$value_json <- match_json
          ops[[k]] <- op
          repairs[[length(repairs)+1L]] <- list(
            decision_id=decisions[[i]]$decision_id,
            option=patches[[j]]$option,
            operation="remove_value",
            path=op$path,
            repair="match_value_json copied to value_json"
          )
        }
      }
      patches[[j]]$operations <- ops
    }
    decisions[[i]]$option_patches <- patches
  }

  list(decisions=decisions, repairs=repairs)
}

web_hd_operation_key <- function(operation) {
  target <- if (is.null(operation$target)) "" else as.character(operation$target[[1L]])
  path <- if (is.null(operation$path)) character() else
    as.character(unlist(operation$path, use.names=FALSE))
  paste(target, paste(path, collapse="."), sep="::")
}

# Canonical verification checks every operation against the final document.
# That is intentionally strict, but it yields a false negative when two
# separately approved decisions compose on the same path (for example SET
# followed by APPEND_UNIQUE, or REMOVE_RECORD_BY_KEY followed by APPEND_RECORD).
# Accept only that narrow composition case; all other canonical checks must pass.
web_hd_composed_verification_ok <- function(e, verification, plan, result,
                                            project_before, search_before,
                                            project_file, search_file) {
  if (!is.list(verification) || !is.list(verification$checks) ||
      !is.list(verification$operation_checks)) return(FALSE)

  other_checks <- verification$checks[
    setdiff(names(verification$checks), "all_patch_operations_verified")
  ]
  if (length(other_checks) &&
      !all(vapply(other_checks, isTRUE, logical(1)))) return(FALSE)

  failed <- which(!vapply(
    verification$operation_checks,
    function(x) isTRUE(x$passed),
    logical(1)
  ))
  if (!length(failed)) return(FALSE)

  operations <- plan$operations
  if (length(operations) != length(verification$operation_checks)) return(FALSE)

  keys <- vapply(operations, web_hd_operation_key, character(1))
  counts <- table(keys)
  failed_keys <- keys[failed]

  # Only operations sharing a path with another selected operation are eligible
  # for composed-path handling.
  if (any(vapply(failed_keys, function(k) is.na(counts[[k]]) || counts[[k]] < 2L, logical(1)))) {
    return(FALSE)
  }

  disk_project <- tryCatch(yaml::read_yaml(project_file), error=function(e) NULL)
  disk_search <- tryCatch(yaml::read_yaml(search_file), error=function(e) NULL)
  if (is.null(disk_project) || is.null(disk_search)) return(FALSE)
  if (!isTRUE(e$hd_values_equal(disk_project, result$project_config)) ||
      !isTRUE(e$hd_values_equal(disk_search, result$search_terms))) return(FALSE)

  # Replay all selected operations for each failed shared path from the original
  # generated document and require an exact match with the final result.
  for (key in unique(failed_keys)) {
    idx <- which(keys == key)
    first <- operations[[idx[[1L]]]]
    target <- as.character(first$target[[1L]])

    replay <- if (identical(target, "project_config")) project_before else
      if (identical(target, "search_terms")) search_before else NULL
    final_doc <- if (identical(target, "project_config")) result$project_config else
      if (identical(target, "search_terms")) result$search_terms else NULL
    if (is.null(replay) || is.null(final_doc)) return(FALSE)

    for (ii in idx) replay <- e$apply_single_hd_operation(replay, operations[[ii]])

    replay_value <- e$hd_get_nested(replay, first$path)
    final_value <- e$hd_get_nested(final_doc, first$path)
    if (!isTRUE(e$hd_values_equal(replay_value, final_value))) return(FALSE)
  }

  TRUE
}
web_initialization_action <- function(request, config = NULL, root) {
  engine <- request$engine_root; web_copy_initialization(engine); paths <- web_initialization_paths()
  e <- new.env(parent=globalenv())
  if (request$action=="initialize_project") {
    provider <- yaml::read_yaml("config/initialization_agent.yml")$initialization_agent$provider
    web_assert(nzchar(Sys.getenv(provider$api_key_env,"")),
      "Initialization requires an operator-configured provider credential. Your project is saved.","USER_ACTION_REQUIRED")

    # Canonical new-project path: preserve the user-supplied project seed exactly
    # and let the original Project Initialization Agent infer the review structure.
    # Legacy web drafts with a complete configuration remain supported below.
    expected_project_id <- ""
    if (is.null(config)) {
      web_assert(file.exists("config/project_seed.yml"),
        "A project seed is required before canonical initialization.", "USER_ACTION_REQUIRED")
      seed_document <- yaml::read_yaml("config/project_seed.yml")
      seed <- seed_document$project_seed
      web_assert(is.list(seed) && !is.null(seed$topic) && nzchar(trimws(as.character(seed$topic[[1L]]))),
        "Enter the research topic or question before canonical initialization.", "USER_ACTION_REQUIRED")
      if (!is.null(seed$project_id) && length(seed$project_id)) {
        expected_project_id <- trimws(as.character(seed$project_id[[1L]]))
      }
    } else {
      # Backward compatibility for pre-v1.0.23 manually completed web drafts.
      expected_project_id <- config$project$project_id
      yaml::write_yaml(list(project_seed=list(topic=config$research_question$question,
        project_id=config$project$project_id,user_supplied_configuration=config)),"config/project_seed.yml")
    }

    # Canonical R/00 requires a project-root .env file even when the provider
    # credential is already inherited from the operator process environment.
    # Web job workspaces intentionally never receive the real operator .env,
    # so provide a secret-free compatibility marker only for this canonical call.
    compat_env <- web_inside(file.path(root, ".env"), root, exists = FALSE)
    web_assert(!file.exists(compat_env),
      "Private environment files must not enter Web project workspaces.", "SYSTEM_FAILED")
    compat_env_active <- TRUE
    on.exit(if (isTRUE(compat_env_active)) unlink(compat_env, force = TRUE), add = TRUE)
    compat_created <- tryCatch({
      writeLines(
        "# Web compatibility marker only; credentials are inherited from the operator process environment.",
        compat_env,
        useBytes = TRUE
      )
      file.exists(compat_env)
    }, error = function(e) FALSE)
    web_assert(isTRUE(compat_created),
      "Could not prepare the isolated initialization credential boundary.", "SYSTEM_FAILED")

    sys.source(file.path(engine,"R/00_run_project_initialization_agent.R"),e)
    unlink(compat_env, force = TRUE)
    compat_env_active <- FALSE
    sys.source(file.path(engine,"R/01_validate_project_initialization.R"),new.env(parent=globalenv()))
    draft <- yaml::read_yaml(paths[['project']]);terms <- yaml::read_yaml(paths[['search']])
    if (nzchar(expected_project_id)) {
      web_assert(identical(draft$project$project_id,expected_project_id),
        "The generated draft changed the project identifier. Ask the operator to review it.")
    }
    web_validate_concepts(engine,terms,draft)
    outputs <- c(paths,file.path(dirname(paths[['project']]),"initialization_report.md"))
  } else if (request$action=="prepare_manual_concepts") {
    terms <- yaml::yaml.load(request$terms_yaml,eval.expr=FALSE)
    web_validate_concepts(engine,terms,config)
    dir.create(dirname(paths[['project']]),recursive=TRUE,showWarnings=FALSE)
    yaml::write_yaml(config,paths[['project']]);yaml::write_yaml(terms,paths[['search']])
    yaml::write_yaml(list(human_decisions_required=list()),paths[['decisions']])
    outputs <- paths
  } else {
    web_assert(request$action=="activate_initialization" && isTRUE(request$human_approved) &&
      nzchar(request$reviewer) && nzchar(request$rationale),"Explicit human approval is required.","USER_ACTION_REQUIRED")
    for (f in c("R/utils/human_decision_patch_utils.R","R/internal/approve_human_decisions.R",
      "R/internal/apply_human_decision_patches.R","R/internal/verify_human_decision_application.R")) sys.source(f,e)
    decisions <- e$normalize_hd_records(yaml::read_yaml(paths[['decisions']])$human_decisions_required)
    normalized <- web_normalize_hd_patch_semantics(decisions)
    decisions <- normalized$decisions
    patch_repairs <- normalized$repairs
    selections <- request$selections
    ids <- vapply(decisions,`[[`,character(1),"decision_id")
    web_assert(length(selections)==length(ids) && setequal(names(selections),ids) && !anyDuplicated(names(selections)),
      "Resolve each listed methodological decision explicitly.")
    responses <- lapply(decisions,function(d) list(decision_id=d$decision_id,selected_option=selections[[d$decision_id]],
      status="approved",approved_at=web_now(),approval_source=paste0("authenticated_web:",request$reviewer)))
    e$validate_human_decision_approvals(decisions,responses)
    plan <- e$build_selected_patch_plan(decisions,responses)
    project_before <- yaml::read_yaml(paths[['project']])
    search_before <- yaml::read_yaml(paths[['search']])
    result <- e$apply_human_decision_patch_plan(project_before,search_before,plan)
    web_assert(identical(result$project_config$project$project_id,config$project$project_id),"Approved configuration must retain this project identifier.")
    web_validate_concepts(engine,result$search_terms,result$project_config)
    yaml::write_yaml(result$project_config,paths[['project']]);yaml::write_yaml(result$search_terms,paths[['search']])
    verification <- e$verify_human_decision_application(decisions,responses,plan,result,paths[['project']],paths[['search']])

    canonical_passed <- isTRUE(verification$passed)
    composed_passed <- FALSE
    if (!canonical_passed) {
      composed_passed <- web_hd_composed_verification_ok(
        e, verification, plan, result, project_before, search_before,
        paths[['project']], paths[['search']]
      )
    }
    verification$canonical_passed <- canonical_passed
    verification$composed_path_verification_passed <- composed_passed
    verification$patch_semantic_repairs <- patch_repairs
    verification$passed <- canonical_passed || composed_passed

    web_assert(isTRUE(verification$passed),"Canonical human-decision verification failed.")
    yaml::write_yaml(list(decisions=decisions,responses=responses,verification=verification,
      patch_semantic_repairs=patch_repairs,rationale=request$rationale),
      "config/generated/web_human_review.yml")
    yaml::write_yaml(list(human_decisions_required=list()),paths[['decisions']])
    # Replace ONLY the console-confirmation function with this authenticated UI
    # confirmation. Every other original activation check/copy/archive/rollback runs.
    activation <- new.env(parent=globalenv())
    activation$activation_confirm <- function() {
      web_assert(isTRUE(request$human_approved)&&nzchar(request$reviewer)&&nzchar(request$rationale),
        "Explicit human approval is required.","USER_ACTION_REQUIRED");invisible(TRUE)
    }
    for (expr in parse(file.path(engine,"R/02_activate_project_configuration.R"))) {
      definition <- is.call(expr)&&identical(expr[[1]],as.name("<-"))&&identical(expr[[2]],as.name("activation_confirm"))
      if (!definition) eval(expr,activation)
    }
    activated<-yaml::read_yaml("config/project_config.yml")
    destination<-web_inside(file.path(root,activated$paths$search_terms_file),root,exists=FALSE)
    active_terms<-yaml::read_yaml("config/initialization_agent.yml")$initialization_agent$activation$active_search_terms
    if(normalizePath(active_terms,mustWork=TRUE)!=normalizePath(destination,mustWork=FALSE)) {
      dir.create(dirname(destination),recursive=TRUE,showWarnings=FALSE)
      web_assert(file.copy(active_terms,destination,overwrite=TRUE),"Could not map activated terms to their canonical configured location.")
    }
    outputs <- c("config/project_config.yml",activated$paths$search_terms_file,"config/generated/web_human_review.yml")
  }
  list(outputs=lapply(unname(outputs),function(p)list(path=web_inside(p,root),logical_name=basename(p),type="initialization_draft")))
}
