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
web_initialization_action <- function(request, config, root) {
  engine <- request$engine_root; web_copy_initialization(engine); paths <- web_initialization_paths()
  e <- new.env(parent=globalenv())
  if (request$action=="initialize_project") {
    provider <- yaml::read_yaml("config/initialization_agent.yml")$initialization_agent$provider
    web_assert(nzchar(Sys.getenv(provider$api_key_env,"")),
      "Initialization requires an operator-configured provider credential. Your project is saved.","USER_ACTION_REQUIRED")
    # The user's complete metadata is supplied verbatim as context, not converted into scientific terms.
    yaml::write_yaml(list(project_seed=list(topic=config$research_question$question,
      project_id=config$project$project_id,user_supplied_configuration=config)),"config/project_seed.yml")
    sys.source(file.path(engine,"R/00_run_project_initialization_agent.R"),e)
    sys.source(file.path(engine,"R/01_validate_project_initialization.R"),new.env(parent=globalenv()))
    draft <- yaml::read_yaml(paths[['project']]);terms <- yaml::read_yaml(paths[['search']])
    web_assert(identical(draft$project$project_id,config$project$project_id),
      "The generated draft changed the project identifier. Ask the operator to review it.")
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
    selections <- request$selections
    ids <- vapply(decisions,`[[`,character(1),"decision_id")
    web_assert(length(selections)==length(ids) && setequal(names(selections),ids) && !anyDuplicated(names(selections)),
      "Resolve each listed methodological decision explicitly.")
    responses <- lapply(decisions,function(d) list(decision_id=d$decision_id,selected_option=selections[[d$decision_id]],
      status="approved",approved_at=web_now(),approval_source=paste0("authenticated_web:",request$reviewer)))
    e$validate_human_decision_approvals(decisions,responses)
    plan <- e$build_selected_patch_plan(decisions,responses)
    result <- e$apply_human_decision_patch_plan(yaml::read_yaml(paths[['project']]),yaml::read_yaml(paths[['search']]),plan)
    web_assert(identical(result$project_config$project$project_id,config$project$project_id),"Approved configuration must retain this project identifier.")
    web_validate_concepts(engine,result$search_terms,result$project_config)
    yaml::write_yaml(result$project_config,paths[['project']]);yaml::write_yaml(result$search_terms,paths[['search']])
    verification <- e$verify_human_decision_application(decisions,responses,plan,result,paths[['project']],paths[['search']])
    web_assert(isTRUE(verification$passed),"Canonical human-decision verification failed.")
    yaml::write_yaml(list(decisions=decisions,responses=responses,verification=verification,rationale=request$rationale),
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
