# ============================================================
# R/01b_prepare_human_decision_review.R
# ============================================================

required_packages <- c("yaml", "jsonlite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0L) {
  stop("Missing package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

utility_file <- "R/utils/human_decision_patch_utils.R"
if (!file.exists(utility_file)) {
  stop("Missing Human Decision utility file: ", utility_file, call. = FALSE)
}

source(utility_file, local = FALSE)

if (!exists("normalize_hd_records", mode = "function")) {
  stop(
    "Function normalize_hd_records() was not found after sourcing ",
    utility_file,
    ". Confirm that human_decision_patch_utils.R is the new version.",
    call. = FALSE
  )
}

policy_file <- "config/human_decision_policy.yml"
if (!file.exists(policy_file)) {
  stop("Missing Human Decision policy file: ", policy_file, call. = FALSE)
}

policy_document <- yaml::read_yaml(policy_file)
policy <- policy_document$human_decision_policy

if (is.null(policy) || !is.list(policy)) {
  stop(
    "Invalid policy structure in ", policy_file,
    ": expected top-level key 'human_decision_policy'.",
    call. = FALSE
  )
}

policy_paths <- policy$paths
if (is.null(policy_paths) || !is.list(policy_paths)) {
  stop(
    "Invalid policy structure in ", policy_file,
    ": expected 'human_decision_policy.paths'.",
    call. = FALSE
  )
}

decisions_file <- policy_paths$generated_decisions
response_file <- policy_paths$response_file

if (is.null(decisions_file) || !nzchar(as.character(decisions_file))) {
  decisions_file <- "config/generated/initialization_decisions_required.yml"
}
if (is.null(response_file) || !nzchar(as.character(response_file))) {
  response_file <- "config/generated/human_decision_responses.yml"
}

decisions_file <- as.character(decisions_file)
response_file <- as.character(response_file)

if (!file.exists(decisions_file)) {
  stop("Missing Human Decision file: ", decisions_file, call. = FALSE)
}

decisions_document <- yaml::read_yaml(decisions_file)
if (is.null(decisions_document) || !is.list(decisions_document)) {
  stop("Could not read a valid YAML document from: ", decisions_file, call. = FALSE)
}

raw_decisions <- decisions_document$human_decisions_required
if (is.null(raw_decisions)) raw_decisions <- list()

human_decisions <- normalize_hd_records(raw_decisions)

if (!length(human_decisions)) {
  cat(
    "\nNo unresolved Human Decisions were found in:\n  ",
    decisions_file,
    "\n",
    sep = ""
  )
} else {
  decision_ids <- vapply(
    human_decisions,
    function(decision) as.character(decision$decision_id %||% ""),
    FUN.VALUE = character(1)
  )

  blank_ids <- which(!nzchar(decision_ids))
  if (length(blank_ids) > 0L) {
    stop(
      "Human Decision record(s) with missing decision_id: ",
      paste(blank_ids, collapse = ", "),
      call. = FALSE
    )
  }

  duplicated_ids <- unique(decision_ids[duplicated(decision_ids)])
  if (length(duplicated_ids) > 0L) {
    stop(
      "Duplicated Human Decision ID(s): ",
      paste(duplicated_ids, collapse = ", "),
      call. = FALSE
    )
  }

  response_records <- lapply(
    human_decisions,
    function(decision) {
      suggested_options <- as.character(
        unlist(decision$suggested_options %||% list(), use.names = FALSE)
      )
      recommended_option <- as.character(decision$recommended_option %||% "")

      if (!length(suggested_options)) {
        stop(
          "Human Decision ", decision$decision_id,
          " has no suggested_options.",
          call. = FALSE
        )
      }
      if (!nzchar(recommended_option)) {
        stop(
          "Human Decision ", decision$decision_id,
          " has no recommended_option.",
          call. = FALSE
        )
      }
      if (!recommended_option %in% suggested_options) {
        stop(
          "Human Decision ", decision$decision_id,
          ": recommended_option is not included in suggested_options.",
          call. = FALSE
        )
      }

      list(
        decision_id = as.character(decision$decision_id),
        question = as.character(decision$question %||% ""),
        why_it_matters = as.character(decision$why_it_matters %||% ""),
        recommended_option = recommended_option,
        recommendation_rationale = as.character(decision$recommendation_rationale %||% ""),
        suggested_options = suggested_options,
        selected_option = recommended_option,
        status = "pending",
        reviewer_note = "",
        reviewed_at = ""
      )
    }
  )

  response_directory <- dirname(response_file)
  if (!dir.exists(response_directory)) {
    dir.create(response_directory, recursive = TRUE, showWarnings = FALSE)
  }

  yaml::write_yaml(
    list(human_decision_responses = response_records),
    response_file
  )

  verification_document <- yaml::read_yaml(response_file)
  written_responses <- verification_document$human_decision_responses
  if (is.null(written_responses)) written_responses <- list()

  if (length(written_responses) != length(human_decisions)) {
    stop(
      "Response file verification failed: expected ",
      length(human_decisions),
      " records but found ",
      length(written_responses),
      ".",
      call. = FALSE
    )
  }

  cat(
    "\nHuman Decision review file prepared successfully.\n",
    "Source:    ", decisions_file, "\n",
    "Created:   ", response_file, "\n",
    "Decisions: ", length(human_decisions), "\n",
    "Status:    all records initialized as pending\n",
    sep = ""
  )
}
