# ============================================================
# Universal SR-MA Agent Toolkit
# R/internal/apply_human_decision_patches.R
# ============================================================

build_selected_patch_plan <- function(
    decisions,
    responses
) {
  selected_patches <- list()
  all_operations <- list()

  for (decision in decisions) {
    response <- hd_find_response(
      responses,
      decision$decision_id
    )

    selected_option <- response$selected_option

    matches <- which(vapply(
      decision$option_patches,
      function(x) identical(x$option, selected_option),
      logical(1)
    ))

    if (length(matches) != 1L) {
      stop(
        decision$decision_id,
        ": expected exactly one option_patch for selected option '",
        selected_option,
        "', found ",
        length(matches),
        "."
      )
    }

    selected_patch <- decision$option_patches[[matches[[1]]]]

    selected_patches[[length(selected_patches) + 1L]] <- list(
      decision_id = decision$decision_id,
      selected_option = selected_option,
      operations = selected_patch$operations
    )

    if (length(selected_patch$operations) > 0L) {
      for (operation in selected_patch$operations) {
        operation$decision_id <- decision$decision_id
        operation$selected_option <- selected_option
        all_operations[[length(all_operations) + 1L]] <- operation
      }
    }
  }

  list(
    selected_patches = selected_patches,
    operations = all_operations
  )
}

apply_single_hd_operation <- function(
    document,
    operation
) {
  target_path <- operation$path
  operation_name <- operation$operation
  value <- hd_decode_json_value(operation$value_json)

  if (length(target_path) == 0L) {
    stop(
      operation$decision_id,
      ": patch path cannot be empty."
    )
  }

  current <- hd_get_nested(
    document,
    target_path
  )

  if (identical(operation_name, "set")) {
    return(hd_set_nested(
      document,
      target_path,
      value
    ))
  }

  if (identical(operation_name, "append_unique")) {
    current_list <- current %||% list()

    if (!is.list(current_list)) {
      current_list <- as.list(current_list)
    }

    already_present <- any(vapply(
      current_list,
      function(item) hd_values_equal(item, value),
      logical(1)
    ))

    if (!already_present) {
      current_list[[length(current_list) + 1L]] <- value
    }

    return(hd_set_nested(
      document,
      target_path,
      current_list
    ))
  }

  if (identical(operation_name, "remove_value")) {
    current_list <- current %||% list()

    if (!is.list(current_list)) {
      current_list <- as.list(current_list)
    }

    keep <- !vapply(
      current_list,
      function(item) hd_values_equal(item, value),
      logical(1)
    )

    return(hd_set_nested(
      document,
      target_path,
      current_list[keep]
    ))
  }

  if (identical(operation_name, "append_record")) {
    current_list <- current %||% list()

    if (!is.list(current_list)) {
      stop(
        operation$decision_id,
        ": append_record target must be a list."
      )
    }

    current_list[[length(current_list) + 1L]] <- value

    return(hd_set_nested(
      document,
      target_path,
      current_list
    ))
  }

  if (identical(
    operation_name,
    "remove_record_by_key"
  )) {
    current_list <- current %||% list()
    match_key <- operation$match_key
    match_value <- hd_decode_json_value(
      operation$match_value_json
    )

    if (!nzchar(match_key)) {
      stop(
        operation$decision_id,
        ": remove_record_by_key requires match_key."
      )
    }

    if (!is.list(current_list)) {
      stop(
        operation$decision_id,
        ": remove_record_by_key target must be a list."
      )
    }

    keep <- vapply(
      current_list,
      function(record) {
        if (!is.list(record) || is.null(record[[match_key]])) {
          return(TRUE)
        }

        !hd_values_equal(
          record[[match_key]],
          match_value
        )
      },
      logical(1)
    )

    return(hd_set_nested(
      document,
      target_path,
      current_list[keep]
    ))
  }

  stop(
    operation$decision_id,
    ": unsupported patch operation: ",
    operation_name
  )
}

apply_human_decision_patch_plan <- function(
    project_config,
    search_terms,
    patch_plan
) {
  applied_operations <- list()

  for (i in seq_along(patch_plan$operations)) {
    operation <- patch_plan$operations[[i]]
    target <- operation$target

    if (identical(target, "project_config")) {
      project_config <- apply_single_hd_operation(
        project_config,
        operation
      )
    } else if (identical(target, "search_terms")) {
      search_terms <- apply_single_hd_operation(
        search_terms,
        operation
      )
    } else {
      stop(
        operation$decision_id,
        ": unsupported patch target: ",
        target
      )
    }

    applied_operations[[length(applied_operations) + 1L]] <- c(
      operation,
      list(
        operation_index = i,
        applied = TRUE
      )
    )
  }

  list(
    project_config = project_config,
    search_terms = search_terms,
    applied_operations = applied_operations
  )
}
