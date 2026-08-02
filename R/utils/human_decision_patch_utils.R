# Universal SR-MA Agent Toolkit
# Human-decision normalization and patch utilities

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

as_character_vector <- function(x) {
  if (is.null(x) || length(x) == 0L) return(character(0))
  out <- trimws(as.character(unlist(x, recursive = TRUE, use.names = FALSE)))
  out[nzchar(out)]
}

normalize_record_array <- function(x, fields, object_name) {
  if (is.null(x) || length(x) == 0L) return(list())

  if (is.data.frame(x)) {
    missing <- setdiff(fields, names(x))
    if (length(missing)) stop(object_name, " is missing: ", paste(missing, collapse=", "))
    return(lapply(seq_len(nrow(x)), function(i) {
      out <- lapply(fields, function(f) x[[f]][[i]])
      names(out) <- fields
      out
    }))
  }

  if (is.list(x) && !is.null(names(x)) && all(fields %in% names(x))) {
    lens <- vapply(fields, function(f) length(x[[f]]), integer(1))
    n <- max(lens)
    bad <- lens[!(lens %in% c(1L, n))]
    if (length(bad)) {
      stop(object_name, " has incompatible lengths: ",
           paste(paste0(names(lens), "=", lens), collapse=", "))
    }
    return(lapply(seq_len(n), function(i) {
      out <- lapply(fields, function(f) {
        v <- x[[f]]
        if (length(v) == 1L) v else v[[i]]
      })
      names(out) <- fields
      out
    }))
  }

  if (is.list(x)) {
    return(lapply(seq_along(x), function(i) {
      item <- x[[i]]
      if (is.data.frame(item)) item <- as.list(item[1,,drop=FALSE])
      if (!is.list(item)) stop(object_name, " record ", i, " is not an object.")
      missing <- setdiff(fields, names(item))
      if (length(missing)) stop(object_name, " record ", i, " is missing: ",
                                paste(missing, collapse=", "))
      item[fields]
    }))
  }

  stop(object_name, " has unsupported class: ", paste(class(x), collapse=", "))
}

normalize_patch_operations <- function(x) {
  if (is.null(x) || length(x) == 0L) return(list())
  ops <- normalize_record_array(
    x,
    c("target","path","operation","value_json","match_key","match_value_json","rationale"),
    "patch operations"
  )
  lapply(ops, function(op) list(
    target = as_character_vector(op$target)[[1]],
    path = as_character_vector(op$path),
    operation = as_character_vector(op$operation)[[1]],
    value_json = paste(as_character_vector(op$value_json), collapse=""),
    match_key = paste(as_character_vector(op$match_key), collapse=""),
    match_value_json = paste(as_character_vector(op$match_value_json), collapse=""),
    rationale = paste(as_character_vector(op$rationale), collapse=" ")
  ))
}

normalize_option_patches <- function(x) {
  patches <- normalize_record_array(x, c("option","operations"), "option_patches")
  lapply(patches, function(p) list(
    option = as_character_vector(p$option)[[1]],
    operations = normalize_patch_operations(p$operations)
  ))
}

normalize_human_decisions_v3 <- function(x) {
  decisions <- normalize_record_array(
    x,
    c("decision_id","question","why_it_matters","suggested_options",
      "recommended_option","recommendation_rationale","affected_components","option_patches"),
    "human_decisions_required"
  )
  lapply(decisions, function(d) list(
    decision_id = as_character_vector(d$decision_id)[[1]],
    question = paste(as_character_vector(d$question), collapse=" "),
    why_it_matters = paste(as_character_vector(d$why_it_matters), collapse=" "),
    suggested_options = as_character_vector(d$suggested_options),
    recommended_option = as_character_vector(d$recommended_option)[[1]],
    recommendation_rationale = paste(as_character_vector(d$recommendation_rationale), collapse=" "),
    affected_components = as_character_vector(d$affected_components),
    option_patches = normalize_option_patches(d$option_patches)
  ))
}

parse_json_value <- function(text) {
  text <- paste(as_character_vector(text), collapse="")
  if (!nzchar(text)) stop("Required JSON patch value is empty.")
  tryCatch(
    jsonlite::fromJSON(text, simplifyVector=FALSE),
    error=function(e) stop("Invalid patch JSON: ", text, "\n", conditionMessage(e))
  )
}

get_nested <- function(x, path) {
  cur <- x
  for (key in path) {
    if (!is.list(cur) || is.null(cur[[key]])) return(NULL)
    cur <- cur[[key]]
  }
  cur
}

set_nested <- function(x, path, value) {
  if (!length(path)) return(value)
  if (!is.list(x)) x <- list()
  key <- path[[1]]
  x[[key]] <- set_nested(x[[key]] %||% list(), path[-1], value)
  x
}

json_key <- function(x) jsonlite::toJSON(x, auto_unbox=TRUE, null="null")

append_unique <- function(current, value) {
  items <- if (is.null(current)) list() else if (is.list(current)) current else as.list(current)
  additions <- if (is.list(value) && is.null(names(value))) value else list(value)
  all <- c(items, additions)
  all[!duplicated(vapply(all, json_key, character(1)))]
}

remove_value <- function(current, value) {
  if (is.null(current)) return(current)
  items <- if (is.list(current)) current else as.list(current)
  target <- json_key(value)
  items[vapply(items, function(i) json_key(i) != target, logical(1))]
}

remove_record_by_key <- function(current, key, value) {
  if (is.null(current)) return(current)
  items <- if (is.list(current)) current else as.list(current)
  items[vapply(items, function(i) {
    !is.list(i) || is.null(i[[key]]) || !identical(i[[key]], value)
  }, logical(1))]
}

apply_patch_operation <- function(object, op) {
  allowed_targets <- c("project_config","search_terms")
  allowed_ops <- c("set","append_unique","remove_value","append_record","remove_record_by_key")
  if (!(op$target %in% allowed_targets)) stop("Unsupported target: ", op$target)
  if (!(op$operation %in% allowed_ops)) stop("Unsupported operation: ", op$operation)
  if (!length(op$path)) stop("Patch path must not be empty.")

  current <- get_nested(object, op$path)

  if (op$operation == "set")
    return(set_nested(object, op$path, parse_json_value(op$value_json)))

  if (op$operation %in% c("append_unique","append_record"))
    return(set_nested(object, op$path, append_unique(current, parse_json_value(op$value_json))))

  if (op$operation == "remove_value")
    return(set_nested(object, op$path, remove_value(current, parse_json_value(op$value_json))))

  if (!nzchar(op$match_key)) stop("match_key is required.")
  set_nested(
    object, op$path,
    remove_record_by_key(current, op$match_key, parse_json_value(op$match_value_json))
  )
}
