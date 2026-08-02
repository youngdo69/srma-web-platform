# ============================================================
# Universal SR-MA Agent Toolkit
# Screening prompt utilities
# ============================================================

collapse_config_text <- function(x, empty_value = "Not specified") {
  if (is.null(x) || length(x) == 0L) return(empty_value)

  if (is.data.frame(x)) {
    return(paste(apply(x, 1, paste, collapse = ": "), collapse = "\n"))
  }

  if (is.list(x)) {
    values <- unlist(x, recursive = TRUE, use.names = FALSE)
  } else {
    values <- as.character(x)
  }

  values <- trimws(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]

  if (length(values) == 0L) empty_value else paste0("- ", values, collapse = "\n")
}

render_prompt_template <- function(template_text, values) {
  rendered <- template_text

  for (name in names(values)) {
    token <- paste0("{{", name, "}}")
    replacement <- values[[name]]
    if (is.null(replacement) || is.na(replacement)) replacement <- ""
    rendered <- gsub(token, replacement, rendered, fixed = TRUE)
  }

  rendered
}

build_exclusion_reason_text <- function(exclusion_reason_table) {
  if (!is.data.frame(exclusion_reason_table) ||
      !all(c("code", "label") %in% names(exclusion_reason_table))) {
    stop("exclusion_reason_table must contain code and label columns.")
  }

  paste0(
    "- ",
    exclusion_reason_table$code,
    ": ",
    exclusion_reason_table$label,
    collapse = "\n"
  )
}

build_agent_screening_prompt <- function(
    template_text,
    record,
    project_id,
    project_title,
    review_question,
    inclusion_criteria,
    exclusion_reason_table
) {
  required_record_fields <- c(
    "screening_id", "title", "abstract", "authors", "year",
    "journal", "publication_type", "language"
  )

  for (field in required_record_fields) {
    if (!field %in% names(record)) record[[field]] <- ""
    if (is.na(record[[field]])) record[[field]] <- ""
  }

  render_prompt_template(
    template_text,
    list(
      project_id = project_id,
      project_title = project_title,
      review_question = collapse_config_text(review_question),
      inclusion_criteria = collapse_config_text(inclusion_criteria),
      exclusion_reasons = build_exclusion_reason_text(exclusion_reason_table),
      screening_id = as.character(record$screening_id),
      title = as.character(record$title),
      abstract = as.character(record$abstract),
      authors = as.character(record$authors),
      year = as.character(record$year),
      journal = as.character(record$journal),
      publication_type = as.character(record$publication_type),
      language = as.character(record$language)
    )
  )
}
