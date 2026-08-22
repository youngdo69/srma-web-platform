# ============================================================
# Universal SR-MA Agent Toolkit
# LLM provider utilities
# ============================================================

load_project_env <- function(env_file = ".env") {
  if (!file.exists(env_file)) {
    stop(
      "Environment file was not found: ", env_file, "\n",
      "Create it from .env.example and add the required API key."
    )
  }

  if (!requireNamespace("dotenv", quietly = TRUE)) {
    stop('Package "dotenv" is required. Install it with install.packages("dotenv").')
  }

  dotenv::load_dot_env(file = env_file)
  invisible(TRUE)
}

get_required_environment_value <- function(variable_name) {
  value <- Sys.getenv(variable_name, unset = "")

  if (!nzchar(value)) {
    stop(
      "Required environment variable is empty: ", variable_name, "\n",
      "Add it to the project-root .env file."
    )
  }

  value
}

build_screening_json_schema <- function(
    decision_options,
    exclusion_reason_codes,
    exclusion_reason_labels,
    confidence_options
) {
  list(
    type = "object",
    additionalProperties = FALSE,
    required = c(
      "screening_id",
      "decision",
      "exclusion_reason_code",
      "exclusion_reason",
      "confidence",
      "rationale"
    ),
    properties = list(
      screening_id = list(type = "string"),
      decision = list(type = "string", enum = unname(decision_options)),
      exclusion_reason_code = list(
        type = "string",
        enum = unname(c("", exclusion_reason_codes))
      ),
      exclusion_reason = list(
        type = "string",
        enum = unname(c("", exclusion_reason_labels))
      ),
      confidence = list(type = "string", enum = unname(confidence_options)),
      rationale = list(type = "string")
    )
  )
}

extract_openai_output_text <- function(response_body) {
  if (!is.list(response_body$output)) {
    stop("OpenAI response did not contain an output array.")
  }

  for (item in response_body$output) {
    if (!is.list(item$content)) next

    for (content_item in item$content) {
      if (identical(content_item$type, "output_text") &&
          is.character(content_item$text) &&
          length(content_item$text) == 1L) {
        return(content_item$text)
      }

      if (identical(content_item$type, "refusal")) {
        stop("The model refused the screening request: ", content_item$refusal)
      }
    }
  }

  stop("No output_text item was found in the OpenAI response.")
}


sanitize_json_control_characters <- function(x) {
  if (is.null(x) || length(x) == 0L) return(x)

  if (!is.character(x)) {
    x <- as.character(x)
  }

  vapply(
    x,
    function(value) {
      if (is.na(value)) return(NA_character_)

      # Normalize to UTF-8 first. Invalid byte sequences are dropped rather
      # than propagated into downstream JSON parsing.
      value <- iconv(value, from = "", to = "UTF-8", sub = "")
      if (is.na(value)) value <- ""

      bytes <- charToRaw(value)
      ints <- as.integer(bytes)

      # RFC 8259 permits SP, HTAB, LF and CR as unescaped whitespace.
      # Remove all other C0 control bytes plus DEL. This guards against
      # model-output artifacts such as 0x13 appearing inside strings.
      keep <- !(
        (ints < 32L & !(ints %in% c(9L, 10L, 13L))) |
          ints == 127L
      )

      rawToChar(bytes[keep], multiple = FALSE)
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}

parse_openai_structured_output_json <- function(output_text) {
  if (!is.character(output_text) || length(output_text) != 1L || is.na(output_text)) {
    stop("OpenAI output_text must be one non-missing character string.")
  }

  # First parse the exact model output unchanged. This preserves strict
  # behavior for normal responses and uses sanitization only as a fallback.
  parsed <- tryCatch(
    jsonlite::fromJSON(
      output_text,
      simplifyVector = FALSE
    ),
    error = function(e) e
  )

  if (!inherits(parsed, "error")) {
    return(parsed)
  }

  sanitized <- sanitize_json_control_characters(output_text)

  # If sanitization made no change, retain the original parse error because
  # the failure is not attributable to removable control characters.
  if (identical(sanitized, output_text)) {
    stop(
      "Failed to parse OpenAI structured output JSON: ",
      conditionMessage(parsed),
      call. = FALSE
    )
  }

  reparsed <- tryCatch(
    jsonlite::fromJSON(
      sanitized,
      simplifyVector = FALSE
    ),
    error = function(e) e
  )

  if (inherits(reparsed, "error")) {
    stop(
      "Failed to parse OpenAI structured output JSON after removing invalid ",
      "control characters. Original error: ", conditionMessage(parsed),
      "\nSanitized parse error: ", conditionMessage(reparsed),
      call. = FALSE
    )
  }

  warning(
    "OpenAI structured output contained invalid control character(s); ",
    "they were removed before JSON parsing. The exact raw model output ",
    "remains available in raw_output_text.",
    call. = FALSE
  )

  reparsed
}

call_openai_structured_screening <- function(
    prompt,
    model,
    api_key,
    base_url,
    json_schema,
    timeout_seconds = 120,
    schema_name = "structured_output"
) {
  required_packages <- c("httr2", "jsonlite")
  missing <- required_packages[
    !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  ]

  if (length(missing) > 0L) {
    stop(
      "Missing required packages: ", paste(missing, collapse = ", "),
      "\nInstall them with install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "), "))."
    )
  }

  endpoint <- paste0(sub("/+$", "", base_url), "/responses")

  request_body <- list(
    model = model,
    input = list(
      list(
        role = "user",
        content = list(
          list(
            type = "input_text",
            text = prompt
          )
        )
      )
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = schema_name,
        strict = TRUE,
        schema = json_schema
      )
    )
  )

  response <- httr2::request(endpoint) |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(request_body, auto_unbox = TRUE) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  status <- httr2::resp_status(response)
  response_text <- httr2::resp_body_string(response)

  if (status < 200L || status >= 300L) {
    stop(
      "OpenAI API request failed with HTTP ", status, ".\n",
      substr(response_text, 1L, 2000L)
    )
  }

  response_body <- jsonlite::fromJSON(
    response_text,
    simplifyVector = FALSE
  )

  output_text <- extract_openai_output_text(response_body)

  # Preserve nested schema records (e.g., option_patches) exactly.
  # Parse the exact model output first; if JSON parsing fails solely because
  # of invalid control bytes, sanitize those bytes and retry once.
  parsed <- parse_openai_structured_output_json(output_text)

  list(
    parsed = parsed,
    response_id = response_body$id %||% NA_character_,
    model = response_body$model %||% model,
    raw_output_text = output_text,
    raw_response = response_text
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

call_llm_screening_provider <- function(
    provider_name,
    prompt,
    provider_config,
    json_schema,
    schema_name = "structured_output"
) {
  provider_name <- tolower(provider_name)

  if (provider_name == "openai") {
    api_key_env <- provider_config$api_key_env %||% "OPENAI_API_KEY"
    api_key <- get_required_environment_value(api_key_env)

    return(
      call_openai_structured_screening(
        prompt = prompt,
        model = provider_config$model,
        api_key = api_key,
        base_url = provider_config$base_url %||% "https://api.openai.com/v1",
        json_schema = json_schema,
        timeout_seconds = provider_config$timeout_seconds %||% 120,
        schema_name = schema_name
      )
    )
  }

  stop(
    "Unsupported provider: ", provider_name, "\n",
    "Add a provider adapter in R/utils/llm_provider_utils.R."
  )
}
