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

      value <- iconv(value, from = "", to = "UTF-8", sub = "")
      if (is.na(value)) value <- ""

      bytes <- charToRaw(value)
      ints <- as.integer(bytes)

      if (length(ints) == 0L) return("")

      out <- raw(0L)
      in_string <- FALSE
      escaped <- FALSE

      for (i in seq_along(ints)) {
        b <- ints[[i]]

        if (in_string) {
          if (escaped) {
            # This byte belongs to a JSON escape sequence. Keep it unchanged.
            out <- c(out, as.raw(b))
            escaped <- FALSE
            next
          }

          if (b == 92L) {  # backslash
            out <- c(out, as.raw(b))
            escaped <- TRUE
            next
          }

          if (b == 34L) {  # double quote
            out <- c(out, as.raw(b))
            in_string <- FALSE
            next
          }

          if (b < 32L) {
            # Unescaped U+0000-U+001F is forbidden inside JSON strings.
            # Replace it with a single ASCII space so adjacent words do not
            # collapse (e.g. "pre<control>post" -> "pre post").
            out <- c(out, charToRaw(" "))
            next
          }

          out <- c(out, as.raw(b))
          next
        }

        # Outside strings, JSON permits ordinary structural whitespace
        # (space, tab, LF, CR). Other control bytes are discarded.
        if (b == 34L) {  # opening double quote
          out <- c(out, as.raw(b))
          in_string <- TRUE
          escaped <- FALSE
          next
        }

        if (b < 32L && !(b %in% c(9L, 10L, 13L))) {
          next
        }

        out <- c(out, as.raw(b))
      }

      rawToChar(out, multiple = FALSE)
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

is_openai_insufficient_quota <- function(status, response_text) {
  if (!identical(as.integer(status), 429L)) return(FALSE)
  api_error <- tryCatch(
    jsonlite::fromJSON(response_text, simplifyVector = FALSE)$error,
    error = function(e) NULL
  )
  if (!is.list(api_error)) return(FALSE)
  code <- tolower(as.character(api_error$code %||% ""))
  type <- tolower(as.character(api_error$type %||% ""))
  message <- as.character(api_error$message %||% "")
  identical(code, "insufficient_quota") ||
    identical(type, "insufficient_quota") ||
    grepl("no credits remaining|exceeded your current quota|billing hard limit",
          message, ignore.case = TRUE)
}


# Actual API usage counts; cached/reasoning tokens are included within totals.
extract_openai_screening_usage <- function(body) {
  u <- body$usage
  if (!is.list(u)) return(NULL)
  numeric_field <- function(x) {
    if (is.null(x) || length(x) != 1L) return(NA_real_)
    y <- suppressWarnings(as.numeric(x))
    if (is.na(y) || !is.finite(y) || y < 0) NA_real_ else y
  }
  list(
    input_tokens = numeric_field(u$input_tokens),
    cached_input_tokens = numeric_field(u$input_tokens_details$cached_tokens),
    cache_write_tokens = numeric_field(u$input_tokens_details$cache_write_tokens),
    output_tokens = numeric_field(u$output_tokens),
    reasoning_tokens = numeric_field(u$output_tokens_details$reasoning_tokens)
  )
}

# Missing or mismatched model price metadata produces NA instead of guesses.
# This excludes charged unsuccessful attempts and cannot replace provider billing.
estimate_openai_screening_usd <- function(usage, rate_card, model) {
  if (!is.list(usage) || !is.list(rate_card) ||
      !identical(as.character(rate_card$model), as.character(model))) return(NA_real_)
  number <- function(x) {
    if (is.null(x) || length(x) != 1L) return(NA_real_)
    y <- suppressWarnings(as.numeric(x))
    if (is.na(y) || !is.finite(y) || y < 0) NA_real_ else y
  }
  ip <- number(rate_card$input_usd_per_million)
  cp <- number(rate_card$cached_input_usd_per_million)
  wp <- number(rate_card$cache_write_usd_per_million)
  outp <- number(rate_card$output_usd_per_million)
  it <- number(usage$input_tokens)
  ot <- number(usage$output_tokens)
  ct <- number(usage$cached_input_tokens)
  wt <- number(usage$cache_write_tokens)
  if (anyNA(c(ip, outp, it, ot))) return(NA_real_)
  if (is.na(ct)) ct <- 0
  if (is.na(wt)) {
    # Responses may omit cache-write counts. Assume zero *reported* cache
    # writes for advisory estimates only; provider billing is authoritative.
    # Do not block scientific screening when this field is absent.
    wt <- 0
  }
  if ((ct > 0 && is.na(cp)) || (wt > 0 && is.na(wp))) return(NA_real_)
  ordinary <- it - ct - wt
  if (ordinary < 0) return(NA_real_)
  (ordinary * ip + ct * ifelse(is.na(cp), 0, cp) +
    wt * ifelse(is.na(wp), 0, wp) + ot * outp) / 1e6
}

# Pure payload builder: can be tested without API credentials or network access.
# Optional controls are opt-in so an unchanged configuration sends the same
# payload as the previously approved screening pipeline.
build_openai_screening_request_body <- function(
    prompt, model, json_schema, schema_name = "structured_output",
    reasoning_effort = NULL, max_output_tokens = NULL
) {
  if (!is.character(model) || length(model) != 1L ||
      is.na(model) || !nzchar(trimws(model))) {
    stop("A nonempty screening model is required.", call. = FALSE)
  }
  request_body <- list(
    model = model,
    input = list(list(role = "user", content = list(list(
      type = "input_text", text = prompt
    )))),
    text = list(format = list(
      type = "json_schema", name = schema_name,
      strict = TRUE, schema = json_schema
    ))
  )
  if (!is.null(reasoning_effort)) {
    effort <- as.character(reasoning_effort)
    if (length(effort) != 1L || is.na(effort) ||
        !effort %in% c("none", "minimal", "low", "medium", "high", "xhigh", "max")) {
      stop("Unsupported reasoning_effort setting.", call. = FALSE)
    }
    request_body$reasoning <- list(effort = effort)
  }
  if (!is.null(max_output_tokens)) {
    n <- suppressWarnings(as.numeric(max_output_tokens))
    if (length(n) != 1L || is.na(n) || !is.finite(n) ||
        n != floor(n) || n < 256 || n > 128000) {
      stop("max_output_tokens must be an integer from 256 to 128000.", call. = FALSE)
    }
    # API token budget includes invisible reasoning; too-low values can cause
    # truncated JSON and costly retries. Not enabled by default.
    request_body$max_output_tokens <- as.integer(n)
  }
  request_body
}

call_openai_structured_screening <- function(
    prompt,
    model,
    api_key,
    base_url,
    json_schema,
    timeout_seconds = 120,
    schema_name = "structured_output",
    reasoning_effort = NULL,
    max_output_tokens = NULL
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

  request_body <- build_openai_screening_request_body(
    prompt = prompt, model = model, json_schema = json_schema,
    schema_name = schema_name,
    reasoning_effort = reasoning_effort,
    max_output_tokens = max_output_tokens
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
    # HTTP 429 covers both temporary rate limiting and exhausted API credits.
    if (is_openai_insufficient_quota(status, response_text)) {
      stop(structure(
        list(
          message = paste0(
            "OpenAI API credits exhausted (HTTP 429: insufficient_quota). ",
            "Screening stopped; add API credits before retrying."
          ),
          call = NULL
        ),
        class = c("srma_ai_quota_exhausted", "error", "condition")
      ))
    }
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
    usage = extract_openai_screening_usage(response_body),
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
        schema_name = schema_name,
        reasoning_effort = provider_config$reasoning_effort %||% NULL,
        max_output_tokens = provider_config$max_output_tokens %||% NULL
      )
    )
  }

  stop(
    "Unsupported provider: ", provider_name, "\n",
    "Add a provider adapter in R/utils/llm_provider_utils.R."
  )
}
