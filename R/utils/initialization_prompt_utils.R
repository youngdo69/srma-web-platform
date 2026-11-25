# ============================================================
# Universal SR-MA Agent Toolkit
# R/utils/initialization_prompt_utils.R
#
# Purpose
# ------------------------------------------------------------
# 1. Render the project-initialization prompt
# 2. Define the structured-output JSON schema
# 3. Preserve compatibility with the established canonical
#    project_config.yml contract used by config_utils.R,
#    path_utils.R, and R/11-R/20
# 4. Keep search_terms.yml concept-based and topic-neutral
# ============================================================


render_initialization_prompt <- function(
    template_text,
    project_seed
) {

  gsub(
    "{{project_seed_yaml}}",
    yaml::as.yaml(project_seed),
    template_text,
    fixed = TRUE
  )
}


string_array_schema <- function() {

  list(
    type = "array",
    items = list(
      type = "string"
    )
  )
}


json_array <- function(...) {

  unname(
    as.list(
      c(...)
    )
  )
}


nullable_string_schema <- function() {

  list(
    type = c(
      "string",
      "null"
    )
  )
}


string_or_number_schema <- function() {

  list(
    type = c(
      "string",
      "number"
    )
  )
}


patch_operation_schema <- function() {

  list(
    type = "object",
    additionalProperties = FALSE,
    required = json_array(
      "target",
      "path",
      "operation",
      "value_json",
      "match_key",
      "match_value_json",
      "rationale"
    ),
    properties = list(
      target = list(
        type = "string",
        enum = json_array(
          "project_config",
          "search_terms"
        )
      ),
      path = string_array_schema(),
      operation = list(
        type = "string",
        enum = json_array(
          "set",
          "append_unique",
          "remove_value",
          "append_record",
          "remove_record_by_key"
        )
      ),
      value_json = list(
        type = "string"
      ),
      match_key = list(
        type = "string"
      ),
      match_value_json = list(
        type = "string"
      ),
      rationale = list(
        type = "string"
      )
    )
  )
}


human_decision_schema <- function() {

  list(
    type = "object",
    additionalProperties = FALSE,
    required = json_array(
      "decision_id",
      "question",
      "why_it_matters",
      "suggested_options",
      "recommended_option",
      "recommendation_rationale",
      "affected_components",
      "option_patches"
    ),
    properties = list(
      decision_id = list(
        type = "string"
      ),
      question = list(
        type = "string"
      ),
      why_it_matters = list(
        type = "string"
      ),
      suggested_options = string_array_schema(),
      recommended_option = list(
        type = "string"
      ),
      recommendation_rationale = list(
        type = "string"
      ),
      affected_components = string_array_schema(),
      option_patches = list(
        type = "array",
        items = list(
          type = "object",
          additionalProperties = FALSE,
          required = json_array(
            "option",
            "operations"
          ),
          properties = list(
            option = list(
              type = "string"
            ),
            operations = list(
              type = "array",
              items = patch_operation_schema()
            )
          )
        )
      )
    )
  )
}


framework_component_schema <- function() {

  list(
    type = "object",
    additionalProperties = FALSE,
    required = json_array(
      "description",
      "values"
    ),
    properties = list(
      description = list(
        type = "string"
      ),
      values = string_array_schema()
    )
  )
}


exclusion_reason_schema <- function() {

  list(
    type = "object",
    additionalProperties = FALSE,
    required = json_array(
      "code",
      "label",
      "description"
    ),
    properties = list(
      code = list(
        type = "string"
      ),
      label = list(
        type = "string"
      ),
      description = list(
        type = "string"
      )
    )
  )
}


database_schema <- function() {

  list(
    type = "object",
    additionalProperties = FALSE,
    required = json_array(
      "enabled",
      "display_name"
    ),
    properties = list(
      enabled = list(
        type = "boolean"
      ),
      display_name = list(
        type = "string"
      )
    )
  )
}


reviewer_schema <- function() {

  list(
    type = "object",
    additionalProperties = FALSE,
    required = json_array(
      "name",
      "initials"
    ),
    properties = list(
      name = list(
        type = "string"
      ),
      initials = list(
        type = "string"
      )
    )
  )
}


build_initialization_json_schema <- function() {

  list(
    type = "object",
    additionalProperties = FALSE,
    required = json_array(
      "project_config",
      "search_terms",
      "human_decisions_required",
      "report"
    ),
    properties = list(

      # ======================================================
      # 1. Canonical project configuration
      # ======================================================

      project_config = list(
        type = "object",
        additionalProperties = FALSE,
        required = json_array(
          "schema_version",
          "project",
          "research_question",
          "eligibility",
          "search",
          "screening",
          "data_extraction",
          "risk_of_bias",
          "certainty_of_evidence",
          "reporting",
          "naming",
          "paths"
        ),
        properties = list(

          schema_version = list(
            type = "string"
          ),

          project = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "project_id",
              "title",
              "short_name",
              "output_prefix",
              "version",
              "status",
              "principal_investigator",
              "clinical_field",
              "protocol_registration"
            ),
            properties = list(
              project_id = list(
                type = "string"
              ),
              title = list(
                type = "string"
              ),
              short_name = list(
                type = "string"
              ),
              output_prefix = list(
                type = "string"
              ),
              version = list(
                type = "string"
              ),
              status = list(
                type = "string"
              ),
              principal_investigator = list(
                type = "string"
              ),
              clinical_field = list(
                type = "string"
              ),
              protocol_registration = list(
                type = "object",
                additionalProperties = FALSE,
                required = json_array(
                  "registry",
                  "registration_number",
                  "registration_date"
                ),
                properties = list(
                  registry = list(
                    type = "string"
                  ),
                  registration_number = list(
                    type = "string"
                  ),
                  registration_date = list(
                    type = "string"
                  )
                )
              )
            )
          ),

          research_question = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "framework",
              "question",
              "population",
              "intervention",
              "comparator",
              "outcomes",
              "framework_elements"
            ),
            properties = list(
              framework = list(
                type = "string"
              ),
              question = list(
                type = "string"
              ),
              population = framework_component_schema(),
              intervention = framework_component_schema(),
              comparator = framework_component_schema(),
              outcomes = list(
                type = "object",
                additionalProperties = FALSE,
                required = json_array(
                  "primary",
                  "secondary"
                ),
                properties = list(
                  primary = string_array_schema(),
                  secondary = string_array_schema()
                )
              ),
              framework_elements = list(
                type = "array",
                items = list(
                  type = "object",
                  additionalProperties = FALSE,
                  required = json_array(
                    "label",
                    "values"
                  ),
                  properties = list(
                    label = list(
                      type = "string"
                    ),
                    values = string_array_schema()
                  )
                )
              )
            )
          ),

          eligibility = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "inclusion",
              "exclusion",
              "study_designs",
              "language_restriction",
              "publication_status",
              "minimum_sample_size"
            ),
            properties = list(
              inclusion = string_array_schema(),
              exclusion = string_array_schema(),
              study_designs = list(
                type = "object",
                additionalProperties = FALSE,
                required = json_array(
                  "include",
                  "exclude"
                ),
                properties = list(
                  include = string_array_schema(),
                  exclude = string_array_schema()
                )
              ),
              language_restriction = list(
                type = "string"
              ),
              publication_status = list(
                type = "object",
                additionalProperties = FALSE,
                required = json_array(
                  "include_peer_reviewed_articles",
                  "include_preprints",
                  "include_conference_abstracts"
                ),
                properties = list(
                  include_peer_reviewed_articles = list(
                    type = "boolean"
                  ),
                  include_preprints = list(
                    type = "boolean"
                  ),
                  include_conference_abstracts = list(
                    type = "boolean"
                  )
                )
              ),
              minimum_sample_size = list(
                type = "integer",
                minimum = 0
              )
            )
          ),

          search = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "terms_file",
              "databases",
              "additional_sources",
              "search_start_date",
              "search_end_date"
            ),
            properties = list(
              terms_file = list(
                type = "string"
              ),
              databases = list(
                type = "object",
                additionalProperties = FALSE,
                required = json_array(
                  "pubmed",
                  "embase",
                  "central"
                ),
                properties = list(
                  pubmed = database_schema(),
                  embase = database_schema(),
                  central = database_schema()
                )
              ),
              additional_sources = string_array_schema(),
              search_start_date = list(
                type = "string"
              ),
              search_end_date = list(
                type = "string"
              )
            )
          ),

          screening = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "number_of_reviewers",
              "independent_screening",
              "disagreement_resolution",
              "decisions",
              "decision_rules",
              "exclusion_reasons",
              "reviewers"
            ),
            properties = list(
              number_of_reviewers = list(
                type = "integer",
                minimum = 1
              ),
              independent_screening = list(
                type = "boolean"
              ),
              disagreement_resolution = list(
                type = "string"
              ),
              decisions = list(
                type = "array",
                minItems = 3,
                maxItems = 3,
                items = list(
                  type = "string",
                  enum = json_array(
                    "Include",
                    "Exclude",
                    "Unclear"
                  )
                )
              ),
              decision_rules = list(
                type = "object",
                additionalProperties = FALSE,
                required = json_array(
                  "include",
                  "exclude",
                  "unclear"
                ),
                properties = list(
                  include = string_array_schema(),
                  exclude = string_array_schema(),
                  unclear = string_array_schema()
                )
              ),
              exclusion_reasons = list(
                type = "array",
                items = exclusion_reason_schema()
              ),
              reviewers = list(
                type = "object",
                additionalProperties = FALSE,
                required = json_array(
                  "reviewer_1",
                  "reviewer_2",
                  "third_reviewer"
                ),
                properties = list(
                  reviewer_1 = reviewer_schema(),
                  reviewer_2 = reviewer_schema(),
                  third_reviewer = reviewer_schema()
                )
              )
            )
          ),

          data_extraction = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "reviewers",
              "independent_extraction",
              "disagreement_resolution"
            ),
            properties = list(
              reviewers = list(
                type = "integer",
                minimum = 1
              ),
              independent_extraction = list(
                type = "boolean"
              ),
              disagreement_resolution = list(
                type = "string"
              )
            )
          ),

          risk_of_bias = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "randomized_trials",
              "nonrandomized_studies"
            ),
            properties = list(
              randomized_trials = list(
                type = "string"
              ),
              nonrandomized_studies = list(
                type = "string"
              )
            )
          ),

          certainty_of_evidence = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "framework"
            ),
            properties = list(
              framework = list(
                type = "string"
              )
            )
          ),

          reporting = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "guideline",
              "target_journal"
            ),
            properties = list(
              guideline = list(
                type = "string"
              ),
              target_journal = list(
                type = "string"
              )
            )
          ),

          naming = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "pubmed_basename",
              "embase_basename",
              "central_basename",
              "merged_records_basename",
              "deduplicated_records_basename",
              "removed_duplicates_basename",
              "deduplication_map_basename",
              "prisma_counts_basename",
              "screening_workbook_basename",
              "completed_screening_workbook_basename"
            ),
            properties = list(
              pubmed_basename = list(
                type = "string"
              ),
              embase_basename = list(
                type = "string"
              ),
              central_basename = list(
                type = "string"
              ),
              merged_records_basename = list(
                type = "string"
              ),
              deduplicated_records_basename = list(
                type = "string"
              ),
              removed_duplicates_basename = list(
                type = "string"
              ),
              deduplication_map_basename = list(
                type = "string"
              ),
              prisma_counts_basename = list(
                type = "string"
              ),
              screening_workbook_basename = list(
                type = "string"
              ),
              completed_screening_workbook_basename = list(
                type = "string"
              )
            )
          ),

          paths = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "search_terms_file",
              "search_strategies_directory",
              "raw_search_directory",
              "pubmed_directory",
              "embase_directory",
              "central_directory",
              "processed_search_directory",
              "processed_screening_directory",
              "screening_results_directory",
              "logs_directory",
              "figures_directory",
              "tables_directory",
              "manuscript_directory"
            ),
            properties = list(
              search_terms_file = list(
                type = "string"
              ),
              search_strategies_directory = list(
                type = "string"
              ),
              raw_search_directory = list(
                type = "string"
              ),
              pubmed_directory = list(
                type = "string"
              ),
              embase_directory = list(
                type = "string"
              ),
              central_directory = list(
                type = "string"
              ),
              processed_search_directory = list(
                type = "string"
              ),
              processed_screening_directory = list(
                type = "string"
              ),
              screening_results_directory = list(
                type = "string"
              ),
              logs_directory = list(
                type = "string"
              ),
              figures_directory = list(
                type = "string"
              ),
              tables_directory = list(
                type = "string"
              ),
              manuscript_directory = list(
                type = "string"
              )
            )
          )
        )
      ),

      # ======================================================
      # 2. Topic-neutral concept-based search configuration
      # ======================================================

      search_terms = list(
        type = "object",
        additionalProperties = FALSE,
        required = json_array(
          "project",
          "search",
          "validation"
        ),
        properties = list(

          project = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "id",
              "title",
              "short_name"
            ),
            properties = list(
              id = list(
                type = "string"
              ),
              title = list(
                type = "string"
              ),
              short_name = list(
                type = "string"
              )
            )
          ),

          search = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "concepts",
              "combination",
              "outcome_terms_policy"
            ),
            properties = list(

              concepts = list(
                type = "array",
                minItems = 2,
                items = list(
                  type = "object",
                  additionalProperties = FALSE,
                  required = json_array(
                    "concept_id",
                    "label",
                    "role",
                    "required",
                    "free_text_terms",
                    "abbreviations",
                    "spelling_variants",
                    "controlled_vocabulary"
                  ),
                  properties = list(
                    concept_id = list(
                      type = "string"
                    ),
                    label = list(
                      type = "string"
                    ),
                    role = list(
                      type = "string"
                    ),
                    required = list(
                      type = "boolean"
                    ),
                    free_text_terms = string_array_schema(),
                    abbreviations = string_array_schema(),
                    spelling_variants = string_array_schema(),
                    controlled_vocabulary = list(
                      type = "object",
                      additionalProperties = FALSE,
                      required = json_array(
                        "pubmed_mesh_candidates",
                        "embase_emtree_candidates"
                      ),
                      properties = list(
                        pubmed_mesh_candidates =
                          string_array_schema(),
                        embase_emtree_candidates =
                          string_array_schema()
                      )
                    )
                  )
                )
              ),

              combination = list(
                type = "object",
                additionalProperties = FALSE,
                required = json_array(
                  "operator_between_concepts",
                  "notes"
                ),
                properties = list(
                  operator_between_concepts = list(
                    type = "string",
                    enum = json_array(
                      "AND"
                    )
                  ),
                  notes = list(
                    type = "string"
                  )
                )
              ),

              outcome_terms_policy = list(
                type = "string"
              )
            )
          ),

          validation = list(
            type = "object",
            additionalProperties = FALSE,
            required = json_array(
              "known_relevant_ids",
              "controlled_vocabulary_requires_verification"
            ),
            properties = list(
              known_relevant_ids = string_array_schema(),
              controlled_vocabulary_requires_verification =
                list(
                  type = "boolean"
                )
            )
          )
        )
      ),

      # ======================================================
      # 3. Human decisions and report
      # ======================================================

      human_decisions_required = list(
        type = "array",
        items = human_decision_schema()
      ),

      report = list(
        type = "object",
        additionalProperties = FALSE,
        required = json_array(
          "summary",
          "assumptions",
          "warnings"
        ),
        properties = list(
          summary = list(
            type = "string"
          ),
          assumptions = string_array_schema(),
          warnings = string_array_schema()
        )
      )
    )
  )
}
