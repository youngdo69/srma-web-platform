# Universal SR-MA Agent Toolkit
# Initialization Structured Outputs schema v3

render_initialization_prompt <- function(template_text, project_seed) {
  gsub("{{project_seed_yaml}}", yaml::as.yaml(project_seed), template_text, fixed=TRUE)
}

string_array_schema <- function() list(type="array", items=list(type="string"))

patch_operation_schema <- function() list(
  type="object", additionalProperties=FALSE,
  required=c("target","path","operation","value_json","match_key","match_value_json","rationale"),
  properties=list(
    target=list(type="string", enum=c("project_config","search_terms")),
    path=string_array_schema(),
    operation=list(
      type="string",
      enum=c("set","append_unique","remove_value","append_record","remove_record_by_key")
    ),
    value_json=list(type="string"),
    match_key=list(type="string"),
    match_value_json=list(type="string"),
    rationale=list(type="string")
  )
)

human_decision_schema <- function() list(
  type="object", additionalProperties=FALSE,
  required=c(
    "decision_id","question","why_it_matters","suggested_options",
    "recommended_option","recommendation_rationale","affected_components","option_patches"
  ),
  properties=list(
    decision_id=list(type="string"),
    question=list(type="string"),
    why_it_matters=list(type="string"),
    suggested_options=string_array_schema(),
    recommended_option=list(type="string"),
    recommendation_rationale=list(type="string"),
    affected_components=string_array_schema(),
    option_patches=list(
      type="array",
      items=list(
        type="object", additionalProperties=FALSE,
        required=c("option","operations"),
        properties=list(
          option=list(type="string"),
          operations=list(type="array", items=patch_operation_schema())
        )
      )
    )
  )
)

build_initialization_json_schema <- function() list(
  type="object", additionalProperties=FALSE,
  required=c("project_config","search_terms","human_decisions_required","report"),
  properties=list(
    project_config=list(
      type="object", additionalProperties=FALSE,
      required=c("toolkit","project","review","eligibility","screening","paths"),
      properties=list(
        toolkit=list(
          type="object", additionalProperties=FALSE,
          required=c("name","version"),
          properties=list(name=list(type="string"), version=list(type="string"))
        ),
        project=list(
          type="object", additionalProperties=FALSE,
          required=c("id","slug","title","short_name"),
          properties=list(
            id=list(type="string"), slug=list(type="string"),
            title=list(type="string"), short_name=list(type="string")
          )
        ),
        review=list(
          type="object", additionalProperties=FALSE,
          required=c("type","objective","question","framework"),
          properties=list(
            type=list(type="string"),
            objective=list(type="string"),
            question=list(type="string"),
            framework=list(
              type="object", additionalProperties=FALSE,
              required=c("name","elements"),
              properties=list(
                name=list(type="string"),
                elements=list(
                  type="array",
                  items=list(
                    type="object", additionalProperties=FALSE,
                    required=c("label","values"),
                    properties=list(label=list(type="string"), values=string_array_schema())
                  )
                )
              )
            )
          )
        ),
        eligibility=list(
          type="object", additionalProperties=FALSE,
          required=c("inclusion","exclusion"),
          properties=list(inclusion=string_array_schema(), exclusion=string_array_schema())
        ),
        screening=list(
          type="object", additionalProperties=FALSE,
          required=c("decisions","decision_rules","exclusion_reasons"),
          properties=list(
            decisions=list(
              type="array", minItems=3, maxItems=3,
              items=list(type="string", enum=c("Include","Exclude","Unclear"))
            ),
            decision_rules=list(
              type="object", additionalProperties=FALSE,
              required=c("include","exclude","unclear"),
              properties=list(
                include=string_array_schema(),
                exclude=string_array_schema(),
                unclear=string_array_schema()
              )
            ),
            exclusion_reasons=list(
              type="array",
              items=list(
                type="object", additionalProperties=FALSE,
                required=c("code","label","description"),
                properties=list(
                  code=list(type="string"),
                  label=list(type="string"),
                  description=list(type="string")
                )
              )
            )
          )
        ),
        paths=list(
          type="object", additionalProperties=FALSE,
          required=c("raw_data","processed_data","screening_results","logs","search_strategies"),
          properties=list(
            raw_data=list(type="string"),
            processed_data=list(type="string"),
            screening_results=list(type="string"),
            logs=list(type="string"),
            search_strategies=list(type="string")
          )
        )
      )
    ),
    search_terms=list(
      type="object", additionalProperties=FALSE,
      required=c("project","search","validation"),
      properties=list(
        project=list(
          type="object", additionalProperties=FALSE,
          required=c("id","title","short_name"),
          properties=list(
            id=list(type="string"), title=list(type="string"), short_name=list(type="string")
          )
        ),
        search=list(
          type="object", additionalProperties=FALSE,
          required=c("concepts","combination","outcome_terms_policy"),
          properties=list(
            concepts=list(
              type="array",
              items=list(
                type="object", additionalProperties=FALSE,
                required=c(
                  "concept_id","label","role","required","free_text_terms",
                  "abbreviations","spelling_variants","controlled_vocabulary"
                ),
                properties=list(
                  concept_id=list(type="string"),
                  label=list(type="string"),
                  role=list(type="string"),
                  required=list(type="boolean"),
                  free_text_terms=string_array_schema(),
                  abbreviations=string_array_schema(),
                  spelling_variants=string_array_schema(),
                  controlled_vocabulary=list(
                    type="object", additionalProperties=FALSE,
                    required=c("pubmed_mesh_candidates","embase_emtree_candidates"),
                    properties=list(
                      pubmed_mesh_candidates=string_array_schema(),
                      embase_emtree_candidates=string_array_schema()
                    )
                  )
                )
              )
            ),
            combination=list(
              type="object", additionalProperties=FALSE,
              required=c("operator_between_concepts","notes"),
              properties=list(
                operator_between_concepts=list(type="string"),
                notes=list(type="string")
              )
            ),
            outcome_terms_policy=list(type="string")
          )
        ),
        validation=list(
          type="object", additionalProperties=FALSE,
          required=c("known_relevant_ids","controlled_vocabulary_requires_verification"),
          properties=list(
            known_relevant_ids=string_array_schema(),
            controlled_vocabulary_requires_verification=list(type="boolean")
          )
        )
      )
    ),
    human_decisions_required=list(type="array", items=human_decision_schema()),
    report=list(
      type="object", additionalProperties=FALSE,
      required=c("summary","assumptions","warnings"),
      properties=list(
        summary=list(type="string"),
        assumptions=string_array_schema(),
        warnings=string_array_schema()
      )
    )
  )
)
