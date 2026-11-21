# Targeted, NO PROVIDER calls, NO DB writes, NO scientific engine execution.
# Run from the canonical repository root: Rscript --vanilla tests/test_outcome_one_by_one.R
stopifnot(file.exists("web/ui/evidence.R"),file.exists("web/R/evidence.R"))
parse(file="web/ui/evidence.R")
parse(file="web/R/evidence.R")
ui <- new.env(parent=globalenv())
model <- new.env(parent=globalenv())
source("web/R/evidence.R",local=model)
source("web/ui/evidence.R",local=ui)
ui$`%||b2%` <- model$`%||b2%`

ai <- list(
 screening_id="SCR_001",study_id="STUDY-1",outcome_row_id="AI-1",
 outcome_name="All-cause mortality and reintervention",
 intervention_events="15",intervention_total="120",
 comparator_events="23",comparator_total="118",
 data_type="dichotomous",timepoint="1 year",
 source_location="Table 2",verification_status="NOT VERIFIED",
 .row_id="saved-ai-unique-one",.source="AI RECOMMENDATION")
ai2 <- ai
ai2$.row_id <- "saved-ai-unique-two"
ai2$outcome_name <- "Reintervention"
ai2$intervention_events <- "4"
ai2$comparator_events <- "6"
other <- ai
other$.row_id <- "other-study"
other$screening_id <- "SCR_002"
other$study_id <- "STUDY-2"
packet<-list(stage="EXTRACTION",targeted_verification_source="CANONICAL_R25B",
             tables=list(AI_outcome_data=list(ai,ai2,other)))
human<-list(screening_id="SCR_001",study_id="STUDY-1",
            outcome_name="All-cause mortality",.row_id="human-1")
editable<-c("human_verified_intervention_events","human_verified_intervention_total",
 "human_verified_comparator_events","human_verified_comparator_total",
 "human_verified_timepoint","human_verified_data_type",
 "source_location","source_evidence","human_include_in_planned_analysis",
 "verification_status","outcome_name","study_id")

stopifnot(ui$web_b2_ui_outcome_label_needs_disambiguation(ai$outcome_name))
stopifnot(!ui$web_b2_ui_outcome_label_needs_disambiguation("All-cause mortality"))
d <- ui$web_b2_ui_ai_outcome_drafts(packet,human,editable)
stopifnot(!is.null(d),length(d$candidates)==2L,
          "saved-ai-unique-one" %in% names(d$candidates),
          !"other-study" %in% names(d$candidates))
a <- d$candidates[["saved-ai-unique-one"]]
stopifnot(isTRUE(a$requires_manual),!isTRUE(a$exact_label),
          identical(a$values$human_verified_intervention_total,"120"),
          identical(a$values$human_verified_intervention_events,"15"),
          is.null(a$values$human_include_in_planned_analysis),
          is.null(a$values$verification_status),
          is.null(a$values$outcome_name))

human2 <- human;human2$outcome_name<-"Reintervention"
d2<-ui$web_b2_ui_ai_outcome_drafts(packet,human2,editable)
stopifnot(isTRUE(d2$candidates[["saved-ai-unique-two"]]$exact_label),
          !isTRUE(d2$candidates[["saved-ai-unique-two"]]$requires_manual),
          isTRUE(d2$candidates[["saved-ai-unique-one"]]$requires_manual))

packet$targeted_verification_source<-NULL
raw_fields<-c("outcome_name","intervention_events","intervention_total",
              "comparator_events","comparator_total","data_type","timepoint")
d3<-ui$web_b2_ui_ai_outcome_drafts(packet,human,raw_fields)
stopifnot(identical(d3$candidates[["saved-ai-unique-one"]]$values$intervention_total,"120"))

# The untouched AI row is never split or modified by a browser-only proposal.
stopifnot(identical(packet$tables$AI_outcome_data[[1L]]$outcome_name,ai$outcome_name),
          identical(packet$tables$AI_outcome_data[[1L]]$intervention_events,"15"))

# New target rows must inherit report/PDF identity but NOT original AI numbers.
source_row<-c(ai,list(.pdf_sha256="pdfsha1",verification_tier="Tier 1",
 human_verified_intervention_events="99",human_verified_intervention_total="100",
 human_include_in_planned_analysis="Yes",.actor="former-actor",.verified_by="other-actor",
 .values=list(intervention_events=list(human_value="99"))))
new_row<-model$web_b2_blank_new_outcome_row(source_row,editable,"new-human-id","SCR_001_NEW")
stopifnot(identical(new_row$screening_id,"SCR_001"),
          identical(new_row$study_id,"STUDY-1"),
          identical(new_row$.pdf_sha256,"pdfsha1"),
          identical(new_row$outcome_row_id,"SCR_001_NEW"),
          identical(new_row$outcome_name,""),
          identical(new_row$intervention_events,""),
          identical(new_row$intervention_total,""),
          identical(new_row$comparator_events,""),
          identical(new_row$comparator_total,""),
          identical(new_row$human_verified_intervention_total,""),
          identical(new_row$human_verified_intervention_events,""),
          identical(new_row$human_include_in_planned_analysis,"No"),
          identical(new_row$verification_status,"Needs clarification"),
          is.null(new_row$.actor),is.null(new_row$.verified_by),
          is.null(new_row$.values))

# No source row may be mapped to a different study by label alone.
human_bad<-human;human_bad$study_id<-"STUDY-3"
stopifnot(is.null(ui$web_b2_ui_ai_outcome_drafts(packet,human_bad,editable)))
human_blank<-human;human_blank$outcome_name<-""
stopifnot(is.null(ui$web_b2_ui_ai_outcome_drafts(packet,human_blank,editable)))
cat("PASS: Outcome one-by-one, compound-label manual mapping, R25B human overlay, no numeric cloning, report/study identity, and no AI/Human gate bypass\n")
