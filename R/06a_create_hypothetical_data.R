# Cardiac Surgery SR-MA Toolkit v2.0
# Hypothetical extraction dataset containing seven studies
#
# IMPORTANT:
# These data are fictional and should be used only for
# testing the toolkit and practicing R analysis.

source_file <- "data/raw/hypothetical_SGLT2i_data_extraction.csv"

dir.create(
  "data/raw",
  recursive = TRUE,
  showWarnings = FALSE
)

hypothetical_data <- tibble::tibble(
  study_id = paste0("study", 1:7),

  first_author = c(
    "Kim",
    "Lee",
    "Park",
    "Choi",
    "Smith",
    "Garcia",
    "Muller"
  ),

  publication_year = c(
    2020,
    2021,
    2021,
    2022,
    2023,
    2024,
    2025
  ),

  journal = c(
    "Journal of Cardiac Surgery",
    "Heart Surgery Forum",
    "European Journal of Cardio-Thoracic Surgery",
    "Annals of Thoracic Surgery",
    "Journal of Thoracic and Cardiovascular Surgery",
    "Interactive Cardiovascular and Thoracic Surgery",
    "Cardiovascular Diabetology"
  ),

  country = c(
    "Korea",
    "Korea",
    "Japan",
    "China",
    "United States",
    "Spain",
    "Germany"
  ),

  study_design = c(
    "retrospective cohort",
    "prospective cohort",
    "retrospective cohort",
    "randomized controlled trial",
    "retrospective cohort",
    "prospective cohort",
    "randomized controlled trial"
  ),

  enrollment_start = c(
    "2017-01-01",
    "2018-03-01",
    "2018-01-01",
    "2019-06-01",
    "2019-01-01",
    "2020-02-01",
    "2021-01-01"
  ),

  enrollment_end = c(
    "2019-12-31",
    "2020-12-31",
    "2020-06-30",
    "2021-12-31",
    "2022-12-31",
    "2023-01-31",
    "2024-06-30"
  ),

  total_sample_size = c(
    180,
    220,
    156,
    240,
    310,
    198,
    260
  ),

  follow_up_duration = c(
    30,
    90,
    30,
    30,
    365,
    90,
    30
  ),

  follow_up_unit = c(
    "days",
    "days",
    "days",
    "days",
    "days",
    "days",
    "days"
  ),

  cardiac_surgery_type = c(
    "CABG",
    "valve surgery",
    "CABG",
    "mixed cardiac surgery",
    "CABG",
    "valve surgery",
    "mixed cardiac surgery"
  ),

  urgent_or_elective = c(
    "elective",
    "elective",
    "mixed",
    "elective",
    "mixed",
    "elective",
    "elective"
  ),

  study_population = c(
    "patients with type 2 diabetes",
    "patients with type 2 diabetes",
    "patients with diabetes undergoing CABG",
    "adults undergoing cardiac surgery",
    "patients with diabetes undergoing CABG",
    "patients with heart failure and diabetes",
    "adults with type 2 diabetes"
  ),

  intervention_drug = c(
    "dapagliflozin",
    "empagliflozin",
    "dapagliflozin",
    "empagliflozin",
    "canagliflozin",
    "dapagliflozin",
    "empagliflozin"
  ),

  intervention_dose = c(
    "10 mg daily",
    "10 mg daily",
    "10 mg daily",
    "10 mg daily",
    "100 mg daily",
    "10 mg daily",
    "10 mg daily"
  ),

  intervention_timing = c(
    "preoperative",
    "preoperative",
    "preoperative",
    "perioperative",
    "preoperative",
    "preoperative",
    "perioperative"
  ),

  drug_discontinuation_before_surgery = c(
    3,
    2,
    3,
    3,
    2,
    3,
    3
  ),

  comparator_description = rep(
    "No SGLT2 inhibitor or standard care",
    7
  ),

  n_intervention = c(
    90,
    110,
    78,
    120,
    155,
    99,
    130
  ),

  n_control = c(
    90,
    110,
    78,
    120,
    155,
    99,
    130
  ),

  mean_age_intervention = c(
    66.2,
    68.4,
    67.1,
    65.8,
    69.2,
    67.6,
    66.9
  ),

  sd_age_intervention = c(
    7.8,
    8.1,
    7.5,
    8.0,
    7.2,
    8.4,
    7.7
  ),

  mean_age_control = c(
    66.8,
    68.0,
    67.6,
    66.1,
    69.5,
    68.1,
    67.3
  ),

  sd_age_control = c(
    7.5,
    8.3,
    7.9,
    8.2,
    7.4,
    8.0,
    7.8
  ),

  male_intervention = c(
    61,
    73,
    54,
    78,
    104,
    65,
    87
  ),

  male_control = c(
    60,
    71,
    52,
    80,
    101,
    63,
    85
  ),

  diabetes_intervention = c(
    90,
    110,
    78,
    120,
    155,
    99,
    130
  ),

  diabetes_control = c(
    90,
    110,
    78,
    120,
    155,
    99,
    130
  ),

  heart_failure_intervention = c(
    25,
    34,
    20,
    39,
    51,
    45,
    41
  ),

  heart_failure_control = c(
    27,
    32,
    22,
    41,
    53,
    47,
    43
  ),

  ckd_intervention = c(
    18,
    25,
    16,
    29,
    35,
    24,
    31
  ),

  ckd_control = c(
    20,
    27,
    18,
    31,
    37,
    26,
    33
  ),

  baseline_egfr_intervention = c(
    72.4,
    68.1,
    70.3,
    74.2,
    66.5,
    69.8,
    71.2
  ),

  baseline_egfr_control = c(
    71.8,
    67.7,
    69.9,
    73.6,
    65.9,
    69.1,
    70.8
  ),

  lvef_intervention = c(
    53.1,
    51.8,
    52.4,
    54.3,
    50.2,
    48.9,
    52.8
  ),

  lvef_control = c(
    52.8,
    51.5,
    52.0,
    53.9,
    49.8,
    48.5,
    52.4
  ),

  cabg_intervention = c(
    90,
    0,
    78,
    60,
    155,
    0,
    65
  ),

  cabg_control = c(
    90,
    0,
    78,
    60,
    155,
    0,
    65
  ),

  valve_surgery_intervention = c(
    0,
    110,
    0,
    60,
    0,
    99,
    65
  ),

  valve_surgery_control = c(
    0,
    110,
    0,
    60,
    0,
    99,
    65
  ),

  outcome_name = rep(
    "postoperative acute kidney injury",
    7
  ),

  outcome_definition = rep(
    "KDIGO criteria",
    7
  ),

  timepoint = rep(
    "in-hospital",
    7
  ),

  event_intervention = c(
    9,
    13,
    8,
    11,
    20,
    10,
    12
  ),

  total_intervention = c(
    90,
    110,
    78,
    120,
    155,
    99,
    130
  ),

  event_control = c(
    16,
    22,
    15,
    19,
    31,
    18,
    24
  ),

  total_control = c(
    90,
    110,
    78,
    120,
    155,
    99,
    130
  ),

  mean_intervention = rep(
    NA_real_,
    7
  ),

  sd_intervention = rep(
    NA_real_,
    7
  ),

  mean_control = rep(
    NA_real_,
    7
  ),

  sd_control = rep(
    NA_real_,
    7
  ),

  effect_measure = rep(
    "RR",
    7
  ),

  effect_estimate = rep(
    NA_real_,
    7
  ),

  lower_ci = rep(
    NA_real_,
    7
  ),

  upper_ci = rep(
    NA_real_,
    7
  ),

  adjusted_effect = rep(
    FALSE,
    7
  ),

  adjustment_variables = rep(
    NA_character_,
    7
  ),

  data_source = c(
    "Table 2",
    "Table 3",
    "Supplementary Table 1",
    "Table 2",
    "Table 4",
    "Table 3",
    "Supplementary Table 2"
  ),

  risk_of_bias_tool = c(
    "ROBINS-I",
    "ROBINS-I",
    "ROBINS-I",
    "RoB 2",
    "ROBINS-I",
    "ROBINS-I",
    "RoB 2"
  ),

  risk_of_bias_judgment = c(
    "moderate",
    "moderate",
    "serious",
    "some concerns",
    "moderate",
    "moderate",
    "low"
  ),

  reviewer_1 = rep(
    "Reviewer A",
    7
  ),

  reviewer_2 = rep(
    "Reviewer B",
    7
  ),

  extraction_status = rep(
    "hypothetical test data",
    7
  ),

  notes = rep(
    "Fictional dataset created for toolkit testing only",
    7
  )
)

readr::write_csv(
  hypothetical_data,
  source_file,
  na = ""
)

extraction_data <- readr::read_csv(
  source_file,
  show_col_types = FALSE,
  na = c("", "NA", "N/A", "Not reported", "NR")
)

cat("\n")
cat("========================================\n")
cat("Hypothetical extraction data created\n")
cat("========================================\n\n")

cat("File:\n")
cat(source_file, "\n\n")

cat("Number of studies:\n")
cat(dplyr::n_distinct(extraction_data$study_id), "\n\n")

cat("Number of rows:\n")
cat(nrow(extraction_data), "\n\n")

cat("Number of columns:\n")
cat(ncol(extraction_data), "\n\n")

print(
  extraction_data |>
    dplyr::select(
      study_id,
      first_author,
      publication_year,
      study_design,
      n_intervention,
      n_control,
      event_intervention,
      event_control
    )
)

assign(
  "extraction_data",
  extraction_data,
  envir = .GlobalEnv
)