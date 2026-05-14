# etl_ccot_notes.R
# ETL script: parse free-text Epic CCOT notes into the structured
# ccot_assessments dataframe consumed by the dashboard.
#
# Usage (interactive or scheduled):
#   source("etl_ccot_notes.R")
#   ccot_assessments <- run_ccot_etl(notes_df)
#
# `notes_df` is expected to have columns:
#   pat_enc_csn  (chr)
#   patient_id   (chr)
#   encounter_id (chr)
#   note_id      (chr)
#   note_datetime (POSIXct)  — datetime the note was filed
#   note_text    (chr)       — full free-text note body
#
# Replace the regex skeletons below with your institution's CCOT note
# template patterns. Sections are typically delimited by bolded headers
# (e.g. "Assessment:", "Recommendation:", "Plan:"); the exact wording
# is institution-specific, so the patterns are intentionally permissive
# placeholders.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(stringr)
})

# ---- Section header patterns -------------------------------------------
# INSTITUTION-SPECIFIC: edit these regexes to match the exact section
# headers in your CCOT SmartPhrase / template.
SECTION_PATTERNS <- list(
  assessment     = stringr::regex(
    "(?:^|\\n)\\s*assessment\\s*[:\\-]\\s*(.*?)(?=\\n\\s*[a-z ]+\\s*[:\\-]|\\Z)",
    ignore_case = TRUE, dotall = TRUE
  ),
  recommendation = stringr::regex(
    "(?:^|\\n)\\s*recommendation(?:s)?\\s*[:\\-]\\s*(.*?)(?=\\n\\s*[a-z ]+\\s*[:\\-]|\\Z)",
    ignore_case = TRUE, dotall = TRUE
  ),
  plan           = stringr::regex(
    "(?:^|\\n)\\s*plan\\s*[:\\-]\\s*(.*?)(?=\\n\\s*[a-z ]+\\s*[:\\-]|\\Z)",
    ignore_case = TRUE, dotall = TRUE
  )
)

# ---- Recommendation-category keyword buckets ---------------------------
# INSTITUTION-SPECIFIC: tune the keyword set per local note style.
RECOMMENDATION_BUCKETS <- tibble::tribble(
  ~category,     ~pattern,
  "transfer",    "transfer\\s+to\\s+(picu|cicu|icu)|upgrade\\s+to\\s+icu",
  "RRT called",  "rrt\\s+(activation|called)|rapid\\s+response",
  "escalate",    "escalat|increase(d)?\\s+monitoring|notify\\s+attending",
  "monitor",     "continue\\s+(current\\s+)?monitor|re[- ]?evaluat|reassess"
)

extract_section <- function(text, pattern) {
  m <- stringr::str_match(text, pattern)
  out <- m[, 2]
  stringr::str_squish(ifelse(is.na(out), NA_character_, out))
}

classify_recommendation <- function(text) {
  txt <- tolower(ifelse(is.na(text), "", text))
  out <- rep(NA_character_, length(txt))
  for (i in seq_len(nrow(RECOMMENDATION_BUCKETS))) {
    hit <- stringr::str_detect(txt, RECOMMENDATION_BUCKETS$pattern[i])
    out[hit & is.na(out)] <- RECOMMENDATION_BUCKETS$category[i]
  }
  # Default to "monitor" when section was parsed but no keyword hit;
  # leave NA when the recommendation section itself was missing.
  out[is.na(out) & !is.na(text) & nzchar(text)] <- "monitor"
  out
}

#' Parse a single note. Returns a one-row tibble.
parse_one_note <- function(note_row) {
  txt <- note_row$note_text
  assessment <- extract_section(txt, SECTION_PATTERNS$assessment)
  recommendation <- extract_section(txt, SECTION_PATTERNS$recommendation)
  plan <- extract_section(txt, SECTION_PATTERNS$plan)

  category <- classify_recommendation(recommendation)
  parse_success <-
    !is.na(assessment) &&
    !is.na(recommendation) &&
    !is.na(category)

  tibble::tibble(
    pat_enc_csn  = note_row$pat_enc_csn,
    patient_id   = note_row$patient_id,
    encounter_id = note_row$encounter_id,
    note_id      = note_row$note_id,
    first_assessment_datetime = note_row$note_datetime,
    assessment_text     = assessment,
    recommendation_text = recommendation,
    plan_text           = plan,
    recommendation_category = category,
    note_parse_success  = parse_success
  )
}

#' Top-level ETL entry point. Returns a dataframe in the schema expected by
#' the dashboard (`ccot_assessments`). One row per parsed note; downstream
#' code in global.R slices to the first assessment per encounter for T0.
run_ccot_etl <- function(notes_df) {
  stopifnot(all(c("pat_enc_csn", "patient_id", "encounter_id",
                  "note_id", "note_datetime", "note_text") %in%
                names(notes_df)))
  notes_df |>
    dplyr::group_split(dplyr::row_number()) |>
    purrr::map_dfr(parse_one_note)
}

# ---- Example smoke test (disabled) -------------------------------------
# fake <- tibble::tibble(
#   pat_enc_csn = "CSN0000001", patient_id = "MRN000001",
#   encounter_id = "ENC0000001", note_id = "NOTE001",
#   note_datetime = Sys.time(),
#   note_text = "Assessment: Increased WOB.\nRecommendation: Transfer to PICU.\nPlan: Continue O2."
# )
# run_ccot_etl(fake)
