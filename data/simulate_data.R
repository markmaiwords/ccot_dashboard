# data/simulate_data.R
# Generates mock dataframes for the CCOT dashboard. Each block is wrapped
# with comments showing what should replace it once the real data source
# is wired up. Set seed so plots are stable between sessions.

set.seed(20260514)

.n_encounters <- 1200

# ---- encounters (base patient/encounter spine) ---------------------------
# REAL SOURCE: Epic Clarity HSP_ACCOUNT / PAT_ENC_HSP, one row per inpatient
# encounter. Keep pat_enc_csn as the join key throughout.
encounters <- tibble::tibble(
  pat_enc_csn = sprintf("CSN%07d", seq_len(.n_encounters)),
  patient_id  = sprintf("MRN%06d", sample(100000:999999, .n_encounters, TRUE)),
  encounter_id = sprintf("ENC%07d", seq_len(.n_encounters)),
  unit         = sample(FLOOR_UNITS, .n_encounters, replace = TRUE,
                        prob = c(0.22, 0.18, 0.20, 0.15, 0.10, 0.15)),
  age_years    = round(stats::runif(.n_encounters, 0, 18), 1),
  admit_datetime = as.POSIXct(PROGRAM_START_DATE) +
    sample(0:as.integer(difftime(PROGRAM_END_DATE, PROGRAM_START_DATE,
                                 units = "secs")),
           .n_encounters, replace = TRUE)
)

# ---- ccot_assessments ---------------------------------------------------
# REAL SOURCE: produced by etl_ccot_notes.R parsing free-text Epic CCOT
# notes. Multiple assessments per encounter allowed; downstream code uses
# the first per encounter as T0.
.n_ccot <- 850
.ccot_csns <- sample(encounters$pat_enc_csn, .n_ccot)

# Inject a realistic time-of-day distribution: CCOT rounds on days/evenings
# more than nights. Sample an hour from a weighted hourly distribution.
.hour_weights <- c(rep(0.4, 7), rep(2.0, 8), rep(2.5, 8), rep(0.6, 1))
.hours <- sample(0:23, .n_ccot, replace = TRUE, prob = .hour_weights)

ccot_assessments <- tibble::tibble(
  pat_enc_csn = .ccot_csns,
  patient_id  = encounters$patient_id[match(.ccot_csns, encounters$pat_enc_csn)],
  encounter_id = encounters$encounter_id[match(.ccot_csns,
                                               encounters$pat_enc_csn)],
  first_assessment_datetime =
    encounters$admit_datetime[match(.ccot_csns, encounters$pat_enc_csn)] +
    lubridate::hours(sample(4:96, .n_ccot, replace = TRUE)),
  recommendation_category = sample(
    RECOMMENDATION_LEVELS, .n_ccot, replace = TRUE,
    prob = c(0.55, 0.25, 0.12, 0.08)
  ),
  note_parse_success = stats::runif(.n_ccot) > 0.06
)

# Force the time-of-day distribution we want.
lubridate::hour(ccot_assessments$first_assessment_datetime) <- .hours
lubridate::minute(ccot_assessments$first_assessment_datetime) <-
  sample(0:59, .n_ccot, replace = TRUE)

ccot_assessments$recommendation_text <- dplyr::case_when(
  ccot_assessments$recommendation_category == "monitor" ~
    "Continue current monitoring on the unit. Re-evaluate in 4 hours.",
  ccot_assessments$recommendation_category == "escalate" ~
    "Escalate to primary team; consider increased monitoring and labs.",
  ccot_assessments$recommendation_category == "RRT called" ~
    "RRT activation in progress for acute respiratory distress.",
  ccot_assessments$recommendation_category == "transfer" ~
    "Recommend transfer to PICU for higher level of care.",
  TRUE ~ "Assessment performed."
)

# Some failed-parse rows get realistic empty text.
ccot_assessments$recommendation_text[!ccot_assessments$note_parse_success] <-
  NA_character_

# ---- deterioration_fact -------------------------------------------------
# REAL SOURCE: structured pediatric deterioration index pulls from Epic
# Caboodle, one row per discrete deterioration event with datetime.
.n_det <- 320
deterioration_fact <- tibble::tibble(
  pat_enc_csn = sample(encounters$pat_enc_csn, .n_det, replace = TRUE),
  deterioration_datetime = as.POSIXct(PROGRAM_START_DATE) +
    sample(0:as.integer(difftime(PROGRAM_END_DATE, PROGRAM_START_DATE,
                                 units = "secs")),
           .n_det, replace = TRUE),
  event_type = sample(
    c("hypotension", "hypoxia", "altered_mentation", "tachycardia"),
    .n_det, replace = TRUE
  ),
  severity = sample(c("mild", "moderate", "severe"), .n_det, TRUE,
                    prob = c(0.5, 0.35, 0.15))
)

# ---- rrt_fact -----------------------------------------------------------
# REAL SOURCE: Epic RRT/Code event log. activation_reason is a single
# free-text field; downstream code in tab_rrt_outcomes uses stringr to
# bucket the common reasons.
.n_rrt <- 220
.rrt_csns <- sample(encounters$pat_enc_csn, .n_rrt)
.rrt_hour_weights <- c(rep(1.2, 7), rep(1.5, 8), rep(1.8, 8), rep(1.0, 1))
.rrt_hours <- sample(0:23, .n_rrt, replace = TRUE, prob = .rrt_hour_weights)

rrt_fact <- tibble::tibble(
  pat_enc_csn = .rrt_csns,
  activation_datetime =
    encounters$admit_datetime[match(.rrt_csns, encounters$pat_enc_csn)] +
    lubridate::hours(sample(6:120, .n_rrt, replace = TRUE)),
  activation_reason = sample(
    c(
      "Respiratory distress, increased work of breathing",
      "Hypotension and poor perfusion",
      "Tachycardia with altered mental status",
      "Seizure activity",
      "Family concern about clinical change",
      "Hypoxia, desaturation to 80s",
      "Bradycardia with apnea",
      "Concern for sepsis"
    ),
    .n_rrt, replace = TRUE
  ),
  disposition = sample(
    c("Remained on floor", "Transferred to PICU", "Transferred to CICU"),
    .n_rrt, replace = TRUE, prob = c(0.45, 0.45, 0.10)
  )
)
lubridate::hour(rrt_fact$activation_datetime) <- .rrt_hours
lubridate::minute(rrt_fact$activation_datetime) <-
  sample(0:59, .n_rrt, replace = TRUE)

# ---- transfers ----------------------------------------------------------
# REAL SOURCE: ADT transfer events from Clarity, joined to intubation
# (procedure/medication) and vasopressor (MAR) flags at time of transfer.
.transfer_csns <- unique(c(
  sample(ccot_assessments$pat_enc_csn,
         floor(0.18 * nrow(ccot_assessments))),
  sample(rrt_fact$pat_enc_csn, floor(0.55 * nrow(rrt_fact)))
))
.n_tx <- length(.transfer_csns)
.tx_hour_weights <- c(rep(1.0, 24))  # transfers reasonably uniform

transfers <- tibble::tibble(
  pat_enc_csn = .transfer_csns,
  transfer_datetime =
    encounters$admit_datetime[match(.transfer_csns, encounters$pat_enc_csn)] +
    lubridate::hours(sample(8:140, .n_tx, replace = TRUE)),
  intubated_at_transfer   = stats::runif(.n_tx) < 0.18,
  vasopressor_at_transfer = stats::runif(.n_tx) < 0.12
)
lubridate::hour(transfers$transfer_datetime) <-
  sample(0:23, .n_tx, replace = TRUE, prob = .tx_hour_weights)
lubridate::minute(transfers$transfer_datetime) <-
  sample(0:59, .n_tx, replace = TRUE)

# ---- flowsheet_rows (PEWS components) -----------------------------------
# REAL SOURCE: Epic flowsheet rows (FLO_MEAS_ID) for the PEWS components.
# One row per component per timestamp. Composite PEWS computed below.
.flowsheet_csns <- sample(encounters$pat_enc_csn,
                          floor(0.85 * nrow(encounters)))
flowsheet_rows <- purrr::map_dfr(.flowsheet_csns, function(csn) {
  admit <- encounters$admit_datetime[encounters$pat_enc_csn == csn]
  n <- sample(8:40, 1)
  ts <- admit + lubridate::hours(sort(sample(0:150, n, replace = TRUE)))
  tibble::tibble(
    pat_enc_csn = csn,
    flowsheet_datetime = ts,
    behavior_score    = sample(0:3, n, replace = TRUE, prob = c(.6,.25,.1,.05)),
    cardiovascular_score = sample(0:3, n, replace = TRUE,
                                  prob = c(.55,.25,.15,.05)),
    respiratory_score = sample(0:3, n, replace = TRUE,
                               prob = c(.5,.3,.15,.05))
  )
})

# Composite PEWS = sum of components.
flowsheet_rows <- flowsheet_rows |>
  dplyr::mutate(
    pews_total = behavior_score + cardiovascular_score + respiratory_score
  )

# ---- charges (pro-fee) --------------------------------------------------
# REAL SOURCE: billing / pro-fee charge feed; links by pat_enc_csn + date.
.providers <- c("Dr. Patel", "Dr. Nguyen", "Dr. Okafor", "Dr. Chen",
                "Dr. Garcia", "Dr. Reyes")
.cpt_codes <- tibble::tribble(
  ~code,   ~descr,                          ~typical_amount,
  "99231", "Subsequent hospital care low",   85,
  "99232", "Subsequent hospital care mod",  135,
  "99233", "Subsequent hospital care high", 195,
  "99291", "Critical care first 30-74 min", 290,
  "99292", "Critical care add'l 30 min",    140
)

.billed_csns <- sample(ccot_assessments$pat_enc_csn,
                       floor(0.72 * nrow(ccot_assessments)))
.n_charges <- length(.billed_csns) * 2
charges <- tibble::tibble(
  pat_enc_csn = sample(.billed_csns, .n_charges, replace = TRUE)
) |>
  dplyr::mutate(
    charge_date = as.Date(
      ccot_assessments$first_assessment_datetime[
        match(pat_enc_csn, ccot_assessments$pat_enc_csn)
      ]
    ) + sample(0:3, dplyr::n(), replace = TRUE),
    cpt_code = sample(.cpt_codes$code, dplyr::n(), replace = TRUE,
                      prob = c(0.30, 0.30, 0.15, 0.18, 0.07)),
    provider = sample(.providers, dplyr::n(), replace = TRUE),
    unit = encounters$unit[match(pat_enc_csn, encounters$pat_enc_csn)]
  ) |>
  dplyr::left_join(.cpt_codes, by = c("cpt_code" = "code")) |>
  dplyr::mutate(
    charge_amount = round(typical_amount * stats::runif(dplyr::n(), 0.85, 1.15))
  ) |>
  dplyr::select(pat_enc_csn, charge_date, cpt_code, cpt_descr = descr,
                provider, unit, charge_amount)

# ---- nomad_survey -------------------------------------------------------
# REAL SOURCE: REDCap export of modified NoMAD instrument. Two tracks:
# CCOT team (weekly, dense) and floor providers (sparse, ad-hoc).
.weeks <- seq.Date(PROGRAM_START_DATE, PROGRAM_END_DATE, by = "week")
.ccot_team_ids <- sprintf("ccot_%02d", 1:6)
nomad_ccot <- tidyr::expand_grid(
  respondent_id = .ccot_team_ids,
  survey_date   = .weeks
) |>
  dplyr::mutate(
    respondent_role = "CCOT team",
    acceptability   = pmin(5, pmax(1, round(stats::rnorm(dplyr::n(), 4.0, 0.6), 1))),
    appropriateness = pmin(5, pmax(1, round(stats::rnorm(dplyr::n(), 4.1, 0.5), 1))),
    feasibility     = pmin(5, pmax(1, round(stats::rnorm(dplyr::n(), 3.7, 0.7), 1))),
    free_text = sample(c(
      "Workflow has become routine for our team.",
      "Note template still cumbersome on overnight admits.",
      "Floor RN engagement has improved this month.",
      ""
    ), dplyr::n(), replace = TRUE)
  )

.floor_ids <- sprintf("floor_%02d", 1:40)
nomad_floor <- tibble::tibble(
  respondent_id   = sample(.floor_ids, 70, replace = TRUE),
  survey_date     = sample(.weeks, 70, replace = TRUE),
  respondent_role = "Floor provider",
  acceptability   = pmin(5, pmax(1, round(stats::rnorm(70, 3.6, 0.8), 1))),
  appropriateness = pmin(5, pmax(1, round(stats::rnorm(70, 3.7, 0.7), 1))),
  feasibility     = pmin(5, pmax(1, round(stats::rnorm(70, 3.4, 0.9), 1))),
  free_text       = sample(c(
    "CCOT recommendations are clear and actionable.",
    "Communication around recommendations could improve.",
    "Helpful having proactive eyes on at-risk patients.",
    ""
  ), 70, replace = TRUE)
)

nomad_survey <- dplyr::bind_rows(nomad_ccot, nomad_floor)

# ---- cleanup intermediate vars ------------------------------------------
rm(list = ls(pattern = "^\\."))
