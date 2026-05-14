# global.R
# Shared libraries, constants, and helper functions for the CCOT dashboard.
# Sourced once at app startup; objects here are visible to ui.R and server.R.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(plotly)
  library(DT)
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(scales)
  library(glue)
  library(forcats)
})

# ---- Constants ------------------------------------------------------------

OBSERVATION_WINDOW_HOURS <- 72
TRANSFER_WINDOWS_HOURS   <- c(2, 12, 24, 48)

PROGRAM_START_DATE <- as.Date("2024-01-01")
PROGRAM_END_DATE   <- Sys.Date()

FLOOR_UNITS <- c(
  "7 Medical", "7 Surgical", "8 Hematology/Oncology",
  "9 Cardiology", "9 Neurology", "10 General Peds"
)

RECOMMENDATION_LEVELS <- c("monitor", "escalate", "RRT called", "transfer")

NOMAD_SUBSCALES <- c("acceptability", "appropriateness", "feasibility")

# Time-of-day bucketing used across tabs. Edit here to change everywhere.
SHIFT_BUCKETS <- tibble::tribble(
  ~shift,        ~start_hour, ~end_hour,
  "Night (00-07)",        0,         7,
  "Day (07-15)",          7,        15,
  "Evening (15-23)",     15,        23,
  "Late (23-00)",        23,        24
)

# ---- Helper functions -----------------------------------------------------

#' Bucket a POSIXct into a shift label (Night / Day / Evening / Late).
shift_of <- function(x) {
  h <- lubridate::hour(x)
  dplyr::case_when(
    h >= 0  & h < 7  ~ "Night (00-07)",
    h >= 7  & h < 15 ~ "Day (07-15)",
    h >= 15 & h < 23 ~ "Evening (15-23)",
    TRUE             ~ "Late (23-00)"
  ) |>
    factor(levels = SHIFT_BUCKETS$shift)
}

#' Hour-of-day as integer 0..23, useful for polar/heatmap plots.
hour_of <- function(x) lubridate::hour(x) + lubridate::minute(x) / 60

#' Day of week as ordered factor starting Monday.
dow_of <- function(x) {
  factor(
    lubridate::wday(x, label = TRUE, abbr = TRUE, week_start = 1),
    levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    ordered = TRUE
  )
}

#' Hours between two POSIXct vectors (numeric, possibly negative).
hours_between <- function(t1, t0) {
  as.numeric(difftime(t1, t0, units = "hours"))
}

#' Format a count + denominator as "n / N (xx%)".
pct_fmt <- function(num, den, digits = 1) {
  if (is.na(den) || den == 0) return("—")
  sprintf("%d / %d (%.*f%%)", num, den, digits, 100 * num / den)
}

#' Currency formatter for charge amounts.
usd <- scales::label_dollar(accuracy = 1)

#' Apply the global sidebar date filter to any dataframe that has a
#' POSIXct/Date column named `date_col`.
filter_by_date <- function(df, date_col, date_range) {
  if (is.null(date_range) || length(date_range) != 2) return(df)
  d <- as.Date(df[[date_col]])
  df[d >= date_range[1] & d <= date_range[2], , drop = FALSE]
}

#' Standard plotly config — strips the modebar clutter we never use.
plotly_config <- function(p) {
  plotly::config(
    p,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "select2d", "lasso2d", "autoScale2d",
      "hoverClosestCartesian", "hoverCompareCartesian",
      "toggleSpikelines"
    )
  )
}

#' bslib value_box with a small footnote line.
kpi_box <- function(title, value, footnote = NULL, icon = "activity",
                    theme_color = "primary") {
  bslib::value_box(
    title = title,
    value = value,
    showcase = bsicons::bs_icon(icon),
    theme = theme_color,
    p(footnote)
  )
}

# ---- Mock data load -------------------------------------------------------
# In production, replace this source() call with database/REDCap pulls
# (e.g. DBI::dbGetQuery against the Epic Clarity / Caboodle warehouse).

source("data/simulate_data.R", local = FALSE)

# Build derived analytic frames that every module reuses. These joins are
# the canonical "CCOT case" and "RRT case" definitions; modules should
# filter further but not redefine T0.

# CCOT cases: one row per encounter, anchored on FIRST assessment.
ccot_cases <- ccot_assessments |>
  dplyr::arrange(pat_enc_csn, first_assessment_datetime) |>
  dplyr::group_by(pat_enc_csn) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::rename(T0 = first_assessment_datetime) |>
  dplyr::left_join(
    encounters |> dplyr::select(pat_enc_csn, unit, age_years),
    by = "pat_enc_csn"
  ) |>
  dplyr::mutate(
    T0_hour  = hour_of(T0),
    T0_shift = shift_of(T0),
    T0_dow   = dow_of(T0)
  )

# Attach transfer / emergency-transfer outcomes to CCOT cases, with the
# 72h attribution window enforced.
ccot_outcomes <- ccot_cases |>
  dplyr::left_join(
    transfers |> dplyr::select(pat_enc_csn, transfer_datetime,
                               intubated_at_transfer, vasopressor_at_transfer),
    by = "pat_enc_csn"
  ) |>
  dplyr::mutate(
    hours_to_transfer  = hours_between(transfer_datetime, T0),
    within_window      = !is.na(hours_to_transfer) &
                         hours_to_transfer >= 0 &
                         hours_to_transfer <= OBSERVATION_WINDOW_HOURS,
    emergency_transfer = within_window &
                         (intubated_at_transfer | vasopressor_at_transfer),
    transfer_hour      = hour_of(transfer_datetime),
    transfer_shift     = shift_of(transfer_datetime)
  )

# RRT cases: index = activation timestamp.
rrt_cases <- rrt_fact |>
  dplyr::rename(T0 = activation_datetime) |>
  dplyr::left_join(
    encounters |> dplyr::select(pat_enc_csn, unit, age_years),
    by = "pat_enc_csn"
  ) |>
  dplyr::left_join(
    transfers |> dplyr::select(pat_enc_csn, transfer_datetime,
                               intubated_at_transfer, vasopressor_at_transfer),
    by = "pat_enc_csn"
  ) |>
  dplyr::mutate(
    hours_to_transfer  = hours_between(transfer_datetime, T0),
    within_window      = !is.na(hours_to_transfer) &
                         hours_to_transfer >= 0 &
                         hours_to_transfer <= OBSERVATION_WINDOW_HOURS,
    emergency_transfer = within_window &
                         (intubated_at_transfer | vasopressor_at_transfer),
    T0_hour  = hour_of(T0),
    T0_shift = shift_of(T0),
    T0_dow   = dow_of(T0)
  )

# ---- Module sources ------------------------------------------------------

source("modules/tab_overview.R",       local = FALSE)
source("modules/tab_ccot_outcomes.R",  local = FALSE)
source("modules/tab_rrt_outcomes.R",   local = FALSE)
source("modules/tab_charges.R",        local = FALSE)
source("modules/tab_implementation.R", local = FALSE)
source("modules/tab_data_quality.R",   local = FALSE)
