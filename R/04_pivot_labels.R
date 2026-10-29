# 04_pivot_labels.R -- define and label singles-to-doubles pivots (Objective 3)
source("R/00_config.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(dplyr)
})

inner_join <- dplyr::inner_join

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "player_id", "window_end_month", "HORIZON_YEARS",
      "already_doubles", "date", "won", "is_tour",
      "follow_singles", "follow_tour_singles", "follow_doubles",
      "d_share", "all_d_share", "event", "first_dbl_date"
    )
  )
}

debut      <- readRDS(file.path(OUT_DIR, "debut.rds"))
features   <- readRDS(file.path(OUT_DIR, "features.rds"))
archetypes <- readRDS(file.path(OUT_DIR, "archetypes.rds"))
singles    <- readRDS(file.path(OUT_DIR, "singles_activity.rds"))
doubles    <- readRDS(file.path(OUT_DIR, "doubles_activity.rds"))

# ---- cohort: players who were clustered AND not already doubles-focused ----
cohort <- features |>
  semi_join(archetypes, by = "player_id") |>
  mutate(
    already_doubles = early_doubles_share >= SHARE_THR &
      early_doubles >= MIN_DOUBLES
  )
message(
  "Excluded as already doubles-focused in window: ",
  sum(cohort$already_doubles)
)
cohort <- cohort |>
  filter(!already_doubles) |>
  select(player_id, window_end_month)

# ---- total activity in the 5-year follow-up window -------------------------
# nolint start: object_usage_linter.
follow_window <- function(act) {
  act |>
    inner_join(cohort, by = "player_id") |>
    filter(
      date >= window_end_month,
      date < window_end_month %m+% months(HORIZON_YEARS * 12)
    )
}
# nolint end: object_usage_linter.

total_singles <- follow_window(singles) |>
  count(player_id, name = "follow_singles")

tour_singles_src <- if ("is_tour" %in% names(singles)) {
  filter(singles, is_tour)
} else {
  singles
}
total_tour_singles <- follow_window(tour_singles_src) |>
  count(player_id, name = "follow_tour_singles")

total_doubles <- follow_window(doubles) |>
  count(player_id, name = "follow_doubles")

# ---- for survival analysis: first doubles match date in follow-up ----------
first_dbl_date <- follow_window(doubles) |>
  group_by(player_id) |>
  summarise(first_dbl_date = min(date), .groups = "drop")

# ---- pivot label: cumulative tour doubles share & volume -------------------
labels <- cohort |>
  left_join(total_singles, by = "player_id") |>
  left_join(total_tour_singles, by = "player_id") |>
  left_join(total_doubles, by = "player_id") |>
  left_join(first_dbl_date, by = "player_id") |>
  mutate(
    follow_singles      = coalesce(follow_singles, 0L),
    follow_tour_singles = coalesce(follow_tour_singles, 0L),
    follow_doubles      = coalesce(follow_doubles, 0L),
    # Tour-level doubles share:
    d_share = if_else(
      follow_tour_singles + follow_doubles > 0,
      follow_doubles / (follow_tour_singles + follow_doubles),
      0
    ),
    all_d_share = if_else(
      follow_singles + follow_doubles > 0,
      follow_doubles / (follow_singles + follow_doubles),
      0
    ),
    event   = as.integer(follow_doubles >= MIN_DOUBLES & d_share >= SHARE_THR),
    pivoted = factor(if_else(event == 1, "yes", "no"), levels = c("no", "yes")),
    # time_months: months from window end to qualifying doubles match or censor
    time_months = if_else(
      event == 1,
      pmin(
        as.integer(
          interval(window_end_month, first_dbl_date) / months(1)
        ) + 1L,
        HORIZON_YEARS * 12L
      ),
      HORIZON_YEARS * 12L
    )
  ) |>
  select(
    player_id, event, pivoted, time_months,
    follow_singles, follow_tour_singles, follow_doubles,
    d_share, all_d_share
  )

# ---- sensitivity of pivot rate to definition -------------------------------
sensitivity <- expand_grid(
  share_thr   = c(0.30, 0.40, 0.50, 0.60),
  min_doubles = c(5, 10, 15, 20)
) |>
  mutate(
    pivot_rate = map2_dbl(share_thr, min_doubles, function(s, m) {
      mean(labels$follow_doubles >= m & labels$d_share >= s)
    }),
    pivot_count = map2_int(share_thr, min_doubles, function(s, m) {
      sum(labels$follow_doubles >= m & labels$d_share >= s)
    })
  )
print(sensitivity)

saveRDS(labels, file.path(OUT_DIR, "pivot_labels.rds"))
write_csv(sensitivity, file.path(OUT_DIR, "pivot_sensitivity.csv"))
message(
  sprintf(
    "Cohort: %d | pivot rate (share>=%.2f, n>=%d): %.1f%%",
    nrow(labels), SHARE_THR, MIN_DOUBLES, 100 * mean(labels$event)
  )
)
