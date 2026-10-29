# 07_prep_dashboard.R -- assemble dashboard_data.rds for app/app.R
source("R/00_config.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
})

# nolint start: object_usage_linter.
labels     <- readRDS(file.path(OUT_DIR, "pivot_labels.rds"))
archetypes <- readRDS(file.path(OUT_DIR, "archetypes.rds"))
features   <- readRDS(file.path(OUT_DIR, "features.rds"))
traj_long  <- readRDS(file.path(OUT_DIR, "traj_long.rds"))
medoids    <- readRDS(file.path(OUT_DIR, "medoids.rds"))

# Load the final model scores (written by 05_analysis_models.R)
scored_csv <- file.path(OUT_DIR, "player_pivot_predictions.csv")
if (file.exists(scored_csv)) {
  scored <- read_csv(scored_csv, show_col_types = FALSE)
} else {
  # Fallback: no model ran; use NA probabilities
  scored <- labels |>
    inner_join(select(features, player_id), by = "player_id") |>
    mutate(prob_pivot = NA_real_)
}

# Test-set player flags (from 05 predictions)
test_pred_csv <- file.path(OUT_DIR, "test_predictions.csv")
if (file.exists(test_pred_csv)) {
  test_preds <- read_csv(test_pred_csv, show_col_types = FALSE)
  if (!"player_id" %in% names(test_preds)) {
    test_player_ids <- integer(0)
  } else {
    test_player_ids <- test_preds$player_id
  }
} else {
  test_player_ids <- integer(0)
}

# ---- Players table ---------------------------------------------------------
players_tbl <- labels |>
  inner_join(archetypes, by = "player_id") |>
  inner_join(
    select(
      features, player_id, name, dob, hand, height,
      best_rank, early_singles, early_win_rate, early_doubles_share
    ),
    by = "player_id"
  ) |>
  left_join(select(scored, player_id, prob_pivot), by = "player_id") |>
  mutate(
    in_test = player_id %in% test_player_ids,
    prob_pivot = coalesce(prob_pivot, NA_real_)
  )

# ---- Trajectory data (only clustered players) ------------------------------
traj_plot <- traj_long |>
  filter(player_id %in% archetypes$player_id) |>
  inner_join(select(archetypes, player_id, archetype), by = "player_id") |>
  mutate(rank = pmin(exp(value * log(RANK_CAP)), RANK_CAP))

# ---- Archetype summary with pivot rate and CI ------------------------------
archetype_summary <- players_tbl |>
  group_by(archetype) |>
  summarise(
    n            = n(),
    pivot_events = sum(pivoted == "yes"),
    pivot_rate   = mean(pivoted == "yes"),
    lo = tryCatch(
      prop.test(sum(pivoted == "yes"), n())$conf.int[1],
      error = function(e) NA_real_
    ),
    hi = tryCatch(
      prop.test(sum(pivoted == "yes"), n())$conf.int[2],
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

# ---- Bundle and save -------------------------------------------------------
dashboard_data <- list(
  players           = players_tbl,
  traj              = traj_plot,
  medoids           = medoids,
  archetype_summary = archetype_summary
)
saveRDS(dashboard_data, file.path(OUT_DIR, "dashboard_data.rds"))

message("dashboard_data.rds written.")
message("  Players: ", nrow(players_tbl))
message("  Archetypes: ", nrow(archetype_summary))
message("  Traj rows: ", nrow(traj_plot))
print(archetype_summary)
# nolint end: object_usage_linter.
