# 02_trajectories.R -- career-age aligned early-career trajectories + features
source("R/00_config.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# nolint start: object_usage_linter.
rankings <- readRDS(file.path(OUT_DIR, "rankings.rds"))
singles  <- readRDS(file.path(OUT_DIR, "singles_activity.rds"))
doubles  <- readRDS(file.path(OUT_DIR, "doubles_activity.rds"))
players  <- readRDS(file.path(OUT_DIR, "players.rds"))

# ---- debut date = first singles match or first ranking ---------------------
debut <- bind_rows(
  select(singles, player_id, date),
  select(rankings, player_id, date)
) |>
  group_by(player_id) |>
  summarise(debut_date = min(date), .groups = "drop") |>
  filter(debut_date >= MIN_DEBUT_DATE, debut_date <= LAST_DEBUT_DATE) |>
  mutate(
    debut_month      = floor_date(debut_date, "month"),
    window_end_month = debut_month %m+% months(12 * EARLY_YEARS)
  )

# ---- monthly best rank inside early-career window --------------------------
monthly <- rankings |>
  inner_join(debut, by = "player_id") |>
  mutate(career_month = month_diff(debut_month, date)) |>
  filter(career_month >= 0, career_month < 12 * EARLY_YEARS) |>
  group_by(player_id, career_month) |>
  summarise(rank = min(rank), .groups = "drop")

months_ranked <- count(monthly, player_id, name = "months_ranked")
eligible <- months_ranked |>
  filter(months_ranked >= MIN_RANKED_MONTHS) |>
  pull(player_id)

# ---- quarterly series (unranked quarters mapped to RANK_CAP) ---------------
quarterly <- monthly |>
  mutate(career_quarter = career_month %/% 3) |>
  group_by(player_id, career_quarter) |>
  summarise(rank = min(rank), .groups = "drop")

traj_long <- expand_grid(
  player_id = eligible,
  career_quarter = seq_len(N_Q) - 1L
) |>
  left_join(quarterly, by = c("player_id", "career_quarter")) |>
  mutate(
    rank  = pmin(coalesce(rank, RANK_CAP), RANK_CAP),
    value = log(rank) / log(RANK_CAP)
  ) |>
  arrange(player_id, career_quarter)

traj_wide <- traj_long |>
  select(player_id, career_quarter, value) |>
  pivot_wider(
    names_from = career_quarter,
    values_from = value,
    names_prefix = "q"
  )

# ---- features computed from early-career window ----------------------------
rank_feats <- traj_long |>
  group_by(player_id) |>
  summarise(
    best_rank       = min(rank),
    mean_log_rank   = mean(log(rank)),
    final_log_rank  = log(last(rank)),
    rank_slope      = unname(coef(lm(log(rank) ~ career_quarter))[2]),
    rank_volatility = sd(diff(log(rank))),
    .groups = "drop"
  )

in_window <- function(act) {
  act |>
    inner_join(
      select(debut, player_id, debut_month, window_end_month),
      by = "player_id"
    ) |>
    filter(date >= debut_month, date < window_end_month)
}

sing_w <- in_window(singles) |>
  group_by(player_id) |>
  summarise(
    early_singles = n(),
    early_win_rate = mean(won),
    .groups = "drop"
  )

dbl_w <- in_window(doubles) |>
  group_by(player_id) |>
  summarise(early_doubles = n(), .groups = "drop")

features <- debut |>
  filter(player_id %in% eligible) |>
  left_join(months_ranked, by = "player_id") |>
  left_join(rank_feats, by = "player_id") |>
  left_join(sing_w, by = "player_id") |>
  left_join(dbl_w,  by = "player_id") |>
  left_join(
    select(players, player_id, name, dob, hand, height),
    by = "player_id"
  ) |>
  mutate(
    early_singles = coalesce(early_singles, 0L),
    early_doubles = coalesce(early_doubles, 0L),
    early_doubles_share = if_else(
      early_singles + early_doubles > 0,
      early_doubles / (early_singles + early_doubles),
      0
    ),
    age_at_debut = as.numeric(debut_date - dob) / 365.25,
    # Map ambiguous/unknown hand codes to NA before factoring
    hand = factor(na_if(na_if(as.character(hand), "A"), "U"))
  )

saveRDS(debut,     file.path(OUT_DIR, "debut.rds"))
saveRDS(traj_long, file.path(OUT_DIR, "traj_long.rds"))
saveRDS(traj_wide, file.path(OUT_DIR, "traj_wide.rds"))
saveRDS(features,  file.path(OUT_DIR, "features.rds"))
message("Players with usable early-career trajectories: ", nrow(traj_wide))
# nolint end: object_usage_linter.
