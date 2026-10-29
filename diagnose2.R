cat("=== FURTHER DIAGNOSIS PART 2 ===\n")
library(tidyverse)
library(lubridate)

debut <- readRDS("outputs/debut.rds")
singles <- readRDS("outputs/singles_activity.rds")
doubles_activity <- readRDS("outputs/doubles_activity.rds")
source("R/00_config.R")

# How many players meet the 'pivot' threshold in any single half-year?
n_per <- 2 * HORIZON_YEARS

half_year_doubles <- doubles_activity |>
  inner_join(select(debut, player_id, window_end_month), by="player_id") |>
  filter(date >= window_end_month) |>
  mutate(period = as.integer(as.numeric(difftime(date, window_end_month, units="days")) %/% 182)) |>
  filter(period < n_per) |>
  count(player_id, period, name="n_d")

half_year_singles <- singles |>
  inner_join(select(debut, player_id, window_end_month), by="player_id") |>
  filter(date >= window_end_month) |>
  mutate(period = as.integer(as.numeric(difftime(date, window_end_month, units="days")) %/% 182)) |>
  filter(period < n_per) |>
  count(player_id, period, name="n_s")

combined <- full_join(half_year_doubles, half_year_singles, by=c("player_id","period")) |>
  mutate(n_d=coalesce(n_d,0L), n_s=coalesce(n_s,0L),
         d_share=ifelse(n_s+n_d>0, n_d/(n_s+n_d), 0))

cat("Half-year periods meeting MIN_DOUBLES=5 AND share>=0.5:\n")
meets <- combined |> filter(n_d >= 5, d_share >= 0.5)
cat("  Unique players:", length(unique(meets$player_id)), "\n")

cat("With n_d>=3 AND share>=0.4:\n")
meets2 <- combined |> filter(n_d >= 3, d_share >= 0.4)
cat("  Unique players:", length(unique(meets2$player_id)), "\n")

cat("With n_d>=2 AND share>=0.3:\n")
meets3 <- combined |> filter(n_d >= 2, d_share >= 0.3)
cat("  Unique players:", length(unique(meets3$player_id)), "\n")

cat("\nDistribution of n_d in half-years where n_d>0:\n")
print(quantile(combined$n_d[combined$n_d > 0], c(0,.25,.5,.75,.9,.95,1)))

# What does the follow-up window look like?  
# LAST_DEBUT_DATE check
cat("\nLAST_DEBUT_DATE:", as.character(LAST_DEBUT_DATE), "\n")
cat("Debut date distribution (eligible players):\n")
print(summary(debut$debut_date))
cat("Players with debut <= LAST_DEBUT_DATE:", sum(debut$debut_date <= LAST_DEBUT_DATE), "\n")

# Critical: what is the LAST_DEBUT_DATE?  4 years window + 5 years follow-up = 9 years before 2020-12-31
# That means LAST_DEBUT_DATE should be about 2011-12-31
# Players who debuted 2012+ cannot have a full 5-year follow-up window by 2020

# Let's also check how many eligible players are in the cohort window
archetypes <- readRDS("outputs/archetypes.rds")
cat("\nNote: archetypes has", nrow(archetypes), "players (sample only)\n")
cat("The cohort in pivot_labels has", 
    nrow(readRDS("outputs/pivot_labels.rds")), "players (from archetypes only)\n")

cat("\n--- SUMMARY OF ISSUES ---\n")
cat("Issue 1: Archetypes has only 300 players (previous run used smaller FINAL_SAMPLE_SIZE)\n")
cat("Issue 2: Only tour-level doubles are available -> very few matches per player\n")
cat("Issue 3: MIN_DOUBLES=5 is too strict for tour-level-only data\n")
cat("  -> pivot_labels only covers the 300 clustered players\n")
cat("  -> of those 300, only 3 meet the pivot threshold\n")
cat("Issue 4: Need to re-run 03 clustering without the 300-player sample cap\n")
cat("  (or with ALL 4368 players)\n")
cat("  and then adjust pivot thresholds to get a sensible pivot rate\n")
