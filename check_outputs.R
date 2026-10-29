cat("=== CHECKING EXISTING OUTPUTS ===\n")
out <- "outputs"

# Rankings, singles, doubles
if (file.exists(file.path(out, "rankings.rds"))) {
  r <- readRDS(file.path(out, "rankings.rds"))
  cat("rankings rows:", nrow(r), "\n")
  cat(
    "rankings date range:",
    as.character(min(r$date)),
    "to",
    as.character(max(r$date)),
    "\n"
  )
}
if (file.exists(file.path(out, "singles_activity.rds"))) {
  s <- readRDS(file.path(out, "singles_activity.rds"))
  cat("singles player-matches:", nrow(s), "\n")
}
if (file.exists(file.path(out, "doubles_activity.rds"))) {
  d <- readRDS(file.path(out, "doubles_activity.rds"))
  cat("doubles player-matches:", nrow(d), "\n")
}

# Trajectories
if (file.exists(file.path(out, "traj_wide.rds"))) {
  tw <- readRDS(file.path(out, "traj_wide.rds"))
  cat("Players with trajectories:", nrow(tw), "\n")
  tl <- readRDS(file.path(out, "traj_long.rds"))
  mr <- as.data.frame(table(tapply(tl$career_quarter, tl$player_id, length)))
  cat("Trajectory length distribution (quarters):\n")
  print(head(mr, 5))
}

# Archetypes
if (file.exists(file.path(out, "archetypes.rds"))) {
  a <- readRDS(file.path(out, "archetypes.rds"))
  cat("\nArchetypes table:\n")
  print(table(a$archetype))
  cat("Total players clustered:", nrow(a), "\n")
}

# CVI
if (file.exists(file.path(out, "cvi_by_k.csv"))) {
  cat("\nCVI by k:\n")
  print(read.csv(file.path(out, "cvi_by_k.csv")))
}

# Pivot labels
if (file.exists(file.path(out, "pivot_labels.rds"))) {
  pl <- readRDS(file.path(out, "pivot_labels.rds"))
  cat("\nCohort size:", nrow(pl), "\n")
  cat("Pivot rate:", round(mean(pl$event) * 100, 1), "%\n")
  cat("Pivoted:", sum(pl$event), "| Not pivoted:", sum(!pl$event), "\n")
}

# Sensitivity
if (file.exists(file.path(out, "pivot_sensitivity.csv"))) {
  cat("\nSensitivity table:\n")
  print(read.csv(file.path(out, "pivot_sensitivity.csv")))
}

# Model results
if (file.exists(file.path(out, "cv_model_comparison.csv"))) {
  cat("\nCV model comparison (pr_auc):\n")
  cv <- read.csv(file.path(out, "cv_model_comparison.csv"), check.names = FALSE)
  metric_col <- if (".metric" %in% names(cv)) ".metric" else "X.metric"
  cols_to_print <- intersect(
    c("wflow_id", metric_col, "mean", "std_err"),
    names(cv)
  )
  print(cv[cv[[metric_col]] == "pr_auc", cols_to_print])
}
if (file.exists(file.path(out, "test_metrics.csv"))) {
  cat("\nTest metrics:\n")
  print(read.csv(file.path(out, "test_metrics.csv")))
}

cat("\nDone.\n")
