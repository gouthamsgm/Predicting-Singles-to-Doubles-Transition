# 06_survival.R -- time-to-pivot survival analysis
source("R/00_config.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(survival)
  library(survminer)
})

labels     <- readRDS(file.path(OUT_DIR, "pivot_labels.rds"))
archetypes <- readRDS(file.path(OUT_DIR, "archetypes.rds"))
features   <- readRDS(file.path(OUT_DIR, "features.rds"))

sdf <- labels |>
  inner_join(archetypes, by = "player_id") |>
  inner_join(features, by = "player_id") |>
  mutate(archetype = factor(archetype))

# Check available observations and pivot events
cat("Players:", nrow(sdf), "\n")
cat("Pivot events:", sum(sdf$event == 1, na.rm = TRUE), "\n")

# Kaplan-Meier: players without a pivot are censored
km <- survfit(Surv(time_months, event) ~ archetype, data = sdf)

p <- ggsurvplot(
  km,
  data = sdf,
  fun = "event",
  pval = TRUE,
  risk.table = TRUE,
  xlab = "Months after the early-career window",
  ylab = "Cumulative probability of pivot",
  legend.title = "",
  legend.labs = levels(sdf$archetype)
)

png(
  file.path(FIG_DIR, "kaplan_meier_by_archetype.png"),
  width = 1800, height = 1200, res = 200
)
print(p)
invisible(dev.off())

# Log-rank test
logrank <- survdiff(
  Surv(time_months, event) ~ archetype,
  data = sdf
)
print(logrank)

# Cox model: association with archetype and early win rate
if (sum(sdf$event == 1, na.rm = TRUE) >= 10) {
  cox_fit <- coxph(
    Surv(time_months, event) ~ archetype + early_win_rate,
    data = sdf
  )

  print(summary(cox_fit))

  # Proportional-hazards check
  try(print(cox.zph(cox_fit)), silent = TRUE)

  # Save hazard ratios
  write_csv(
    broom::tidy(cox_fit, exponentiate = TRUE, conf.int = TRUE),
    file.path(OUT_DIR, "cox_hazard_ratios.csv")
  )
} else {
  message(
    "Cox model skipped: fewer than 10 pivot events. ",
    "The event count is too small for reliable inference."
  )
}
