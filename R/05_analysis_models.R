
# ============================================================
# R/05_analysis_models.R
# ATP Singles-to-Doubles Pivot Prediction
# ============================================================

source("R/00_config.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(recipes)
  library(parsnip)
  library(workflows)
  library(workflowsets)
  library(themis)   # step_smote (class imbalance)
  library(glmnet)   # penalised logistic regression
})

tidymodels_prefer()
options(yardstick.event_level = "second")
set.seed(SEED)

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------

labels    <- readRDS(file.path(OUT_DIR, "pivot_labels.rds"))
archetypes <- readRDS(file.path(OUT_DIR, "archetypes.rds"))
features  <- readRDS(file.path(OUT_DIR, "features.rds"))

# ------------------------------------------------------------
# 2. PREPARE DATA
# ------------------------------------------------------------

model_df <- labels |>
  inner_join(archetypes, by = "player_id") |>
  inner_join(features, by = "player_id") |>
  mutate(
    # Ensure outcome has consistent levels:
    # no = did not pivot, yes = pivoted
    pivoted = factor(
      if_else(event == 1, "yes", "no"),
      levels = c("no", "yes")
    ),
    archetype = factor(archetype)
  ) |>
  filter(!is.na(pivoted)) |>
  distinct(player_id, .keep_all = TRUE)

# Report cohort and class balance
cat("\n========== DATA SUMMARY ==========\n")
cat("Players:", nrow(model_df), "\n")
cat("Pivot events:", sum(model_df$pivoted == "yes"), "\n")
cat("Non-pivot events:", sum(model_df$pivoted == "no"), "\n")
cat("Pivot rate:",
    round(mean(model_df$pivoted == "yes") * 100, 2), "%\n")

print(table(model_df$pivoted))

# Save class balance
class_balance <- model_df |>
  count(pivoted) |>
  mutate(percentage = round(n / sum(n) * 100, 2))

write_csv(
  class_balance,
  file.path(OUT_DIR, "class_balance.csv")
)

# ------------------------------------------------------------
# 3. CHI-SQUARE TEST: archetype vs pivot (Objective 4)
# FIX B4: added missing chi-square test required by the project objectives.
# ------------------------------------------------------------

if ("archetype" %in% names(model_df)) {
  cat("\n========== CHI-SQUARE: archetype vs pivot ==========\n")
  ct <- table(model_df$archetype, model_df$pivoted)
  print(ct)
  chi_res <- chisq.test(ct)
  print(chi_res)
  cat("Chi-square X^2 =", round(chi_res$statistic, 3),
      " df =", chi_res$parameter,
      " p =", format.pval(chi_res$p.value, digits = 3), "\n")

  # Pivot rate by archetype with Wilson 95% CI
  archetype_rates <- model_df |>
    group_by(archetype) |>
    summarise(
      n           = n(),
      pivot_events = sum(pivoted == "yes"),
      pivot_rate   = mean(pivoted == "yes"),
      lo = prop.test(sum(pivoted == "yes"), n())$conf.int[1],
      hi = prop.test(sum(pivoted == "yes"), n())$conf.int[2],
      .groups = "drop"
    )
  print(archetype_rates)
  write_csv(archetype_rates, file.path(OUT_DIR, "archetype_pivot_rates.csv"))

  # Logistic odds ratios for archetype (univariate)
  arch_logit <- glm(pivoted ~ archetype, data = model_df, family = binomial)
  arch_or <- broom::tidy(arch_logit, exponentiate = TRUE, conf.int = TRUE)
  cat("\nArchetype logistic odds ratios:\n")
  print(arch_or)
  write_csv(arch_or, file.path(OUT_DIR, "archetype_logit_oddsratios.csv"))
} else {
  cat("WARNING: 'archetype' column not found; skipping chi-square test.\n")
}

# ------------------------------------------------------------
# 4. SELECT PREDICTORS
# ------------------------------------------------------------

trajectory_vars <- c(
  "archetype",
  "best_rank",
  "mean_log_rank",
  "final_log_rank",
  "rank_slope",
  "rank_volatility",
  "months_ranked"
)

full_vars <- c(
  trajectory_vars,
  "archetype",
  "early_singles",
  "early_win_rate",
  "early_doubles",
  "early_doubles_share",
  "age_at_debut",
  "height",
  "hand"
)

# Use only predictors that actually exist in the data.
trajectory_vars <- intersect(
  trajectory_vars,
  names(model_df)
)

full_vars <- intersect(
  full_vars,
  names(model_df)
)

# Exclude identifiers, outcome columns and post-pivot information.
# Predictors should describe the player before the prediction point.
# Variables from the follow-up window are excluded to prevent leakage.
forbidden_vars <- c(
  "player_id", "name", "event", "pivoted",
  "pivot_date", "time_months", "doubles_pivot_year",
  "follow_singles", "follow_tour_singles", "follow_doubles",
  "d_share", "all_d_share"
)

trajectory_vars <- setdiff(trajectory_vars, forbidden_vars)
full_vars <- setdiff(full_vars, forbidden_vars)


cat("\nTrajectory predictors:\n")
print(trajectory_vars)

cat("\nFull-model predictors:\n")
print(full_vars)

# ------------------------------------------------------------
# 4. CHECK WHETHER MODEL EVALUATION IS POSSIBLE
# ------------------------------------------------------------

positive_count <- sum(model_df$pivoted == "yes")
negative_count <- sum(model_df$pivoted == "no")

# A stratified train/test split and cross-validation are not
# dependable when there are too few positive examples.
if (positive_count < 10 || negative_count < 10) {
  message(
    "\nMODELLING STOPPED: Too few positive pivot cases ",
    "for dependable train/test evaluation.\n",
    "Current positive cases: ", positive_count, "\n",
    "Use a larger cohort and rerun the pipeline.\n",
    "Descriptive summaries have been saved."
  )

  # Descriptive pivot rate by archetype
  if ("archetype" %in% names(model_df)) {
    archetype_summary <- model_df |>
      group_by(archetype) |>
      summarise(
        players = n(),
        pivot_events = sum(pivoted == "yes"),
        pivot_rate = mean(pivoted == "yes"),
        .groups = "drop"
      ) |>
      mutate(pivot_rate_percent = round(pivot_rate * 100, 2))

    write_csv(
      archetype_summary,
      file.path(OUT_DIR, "archetype_pivot_summary.csv")
    )
    print(archetype_summary)
  }

  stop(
    "Increase cohort before fitting and evaluating predictive models."
  )
}

# ------------------------------------------------------------
# 5. TRAIN / TEST SPLIT
# ------------------------------------------------------------

set.seed(SEED)

data_split <- initial_split(
  model_df,
  prop = 0.80,
  strata = pivoted
)

train_data <- training(data_split)
test_data  <- testing(data_split)

cat("\nTraining rows:", nrow(train_data), "\n")
cat("Testing rows:", nrow(test_data), "\n")
cat("Training pivot events:",
    sum(train_data$pivoted == "yes"), "\n")
cat("Testing pivot events:",
    sum(test_data$pivoted == "yes"), "\n")

# Five-fold CV needs enough positive cases to distribute across
# folds. Reduce the number of folds if necessary.
train_positive <- sum(train_data$pivoted == "yes")
fold_count <- min(5, floor(train_positive / 2))

if (fold_count < 2) {
  stop("Not enough training pivot cases for cross-validation.")
}

set.seed(SEED)

folds <- vfold_cv(
  train_data,
  v = fold_count,
  strata = pivoted
)

# ------------------------------------------------------------
# 6. PREPROCESSING RECIPES
# ------------------------------------------------------------

make_recipe <- function(vars) {
  recipe(
    reformulate(vars, response = "pivoted"),
    data = train_data
  ) |>
    step_impute_median(all_numeric_predictors()) |>
    step_unknown(all_nominal_predictors()) |>
    step_novel(all_nominal_predictors()) |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    # SMOTE: oversample the minority (pivot=yes) class to balance training data
    step_smote(pivoted, over_ratio = 0.5, seed = SEED)
}

# ------------------------------------------------------------
# 7. DEFINE MODELS
# ------------------------------------------------------------

# Penalised logistic regression (Elastic Net: penalty and mixture tuned).
# Balances L1 sparsity and L2 shrinkage while avoiding separation warnings.
logistic_model <- logistic_reg(
  penalty  = tune(),
  mixture  = tune()
) |>
  set_engine("glmnet") |>
  set_mode("classification")

# Random forest: tune number of trees and minimum node size
rf_model <- rand_forest(
  trees = tune(),
  min_n = tune()
) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

# Gradient boosting: tune tree count, depth, learning rate, node size
xgb_model <- boost_tree(
  trees      = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  min_n      = tune()
) |>
  set_engine("xgboost") |>
  set_mode("classification")

# ------------------------------------------------------------
# 8. BUILD WORKFLOWS
# ------------------------------------------------------------

workflow_set_obj <- workflow_set(
  preproc = list(
    trajectory = make_recipe(trajectory_vars),
    full = make_recipe(full_vars)
  ),
  models = list(
    logistic = logistic_model,
    random_forest = rf_model,
    xgboost = xgb_model
  )
)

# PR-AUC is most informative for rare events. ROC-AUC is included too.
# event_level="second" targets "yes" (level 2 in c("no","yes")) as positive.
evaluation_metrics <- metric_set(
  roc_auc,
  pr_auc,
  accuracy,
  bal_accuracy,
  sens,
  precision,
  f_meas
)

# ------------------------------------------------------------
# 9. CROSS-VALIDATION
# ------------------------------------------------------------

set.seed(SEED)

# tune_grid evaluates each model over a 15-point Latin hypercube parameter grid.
# save_pred=TRUE is needed for threshold tuning on CV fold predictions.
workflow_set_obj <- workflow_map(
  workflow_set_obj,
  "tune_grid",
  resamples = folds,
  grid      = 20,    # Latin hypercube, 20 candidate hyperparameter sets
  metrics   = evaluation_metrics,
  control   = control_grid(
    save_pred      = TRUE,
    save_workflow  = TRUE,
    verbose        = FALSE
  ),
  seed    = SEED,
  verbose = TRUE
)

cv_results <- rank_results(
  workflow_set_obj,
  rank_metric  = "pr_auc",
  select_best  = TRUE   # one row per workflow (best hyperparameter config)
)

write_csv(
  cv_results,
  file.path(OUT_DIR, "cv_model_comparison.csv")
)

cat("\n========== CROSS-VALIDATION RESULTS ==========\n")
print(cv_results)

# Save a plot of model comparison
cv_plot <- autoplot(
  workflow_set_obj,
  metric = "pr_auc"
)

ggsave(
  file.path(OUT_DIR, "cv_model_comparison.png"),
  plot = cv_plot,
  width = 10,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 10. SELECT BEST MODEL USING PR-AUC
# ------------------------------------------------------------

best_model_row <- cv_results |>
  filter(.metric == "pr_auc", !is.na(mean)) |>
  arrange(desc(mean)) |>
  slice(1)

if (nrow(best_model_row) == 0) {
  stop("No valid PR-AUC results. Check the class balance and folds.")
}

best_id <- best_model_row$wflow_id

cat("\nSelected model:", best_id, "\n")
cat("Cross-validation PR-AUC:", best_model_row$mean, "\n")

# Extract best hyperparameters for the winning workflow, then finalize.
# This is required when models were tuned (tune_grid); for fixed-param
# models finalize_workflow() is a no-op so the same code works for both.
best_params <- workflow_set_obj |>
  extract_workflow_set_result(best_id) |>
  select_best(metric = "pr_auc")

best_workflow <- workflow_set_obj |>
  extract_workflow(best_id) |>
  finalize_workflow(best_params)

cat("Best hyperparameters:\n")
print(best_params)

# ------------------------------------------------------------
# 11. FIT ON TRAINING DATA AND EVALUATE TEST DATA
# ------------------------------------------------------------

final_fit <- last_fit(
  best_workflow,
  split = data_split,
  metrics = evaluation_metrics
)

test_metrics <- collect_metrics(final_fit)

write_csv(
  test_metrics,
  file.path(OUT_DIR, "test_metrics.csv")
)

cat("\n========== TEST METRICS ==========\n")
print(test_metrics)

test_predictions <- collect_predictions(final_fit)
test_predictions$player_id <- model_df$player_id[test_predictions$.row]

# Tune decision threshold on CV predictions to balance precision and recall.
# For tuned workflows collect_predictions() needs the best parameter config.
cv_preds <- workflow_set_obj |>
  extract_workflow_set_result(best_id) |>
  collect_predictions(parameters = best_params)

threshold_candidates <- seq(0.02, 0.50, by = 0.01)
f1_scores <- sapply(threshold_candidates, function(th) {
  pred_th <- factor(
    if_else(cv_preds$.pred_yes >= th, "yes", "no"),
    levels = c("no", "yes")
  )
  cm <- yardstick::f_meas_vec(cv_preds$pivoted, pred_th, event_level = "second")
  if (is.na(cm)) 0 else cm
})
best_thresh <- threshold_candidates[which.max(f1_scores)]
cat("\nTuned probability cutoff (optimizing CV F1):", best_thresh, "\n")

# Save probabilities and predicted classes at both 0.5 cutoff and tuned cutoff
test_predictions <- test_predictions |>
  mutate(
    predicted_class_05 = factor(
      if_else(.pred_yes >= 0.5, "yes", "no"),
      levels = c("no", "yes")
    ),
    predicted_class = factor(
      if_else(.pred_yes >= best_thresh, "yes", "no"),
      levels = c("no", "yes")
    )
  )

write_csv(
  test_predictions,
  file.path(OUT_DIR, "test_predictions.csv")
)

# Confusion matrices
conf_matrix_05 <- conf_mat(
  test_predictions,
  truth = pivoted,
  estimate = predicted_class_05
)

conf_matrix <- conf_mat(
  test_predictions,
  truth = pivoted,
  estimate = predicted_class
)

cat("\n========== CONFUSION MATRIX (Standard 0.50 Cutoff) ==========\n")
print(conf_matrix_05)
cat(
  sprintf(
    "\n========== CONFUSION MATRIX (Tuned Cutoff = %.2f) ==========\n",
    best_thresh
  )
)
print(conf_matrix)

# ------------------------------------------------------------
# 12. ROC AND PRECISION-RECALL CURVES
# ------------------------------------------------------------

roc_data <- roc_curve(
  test_predictions,
  truth = pivoted,
  .pred_yes,
  event_level = "second"
)

pr_data <- pr_curve(
  test_predictions,
  truth = pivoted,
  .pred_yes,
  event_level = "second"
)

roc_plot <- autoplot(roc_data)

ggsave(
  file.path(OUT_DIR, "test_roc_curve.png"),
  plot = roc_plot,
  width = 7,
  height = 6,
  dpi = 300
)

pr_plot <- autoplot(pr_data)

ggsave(
  file.path(OUT_DIR, "test_precision_recall_curve.png"),
  plot = pr_plot,
  width = 7,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 13. FIT SELECTED MODEL ON ALL AVAILABLE DATA
# ------------------------------------------------------------

# This fit is for producing player-level scores after evaluation.
# Do not use these in-sample scores as independent test results.

full_fit <- fit(
  best_workflow,
  data = model_df
)

all_probabilities <- predict(
  full_fit,
  new_data = model_df,
  type = "prob"
)

scored_players <- model_df |>
  mutate(prob_pivot = all_probabilities$.pred_yes) |>
  select(
    any_of(c(
      "player_id", "name", "archetype", "pivoted",
      "prob_pivot"
    ))
  ) |>
  arrange(desc(prob_pivot))

write_csv(
  scored_players,
  file.path(OUT_DIR, "player_pivot_predictions.csv")
)

saveRDS(
  full_fit,
  file.path(OUT_DIR, "final_pivot_model.rds")
)

# ------------------------------------------------------------
# 14. SAVE SUMMARY FOR REPORT
# ------------------------------------------------------------

analysis_summary <- list(
  total_players = nrow(model_df),
  pivot_events = positive_count,
  non_pivot_events = negative_count,
  pivot_rate = positive_count / nrow(model_df),
  best_model = best_id,
  cross_validation_results = cv_results,
  test_metrics = test_metrics,
  confusion_matrix = conf_matrix
)

saveRDS(
  analysis_summary,
  file.path(OUT_DIR, "analysis_summary.rds")
)

cat("\n========== ANALYSIS COMPLETE ==========\n")
cat("Outputs saved in:", OUT_DIR, "\n")
