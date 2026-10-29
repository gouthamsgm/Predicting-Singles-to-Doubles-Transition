# scratch_tune.R -- test model tuning with archetype and event_level="second"
source("R/00_config.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(themis)
  library(glmnet)
})

tidymodels_prefer()
options(yardstick.event_level = "second")
set.seed(SEED)

labels     <- readRDS(file.path(OUT_DIR, "pivot_labels.rds"))
archetypes <- readRDS(file.path(OUT_DIR, "archetypes.rds"))
features   <- readRDS(file.path(OUT_DIR, "features.rds"))

model_df <- labels |>
  inner_join(archetypes, by = "player_id") |>
  inner_join(features, by = "player_id") |>
  mutate(
    pivoted = factor(
      if_else(event == 1, "yes", "no"),
      levels = c("no", "yes")
    ),
    archetype = factor(archetype)
  ) |>
  distinct(player_id, .keep_all = TRUE)

trajectory_vars <- c(
  "best_rank", "mean_log_rank", "final_log_rank",
  "rank_slope", "rank_volatility", "months_ranked"
)
full_vars <- c(
  trajectory_vars, "archetype", "early_singles",
  "early_win_rate", "early_doubles", "early_doubles_share",
  "age_at_debut", "height", "hand"
)

full_vars <- intersect(full_vars, names(model_df))
cat("Full vars with archetype:\n")
print(full_vars)

data_split <- initial_split(model_df, prop = 0.80, strata = pivoted)
train_data <- training(data_split)
test_data  <- testing(data_split)

folds <- vfold_cv(train_data, v = 5, strata = pivoted)

make_recipe <- function(vars) {
  recipe(reformulate(vars, response = "pivoted"), data = train_data) |>
    step_impute_median(all_numeric_predictors()) |>
    step_unknown(all_nominal_predictors()) |>
    step_novel(all_nominal_predictors()) |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    step_smote(pivoted, over_ratio = 0.5, seed = SEED)
}

rec <- make_recipe(full_vars)

# XGBoost model
xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  min_n = tune()
) |>
  set_engine("xgboost") |>
  set_mode("classification")

# RF model
rf_spec <- rand_forest(
  trees = 500,
  min_n = tune(),
  mtry = tune()
) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

# Elastic Net Logistic Regression
glm_spec <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) |>
  set_engine("glmnet") |>
  set_mode("classification")

eval_metrics <- metric_set(
  roc_auc, pr_auc, accuracy, bal_accuracy, sens, precision, f_meas
)

wf_set <- workflow_set(
  preproc = list(full = rec),
  models = list(xgb = xgb_spec, rf = rf_spec, glm = glm_spec)
)

cat("Tuning workflows with Latin hypercube grid...\n")
wf_res <- workflow_map(
  wf_set,
  "tune_grid",
  resamples = folds,
  grid = 20,
  metrics = eval_metrics,
  control = control_grid(save_pred = TRUE, save_workflow = TRUE),
  seed = SEED,
  verbose = FALSE
)

ranked <- rank_results(wf_res, rank_metric = "pr_auc", select_best = TRUE)
print(
  ranked |>
    filter(.metric %in% c("pr_auc", "roc_auc", "bal_accuracy", "f_meas")) |>
    select(wflow_id, .metric, mean, std_err)
)

best_wflow_id <- ranked |>
  filter(.metric == "pr_auc") |>
  slice(1) |>
  pull(wflow_id)
cat("Best workflow:", best_wflow_id, "\n")

best_params <- wf_res |>
  extract_workflow_set_result(best_wflow_id) |>
  select_best(metric = "pr_auc")
print(best_params)

final_wf <- wf_res |>
  extract_workflow(best_wflow_id) |>
  finalize_workflow(best_params)
fitted <- last_fit(final_wf, split = data_split, metrics = eval_metrics)

test_met <- collect_metrics(fitted)
cat("\nTest metrics on held-out test data:\n")
print(test_met)

test_pred <- collect_predictions(fitted)
cat("\nConfusion matrix at 0.5 cutoff:\n")
print(table(truth = test_pred$pivoted, pred = test_pred$.pred_class))

# CV threshold tuning
cv_p <- wf_res |>
  extract_workflow_set_result(best_wflow_id) |>
  collect_predictions(parameters = best_params)
th_seq <- seq(0.05, 0.50, by = 0.01)
f1_vals <- sapply(th_seq, function(th) {
  p_th <- factor(
    if_else(cv_p$.pred_yes >= th, "yes", "no"),
    levels = c("no", "yes")
  )
  yardstick::f_meas_vec(cv_p$pivoted, p_th, event_level = "second")
})
best_th <- th_seq[which.max(f1_vals)]
cat(
  "Optimal threshold for F1:", best_th,
  "Max CV F1:", max(f1_vals, na.rm = TRUE), "\n"
)

tuned_pred <- factor(
  if_else(test_pred$.pred_yes >= best_th, "yes", "no"),
  levels = c("no", "yes")
)
cat("\nConfusion matrix at tuned cutoff (", best_th, "):\n")
print(table(truth = test_pred$pivoted, pred = tuned_pred))

cat(
  "Balanced accuracy at tuned cutoff:",
  yardstick::bal_accuracy_vec(
    test_pred$pivoted, tuned_pred, event_level = "second"
  ),
  "\n"
)
cat(
  "Sensitivity (recall) at tuned cutoff:",
  yardstick::sens_vec(
    test_pred$pivoted, tuned_pred, event_level = "second"
  ),
  "\n"
)
cat(
  "Precision at tuned cutoff:",
  yardstick::precision_vec(
    test_pred$pivoted, tuned_pred, event_level = "second"
  ),
  "\n"
)
