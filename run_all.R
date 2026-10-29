# Run the whole pipeline from the project root:  source("run_all.R")
steps <- c(
  "R/01_clean.R", "R/02_trajectories.R", "R/03_clustering.R",
  "R/04_pivot_labels.R", "R/05_analysis_models.R", "R/06_survival.R",
  "R/07_prep_dashboard.R"
)
for (s in steps) {
  message("\n==> ", s)
  source(s)
}
message("\nDone. Launch the dashboard with: shiny::runApp('app')")
