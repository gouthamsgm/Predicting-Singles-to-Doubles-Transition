# Run once. Requires R >= 4.1 (native pipe |> and \(x) lambdas)
pkgs <- c("tidyverse", "lubridate", "dtwclust", "tidymodels", "workflowsets",
          "ranger", "xgboost", "glmnet", "vip", "survival", "survminer", "broom",
          "themis",  # SMOTE oversampling for class imbalance
          "shiny", "bslib", "plotly", "DT", "scales")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
