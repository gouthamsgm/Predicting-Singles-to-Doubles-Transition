
# ============================================================
# R/03_clustering.R
# Fast K-means Clustering of Early-Career ATP Trajectories
# Objective 2: Identify player career archetypes
# ============================================================

source("R/00_config.R")

suppressPackageStartupMessages({
  library(tidyverse)
  library(cluster)
})

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------

cat("\n[1/7] Loading trajectory data...\n")
flush.console()

traj_wide <- readRDS(file.path(OUT_DIR, "traj_wide.rds"))

ids <- traj_wide$player_id

# nolint start: object_name_linter.
X <- as.matrix(
  dplyr::select(traj_wide, -player_id)
)

storage.mode(X) <- "double"
# nolint end: object_name_linter.

cat("Total available players:", nrow(X), "\n")
cat("Trajectory features:", ncol(X), "\n")

if (nrow(X) < 3 || ncol(X) < 1) {
  stop("Not enough players or trajectory features.")
}

if (any(is.infinite(X))) {
  stop("Infinite values found in trajectory data.")
}

# Replace missing values with column medians
for (j in seq_len(ncol(X))) {
  missing <- is.na(X[, j])

  if (all(missing)) {
    stop("A trajectory feature contains only missing values.")
  }

  if (any(missing)) {
    X[missing, j] <- median(X[, j], na.rm = TRUE)
  }
}

# Standardize features so each quarter contributes comparably
feature_means <- colMeans(X)
feature_sds <- apply(X, 2, sd)

feature_sds[is.na(feature_sds) | feature_sds == 0] <- 1

# nolint start: object_name_linter.
X_scaled <- sweep(X, 2, feature_means, "-")
X_scaled <- sweep(X_scaled, 2, feature_sds, "/")
# nolint end: object_name_linter.

# ------------------------------------------------------------
# 2. K-MEANS FUNCTION
# ------------------------------------------------------------

fit_k <- function(data, k) {

  cat("\nStarting K-means: n =", nrow(data),
      "; k =", k, "\n")
  flush.console()

  start_time <- Sys.time()

  result <- kmeans(
    data,
    centers = k,
    nstart = 10,
    iter.max = 100
  )

  elapsed <- as.numeric(
    difftime(Sys.time(), start_time, units = "mins")
  )

  cat("Completed k =", k,
      "in", round(elapsed, 2), "minutes.\n")
  flush.console()

  result
}

# ------------------------------------------------------------
# 3. SELECT K USING A SAMPLE
# ------------------------------------------------------------

cat("\n[2/7] Selecting validation sample...\n")

set.seed(SEED)

idx <- if (nrow(X_scaled) > K_SELECT_SAMPLE) {
  sample(seq_len(nrow(X_scaled)), K_SELECT_SAMPLE)
} else {
  seq_len(nrow(X_scaled))
}

# nolint next: object_name_linter.
X_sample <- X_scaled[idx, , drop = FALSE]

cat("Players used for k selection:", length(idx), "\n")
cat("Candidate k values:",
    paste(K_RANGE, collapse = ", "), "\n")

# Silhouette uses Euclidean distances on the validation sample.
# Calculate the distance matrix only once.
cat("\nCalculating sample distances...\n")
sample_dist <- dist(X_sample)

# ------------------------------------------------------------
# 4. EVALUATE CANDIDATE CLUSTER COUNTS
# ------------------------------------------------------------

cat("\n[3/7] Evaluating candidate cluster counts...\n")

cvi_tab <- purrr::map_dfr(K_RANGE, function(k) {

  if (k < 2 || k >= nrow(X_sample)) {
    stop("Each candidate k must be between 2 and sample size - 1.")
  }

  cat("\n========== Evaluating k =", k, "==========\n")

  model <- fit_k(X_sample, k)

  sil <- cluster::silhouette(
    model$cluster,
    sample_dist
  )

  # Calinski-Harabasz index
  ch <- (model$betweenss / (k - 1)) /
    (model$tot.withinss / (nrow(X_sample) - k))

  # Davies-Bouldin index
  centers <- model$centers
  cluster_ids <- model$cluster

  scatter <- sapply(seq_len(k), function(g) {
    members <- X_sample[cluster_ids == g, , drop = FALSE]

    if (nrow(members) == 0) return(0)

    mean(sqrt(rowSums(
      sweep(
        members, 2, centers[g, ], "-"
      )^2
    )))
  })

  center_dist <- as.matrix(dist(centers))
  diag(center_dist) <- Inf

  ratios <- outer(scatter, scatter, "+") / center_dist
  db <- mean(apply(ratios, 1, max))

  result <- tibble(
    k = k,
    Sil = mean(sil[, "sil_width"]),
    CH = ch,
    DB = db
  )

  print(result)
  flush.console()

  result
})

write_csv(cvi_tab, file.path(OUT_DIR, "cvi_by_k.csv"))

# Select k
k_final <- if (is.null(K_CHOSEN)) {
  cvi_tab$k[which.max(cvi_tab$Sil)]
} else {
  K_CHOSEN
}

message("Selected number of clusters: k = ", k_final)

# ------------------------------------------------------------
# 5. FINAL CLUSTERING ON ALL ELIGIBLE PLAYERS
# ------------------------------------------------------------

cat("\n[4/7] Final clustering on all players...\n")
cat("Players:", nrow(X_scaled), "\n")
cat("Clusters:", k_final, "\n")
flush.console()

cl <- fit_k(X_scaled, k_final)

archetypes <- tibble(
  player_id = ids,
  cluster = as.integer(cl$cluster)
)

# Select one actual player closest to each cluster center.
# This preserves representative real-player trajectories.
representative_rows <- integer(k_final)

for (g in seq_len(k_final)) {

  members <- which(cl$cluster == g)

  differences <- sweep(
    X_scaled[members, , drop = FALSE],
    2,
    cl$centers[g, ],
    "-"
  )

  distances <- rowSums(differences^2)

  representative_rows[g] <- members[which.min(distances)]
}

# Reuse actual trajectories, not standardized cluster centers
medoids <- purrr::map_dfr(seq_len(k_final), function(g) {

  player_row <- representative_rows[g]

  tibble(
    cluster = g,
    career_quarter = seq_len(ncol(X)) - 1,
    value = as.numeric(X[player_row, ])
  )
})

# ------------------------------------------------------------
# 6. LABEL ARCHETYPES AND GENERATE FIGURES
# ------------------------------------------------------------

cat("\n[5/7] Creating archetype labels...\n")

labels_tbl <- medoids |>
  group_by(cluster) |>
  summarise(
    start = mean(value[career_quarter < 4], na.rm = TRUE),
    end = mean(
      value[career_quarter >= N_Q - 4],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    rank_end = value_to_rank(end),
    delta = end - start,
    trend = case_when(
      delta < -0.04 ~ "rising",
      delta > 0.04 ~ "declining",
      TRUE ~ "flat"
    ),
    level = case_when(
      rank_end <= 200 ~ "top-200",
      rank_end <= 600 ~ "mid-tier",
      TRUE ~ "low-ranked"
    ),
    archetype = paste0("A", cluster, ": ", level, ", ", trend)
  )

archetypes <- archetypes |>
  left_join(
    select(labels_tbl, cluster, archetype),
    by = "cluster"
  )

medoids <- medoids |>
  left_join(
    select(labels_tbl, cluster, archetype),
    by = "cluster"
  ) |>
  mutate(rank = value_to_rank(value))

cat("\nPlayers per archetype:\n")
print(count(archetypes, archetype))

cat("\n[6/7] Generating figures...\n")

p_cvi <- cvi_tab |>
  pivot_longer(
    cols = -k,
    names_to = "index",
    values_to = "value"
  ) |>
  ggplot(aes(x = k, y = value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~index, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Cluster Validity Indices vs k",
    x = "Number of clusters (k)",
    y = "Index value"
  )

ggsave(
  file.path(FIG_DIR, "cvi_by_k.png"),
  p_cvi, width = 8, height = 4, dpi = 150
)

p_med <- ggplot(
  medoids,
  aes(x = career_quarter, y = rank, colour = archetype)
) +
  geom_line(linewidth = 1.2) +
  scale_y_rank() +
  theme_minimal() +
  labs(
    x = "Quarters since debut",
    y = "ATP rank (log scale)",
    colour = NULL,
    title = "Representative Player Trajectories"
  )

ggsave(
  file.path(FIG_DIR, "archetype_medoids.png"),
  p_med, width = 8, height = 5, dpi = 150
)

traj_long <- readRDS(file.path(OUT_DIR, "traj_long.rds"))

p_all <- traj_long |>
  inner_join(archetypes, by = "player_id") |>
  ggplot(
    aes(
      x = career_quarter,
      y = rank,
      group = player_id
    )
  ) +
  geom_line(alpha = 0.05) +
  scale_y_rank() +
  facet_wrap(~archetype) +
  theme_minimal() +
  labs(
    x = "Quarters since debut",
    y = "ATP rank (log scale)",
    title = "Player Trajectories by Archetype"
  )

ggsave(
  file.path(FIG_DIR, "archetype_all_trajectories.png"),
  p_all, width = 10, height = 6, dpi = 150
)

# ------------------------------------------------------------
# 7. SAVE OUTPUTS
# ------------------------------------------------------------

cat("\n[7/7] Saving outputs...\n")

saveRDS(archetypes, file.path(OUT_DIR, "archetypes.rds"))
saveRDS(medoids, file.path(OUT_DIR, "medoids.rds"))
write_csv(cvi_tab, file.path(OUT_DIR, "cvi_by_k.csv"))

cat("\n========================================\n")
cat("K-MEANS CLUSTERING COMPLETED\n")
cat("Players clustered:", nrow(archetypes), "\n")
cat("Clusters:", k_final, "\n")
cat("Outputs saved in:", OUT_DIR, "\n")
cat("Figures saved in:", FIG_DIR, "\n")
cat("Finished at:", format(Sys.time()), "\n")
cat("========================================\n")

print(count(archetypes, archetype))
