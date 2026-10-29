# 00_config.R -- packages, paths, helpers and ALL tunable parameters
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(ggplot2)
})

# nolint start: object_name_linter.
# ---- paths -------------------------------------------------------------
RAW_DIR  <- "data/raw"    # CSVs are extracted here from the archive
ZIP_PATH <- "data/tennis-sackmann-archive-main.zip"
OUT_DIR  <- "outputs"
FIG_DIR  <- "figures"
for (d in c(OUT_DIR, FIG_DIR)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ---- study design ------------------------------------------------------
DATA_START     <- as.Date("2000-01-01")
DATA_END       <- as.Date("2020-12-31")
MIN_DEBUT_DATE <- as.Date("2001-01-01")  # avoids left-censored careers
EARLY_YEARS    <- 4                      # early window for trajectories
HORIZON_YEARS  <- 5                      # follow-up horizon for pivot
N_Q            <- EARLY_YEARS * 4        # quarterly points per trajectory
LAST_DEBUT_DATE <- DATA_END %m-% months(12 * (EARLY_YEARS + HORIZON_YEARS))
# later debuts lack a full follow-up horizon

RANK_CAP          <- 2000  # unranked / very low ranks capped here
MIN_RANKED_MONTHS <- 6     # minimum ranked months required inside window

# ---- pivot definition (cumulative across full follow-up window) --------
# Follow-up doubles volume and share relative to tour singles matches.
SHARE_THR   <- 0.30  # doubles share of tour matches in follow-up window
MIN_DOUBLES <- 5     # minimum total doubles matches in follow-up window

# ---- clustering --------------------------------------------------------
K_RANGE         <- 2:4
K_CHOSEN        <- 3     # set e.g. 4 to override silhouette choice
K_SELECT_SAMPLE <- 1000  # sample size used when comparing candidate k
DTW_WINDOW      <- 3L

SEED <- 42
# nolint end: object_name_linter.

# ---- helpers -----------------------------------------------------------
month_diff <- function(from, to) {
  (year(to) - year(from)) * 12 + (month(to) - month(from))
}

value_to_rank <- function(v) {
  exp(v * log(RANK_CAP))
}

scale_y_rank <- function(...) {
  ggplot2::scale_y_continuous(
    transform = scales::compose_trans("log10", "reverse"),
    breaks = c(1, 10, 50, 100, 250, 500, 1000, 2000),
    ...
  )
}
