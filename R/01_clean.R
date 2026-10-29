# 01_clean.R -- read the raw ATP files and save tidy activity tables
source("R/00_config.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readr)
  library(dplyr)
  library(tidyselect)
  library(rlang)
})

read_csv <- readr::read_csv
cols <- readr::cols
col_character <- readr::col_character
any_of <- tidyselect::any_of
mutate <- dplyr::mutate
set_names <- rlang::set_names

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "ranking_date", "rank", "player", "player_id",
      "name_first", "name_last", "hand", "dob", "height", "ioc",
      "tourney_date", "winner_id", "loser_id", "is_tour", "won",
      "winner1_id", "winner2_id", "loser1_id", "loser2_id", "date"
    )
  )
}

# ---- extract the atp/ folder from the zip on first run ------------------
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(RAW_DIR, "atp_players.csv"))) {
  if (file.exists(ZIP_PATH)) {
    zl   <- unzip(ZIP_PATH, list = TRUE)$Name
    keep <- zl[grepl("/atp/atp_.*\\.csv$", zl)]
    unzip(ZIP_PATH, files = keep, exdir = RAW_DIR, junkpaths = TRUE)
    message("Extracted ", length(keep), " ATP files from zip to ", RAW_DIR)
  } else {
    # Check if unzipped folder already exists in data/
    unzipped_atp <- list.files(
      "data",
      pattern = "^atp_players\\.csv$",
      recursive = TRUE,
      full.names = TRUE
    )
    if (length(unzipped_atp) > 0) {
      src_dir <- dirname(unzipped_atp[1])
      files_to_copy <- list.files(
        src_dir,
        pattern = "^atp_.*\\.csv$",
        full.names = TRUE
      )
      file.copy(files_to_copy, RAW_DIR, overwrite = TRUE)
      message(
        "Copied ", length(files_to_copy), " ATP files from ",
        src_dir, " to ", RAW_DIR
      )
    } else {
      stop(
        "Put zip at data/tennis-sackmann-archive-main.zip or files in data/raw"
      )
    }
  }
}

read_sel <- function(f, keep) {
  read_csv(
    f,
    col_select = any_of(keep),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )
}

# ---- rankings ----------------------------------------------------------
rank_files <- list.files(
  RAW_DIR,
  pattern = "^atp_rankings_.*\\.csv$",
  full.names = TRUE
)
stopifnot("No atp_rankings_*.csv in data/raw" = length(rank_files) > 0)
rankings <- map_dfr(
  rank_files,
  read_sel,
  keep = c("ranking_date", "rank", "player")
) |>
  transmute(
    player_id = as.integer(player),
    date = ymd(ranking_date),
    rank = as.integer(rank)
  ) |>
  filter(!is.na(date), date >= DATA_START, date <= DATA_END)

# ---- players -----------------------------------------------------------
players <- read_csv(
  file.path(RAW_DIR, "atp_players.csv"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) |>
  transmute(
    player_id = as.integer(player_id),
    name   = str_squish(paste(name_first, name_last)),
    hand   = na_if(hand, ""),
    dob    = ymd(dob, quiet = TRUE),
    height = suppressWarnings(as.numeric(height)),
    ioc
  )

# ---- singles matches (tour + qualifying/challenger + futures if present) --
singles_files <- list.files(
  RAW_DIR,
  pattern = "^atp_matches_((qual_chall_|futures_)?[0-9]{4})\\.csv$",
  full.names = TRUE
)
stopifnot("No singles match files in data/raw" = length(singles_files) > 0)

read_singles <- function(f) {
  is_tour <- grepl("^atp_matches_[0-9]{4}\\.csv$", basename(f))
  read_sel(f, keep = c("tourney_date", "winner_id", "loser_id")) |>
    mutate(is_tour = is_tour)
}

singles_raw <- map_dfr(singles_files, read_singles) |>
  mutate(date = ymd(tourney_date)) |>
  filter(!is.na(date), date >= DATA_START, date <= DATA_END)

singles_activity <- bind_rows(
  transmute(
    singles_raw,
    player_id = as.integer(winner_id),
    date,
    won = 1L,
    is_tour
  ),
  transmute(
    singles_raw,
    player_id = as.integer(loser_id),
    date,
    won = 0L,
    is_tour
  )
) |> filter(!is.na(player_id))

# ---- doubles matches ---------------------------------------------------
doubles_files <- list.files(
  RAW_DIR,
  pattern = "^atp_matches_doubles_[0-9]{4}\\.csv$",
  full.names = TRUE
)
stopifnot(
  "No atp_matches_doubles_*.csv in data/raw" = length(doubles_files) > 0
)

read_doubles <- function(f) {
  hdr <- names(
    read_csv(f, n_max = 0, show_col_types = FALSE, progress = FALSE)
  )
  stopifnot(
    hdr[c(6, 8, 9, 12, 13)] == c(
      "tourney_date", "winner1_id", "winner2_id", "loser1_id", "loser2_id"
    )
  )
  suppressWarnings(
    read_csv(
      f,
      skip = 1,
      col_names = FALSE,
      col_select = c(6, 8, 9, 12, 13),
      col_types = cols(.default = col_character()),
      show_col_types = FALSE,
      progress = FALSE
    )
  ) |> set_names(
    c("tourney_date", "winner1_id", "winner2_id", "loser1_id", "loser2_id")
  )
}

doubles_activity <- map_dfr(doubles_files, read_doubles) |>
  mutate(date = ymd(tourney_date, quiet = TRUE)) |>
  filter(!is.na(date), date >= DATA_START, date <= DATA_END) |>
  select(date, winner1_id, winner2_id, loser1_id, loser2_id) |>
  pivot_longer(-date, values_to = "player_id") |>
  transmute(player_id = as.integer(player_id), date) |>
  filter(!is.na(player_id))

saveRDS(rankings,         file.path(OUT_DIR, "rankings.rds"))
saveRDS(players,          file.path(OUT_DIR, "players.rds"))
saveRDS(singles_activity, file.path(OUT_DIR, "singles_activity.rds"))
saveRDS(doubles_activity, file.path(OUT_DIR, "doubles_activity.rds"))

message(
  sprintf(
    "Rankings: %s | singles matches: %s | doubles matches: %s",
    format(nrow(rankings), big.mark = ","),
    format(nrow(singles_activity), big.mark = ","),
    format(nrow(doubles_activity), big.mark = ",")
  )
)
