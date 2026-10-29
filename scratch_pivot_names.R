source("R/00_config.R")
suppressPackageStartupMessages(library(tidyverse))

labels   <- readRDS(file.path(OUT_DIR, "pivot_labels.rds"))
players  <- readRDS(file.path(OUT_DIR, "players.rds"))
features <- readRDS(file.path(OUT_DIR, "features.rds"))
arch     <- readRDS(file.path(OUT_DIR, "archetypes.rds"))

piv <- labels |>
  filter(event == 1) |>
  left_join(
    select(players, player_id, name, ioc),
    by = "player_id"
  ) |>
  left_join(
    select(features, player_id, debut_date, best_rank),
    by = "player_id"
  ) |>
  left_join(select(arch, player_id, archetype), by = "player_id") |>
  arrange(name)

cat(sprintf("Found %d pivoted players:\n", nrow(piv)))
for (i in seq_len(nrow(piv))) {
  cat(sprintf(
    "%3d. %-28s | Country: %-3s | Peak: %4d | Debut: %s | Archetype: %s\n",
    i,
    piv$name[i],
    ifelse(is.na(piv$ioc[i]), "---", piv$ioc[i]),
    as.integer(piv$best_rank[i]),
    as.character(piv$debut_date[i]),
    as.character(piv$archetype[i])
  ))
}

write_csv(piv, file.path(OUT_DIR, "pivoted_players_list.csv"))
