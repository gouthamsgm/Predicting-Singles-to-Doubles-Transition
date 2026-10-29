p <- read.csv("outputs/pivoted_players_list.csv", stringsAsFactors = FALSE)
for (i in 1:68) {
  cat(sprintf("%3d. %-26s (%s) - Peak Rank: %4d | Debut: %s | Archetype: %s\n",
              i, p$name[i], ifelse(is.na(p$ioc[i]), "---", p$ioc[i]),
              as.integer(p$best_rank[i]), p$debut_date[i], p$archetype[i]))
}
