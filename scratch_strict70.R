p <- read.csv("outputs/pivoted_players_list.csv", stringsAsFactors = FALSE)
strict <- p[which(p$follow_doubles >= 10 & p$d_share >= 0.40), ]
cat("Strict 70 players count:", nrow(strict), "\n")
write.csv(
  strict,
  "outputs/original_70_pivoted_players.csv",
  row.names = FALSE
)

for (i in seq_len(nrow(strict))) {
  cat(sprintf(
    "%2d. %-26s (%s) | Follow Doubles: %3d | Share: %4.1f%% | Peak: %4d\n",
    i,
    strict$name[i],
    ifelse(is.na(strict$ioc[i]), "---", strict$ioc[i]),
    strict$follow_doubles[i],
    strict$d_share[i] * 100,
    as.integer(strict$best_rank[i])
  ))
}
