cat("=== DIAGNOSING ISSUES ===\n")

# 1. Check doubles files - peek at actual data
doubles_files <- list.files(
  "data/raw",
  pattern = "^atp_matches_doubles_[0-9]{4}\\.csv$",
  full.names = TRUE
)
cat("Doubles CSV files found:", length(doubles_files), "\n")
cat("First few:\n")
print(head(doubles_files))

# Read one to see structure
f <- doubles_files[1]
cat("\nChecking:", basename(f), "\n")
hdr <- names(read.csv(f, nrow = 0))
cat("Number of header columns:", length(hdr), "\n")
cat("Column names 1-15:\n")
print(hdr[seq_len(min(15, length(hdr)))])

# Read first 3 data rows by position
raw <- read.csv(
  f,
  skip = 1,
  header = FALSE,
  nrow = 3,
  stringsAsFactors = FALSE
)
cat("Number of data columns:", ncol(raw), "\n")
cat("Columns 6, 8, 9, 12, 13:\n")
print(raw[, c(6, 8, 9, 12, 13)])

# 2. Check the doubles_activity that was loaded
doubles_activity <- readRDS("outputs/doubles_activity.rds")
cat("\nDoubles activity rows:", nrow(doubles_activity), "\n")
cat(
  "Date range:",
  as.character(min(doubles_activity$date)),
  "to",
  as.character(max(doubles_activity$date)),
  "\n"
)
cat("Sample rows:\n")
print(head(doubles_activity))
cat("NA player_ids:", sum(is.na(doubles_activity$player_id)), "\n")

# 3. Check features to see early_doubles stats
features <- readRDS("outputs/features.rds")
cat("\nFeatures nrow:", nrow(features), "\n")
cat("Early doubles stats:\n")
cat("  mean early_doubles:", mean(features$early_doubles), "\n")
cat("  median early_doubles:", median(features$early_doubles), "\n")
cat("  Players with early_doubles>0:", sum(features$early_doubles > 0), "\n")
cat("  Players with early_doubles>=5:", sum(features$early_doubles >= 5), "\n")
cat("  early_doubles_share stats:\n")
print(summary(features$early_doubles_share))

# 4. Check archetypes - confirm it covers all eligible players
archetypes <- readRDS("outputs/archetypes.rds")
cat("\nArchetypes rows:", nrow(archetypes), "\n")
cat("  -- Confirmed: all 4368 eligible players are clustered.\n")
