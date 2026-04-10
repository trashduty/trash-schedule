# nfl_line_movement.R
#
# Analyzes NFL spread line movement from opening (OddsAPI, FanDuel/DraftKings)
# to closing (nflreadr spread_line) for the 2021-2025 regular seasons and playoffs.
#
# Opening line logic:
#   - Query OddsAPI at Sunday 23:59 UTC of the week prior to each game.
#   - If no line exists for a game on Sunday, fall back to Monday 12:00 UTC.
#   - Use whichever of FanDuel/DraftKings shows the *smaller* spread for the
#     favorite at open (i.e. smaller absolute value / fewer points given).
#
# Output: Excel workbook with two sheets
#   Sheet 1 – Summary statistics (counts and avg movement for favs & dogs)
#   Sheet 2 – Master game list (game_date, teams, open spread, close spread, movement)
#
# Usage:
#   Sys.setenv(ODDS_API_KEY = "your_key_here")
#   source("nfl_line_movement.R")
#
# Intermediate API results are cached in NFL_Odds/Data/cache/ to avoid
# re-fetching when the script is re-run.

library(dplyr)
library(tidyr)
library(readr)
library(nflreadr)
library(httr)
library(jsonlite)
library(glue)
library(lubridate)
library(purrr)
library(openxlsx)

# ── Configuration ──────────────────────────────────────────────────────────────
API_KEY    <- Sys.getenv("ODDS_API_KEY")
SPORT      <- "americanfootball_nfl"
SEASONS    <- 2021:2025
BOOKMAKERS <- "fanduel,draftkings"
GAME_TYPES <- c("REG", "WC", "DIV", "CON", "SB")
CACHE_DIR  <- "NFL_Odds/Data/cache"
OUT_FILE   <- "NFL_Odds/Data/nfl_line_movement.xlsx"

if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

# ── Helper: parse OddsAPI historical response text ─────────────────────────────
# Returns a tidy data frame with one row per (game × bookmaker × team).
parse_historical_response <- function(response_text) {
  if (is.null(response_text) || nchar(trimws(response_text)) == 0) {
    return(tibble())
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(response_text, flatten = FALSE),
    error = function(e) {
      message("JSON parse error: ", e$message)
      NULL
    }
  )

  if (is.null(parsed) || is.null(parsed$data) || length(parsed$data) == 0) {
    return(tibble())
  }

  games <- parsed$data
  rows  <- list()

  for (i in seq_along(games$id)) {
    bk_list <- games$bookmakers[[i]]
    if (is.null(bk_list) || nrow(bk_list) == 0) next

    for (j in seq_len(nrow(bk_list))) {
      bk_key   <- bk_list$key[j]
      mkt_list <- bk_list$markets[[j]]
      if (is.null(mkt_list) || nrow(mkt_list) == 0) next

      spreads_idx <- which(mkt_list$key == "spreads")
      if (length(spreads_idx) == 0) next

      outcomes <- mkt_list$outcomes[[spreads_idx[1]]]
      if (is.null(outcomes) || nrow(outcomes) == 0) next

      rows[[length(rows) + 1L]] <- tibble(
        api_game_id   = games$id[i],
        commence_time = games$commence_time[i],
        home_team     = games$home_team[i],
        away_team     = games$away_team[i],
        bookmaker     = bk_key,
        team_name     = outcomes$name,
        spread_open   = outcomes$point
      )
    }
  }

  if (length(rows) == 0L) return(tibble())
  bind_rows(rows)
}

# ── Helper: fetch historical odds (with caching) ───────────────────────────────
# Parameters:
#   date_str  – ISO 8601 snapshot timestamp, e.g. "2021-09-05T23:59:00Z".
#               The API returns the state of the odds market at that moment.
#   sleep_sec – seconds to wait before each live API call (rate-limit courtesy).
# Returns a tibble with columns:
#   api_game_id, commence_time, home_team, away_team, bookmaker, team_name,
#   spread_open.
# Results are written to CACHE_DIR as .rds files keyed on the timestamp digits,
# so re-running the script skips already-fetched dates.
fetch_historical_odds <- function(date_str, sleep_sec = 1.5) {
  cache_file <- file.path(CACHE_DIR, paste0("odds_", gsub("[^0-9]", "", date_str), ".rds"))

  if (file.exists(cache_file)) {
    message("  [cache] ", date_str)
    return(readRDS(cache_file))
  }

  Sys.sleep(sleep_sec)
  message("  [API]   ", date_str)

  url  <- glue("https://api.the-odds-api.com/v4/historical/sports/{SPORT}/odds")
  resp <- tryCatch(
    httr::GET(url, query = list(
      apiKey     = API_KEY,
      regions    = "us",
      markets    = "spreads",
      date       = date_str,
      bookmakers = BOOKMAKERS,
      oddsFormat = "american"
    )),
    error = function(e) {
      message("  HTTP error: ", e$message)
      NULL
    }
  )

  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("  Failed (status ",
            if (!is.null(resp)) httr::status_code(resp) else "N/A", ")")
    saveRDS(tibble(), cache_file)
    return(tibble())
  }

  result <- parse_historical_response(
    httr::content(resp, "text", encoding = "UTF-8")
  )
  saveRDS(result, cache_file)
  result
}

# ── Step 1: Load NFL schedules ─────────────────────────────────────────────────
message("Loading NFL schedules for seasons ", paste(SEASONS, collapse = ", "), " ...")

schedules <- nflreadr::load_schedules(seasons = SEASONS) |>
  filter(game_type %in% GAME_TYPES) |>
  mutate(game_date = as_date(gameday)) |>
  select(season, week, game_id, game_date, home_team, away_team,
         spread_line, game_type) |>
  filter(!is.na(spread_line), spread_line != 0)   # exclude pick'ems

message("  ", nrow(schedules), " games loaded.")

# ── Step 2: Compute opening-line dates for each game ──────────────────────────
# "Sunday of the prior calendar week" (week_start = 7 = Sunday in lubridate).
# For a Sunday game on D, this gives D - 7 (previous Sunday).
# For a Thursday TNF game, this gives the Sunday 4 days earlier.
# Lines open that Sunday evening (after prior-week games end) or Monday morning.

schedules <- schedules |>
  mutate(
    open_sunday = floor_date(game_date, "week", week_start = 7) - weeks(1),
    open_monday = open_sunday + days(1)
  )

# ── Step 3: Load team-name mapping ────────────────────────────────────────────
teams <- nflreadr::load_teams() |>
  select(team_abbr, team_name)

schedules <- schedules |>
  left_join(teams, by = c("home_team" = "team_abbr")) |>
  rename(home_team_name = team_name) |>
  left_join(teams, by = c("away_team" = "team_abbr")) |>
  rename(away_team_name = team_name)

# ── Step 4: Fetch OddsAPI historical odds ─────────────────────────────────────
# Sunday snapshot: 23:59 UTC (≈ 7–8 PM ET) – captures lines opened that evening.
# Monday snapshot: 12:00 UTC (≈ 8 AM ET) – catches lines that opened overnight.

unique_sundays <- sort(unique(schedules$open_sunday))
unique_mondays <- sort(unique(schedules$open_monday))

message("Fetching Sunday opening odds (", length(unique_sundays), " dates) ...")
sunday_odds_raw <- map_dfr(unique_sundays, function(sun) {
  date_str <- format(
    as.POSIXct(paste0(sun, " 23:59:00"), tz = "UTC"),
    format = "%Y-%m-%dT%H:%M:%SZ"
  )
  result <- fetch_historical_odds(date_str)
  if (nrow(result) > 0) mutate(result, query_date = sun) else tibble()
})

message("Fetching Monday opening odds (", length(unique_mondays), " dates) ...")
monday_odds_raw <- map_dfr(unique_mondays, function(mon) {
  date_str <- format(
    as.POSIXct(paste0(mon, " 12:00:00"), tz = "UTC"),
    format = "%Y-%m-%dT%H:%M:%SZ"
  )
  result <- fetch_historical_odds(date_str)
  if (nrow(result) > 0) mutate(result, query_date = mon) else tibble()
})

# ── Step 5: Process raw odds into game-level favorite/underdog spreads ─────────
# Converts the raw per-team odds rows into one row per (game × bookmaker),
# identifying the favorite (team with the most-negative spread point) and
# underdog (most-positive point).  Also converts commence_time from UTC to
# Eastern Time before extracting the game date so that late Monday night games
# (which cross midnight UTC) resolve to the correct calendar date.
# Returns a tibble with columns:
#   api_game_id, commence_time, game_date_et, home_team, away_team,
#   query_date, bookmaker, fav_spread, dog_spread, fav_team, dog_team.
# Add an ET game date (corrects for UTC↔ET shift on late Monday night games).
process_raw_odds <- function(raw_df) {
  if (nrow(raw_df) == 0) return(tibble())

  raw_df |>
    mutate(
      game_date_et = as_date(
        with_tz(ymd_hms(commence_time, quiet = TRUE), "America/New_York")
      )
    ) |>
    filter(bookmaker %in% c("fanduel", "draftkings")) |>
    # For each game × bookmaker, identify fav (min spread) and dog (max spread)
    group_by(api_game_id, bookmaker) |>
    mutate(
      fav_spread = min(spread_open),
      dog_spread = max(spread_open),
      fav_team   = team_name[which.min(spread_open)],
      dog_team   = team_name[which.max(spread_open)]
    ) |>
    ungroup() |>
    select(api_game_id, commence_time, game_date_et, home_team, away_team,
           query_date, bookmaker, fav_spread, dog_spread, fav_team, dog_team) |>
    distinct()
}

sunday_odds <- process_raw_odds(sunday_odds_raw)
monday_odds <- process_raw_odds(monday_odds_raw)

# ── Step 6: Match schedules to opening odds ───────────────────────────────────
# OddsAPI and nflreadr may disagree on which team is "home" for a given game,
# so we join on a canonical team-pair key (alphabetical concatenation of both
# team names) rather than individual home/away columns.  This ensures a game
# like "Bills vs Chiefs" matches regardless of which source calls which team
# "home".

add_team_pair <- function(df, col1, col2) {
  df |> mutate(
    team_pair = paste(pmin(.data[[col1]], .data[[col2]]),
                      pmax(.data[[col1]], .data[[col2]]),
                      sep = "|")
  )
}

schedules    <- add_team_pair(schedules, "home_team_name", "away_team_name")
sunday_odds  <- add_team_pair(sunday_odds, "home_team", "away_team")
monday_odds  <- add_team_pair(monday_odds, "home_team", "away_team")

# --- Sunday match ---
sched_sunday <- schedules |>
  left_join(
    sunday_odds |> rename(open_date_used = query_date),
    by = c(
      "team_pair",
      "game_date"   = "game_date_et",
      "open_sunday" = "open_date_used"
    )
  ) |>
  mutate(open_day_used = if_else(!is.na(fav_spread), "sunday", NA_character_))

# Identify games that still need a Monday lookup
games_need_monday <- sched_sunday |>
  filter(is.na(fav_spread)) |>
  select(season, week, game_id, game_date, home_team, away_team,
         spread_line, game_type, open_sunday, open_monday,
         home_team_name, away_team_name, team_pair)

# --- Monday match (for games with no Sunday line) ---
sched_monday_matched <- games_need_monday |>
  left_join(
    monday_odds |> rename(open_date_used = query_date),
    by = c(
      "team_pair",
      "game_date"   = "game_date_et",
      "open_monday" = "open_date_used"
    )
  ) |>
  mutate(open_day_used = if_else(!is.na(fav_spread), "monday", NA_character_))

# --- Combine: Sunday matches + Monday fills ---
games_with_sunday <- sched_sunday |>
  filter(!is.na(fav_spread))

games_combined <- bind_rows(games_with_sunday, sched_monday_matched) |>
  filter(!is.na(fav_spread))   # drop games with no opening line found

message(nrow(games_combined), " games matched to opening lines out of ",
        nrow(schedules), " total.")

# ── Step 7: Select best book (FanDuel vs DraftKings) ─────────────────────────
# For each game, pick the bookmaker whose opening favorite spread has the
# *smaller absolute value* (i.e. fewer points given by the favorite).
# If only one book has the game, use that book.

best_book_per_game <- games_combined |>
  group_by(game_id, bookmaker) |>
  slice(1) |>                        # one row per game × bookmaker
  ungroup() |>
  group_by(game_id) |>
  slice_min(abs(fav_spread), n = 1, with_ties = FALSE) |>   # smaller fav spread
  ungroup()

# ── Step 8: Compute spread movement (open → close) ────────────────────────────
# nflreadr spread_line is from the home team's perspective:
#   negative  → home team is the favorite
#   positive  → away team is the favorite
# We normalise to "favorite's closing spread" = -abs(spread_line).

movement_df <- best_book_per_game |>
  mutate(
    fav_close  = -abs(spread_line),          # always negative
    dog_close  =  abs(spread_line),          # always positive
    # fav_spread (open) is already negative; dog_spread (open) is positive
    fav_movement = fav_close - fav_spread,   # neg → got bigger fav; pos → got smaller
    dog_movement = dog_close - dog_spread,   # pos → got bigger dog; neg → got smaller
    fav_direction = case_when(
      fav_movement < 0 ~ "larger",           # e.g. -6 → -7
      fav_movement > 0 ~ "smaller",          # e.g. -7 → -6
      TRUE             ~ "neutral"
    ),
    dog_direction = case_when(
      dog_movement > 0 ~ "larger",           # e.g. +6 → +7
      dog_movement < 0 ~ "smaller",          # e.g. +7 → +6
      TRUE             ~ "neutral"
    )
  )

# ── Step 9: Build summary sheet ───────────────────────────────────────────────
# Aggregates movement_df by direction category for a single perspective
# (favorites or underdogs).
# Parameters:
#   df             – movement_df data frame.
#   direction_col  – name of the column holding "larger" / "smaller" / "neutral"
#                    labels (character string).
#   movement_col   – name of the numeric movement column; abs() is applied so
#                    the average is always a positive number of points.
#   group_label    – label used in the output "Group" column ("Favorites" or
#                    "Underdogs").
# Returns a 3-row tibble (one per direction level) with columns:
#   group, direction, count, avg_movement.
summarise_direction <- function(df, direction_col, movement_col, group_label) {
  df |>
    group_by(direction = .data[[direction_col]]) |>
    summarise(
      count        = n(),
      avg_movement = mean(abs(.data[[movement_col]]), na.rm = TRUE),
      .groups      = "drop"
    ) |>
    mutate(
      group = group_label,
      direction = factor(direction, levels = c("larger", "smaller", "neutral"))
    ) |>
    arrange(direction) |>
    select(group, direction, count, avg_movement)
}

fav_summary <- summarise_direction(movement_df, "fav_direction", "fav_movement", "Favorites")
dog_summary <- summarise_direction(movement_df, "dog_direction", "dog_movement", "Underdogs")

summary_sheet <- bind_rows(fav_summary, dog_summary) |>
  rename(
    Group                              = group,
    `Spread Direction at Close`        = direction,
    `Number of Games`                  = count,
    `Avg Points of Movement`           = avg_movement
  )

# ── Step 10: Build master-list sheet ──────────────────────────────────────────
master_sheet <- movement_df |>
  mutate(
    favorite = fav_team,
    underdog = dog_team
  ) |>
  select(
    Season          = season,
    `Game Type`     = game_type,
    `NFL Week`      = week,
    `Game Date`     = game_date,
    `Home Team`     = home_team,
    `Away Team`     = away_team,
    `Favorite`      = favorite,
    `Underdog`      = underdog,
    `Open Book`     = bookmaker,
    `Open Day`      = open_day_used,
    `Fav Spread Open`  = fav_spread,
    `Fav Spread Close` = fav_close,
    `Fav Movement`     = fav_movement,
    `Fav Direction`    = fav_direction,
    `Dog Spread Open`  = dog_spread,
    `Dog Spread Close` = dog_close,
    `Dog Movement`     = dog_movement,
    `Dog Direction`    = dog_direction
  ) |>
  arrange(Season, `Game Date`)

# ── Step 11: Write Excel workbook ─────────────────────────────────────────────
message("Writing output to ", OUT_FILE, " ...")

wb <- createWorkbook()

# --- Sheet 1: Summary ---
addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_sheet, startRow = 1, startCol = 1)

# Header style
header_style <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#2E4057",
  halign = "CENTER", textDecoration = "Bold",
  border = "Bottom"
)
addStyle(wb, "Summary", header_style, rows = 1, cols = 1:4, gridExpand = TRUE)

# Group row shading
fav_rows <- which(summary_sheet$Group == "Favorites") + 1L
dog_rows <- which(summary_sheet$Group == "Underdogs") + 1L
addStyle(wb, "Summary", createStyle(fgFill = "#D9E8F5"),
         rows = fav_rows, cols = 1:4, gridExpand = TRUE)
addStyle(wb, "Summary", createStyle(fgFill = "#FFF3CD"),
         rows = dog_rows, cols = 1:4, gridExpand = TRUE)

# Number format for avg movement
addStyle(wb, "Summary", createStyle(numFmt = "0.00"),
         rows = 2:(nrow(summary_sheet) + 1), cols = 4, gridExpand = TRUE)

setColWidths(wb, "Summary", cols = 1:4, widths = c(14, 26, 18, 24))

# --- Sheet 2: Master Game List ---
addWorksheet(wb, "Master Game List")
writeData(wb, "Master Game List", master_sheet, startRow = 1, startCol = 1)

addStyle(wb, "Master Game List", header_style,
         rows = 1, cols = 1:ncol(master_sheet), gridExpand = TRUE)

# Colour-code movement direction column (Fav Direction = col 14, Dog Direction = col 18)
fav_dir_col <- which(names(master_sheet) == "Fav Direction")
dog_dir_col <- which(names(master_sheet) == "Dog Direction")

for (r in seq_len(nrow(master_sheet))) {
  data_row <- r + 1L
  fav_dir  <- master_sheet$`Fav Direction`[r]
  dog_dir  <- master_sheet$`Dog Direction`[r]

  fav_fill <- switch(fav_dir, larger = "#C6EFCE", smaller = "#FFC7CE", "#FFEB9C")
  dog_fill <- switch(dog_dir, larger = "#C6EFCE", smaller = "#FFC7CE", "#FFEB9C")

  addStyle(wb, "Master Game List", createStyle(fgFill = fav_fill),
           rows = data_row, cols = fav_dir_col)
  addStyle(wb, "Master Game List", createStyle(fgFill = dog_fill),
           rows = data_row, cols = dog_dir_col)
}

# Freeze top row and auto-filter
freezePane(wb, "Master Game List", firstRow = TRUE)
addFilter(wb, "Master Game List", row = 1, cols = 1:ncol(master_sheet))

setColWidths(wb, "Master Game List", cols = 1:ncol(master_sheet),
             widths = c(8, 10, 10, 12, 14, 14, 24, 24, 12, 10,
                        16, 17, 14, 14, 16, 17, 14, 14))

saveWorkbook(wb, OUT_FILE, overwrite = TRUE)
message("Done. Output saved to: ", OUT_FILE)
message("Games included : ", nrow(master_sheet))
message("Games excluded (no opening line found): ",
        nrow(schedules) - nrow(master_sheet))
