# cfb_line_movement.R
#
# Analyzes CFB (FBS) spread line movement from opening to closing for the
# 2021-2025 seasons.
#
# Primary data source: cfbfastR / CFBD (cfbd_betting_lines)
#   - Both opening spread (spread_open) and closing spread (spread) come from
#     the CFBD API when available.
#   - If CFBD is missing opening or closing lines for a game, OddsAPI historical
#     snapshots are used as a fallback.
#
# Opening line fallback (OddsAPI):
#   - Query at Sunday 23:59 UTC of the week prior to each game.
#   - If no line on Sunday, fall back to Monday 12:00 UTC.
#   - If still no line, fall back to Tuesday 12:00 UTC (covers Tue/Wed games).
#   - Pick'em games (spread == 0) are included.
#
# Output: Excel workbook with two sheets
#   Sheet 1 – Summary statistics (counts and avg movement for favs & dogs)
#   Sheet 2 – Master game list (game_date, teams, open spread, close spread, movement)
# Also writes a CSV for easy diffing.
#
# Intermediate API results are cached in CFB_Odds/Data/cache/ to avoid
# re-fetching when the script is re-run.

library(dplyr)
library(tidyr)
library(readr)
library(cfbfastR)
library(httr)
library(jsonlite)
library(glue)
library(lubridate)
library(purrr)
library(openxlsx)

# ── Configuration ─────────────────────────────────────────────────────────────
CFBD_KEY   <- Sys.getenv("CFBD_API_KEY")
API_KEY    <- Sys.getenv("ODDS_API_KEY")
SPORT      <- "americanfootball_ncaaf"
SEASONS    <- 2021:2025
BOOKMAKERS <- "fanduel,draftkings"
CACHE_DIR  <- "CFB_Odds/Data/cache"
DATA_DIR   <- "CFB_Odds/Data"
OUT_FILE   <- "CFB_Odds/Data/cfb_line_movement.xlsx"
CSV_FILE   <- "CFB_Odds/Data/cfb_line_movement.csv"

if (!dir.exists(DATA_DIR))  dir.create(DATA_DIR,  recursive = TRUE)
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

# Log whether CFBD_API_KEY is set (cfbfastR reads this from env automatically)
message("CFBD_API_KEY set: ", if (nchar(CFBD_KEY) > 0) "TRUE" else "FALSE (CFBD API calls may fail or be rate-limited)")

# ── Helper: parse OddsAPI historical response text ────────────────────────────
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
        spread_pt     = outcomes$point
      )
    }
  }

  if (length(rows) == 0L) return(tibble())
  bind_rows(rows)
}

# ── Helper: fetch historical odds (with caching) ──────────────────────────────
fetch_historical_odds <- function(date_str, sleep_sec = 1.5) {
  cache_file <- file.path(CACHE_DIR, paste0("cfb_odds_", gsub("[^0-9]", "", date_str), ".rds"))

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

# ── Helper: process raw OddsAPI rows into game-level fav/dog tibble ───────────
process_raw_odds <- function(raw_df) {
  empty_result <- tibble(
    api_game_id   = character(),
    commence_time = character(),
    game_date_et  = as_date(character()),
    home_team     = character(),
    away_team     = character(),
    query_date    = as_date(character()),
    bookmaker     = character(),
    fav_spread    = numeric(),
    dog_spread    = numeric(),
    fav_team      = character(),
    dog_team      = character()
  )

  if (nrow(raw_df) == 0 || !"home_team" %in% names(raw_df)) return(empty_result)

  processed <- raw_df |>
    mutate(
      game_date_et = as_date(
        with_tz(ymd_hms(commence_time, quiet = TRUE), "America/New_York")
      )
    ) |>
    filter(bookmaker %in% c("fanduel", "draftkings")) |>
    filter(query_date != game_date_et)

  if (nrow(processed) == 0) return(empty_result)

  processed |>
    group_by(api_game_id, bookmaker) |>
    mutate(
      fav_spread = min(spread_pt),
      dog_spread = max(spread_pt),
      fav_team   = team_name[which.min(spread_pt)],
      dog_team   = team_name[which.max(spread_pt)]
    ) |>
    ungroup() |>
    select(api_game_id, commence_time, game_date_et, home_team, away_team,
           query_date, bookmaker, fav_spread, dog_spread, fav_team, dog_team) |>
    distinct()
}

# ── Helper: canonical team-pair key (alphabetical) ────────────────────────────
add_team_pair <- function(df, col1, col2) {
  df |> mutate(
    team_pair = paste(pmin(.data[[col1]], .data[[col2]]),
                      pmax(.data[[col1]], .data[[col2]]),
                      sep = "|")
  )
}

# ── Step 1: Load CFB schedules (FBS, regular + postseason) ───────────────────
message("Loading CFB schedules for seasons ", paste(SEASONS, collapse = ", "), " ...")

load_cfb_schedule_season <- function(yr) {
  cache_file <- file.path(CACHE_DIR, paste0("cfb_schedule_", yr, ".rds"))
  if (file.exists(cache_file)) {
    message("  [cache] schedule ", yr)
    return(readRDS(cache_file))
  }
  message("  [API]   schedule ", yr)
  # Load both regular season and postseason games
  reg  <- tryCatch(
    cfbfastR::cfbd_game_info(year = yr, season_type = "regular"),
    error = function(e) { message("    Error loading regular season: ", e$message); tibble() }
  )
  post <- tryCatch(
    cfbfastR::cfbd_game_info(year = yr, season_type = "postseason"),
    error = function(e) { message("    Error loading postseason: ", e$message); tibble() }
  )
  combined <- bind_rows(
    if (nrow(reg)  > 0) mutate(reg,  season_type = "regular")   else tibble(),
    if (nrow(post) > 0) mutate(post, season_type = "postseason") else tibble()
  )
  saveRDS(combined, cache_file)
  combined
}

schedules_raw <- map_dfr(SEASONS, load_cfb_schedule_season)

if (nrow(schedules_raw) == 0) {
  message("No CFB schedule data found. Exiting.")
  quit(status = 0)
}

# Detect game id column across cfbfastR versions
id_col <- dplyr::case_when(
  "id" %in% names(schedules_raw)      ~ "id",
  "game_id" %in% names(schedules_raw) ~ "game_id",
  "gameId" %in% names(schedules_raw)  ~ "gameId",
  TRUE ~ NA_character_
)

if (is.na(id_col)) {
  stop("Could not find a game id column in schedules_raw. Columns are: ",
       paste(names(schedules_raw), collapse = ", "))
}

schedules <- schedules_raw |>
  filter(!is.na(home_team), !is.na(away_team)) |>
  mutate(
    game_date = as_date(with_tz(ymd_hms(start_date, quiet = TRUE), "America/New_York")),
    game_id = as.character(.data[[id_col]])
  ) |>
  select(season, week, game_id, game_date, home_team, away_team, season_type) |>
  filter(!is.na(game_date))

# ── Step 1b: Load team crosswalk for OddsAPI name mapping ────────────────────
CROSSWALK_FILE <- file.path(DATA_DIR, "CFB Teams Full Crosswalk.csv")
cfb_crosswalk  <- tryCatch(
  readr::read_csv(CROSSWALK_FILE, show_col_types = FALSE) |>
    select(cfbd_short = btb_team_short, api_name = api_team) |>
    filter(!is.na(api_name)),
  error = function(e) {
    message("  Crosswalk not found; OddsAPI name matching will use raw CFBD names.")
    tibble(cfbd_short = character(), api_name = character())
  }
)

# Map CFBD home/away team names → OddsAPI team names for fallback matching
resolve_api_name <- function(team_name, crosswalk) {
  idx <- match(team_name, crosswalk$cfbd_short)
  ifelse(!is.na(idx), crosswalk$api_name[idx], team_name)
}

schedules <- schedules |>
  mutate(
    home_api = resolve_api_name(home_team, cfb_crosswalk),
    away_api = resolve_api_name(away_team, cfb_crosswalk)
  )

message("  ", nrow(schedules), " games loaded.")

# ── Step 2: Load CFBD betting lines for each season ──────────────────────────
message("Loading CFBD betting lines ...")

load_cfb_lines_season <- function(yr) {
  cache_file <- file.path(CACHE_DIR, paste0("cfb_lines_", yr, ".rds"))
  if (file.exists(cache_file)) {
    message("  [cache] lines ", yr)
    return(readRDS(cache_file))
  }
  message("  [API]   lines ", yr)
  lines_reg  <- tryCatch(
    cfbfastR::cfbd_betting_lines(year = yr, season_type = "regular"),
    error = function(e) { message("    Error loading regular lines: ", e$message); tibble() }
  )
  lines_post <- tryCatch(
    cfbfastR::cfbd_betting_lines(year = yr, season_type = "postseason"),
    error = function(e) { message("    Error loading postseason lines: ", e$message); tibble() }
  )
  combined <- bind_rows(
    if (nrow(lines_reg)  > 0) lines_reg  else tibble(),
    if (nrow(lines_post) > 0) lines_post else tibble()
  )
  saveRDS(combined, cache_file)
  combined
}

lines_raw <- map_dfr(SEASONS, load_cfb_lines_season)

message("  ", nrow(lines_raw), " betting line rows loaded.")

# ── Step 3: Extract best opening + closing spread from CFBD ──────────────────
extract_cfbd_lines <- function(lines_df) {
  if (nrow(lines_df) == 0 || !"game_id" %in% names(lines_df)) {
    return(tibble(
      game_id      = character(),
      cfbd_open    = numeric(),
      cfbd_close   = numeric(),
      cfbd_provider = character()
    ))
  }

  if ("spread_open" %in% names(lines_df)) {
    lines_df <- lines_df |> rename(open_spread = spread_open)
  }
  if (!"open_spread" %in% names(lines_df)) {
    lines_df$open_spread <- NA_real_
  }
  if (!"spread" %in% names(lines_df)) {
    lines_df$spread <- NA_real_
  }
  if (!"provider" %in% names(lines_df)) {
    lines_df$provider <- NA_character_
  }

  lines_df |>
    mutate(
      game_id    = as.character(game_id),
      open_spread = suppressWarnings(as.numeric(open_spread)),
      spread      = suppressWarnings(as.numeric(spread))
    ) |>
    filter(!is.na(open_spread) | !is.na(spread)) |>
    arrange(game_id,
            desc(!is.na(open_spread) & !is.na(spread)),
            desc(tolower(provider) == "consensus")) |>
    group_by(game_id) |>
    slice(1) |>
    ungroup() |>
    select(game_id, cfbd_open = open_spread, cfbd_close = spread, cfbd_provider = provider)
}

cfbd_lines <- extract_cfbd_lines(lines_raw)
message("  ", nrow(cfbd_lines), " unique games with CFBD line data.")

# ── Step 4: Join CFBD lines to schedule ──────────────────────────────────────
schedules <- schedules |>
  mutate(game_id = as.character(game_id)) |>
  left_join(cfbd_lines, by = "game_id")

n_have_both  <- sum(!is.na(schedules$cfbd_open) & !is.na(schedules$cfbd_close))
n_need_open  <- sum( is.na(schedules$cfbd_open))
n_need_close <- sum( is.na(schedules$cfbd_close))

message("  Games with CFBD open + close : ", n_have_both)
message("  Games missing CFBD open      : ", n_need_open)
message("  Games missing CFBD close     : ", n_need_close)

# ── Step 5: Compute opening-line fallback dates (OddsAPI) ────────────────────
schedules <- schedules |>
  mutate(
    open_sunday  = floor_date(game_date, "week", week_start = 7) - weeks(1),
    open_monday  = open_sunday + days(1),
    open_tuesday = open_sunday + days(2)
  )

# ── Step 6: Fetch OddsAPI historical odds (only if fallback needed) ───────────
games_need_fallback <- schedules |>
  filter(is.na(cfbd_open) | is.na(cfbd_close))

if (nrow(games_need_fallback) > 0 && nchar(API_KEY) > 0) {
  message("Fetching OddsAPI fallback for ", nrow(games_need_fallback), " games ...")

  unique_sundays  <- sort(unique(games_need_fallback$open_sunday))
  unique_mondays  <- sort(unique(games_need_fallback$open_monday))
  unique_tuesdays <- sort(unique(games_need_fallback$open_tuesday))

  message("Fetching Sunday opening odds (", length(unique_sundays), " dates) ...")
  sunday_odds_raw <- map_dfr(unique_sundays, function(sun) {
    date_str <- format(as.POSIXct(paste0(sun, " 23:59:00"), tz = "UTC"), format = "%Y-%m-%dT%H:%M:%SZ")
    result <- fetch_historical_odds(date_str)
    if (nrow(result) > 0) mutate(result, query_date = sun) else tibble()
  })

  message("Fetching Monday opening odds (", length(unique_mondays), " dates) ...")
  monday_odds_raw <- map_dfr(unique_mondays, function(mon) {
    date_str <- format(as.POSIXct(paste0(mon, " 12:00:00"), tz = "UTC"), format = "%Y-%m-%dT%H:%M:%SZ")
    result <- fetch_historical_odds(date_str)
    if (nrow(result) > 0) mutate(result, query_date = mon) else tibble()
  })

  message("Fetching Tuesday opening odds (", length(unique_tuesdays), " dates) ...")
  tuesday_odds_raw <- map_dfr(unique_tuesdays, function(tue) {
    date_str <- format(as.POSIXct(paste0(tue, " 12:00:00"), tz = "UTC"), format = "%Y-%m-%dT%H:%M:%SZ")
    result <- fetch_historical_odds(date_str)
    if (nrow(result) > 0) mutate(result, query_date = tue) else tibble()
  })

  sunday_odds  <- process_raw_odds(sunday_odds_raw)
  monday_odds  <- process_raw_odds(monday_odds_raw)
  tuesday_odds <- process_raw_odds(tuesday_odds_raw)

} else {
  if (nrow(games_need_fallback) == 0) {
    message("All games have CFBD lines; skipping OddsAPI fetch.")
  } else {
    message("ODDS_API_KEY not set; skipping OddsAPI fallback.")
  }
  empty_odds <- tibble(
    api_game_id   = character(),
    commence_time = character(),
    game_date_et  = as_date(character()),
    home_team     = character(),
    away_team     = character(),
    query_date    = as_date(character()),
    bookmaker     = character(),
    fav_spread    = numeric(),
    dog_spread    = numeric(),
    fav_team      = character(),
    dog_team      = character()
  )
  sunday_odds  <- empty_odds
  monday_odds  <- empty_odds
  tuesday_odds <- empty_odds
}

# ── Step 7: Match schedule to OddsAPI opening lines (for fallback games) ──────
games_need_odds_open <- schedules |>
  filter(is.na(cfbd_open)) |>
  add_team_pair("home_api", "away_api")

sunday_odds_tp  <- add_team_pair(sunday_odds,  "home_team", "away_team")
monday_odds_tp  <- add_team_pair(monday_odds,  "home_team", "away_team")
tuesday_odds_tp <- add_team_pair(tuesday_odds, "home_team", "away_team")

# --- Sunday match ---
sched_sun <- games_need_odds_open |>
  left_join(
    sunday_odds_tp |>
      rename(open_date_used = query_date) |>
      select(team_pair, game_date_et, open_date_used, bookmaker, fav_spread, dog_spread, fav_team, dog_team),
    by = c("team_pair", "game_date" = "game_date_et", "open_sunday" = "open_date_used")
  ) |>
  mutate(open_day_used = if_else(!is.na(fav_spread), "sunday", NA_character_))

games_need_mon <- sched_sun |>
  filter(is.na(fav_spread)) |>
  select(-bookmaker, -fav_spread, -dog_spread, -fav_team, -dog_team, -open_day_used)

# --- Monday match ---
sched_mon <- games_need_mon |>
  left_join(
    monday_odds_tp |>
      rename(open_date_used = query_date) |>
      select(team_pair, game_date_et, open_date_used, bookmaker, fav_spread, dog_spread, fav_team, dog_team),
    by = c("team_pair", "game_date" = "game_date_et", "open_monday" = "open_date_used")
  ) |>
  mutate(open_day_used = if_else(!is.na(fav_spread), "monday", NA_character_))

games_need_tue <- sched_mon |>
  filter(is.na(fav_spread)) |>
  select(-bookmaker, -fav_spread, -dog_spread, -fav_team, -dog_team, -open_day_used)

# --- Tuesday match ---
sched_tue <- games_need_tue |>
  left_join(
    tuesday_odds_tp |>
      rename(open_date_used = query_date) |>
      select(team_pair, game_date_et, open_date_used, bookmaker, fav_spread, dog_spread, fav_team, dog_team),
    by = c("team_pair", "game_date" = "game_date_et", "open_tuesday" = "open_date_used")
  ) |>
  mutate(open_day_used = if_else(!is.na(fav_spread), "tuesday", NA_character_))

# Combine OddsAPI opening-line matches, enforcing strict day priority
# (Sunday > Monday > Tuesday) so that a later snapshot never supersedes
# an earlier one for the same game.
odds_open_matches <- bind_rows(
  sched_sun |> filter(!is.na(fav_spread)),
  sched_mon |> filter(!is.na(fav_spread)),
  sched_tue |> filter(!is.na(fav_spread))
) |>
  mutate(day_priority = case_when(
    open_day_used == "sunday"  ~ 1L,
    open_day_used == "monday"  ~ 2L,
    open_day_used == "tuesday" ~ 3L,
    TRUE                       ~ 99L
  )) |>
  group_by(game_id) |>
  filter(day_priority == min(day_priority)) |>
  ungroup() |>
  select(game_id, bookmaker, fav_spread_odds = fav_spread, dog_spread_odds = dog_spread, fav_team_odds = fav_team, dog_team_odds = dog_team, open_day_used)

# ── Step 8: Build unified spread data ─────────────────────────────────────────
schedules_with_lines <- schedules |>
  left_join(odds_open_matches, by = "game_id") |>
  mutate(
    cfbd_fav_open  = if_else(!is.na(cfbd_open),  -abs(cfbd_open),  NA_real_),
    cfbd_fav_close = if_else(!is.na(cfbd_close), -abs(cfbd_close), NA_real_),
    cfbd_dog_open  = if_else(!is.na(cfbd_open),   abs(cfbd_open),  NA_real_),
    cfbd_dog_close = if_else(!is.na(cfbd_close),  abs(cfbd_close), NA_real_),
    cfbd_fav_team  = case_when(
      !is.na(cfbd_open)  & cfbd_open  < 0 ~ home_team,
      !is.na(cfbd_open)  & cfbd_open  > 0 ~ away_team,
      !is.na(cfbd_close) & cfbd_close < 0 ~ home_team,
      !is.na(cfbd_close) & cfbd_close > 0 ~ away_team,
      TRUE ~ NA_character_
    ),
    cfbd_dog_team  = case_when(
      !is.na(cfbd_open)  & cfbd_open  < 0 ~ away_team,
      !is.na(cfbd_open)  & cfbd_open  > 0 ~ home_team,
      !is.na(cfbd_close) & cfbd_close < 0 ~ away_team,
      !is.na(cfbd_close) & cfbd_close > 0 ~ home_team,
      TRUE ~ NA_character_
    ),
    final_fav_open  = coalesce(cfbd_fav_open,  fav_spread_odds),
    final_dog_open  = coalesce(cfbd_dog_open,  dog_spread_odds),
    final_fav_close = cfbd_fav_close,
    final_dog_close = cfbd_dog_close,
    final_fav_team  = coalesce(cfbd_fav_team,  fav_team_odds),
    final_dog_team  = coalesce(cfbd_dog_team,  dog_team_odds),
    open_source     = case_when(
      !is.na(cfbd_open)    ~ "cfbd",
      !is.na(fav_spread_odds) ~ paste0("oddsapi_", open_day_used),
      TRUE                 ~ NA_character_
    ),
    close_source    = if_else(!is.na(cfbd_close), "cfbd", NA_character_),
    open_book       = coalesce(cfbd_provider, bookmaker)
  )

# Keep only games where we have both an opening and closing line
movement_df <- schedules_with_lines |>
  filter(!is.na(final_fav_open) & !is.na(final_fav_close)) |>
  mutate(
    fav_spread    = final_fav_open,
    dog_spread    = final_dog_open,
    fav_close     = final_fav_close,
    dog_close     = final_dog_close,
    fav_team      = final_fav_team,
    dog_team      = final_dog_team,
    fav_movement  = fav_close - fav_spread,
    dog_movement  = dog_close - dog_spread,
    fav_direction = case_when(
      fav_movement < 0 ~ "larger",
      fav_movement > 0 ~ "smaller",
      TRUE             ~ "neutral"
    ),
    dog_direction = case_when(
      dog_movement > 0 ~ "larger",
      dog_movement < 0 ~ "smaller",
      TRUE             ~ "neutral"
    )
  )

message(nrow(movement_df), " games have both opening and closing lines out of ", nrow(schedules), " total.")

# ── Step 9: Build summary sheet ───────────────────────────────────────────────
summarise_direction <- function(df, direction_col, movement_col, group_label) {
  df |>
    group_by(direction = .data[[direction_col]]) |>
    summarise(
      count        = n(),
      avg_movement = mean(abs(.data[[movement_col]]), na.rm = TRUE),
      .groups      = "drop"
    ) |>
    mutate(group = group_label, direction = factor(direction, levels = c("larger", "smaller", "neutral"))) |>
    arrange(direction) |>
    select(group, direction, count, avg_movement)
}

fav_summary <- summarise_direction(movement_df, "fav_direction", "fav_movement", "Favorites")
dog_summary <- summarise_direction(movement_df, "dog_direction", "dog_movement", "Underdogs")

summary_sheet <- bind_rows(fav_summary, dog_summary) |>
  rename(Group = group, `Spread Direction at Close` = direction, `Number of Games` = count, `Avg Points of Movement` = avg_movement)

# ── Step 10: Build master-list sheet ──────────────────────────────────────────
master_sheet <- movement_df |>
  select(
    Season = season, `Season Type` = season_type, `CFB Week` = week, `Game Date` = game_date, `Home Team` = home_team, `Away Team` = away_team,
    `Favorite` = fav_team, `Underdog` = dog_team, `Open Source` = open_source, `Close Source` = close_source, `Open Book` = open_book, `Open Day` = open_day_used,
    `Fav Spread Open` = fav_spread, `Fav Spread Close` = fav_close, `Fav Movement` = fav_movement, `Fav Direction` = fav_direction,
    `Dog Spread Open` = dog_spread, `Dog Spread Close` = dog_close, `Dog Movement` = dog_movement, `Dog Direction` = dog_direction
  ) |>
  arrange(Season, `Game Date`)

# ── Step 11: Write Excel workbook ─────────────────────────────────────────────
message("Writing output to ", OUT_FILE, " ...")
wb <- createWorkbook()
addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_sheet, startRow = 1, startCol = 1)
header_style <- createStyle(fontColour = "#FFFFFF", fgFill = "#2E4057", halign = "CENTER", textDecoration = "Bold", border = "Bottom")
addStyle(wb, "Summary", header_style, rows = 1, cols = 1:4, gridExpand = TRUE)
setColWidths(wb, "Summary", cols = 1:4, widths = c(14, 26, 18, 24))

addWorksheet(wb, "Master Game List")
writeData(wb, "Master Game List", master_sheet, startRow = 1, startCol = 1)
addStyle(wb, "Master Game List", header_style, rows = 1, cols = 1:ncol(master_sheet), gridExpand = TRUE)
freezePane(wb, "Master Game List", firstRow = TRUE)
addFilter(wb, "Master Game List", row = 1, cols = 1:ncol(master_sheet))
setColWidths(wb, "Master Game List", cols = 1:ncol(master_sheet), widths = c(8, 12, 10, 12, 18, 18, 24, 24, 12, 12, 14, 10, 16, 17, 14, 14, 16, 17, 14, 14))

saveWorkbook(wb, OUT_FILE, overwrite = TRUE)
write_csv(master_sheet, CSV_FILE)
message("Done. Output saved to: ", OUT_FILE)

# ── Step 12: Failsafe audit report ────────────────────────────────────────────
n_scheduled    <- nrow(schedules)
n_have_both_lines <- nrow(movement_df)
n_missing_either <- sum(is.na(schedules_with_lines$final_fav_open) | is.na(schedules_with_lines$final_fav_close))

missing_games <- schedules_with_lines |>
  filter(is.na(final_fav_open) | is.na(final_fav_close)) |>
  select(season, week, season_type, game_date, home_team, away_team, final_fav_open, final_fav_close) |>
  arrange(season, game_date)

cat("\n")
cat("══════════════════════════════════════════════════════════\n")
cat("  CFB LINE MOVEMENT – AUDIT REPORT\n")
cat("══════════════════════════════════════════════════════════\n")
cat(sprintf("  Seasons                         : %s\n", paste(range(SEASONS), collapse = "–")))
cat(sprintf("  Total scheduled games           : %d\n", n_scheduled))
cat(sprintf("  Games with both open+close lines: %d\n", n_have_both_lines))
cat(sprintf("  Games missing either line       : %d\n", n_missing_either))
cat("\n")
cat("  Missing games (season | week | type | date | away @ home):\n")
if (nrow(missing_games) == 0) {
  cat("    (none – all games have lines)\n")
} else {
  for (i in seq_len(nrow(missing_games))) {
    g <- missing_games[i, ]
    miss_tag <- paste0(if (is.na(g$final_fav_open)) "no_open " else "", if (is.na(g$final_fav_close)) "no_close" else "")
    cat(sprintf("    %s  Wk%-3s [%-10s]  %s  %s @ %s  [%s]\n", g$season, g$week, g$season_type, g$game_date, g$away_team, g$home_team, trimws(miss_tag)))
  }
}
cat("══════════════════════════════════════════════════════════\n")
flush.console()
