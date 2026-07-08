# (Use your current file, but apply these full integrated edits)
# Key additions:
# 1) keep commence_time in api_spreads
# 2) carry commence_time through odds_lookup_joined
# 3) add commence_time + game_date_et + game_time_et to spread_summary
# 4) write those columns to spreads_odds.csv

library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(nflreadr)
library(httr)
library(jsonlite)
library(glue)
library(lubridate)
library(stringr)
library(cfbfastR)

options(scipen=999)

cfb_crosswalk_path <- "CFB_Odds/Data/CFB Teams Full Crosswalk.csv"
lookup_path <- "CFB_Odds/Data/Expanded_CFB_Spread_Pricing_Table_Binned.csv"
model_output_path <- "cfb model output_new.csv"

cfb_crosswalk <- read_csv(cfb_crosswalk_path, show_col_types = FALSE)

lookup <- read_csv(lookup_path, show_col_types = FALSE) |>
  janitor::clean_names() |>
  select(total_bin, market_spread, true_spread, cover_probability, push_probability)

model_raw <- read_csv(model_output_path, show_col_types = FALSE) |>
  janitor::clean_names()

week_zero_start <- as.Date("2026-08-29")
week_zero_end   <- as.Date("2026-08-31")
week_one_start  <- as.Date("2026-09-01")
max_preview_games <- 10

calculate_cfb_week <- function(game_date) {
  case_when(
    game_date >= week_zero_start & game_date <= week_zero_end ~ 0,
    TRUE ~ as.numeric(floor((game_date - week_one_start) / 7) + 1)
  )
}

team_id_lookup <- cfb_crosswalk |>
  select(team_id, btb_team)

model_joined <- model_raw |>
  left_join(
    rename(team_id_lookup, btb_home_name = btb_team),
    by = c("home_team_id" = "team_id")
  ) |>
  left_join(
    rename(team_id_lookup, btb_away_name = btb_team),
    by = c("away_team_id" = "team_id")
  ) |>
  mutate(week = as.numeric(week))

unmatched_home_ids <- model_joined |> filter(is.na(btb_home_name)) |> pull(home_team_id) |> unique()
unmatched_away_ids <- model_joined |> filter(is.na(btb_away_name)) |> pull(away_team_id) |> unique()
if (length(unmatched_home_ids) > 0)
  warning(glue("home_team_id(s) not found in crosswalk: {paste(unmatched_home_ids, collapse = ', ')}"))
if (length(unmatched_away_ids) > 0)
  warning(glue("away_team_id(s) not found in crosswalk: {paste(unmatched_away_ids, collapse = ', ')}"))

if ("cover_probability" %in% names(model_joined)) {
  model_joined <- model_joined |>
    rename(static_cover_probability = cover_probability)
}

model_raw <- model_joined |>
  mutate(
    team     = btb_home_name,
    opponent = btb_away_name,
    total_bin = total_bin_close
  ) |>
  select(-btb_home_name, -btb_away_name)

get_odds_api <- function(cfb_crosswalk = NULL,
                         sport = "americanfootball_ncaaf",
                         apiKey = Sys.getenv("ODDS_API_KEY"),
                         regions = "us",
                         markets = "spreads,totals",
                         year = nflreadr::get_current_season(roster = TRUE),
                         oddsFormat = "american"){

  if (is.null(cfb_crosswalk)) {
    cfb_crosswalk <- read_csv(cfb_crosswalk_path, show_col_types = FALSE)
  }

  url <- glue("https://api.the-odds-api.com/v4/sports/{sport}/odds/?apiKey={apiKey}&regions={regions}&markets={markets}&oddsFormat={oddsFormat}")

  response <- httr::GET(url, httr::add_headers(
    'Accept' = 'application/json',
    'Authorization' = apiKey))

  if (httr::status_code(response) != 200) {
    print(paste0("Request failed with status ", httr::status_code(response)))
  }

  extracted_data <- httr::content(response, "text", encoding = "UTF-8")
  api_data <- jsonlite::fromJSON(extracted_data, flatten = TRUE)

  api_unnested <- api_data |>
    unnest(bookmakers, names_repair = "unique") |>
    unnest(markets, names_repair = "unique") |>
    unnest(outcomes, names_repair = "unique") |>
    janitor::clean_names() |>
    rename(bookmaker_id = key_7, market = key_10, bookmaker = title,
           last_update_api = last_update_9, last_update_markets = last_update_11) |>
    filter(bookmaker_id %in% c("betmgm", "betrivers", "draftkings", "fanduel",
                               "espnbet", "fanatics", "caesars")) |>
    mutate(commence_ny = lubridate::as_date(lubridate::ymd_hms(commence_time, tz = "America/New_York")),
           .after = commence_time) |>
    left_join(select(cfb_crosswalk, btb_team, logo),
              by = c("home_team" = "btb_team")) |>
    rename(home_logo = logo) |>
    left_join(select(cfb_crosswalk, btb_team, logo),
              by = c("away_team" = "btb_team")) |>
    rename(away_logo = logo) |>
    mutate(week = calculate_cfb_week(commence_ny)) |>
    mutate(week = if_else(week == 23, 22, week)) |>
    select(week, commence_time, commence_ny, bookmaker_id,
           bookmaker, last_update_api, last_update_markets, market,
           home_team, away_team, home_logo, away_logo, name, price, point)

  return(api_unnested)
}

api_data <- get_odds_api(cfb_crosswalk = cfb_crosswalk)

required_model_columns <- c("model_prediction_raw", "home_team_id", "away_team_id", "team", "opponent", "week", "total_bin_close")
missing_model_columns <- setdiff(required_model_columns, names(model_raw))

if (length(missing_model_columns) > 0) {
  stop(glue(
    "Model output file is missing required column(s): {paste(missing_model_columns, collapse = ', ')}"
  ))
}

calc_implied_odds <- function(odds) {
  ifelse(odds < 0, abs(odds) / (abs(odds) + 100), 100 / (odds + 100))
}

safe_median <- function(x) {
  if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
}

safe_max_datetime <- function(x) {
  if (all(is.na(x))) as.POSIXct(NA, tz = "America/New_York") else max(x, na.rm = TRUE)
}

api_spreads <- api_data |>
  filter(market == "spreads") |>
  mutate(game = paste0(away_team, "@", home_team)) |>
  filter(game != "NA@UNLV") |>
  left_join(
    select(cfb_crosswalk, btb_team, logo),
    by = c("name" = "btb_team")
  ) |>
  rename(team = name) |>
  select(
    week,
    game,
    commence_time,   # kept
    last_update_api,
    bookmaker,
    team,
    spread = point,
    spread_price = price,
    logo
  )

odds_lookup_joined <- model_raw |>
  select(-opening_spread) |>
  left_join(
    select(
      api_spreads,
      week,
      team,
      game,
      commence_time,  # carried forward
      spread,
      spread_price,
      last_update_api,
      bookmaker,
      logo
    ),
    by = c("week", "team"),
    relationship = "many-to-many"
  ) |>
  filter(!is.na(spread), !is.na(spread_price), !is.na(last_update_api), !is.na(logo)) |>
  mutate(
    median_spread_raw = safe_median(spread),
    .by = c(game, team),
    .after = spread
  ) |>
  mutate(median_spread = round(median_spread_raw * 2) / 2) |>
  mutate(
    true_spread = ((model_prediction_raw * 0.35) + (median_spread * 0.65)),
    .after = spread
  ) |>
  mutate(true_spread = round(true_spread * 2) / 2) |>
  mutate(spread = round(spread * 2) / 2, .after = spread) |>
  mutate(implied_odds_spread = calc_implied_odds(spread_price)) |>
  left_join(
    lookup,
    by = c("total_bin", "spread" = "market_spread", "true_spread"),
    suffix = c("_model", "")
  )

odds_calculated <- odds_lookup_joined |>
  mutate(cover_edge = cover_probability - implied_odds_spread) |>
  mutate(no_cover_edge = (1 - cover_probability) - implied_odds_spread) |>
  group_by(week, game, team) |>
  mutate(
    median_cover_row = row_number(-cover_edge) == ceiling(n() / 2),
    highest_cover_row = row_number(-cover_edge) == 1
  ) |>
  ungroup()

spread_summary <- odds_calculated |>
  summarise(
    last_update_api = safe_max_datetime(last_update_api),
    commence_time = safe_max_datetime(lubridate::ymd_hms(commence_time, quiet = TRUE)),
    model_prediction = true_spread[median_cover_row],
    market_line = spread[median_cover_row],
    market_price = spread_price[median_cover_row],
    median_cover_probability = cover_probability[median_cover_row],
    edge = cover_edge[median_cover_row],
    best_book = bookmaker[highest_cover_row],
    best_line = spread[highest_cover_row],
    best_price = spread_price[highest_cover_row],
    best_cover_probability = cover_probability[highest_cover_row],
    best_edge = cover_edge[highest_cover_row],
    .by = c(week, game, team, logo)
  ) |>
  mutate(
    commence_time = with_tz(commence_time, tzone = "America/New_York"),
    game_date_et = format(commence_time, "%m/%d"),
    game_time_et = format(commence_time, "%I:%M %p"),
    game_time_et = sub("^0", "", game_time_et)
  ) |>
  rename(cover_probability = median_cover_probability)

write_csv(spread_summary, "CFB_Odds/Data/spreads_odds.csv")
