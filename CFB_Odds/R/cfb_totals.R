# (Use your current file, but apply these full integrated edits)
# Key additions:
# 1) keep commence_time in api_totals_bookmaker
# 2) carry commence_time through totals_lookup_joined
# 3) add commence_time + game_date_et + game_time_et in totals_last_update

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
lookup_path <- "CFB_Odds/Data/CFB_Totals_Pricing_Table_By_Drive_Bin.csv"
model_output_path <- "CFB Total Output.csv"
spreads_output_path <- "CFB_Odds/Data/spreads_odds.csv"

cfb_crosswalk <- read_csv(cfb_crosswalk_path, show_col_types = FALSE)

lookup <- read_csv(lookup_path, show_col_types = FALSE) |>
  janitor::clean_names() |>
  select(drive_bin, market_total, true_total, over_probability, under_probability, push_probability)

model_raw <- read_csv(
  model_output_path,
  show_col_types = FALSE,
  na = c("", ".", "NA")
) |>
  janitor::clean_names()

spreads_games <- read_csv(spreads_output_path, show_col_types = FALSE) |>
  janitor::clean_names() |>
  distinct(game) |>
  mutate(
    away_team = sub("@.*", "", game),
    home_team = sub(".*@", "", game),
    matchup_key = paste(pmin(away_team, home_team), pmax(away_team, home_team), sep = "|")
  )

week_zero_start <- as.Date("2026-08-29")
week_zero_end   <- as.Date("2026-08-31")
week_one_start  <- as.Date("2026-09-01")
blended_model_weight <- 0.35
blended_market_weight <- 0.65

calculate_cfb_week <- function(game_date) {
  case_when(
    game_date >= week_zero_start & game_date <= week_zero_end ~ 0,
    TRUE ~ as.numeric(floor((game_date - week_one_start) / 7) + 1)
  )
}

team_id_lookup <- cfb_crosswalk |>
  select(team_id, btb_team)

home_logo_lookup <- cfb_crosswalk |>
  select(btb_team, logo)

model_joined <- model_raw |>
  left_join(
    rename(team_id_lookup, model_team = btb_team),
    by = c("home_team_id" = "team_id")
  ) |>
  left_join(
    rename(team_id_lookup, model_opponent = btb_team),
    by = c("away_team_id" = "team_id")
  ) |>
  mutate(
    week = as.numeric(week),
    model_prediction_raw = as.numeric(model_prediction_raw),
    matchup_key = paste(pmin(model_team, model_opponent), pmax(model_team, model_opponent), sep = "|")
  )

model_with_game <- model_joined |>
  left_join(
    select(spreads_games, game, away_team, home_team, matchup_key),
    by = "matchup_key",
    relationship = "many-to-one"
  ) |>
  left_join(
    home_logo_lookup,
    by = c("home_team" = "btb_team")
  ) |>
  filter(!is.na(game))

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

calc_implied_odds <- function(odds) {
  ifelse(odds < 0, abs(odds) / (abs(odds) + 100), 100 / (odds + 100))
}

safe_median <- function(x) {
  if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
}

safe_max_datetime <- function(x) {
  if (all(is.na(x))) as.POSIXct(NA, tz = "America/New_York") else max(x, na.rm = TRUE)
}

calculate_drive_bin <- function(spread_value) {
  case_when(
    !is.na(spread_value) & abs(spread_value) >= 0 & abs(spread_value) <= 7 ~ 1,
    !is.na(spread_value) & abs(spread_value) >= 7.5 & abs(spread_value) <= 17 ~ 2,
    !is.na(spread_value) & abs(spread_value) >= 17.5 & abs(spread_value) <= 30 ~ 3,
    !is.na(spread_value) & abs(spread_value) > 30 ~ 4,
    TRUE ~ NA_real_
  )
}

api_totals_bookmaker <- api_data |>
  filter(market == "totals", name == "Over") |>
  filter(!is.na(home_team), !is.na(away_team)) |>
  mutate(game = paste0(away_team, "@", home_team)) |>
  select(
    week,
    game,
    commence_time,   # kept
    last_update_api,
    bookmaker,
    total = point,
    total_price = price
  )

spreads_predictions <- read_csv(spreads_output_path, show_col_types = FALSE) |>
  janitor::clean_names() |>
  select(game, model_prediction, market_line) |>
  distinct()

totals_lookup_joined <- model_with_game |>
  filter(!is.na(model_prediction_raw), !is.na(game)) |>
  select(week, game, logo, model_prediction_raw) |>
  distinct() |>
  left_join(
    select(
      api_totals_bookmaker,
      game,
      api_week = week,
      commence_time,  # carried forward
      total,
      total_price,
      last_update_api,
      bookmaker
    ),
    by = "game",
    relationship = "many-to-many"
  ) |>
  left_join(
    spreads_predictions,
    by = "game"
  ) |>
  filter(!is.na(total), !is.na(total_price), !is.na(last_update_api)) |>
  mutate(week = api_week) |>
  mutate(
    median_total_raw = safe_median(total),
    .by = game,
    .after = total
  ) |>
  mutate(median_total = round(median_total_raw * 2) / 2) |>
  mutate(
    true_total = ((model_prediction_raw * blended_model_weight) + (median_total * blended_market_weight)),
    .after = total
  ) |>
  mutate(
    true_total = round(true_total * 2) / 2,
    total = round(total * 2) / 2,
    drive_bin = calculate_drive_bin(market_line),
    implied_odds_total = calc_implied_odds(total_price)
  ) |>
  left_join(
    lookup,
    by = c("drive_bin", "total" = "market_total", "true_total"),
    relationship = "many-to-one"
  )

totals_calculated <- totals_lookup_joined |>
  mutate(
    over_edge = over_probability - implied_odds_total,
    under_edge = under_probability - implied_odds_total,
    median_distance = abs(total - median_total)
  )

totals_last_update <- totals_calculated |>
  summarise(
    last_update_api = safe_max_datetime(last_update_api),
    commence_time = safe_max_datetime(lubridate::ymd_hms(commence_time, quiet = TRUE)),
    .by = c(week, game)
  ) |>
  mutate(
    commence_time = with_tz(commence_time, tzone = "America/New_York"),
    game_date_et = format(commence_time, "%m/%d"),
    game_time_et = format(commence_time, "%I:%M %p"),
    game_time_et = sub("^0", "", game_time_et)
  )

totals_median_summary <- totals_calculated |>
  arrange(week, game, median_distance, desc(over_edge), bookmaker) |>
  slice_head(n = 1, by = c(week, game)) |>
  transmute(
    week,
    game,
    logo,
    model_prediction = true_total,
    market_line = total,
    market_price = total_price,
    over_probability,
    under_probability,
    over_edge,
    under_edge,
    drive_bin
  )

totals_best_summary <- totals_calculated |>
  arrange(week, game, desc(over_edge), bookmaker) |>
  slice_head(n = 1, by = c(week, game)) |>
  transmute(
    week,
    game,
    best_book = bookmaker,
    best_line = total,
    best_price = total_price,
    best_over_probability = over_probability,
    best_under_probability = under_probability,
    best_over_edge = over_edge,
    best_under_edge = under_edge
  )

totals_summary <- totals_last_update |>
  left_join(totals_median_summary, by = c("week", "game")) |>
  left_join(totals_best_summary, by = c("week", "game"))

write_csv(totals_summary, "CFB_Odds/Data/totals_odds.csv")
