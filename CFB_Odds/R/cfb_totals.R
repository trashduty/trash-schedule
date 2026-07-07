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
  select(drive_bin, market_total, true_total, over_probability, push_probability)

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
max_preview_games <- 10
model_weight <- 0.35
market_weight <- 0.65

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

required_model_columns <- c("model_prediction", "home_team_id", "away_team_id", "team", "opponent", "week", "over_under")
missing_model_columns <- setdiff(required_model_columns, names(model_raw))

if (length(missing_model_columns) > 0) {
  stop(glue::glue(
    "Model output file is missing required column(s): {paste(missing_model_columns, collapse = ', ')}"
  ))
}

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
    model_prediction = as.numeric(model_prediction),
    over_under = as.numeric(over_under),
    matchup_key = paste(pmin(model_team, model_opponent), pmax(model_team, model_opponent), sep = "|")
  )

unmatched_home_ids <- model_joined |> filter(is.na(model_team)) |> pull(home_team_id) |> unique()
unmatched_away_ids <- model_joined |> filter(is.na(model_opponent)) |> pull(away_team_id) |> unique()
if (length(unmatched_home_ids) > 0)
  warning(glue::glue("home_team_id(s) not found in crosswalk: {paste(unmatched_home_ids, collapse = ', ')}"))
if (length(unmatched_away_ids) > 0)
  warning(glue::glue("away_team_id(s) not found in crosswalk: {paste(unmatched_away_ids, collapse = ', ')}"))

model_with_game <- model_joined |>
  left_join(
    select(spreads_games, game, away_team, home_team, matchup_key),
    by = "matchup_key",
    relationship = "many-to-one"
  ) |>
  left_join(
    home_logo_lookup,
    by = c("home_team" = "btb_team")
  )

missing_game_rows <- model_with_game |>
  filter(is.na(game)) |>
  distinct(team, opponent, model_team, model_opponent)

if (nrow(missing_game_rows) > 0) {
  overflow_note <- if_else(
    nrow(missing_game_rows) > max_preview_games,
    glue::glue(" (showing first {max_preview_games} of {nrow(missing_game_rows)})"),
    ""
  )
  preview_games <- missing_game_rows |>
    mutate(
      game_key = glue::glue(
        "{team} vs {opponent} [matched names {model_team} / {model_opponent}]"
      )
    ) |>
    pull(game_key)
  stop(glue::glue(
    "Unable to match totals games to spreads_odds.csv for {nrow(missing_game_rows)} row(s){overflow_note}: ",
    "{paste(head(preview_games, max_preview_games), collapse = '; ')}"
  ))
}

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

  url <- glue::glue("https://api.the-odds-api.com/v4/sports/{sport}/odds/?apiKey={apiKey}&regions={regions}&markets={markets}&oddsFormat={oddsFormat}")

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

if (!exists("api_data")) {
  api_data <- get_odds_api(cfb_crosswalk = cfb_crosswalk)
}

calc_implied_odds <- function(odds) {
  ifelse(odds < 0,
         abs(odds) / (abs(odds) + 100),
         100 / (odds + 100))
}

safe_median <- function(x) {
  if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
}

safe_max_datetime <- function(x) {
  if (all(is.na(x))) as.POSIXct(NA, tz = "America/New_York") else max(x, na.rm = TRUE)
}

calculate_drive_bin <- function(total_value) {
  case_when(
    !is.na(total_value) & total_value >= 0 & total_value <= 7 ~ 1,
    !is.na(total_value) & total_value >= 7.5 & total_value <= 17 ~ 2,
    !is.na(total_value) & total_value >= 17.5 & total_value <= 30 ~ 3,
    !is.na(total_value) & total_value > 30 ~ 4,
    TRUE ~ NA_real_
  )
}

api_totals <- api_data |>
  filter(market == "totals", name == "Over") |>
  filter(!is.na(home_team), !is.na(away_team)) |>
  mutate(game = paste0(away_team, "@", home_team)) |>
  select(
    week,
    game,
    last_update_api,
    bookmaker,
    over_under = point,
    market_price = price
  )

missing_total_keys <- model_with_game |>
  filter(!is.na(model_prediction)) |>
  distinct(game) |>
  anti_join(
    api_totals |> distinct(game),
    by = "game"
  )

if (nrow(missing_total_keys) > 0) {
  overflow_note <- if_else(
    nrow(missing_total_keys) > max_preview_games,
    glue::glue(" (showing first {max_preview_games} of {nrow(missing_total_keys)})"),
    ""
  )
  preview_games <- paste0(
    paste(head(missing_total_keys$game, max_preview_games), collapse = ", "),
    overflow_note
  )
  warning(glue::glue(
    "Model games missing API totals match: {nrow(missing_total_keys)} row(s): {preview_games}"
  ))
}

totals_lookup_joined <- model_with_game |>
  filter(!is.na(model_prediction), !is.na(game)) |>
  left_join(
    select(
      api_totals,
      game,
      api_week = week,
      over_under,
      market_price,
      last_update_api,
      bookmaker
    ),
    by = "game",
    relationship = "many-to-many"
  ) |>
  filter(!is.na(over_under), !is.na(market_price), !is.na(last_update_api)) |>
  mutate(week = api_week) |>
  mutate(
    median_total_raw = safe_median(over_under),
    .by = game,
    .after = over_under
  ) |>
  mutate(median_total = round(median_total_raw * 2) / 2) |>
  mutate(
    true_total = ((model_prediction * model_weight) + (median_total * market_weight)),
    .after = over_under
  ) |>
  mutate(
    true_total = round(true_total * 2) / 2,
    over_under = round(over_under * 2) / 2,
    drive_bin = calculate_drive_bin(true_total),
    implied_odds_total = calc_implied_odds(market_price)
  ) |>
  left_join(
    lookup,
    by = c("drive_bin", "over_under" = "market_total", "true_total"),
    relationship = "many-to-one"
  )

missing_lookup_rows <- totals_lookup_joined |>
  filter(is.na(over_probability) | is.na(push_probability)) |>
  distinct(week, game, drive_bin, over_under, true_total)

if (nrow(missing_lookup_rows) > 0) {
  overflow_note <- if_else(
    nrow(missing_lookup_rows) > max_preview_games,
    glue::glue(" (showing first {max_preview_games} of {nrow(missing_lookup_rows)})"),
    ""
  )
  preview_rows <- missing_lookup_rows |>
    mutate(
      lookup_key = glue::glue(
        "{game} drive_bin={drive_bin}, market_total={over_under}, true_total={true_total}"
      )
    ) |>
    pull(lookup_key)
  stop(glue::glue(
    "Totals pricing lookup failed for {nrow(missing_lookup_rows)} row(s){overflow_note}: ",
    "{paste(head(preview_rows, max_preview_games), collapse = '; ')}"
  ))
}

totals_calculated <- totals_lookup_joined |>
  mutate(edge = over_probability - implied_odds_total) |>
  group_by(week, game) |>
  mutate(
    median_over_row = row_number(-edge) == ceiling(n() / 2),
    highest_over_row = row_number(-edge) == 1
  ) |>
  ungroup()

totals_summary <- totals_calculated |>
  summarise(
    logo = logo[median_over_row],
    last_update_api = safe_max_datetime(last_update_api),
    model_prediction = true_total[median_over_row],
    market_line = over_under[median_over_row],
    market_price = market_price[median_over_row],
    median_over_probability = over_probability[median_over_row],
    edge = edge[median_over_row],
    best_book = bookmaker[highest_over_row],
    best_line = over_under[highest_over_row],
    best_price = market_price[highest_over_row],
    best_over_probability = over_probability[highest_over_row],
    best_edge = edge[highest_over_row],
    .by = c(week, game)
  ) |>
  rename(over_probability = median_over_probability)

write_csv(totals_summary, "CFB_Odds/Data/totals_odds.csv")
