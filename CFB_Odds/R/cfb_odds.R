
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
  select(total_bin, market_spread, true_spread, push_probability)

model_raw <- read_csv(model_output_path, show_col_types = FALSE) |>
  janitor::clean_names()

week_zero_start <- as.Date("2026-08-29")  # Week 0: Sat 8/29 - Mon 8/31
week_zero_end   <- as.Date("2026-08-31")
week_one_start  <- as.Date("2026-09-01")  # Week 1 begins Tuesday 9/1
max_preview_games <- 10

#' Convert game date to CFB week index
#'
#' @param game_date Date vector in America/New_York game-date context.
#' @return Numeric week index: 0 for week 0 (Sat 8/29 - Mon 8/31), 1 for week 1
#'   (Tue 9/1 - Mon 9/7), 2 for week 2 (Tue 9/8 - Mon 9/14), and so on.
calculate_cfb_week <- function(game_date) {
  case_when(
    game_date >= week_zero_start & game_date <= week_zero_end ~ 0,  # Week 0: Sat-Mon 8/29-8/31
    TRUE ~ as.numeric(floor((game_date - week_one_start) / 7) + 1)  # Week 1+: starts Tuesday 9/1
  )
}

team_id_lookup <- cfb_crosswalk |>
  select(team_id, btb_team)

model_raw <- model_raw |>
  left_join(
    rename(team_id_lookup, btb_home_name = btb_team),
    by = c("home_team_id" = "team_id")
  ) |>
  left_join(
    rename(team_id_lookup, btb_away_name = btb_team),
    by = c("away_team_id" = "team_id")
  ) |>
  mutate(
    team = btb_home_name,
    opponent = btb_away_name,
    # Model CSV exports can type week as character (e.g., with BOM/text parsing); normalize for numeric joins.
    week = as.numeric(week),
    # Keep game key format aligned with API: away@home.
    game = paste0(opponent, "@", team)
  ) |>
  select(-btb_home_name, -btb_away_name)

unmatched_home_ids <- model_raw |> filter(is.na(team)) |> pull(home_team_id) |> unique()
unmatched_away_ids <- model_raw |> filter(is.na(opponent)) |> pull(away_team_id) |> unique()
if (length(unmatched_home_ids) > 0)
  warning(glue::glue("home_team_id(s) not found in crosswalk: {paste(unmatched_home_ids, collapse = ', ')}"))
if (length(unmatched_away_ids) > 0)
  warning(glue::glue("away_team_id(s) not found in crosswalk: {paste(unmatched_away_ids, collapse = ', ')}"))

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
  
  # Betting data doesn't have the `week` column necessary for CFB data, so 
  # we have to create it. Essentially, I'm pulling the first game of the season
  # here, and regressing that to Wednesday of that week. Later, I will use that 
  # to calculate the CFB week by using a Wednesday to following Tuesday CFB week.
  
  
  # week_one_wednesday <- cfbfastR::espn_cfb_schedule(year = 2025) |> 
  #   filter(type == "regular") |> 
  #   summarise(first_game_date = min(game_date)) |>
  #   mutate(
  #     first_game_weekday = wday(first_game_date, week_start = 1),  # Monday = 1, Sunday = 7
  #     # Find the Wednesday before (or on) the first game
  #     # Wednesday = 3 in this system
  #     days_back_to_wednesday = case_when(
  #       first_game_weekday >= 3 ~ first_game_weekday - 3,  # If Thu-Sun, go back to Wed
  #       first_game_weekday < 3 ~ first_game_weekday + 4    # If Mon-Tue, go back to previous Wed
  #     ),
  #     week_one_wednesday = first_game_date - days(days_back_to_wednesday)
  #   ) |>
  #   pull(week_one_wednesday)
  
  # URL
  url <- glue::glue("https://api.the-odds-api.com/v4/sports/{sport}/odds/?apiKey={apiKey}&regions={regions}&markets={markets}&oddsFormat={oddsFormat}")
  
  response <- httr::GET(url, httr::add_headers(
    'Accept' = 'application/json',
    'Authorization' = apiKey))
  
  # check if the request was successful
  if (httr::status_code(response) != 200) {
    print(paste0("Request failed with status ", httr::status_code(response)))
  }
  
  # parsing and converting JSON into a data frame
  extracted_data <- httr::content(response, "text", encoding = "UTF-8")
  api_data <- jsonlite::fromJSON(extracted_data, flatten = TRUE)
  
  clean_team_names <- function(names) {
    names %>%
      # Remove various apostrophe types
      str_replace_all("['']", "") %>%
      # Convert accented characters to ASCII equivalents
      iconv(to = "ASCII//TRANSLIT") %>%
      # Remove any remaining non-alphanumeric characters except spaces and hyphens
      str_replace_all("[^A-Za-z0-9 -]", "") %>%
      # Clean up extra spaces
      str_squish()
  }
  
  # Unnest the data, which has multiple nested list columns
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
    left_join(select(cfb_crosswalk, btb_team, api_team, logo), 
              by = c("home_team" = "api_team")) |> 
    rename(home_name = btb_team, home_logo = logo) |> 
    left_join(select(cfb_crosswalk, btb_team, api_team, logo), 
              by = c("away_team" = "api_team")) |> 
    rename(away_name = btb_team, away_logo = logo) |> 
    mutate(week = calculate_cfb_week(commence_ny)) |> 
    mutate(week = if_else(week == 23, 22, week)) |> 
    select(week, commence_time, commence_ny, bookmaker_id, 
           bookmaker, last_update_api, last_update_markets, market, 
           home_team, away_team, home_name, home_logo, away_name, away_logo, name, price, point)
  
  return(api_unnested)
}

api_data <- get_odds_api(cfb_crosswalk = cfb_crosswalk)

required_model_columns <- c("model_prediction_raw", "home_team_id", "away_team_id", "team", "opponent", "week", "cover_probability")
missing_model_columns <- setdiff(required_model_columns, names(model_raw))

if (length(missing_model_columns) > 0) {
  stop(glue::glue(
    "Model output file is missing required column(s): {paste(missing_model_columns, collapse = ', ')}"
  ))
}

WEEK <- max(model_raw$week)

calc_implied_odds <- function(odds) {
  ifelse(odds < 0,
         abs(odds) / (abs(odds) + 100),  # Negative odds (favorites)
         100 / (odds + 100))             # Positive odds (underdogs)
}

#' Median with all-NA safety
#'
#' @param x Numeric vector.
#' @return NA_real_ when all values are NA, otherwise median(x, na.rm = TRUE).
safe_median <- function(x) {
  if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
}

#' Datetime max with all-NA safety
#'
#' @param x Datetime vector.
#' @return POSIXct NA in America/New_York when all values are NA, otherwise
#'   max(x, na.rm = TRUE).
safe_max_datetime <- function(x) {
  if (all(is.na(x))) as.POSIXct(NA, tz = "America/New_York") else max(x, na.rm = TRUE)
}

api_spreads <- api_data |>
  # filter(market == "spreads", week == WEEK) |>
  filter(market == "spreads") |>
  mutate(game = paste0(away_name, "@", home_name)) |>
  filter(game != "NA@UNLV") |>
  left_join(
    select(cfb_crosswalk, btb_team, api_team, logo),
    by = c("name" = "api_team")
  ) |>
  rename(team = btb_team) |>
  select(
    week,
    game,
    last_update_api,
    bookmaker,
    team,
    spread = point,
    spread_price = price,
    logo
  )

api_totals <- api_data |>
  # filter(market == "totals", name == "Over", week == WEEK) |>
  filter(market == "totals", name == "Over") |>
  mutate(game = paste0(away_name, "@", home_name)) |>
  summarize(median_total = median(point, na.rm = TRUE), .by = c(week, game))

missing_spread_keys <- model_raw |>
  distinct(week, team, game) |>
  anti_join(
    api_spreads |> distinct(week, team, game),
    by = c("week", "team", "game")
  )

if (nrow(missing_spread_keys) > 0) {
  overflow_note <- if_else(
    nrow(missing_spread_keys) > max_preview_games,
    glue::glue(" (showing first {max_preview_games} of {nrow(missing_spread_keys)})"),
    ""
  )
  preview_games <- paste0(
    paste(head(missing_spread_keys$game, max_preview_games), collapse = ", "),
    overflow_note
  )
  warning(glue::glue(
    "Model games missing API spread match (week+team+game): {nrow(missing_spread_keys)} row(s): {preview_games}"
  ))
}

odds_calculated <- model_raw |>
  select(-opening_spread) |>
  # Each model team can map to multiple sportsbook spread rows for the same week/team.
  left_join(
    select(
      api_spreads,
      week,
      team,
      game,
      spread,
      spread_price,
      last_update_api,
      bookmaker,
      logo
    ),
    by = c("week", "team", "game"),
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
  left_join(select(api_totals, -week), by = c("game")) |>
  mutate(
    total_bin = case_when(
      abs(median_total) <= 50 ~ 1,
      abs(median_total) <= 59.6 ~ 2,
      abs(median_total) >= 60 ~ 3
    )
  ) |>
  left_join(
    lookup,
    by = c("total_bin", "spread" = "market_spread", "true_spread")
  ) |>
  mutate(implied_odds_spread = calc_implied_odds(spread_price)) |>
  mutate(cover_edge = cover_probability - implied_odds_spread) |>
  mutate(no_cover_edge = (1 - cover_probability) - implied_odds_spread) |>
  group_by(week, game, team) |>
  mutate(
    median_cover_row = row_number(-cover_edge) == ceiling(n() / 2),
    highest_cover_row = row_number(-cover_edge) == 1,
    median_no_cover_row = row_number(-no_cover_edge) == ceiling(n() / 2),
    highest_no_cover_row = row_number(-no_cover_edge) == 1
  ) |>
  ungroup() |> 
  # Extract home and away teams
  mutate(
    away_temp = sub("@.*", "", game),
    home_temp = sub(".*@", "", game)
  ) %>%
  # Identify which team name is valid (not "NA")
  mutate(
    valid_team = case_when(
      away_temp == "NA" & home_temp != "NA" ~ home_temp,
      home_temp == "NA" & away_temp != "NA" ~ away_temp,
      TRUE ~ NA_character_  # both valid or both NA
    )
  ) %>%
  # Determine home and away from team/opponent columns
  mutate(
    home_team = case_when(
      !is.na(valid_team) & team == valid_team ~ opponent,
      !is.na(valid_team) & opponent == valid_team ~ team,
      TRUE ~ home_temp  # no fix needed
    ),
    away_team = case_when(
      !is.na(valid_team) & team == valid_team ~ team,
      !is.na(valid_team) & opponent == valid_team ~ opponent,
      TRUE ~ away_temp  # no fix needed
    )
  ) %>%
  # Rebuild the game column
  mutate(
    game = paste0(away_team, "@", home_team)
  ) %>%
  # Clean up temporary columns
  select(-away_temp, -home_temp, -valid_team) 
  

spread_summary <- odds_calculated |>
  summarise(
    last_update_api = safe_max_datetime(last_update_api),
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
  rename(cover_probability = median_cover_probability)


write_csv(spread_summary, "CFB_Odds/Data/spreads_odds.csv")
# write_csv(spread_summary, "spreads_odds.csv")
