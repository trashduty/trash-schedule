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

get_odds_api <- function(sport = "americanfootball_ncaaf", 
                         apiKey = Sys.getenv("ODDS_API_KEY"), 
                         regions = "us", 
                         markets = "spreads,totals", 
                         year = nflreadr::get_current_season(roster = TRUE), 
                         oddsFormat = "american"){
  
  # Betting data doesn't have the `week` column necessary for CFB data, so 
  # we have to create it. Essentially, I'm pulling the first game of the season
  # here, and regressing that to Wednesday of that week. Later, I will use that 
  # to calculate the CFB week by using a Wednesday to following Tuesday CFB week.
  
  week_one_wednesday <- cfbfastR::espn_cfb_schedule(year = year) |> 
    filter(type == "regular") |> 
    mutate(game_date = as_date(with_tz(ymd_hm(game_date), "America/New_York"))) |>
    summarise(first_game_date = min(game_date)) |>
    mutate(
      first_game_weekday = wday(first_game_date, week_start = 1),  # Monday = 1, Sunday = 7
      # Find the Wednesday before (or on) the first game
      # Wednesday = 3 in this system
      days_back_to_wednesday = case_when(
        first_game_weekday >= 3 ~ first_game_weekday - 3,  # If Thu-Sun, go back to Wed
        first_game_weekday < 3 ~ first_game_weekday + 4    # If Mon-Tue, go back to previous Wed
      ),
      week_one_wednesday = first_game_date - days(days_back_to_wednesday)
    ) |>
    pull(week_one_wednesday)
  
  
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
  
  cfb_crosswalk <- read_csv("CFB Teams Full Crosswalk.csv")
  
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
    left_join(select(cfb_crosswalk, btb_team_short, api_team, logo), 
              by = c("home_team" = "api_team")) |> 
    rename(home_name = btb_team_short, home_logo = logo) |> 
    left_join(select(cfb_crosswalk, btb_team_short, api_team, logo), 
              by = c("away_team" = "api_team")) |> 
    rename(away_name = btb_team_short, away_logo = logo) |> 
    mutate(week = case_when(
      commence_ny < week_one_wednesday ~ 0,  # Pre-season or invalid
      TRUE ~ as.numeric(floor((commence_ny - week_one_wednesday) / 7) + 1)
    )) |> 
    mutate(week = if_else(week == 23, 22, week)) |> 
    select(week, commence_time, commence_ny, bookmaker_id, 
           bookmaker, last_update_api, last_update_markets, market, 
           home_team, away_team, home_name, home_logo, away_name, away_logo, name, price, point)
  
  return(api_unnested)
}

api_data <- get_odds_api()

lookup <- read_csv("Expanded_CFB_Spread_Pricing_Table_Binned.csv",
                   show_col_types = FALSE) |>
  janitor::clean_names()

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

model_raw <- read_csv("cfb model output_new.csv",
                      show_col_types = FALSE) |>
  janitor::clean_names() 

WEEK <- max(model_raw$week)

# margin <- read_csv("https://github.com/trashduty/trash-schedule/raw/refs/heads/main/NFL_Odds/Data/interpolated_2d_margin_probs.csv",
#                    show_col_types = FALSE) |>
#   janitor::clean_names()
# 
# lookup <- read_csv("https://github.com/trashduty/trash-schedule/raw/refs/heads/main/NFL_Odds/Data/NFL_Totals_Lookup_Stratified_By_Spread.csv",
#                    show_col_types = FALSE) |>
#   janitor::clean_names()
# 
# model_raw <- read_csv("https://github.com/trashduty/trash-schedule/raw/refs/heads/main/Week%201%20model%20pred_updated.csv",
#                       show_col_types = FALSE) |>
#   janitor::clean_names()

calc_implied_odds <- function(odds) {
  ifelse(odds < 0,
         abs(odds) / (abs(odds) + 100),  # Negative odds (favorites)
         100 / (odds + 100))             # Positive odds (underdogs)
}

api_spreads <- api_data |> 
  filter(market == "spreads", week == WEEK) |>
  mutate(game = paste0(away_name, "@", home_name)) |> 
  filter(game != "NA@UNLV") |> 
  left_join(select(cfb_crosswalk, btb_team_short, api_team, logo), 
            by = c("name" = "api_team")) |> 
  rename(team = btb_team_short) |> 
  select(week, game, last_update_api, bookmaker, team, game,
         spread = point, spread_price = price, logo)

api_totals <- api_data |> 
  filter(market == "totals", name == "Over", week == WEEK) |> 
  mutate(game = paste0(away_name, "@", home_name)) |> 
  select(week, game, bookmaker, name, total_price = price, total = point)

odds_calculated <- model_raw |> 
  select(-opening_spread) |> 
  left_join(select(api_spreads, week, team, game, spread, spread_price,
                   last_update_api, bookmaker, logo), 
            by = c("week", "team")) |> 
  filter(!is.na(game)) |> 
  mutate(median_spread = median(spread, na.rm = TRUE), 
         .by = c(game, team), .after = spread) |> 
  mutate(true_spread = ((model_prediction_raw * 0.3) + (median_spread * 0.7)),  
         .after = spread) |> 
  mutate(true_spread = round(true_spread * 2) / 2) |> 
  mutate(spread = round(spread * 2) / 2, .after = spread) |> 
  mutate(implied_odds_spread = calc_implied_odds(spread_price)) |> 
  left_join(api_totals, by = c("week", "game", "bookmaker"), 
            relationship = "many-to-many") |> 
  mutate(median_total = median(total, na.rm = TRUE), 
         .by = game) |> 
  mutate(total_bin = case_when(
    abs(spread) <= 50 ~ 1, 
    abs(spread) <= 59.6 ~ 2, 
    abs(spread) >= 60 ~ 3
  )) |> 
  left_join(lookup, 
            by = c("total_bin", 
                   "median_spread" = "market_spread", 
                   "true_spread")) |> 
  mutate(implied_odds_spread = calc_implied_odds(spread_price)) |> 
  mutate(cover_edge = cover_probability - implied_odds_spread) |>
  mutate(no_cover_edge = no_cover_probability - implied_odds_spread) |>
  group_by(week, game, team) |> 
  mutate(
    median_cover_row = row_number(-cover_edge) == ceiling(n()/2),
    highest_cover_row = row_number(-cover_edge) == 1, 
    median_no_cover_row = row_number(-no_cover_edge) == ceiling(n()/2),
    highest_no_cover_row = row_number(-no_cover_edge) == 1
  ) |> 
  ungroup()

  spread_summary <- odds_calculated |> 
    summarise(
      last_update_api = max(last_update_api, na.rm = TRUE), 
      model_prediction = true_spread[median_cover_row],
      market_line = spread[median_cover_row],
      market_price = spread_price[median_cover_row], 
      cover_probability = cover_probability[median_cover_row],
      edge = cover_edge[median_cover_row],
      best_book = bookmaker[highest_cover_row],
      best_line = spread[highest_cover_row],
      best_price = spread_price[highest_cover_row], 
      best_cover_probability = cover_probability[highest_cover_row], 
      best_edge = cover_edge[highest_cover_row],
      .by = c(week, game, team, logo)
    )


write_csv(spread_summary, "CFB_Odds/Data/spreads_odds.csv")

