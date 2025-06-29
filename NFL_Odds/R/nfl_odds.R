library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(nflreadr)
library(httr)
library(jsonlite)
library(glue)
library(lubridate)

get_odds_api <- function(sport = "americanfootball_nfl", 
                         apiKey = Sys.getenv("ODDS_API_KEY"), 
                         regions = "us", 
                         markets = "spreads,totals", 
                         year = nflreadr::get_current_season(roster = TRUE), 
                         oddsFormat = "american"){
  
  # Betting data doesn't have the `week` column necessary for NFL data, so 
  # we have to create it. Essentially, I'm pulling the first game of the season
  # here, and regressing that to wednesday of that week. Later, I will use that 
  # to calculate the NFL week by using a Wednesday to following Tuesday NFL week. 
  week_one_wednesday <- nflreadr::load_schedules(seasons = year) |> 
    filter(game_type == "REG") |> 
    mutate(gameday = as_date(gameday)) |>
    summarise(first_game_date = min(gameday)) |>
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
  
  # Unnest the data, which has multiple nested list columns
  api_unnested <- api_data |> 
    unnest(bookmakers, names_repair = "unique") |>
    unnest(markets, names_repair = "unique") |>
    unnest(outcomes, names_repair = "unique") |> 
    janitor::clean_names() |> 
    rename(bookmaker_id = key_7, market = key_10, bookmaker = title, 
           last_update_api = last_update_9, last_update_markets = last_update_11) |> 
    mutate(commence_ny = lubridate::as_date(lubridate::ymd_hms(commence_time, tz = "America/New_York")), 
           .after = commence_time) |> 
    mutate(home_abbr = nflreadr::clean_team_abbrs(home_team)) |>     
    mutate(away_abbr = nflreadr::clean_team_abbrs(away_team)) |> 
    mutate(week = case_when(
      commence_ny < week_one_wednesday ~ 0,  # Pre-season or invalid
      TRUE ~ as.numeric(floor((commence_ny - week_one_wednesday) / 7) + 1)
    )) |> 
    mutate(week = if_else(week == 23, 22, week)) |> 
    select(week, commence_time, commence_ny, bookmaker_id, 
           bookmaker, last_update_api, last_update_markets, market, 
           home_team, home_abbr, away_team, away_abbr, name, price, point)
  
  api_spreads_median <- api_unnested |> 
    filter(market == "spreads") |> 
    mutate(team = nflreadr::clean_team_abbrs(name)) |> 
    summarize(last_update_api = max(last_update_api, na.rm = TRUE), 
              median_spread = median(point), 
              median_spread_price = median(price), 
              .by = c(week, home_abbr, away_abbr, team)) 
  
  api_totals_median <- api_unnested |> 
    filter(market == "totals", name == "Over") |> 
    summarize(median_total = median(point), 
              median_total_price = median(price), 
              .by = c(week, home_abbr, away_abbr)) |> 
    mutate(home = home_abbr) |> 
    mutate(away = away_abbr) |>
    pivot_longer(
      cols = c(home, away),
      names_to = "location",
      values_to = "team"
    ) |>
    select(week, home_abbr, away_abbr, location, team, median_total, median_total_price)
  
  api_odds <- api_spreads_median |> 
    left_join(api_totals_median, by = c("week", "home_abbr", "away_abbr", "team")) |> 
    select(week, home_abbr, away_abbr, location, team, median_spread, 
           median_spread_price, median_total, median_total_price, last_update_api) |> 
    mutate(game = paste0(away_abbr, "@", home_abbr))
  
  
  return(api_odds)
  # write_csv(api_odds, "nfl_api_spreads_totals.csv")
  
}

update_nfl_odds <- function(){
  
  api_odds <- get_odds_api()
  
  margin <- read_csv("NFL_Odds/Data/interpolated_2d_margin_probs.csv", 
                     show_col_types = FALSE) |> 
    janitor::clean_names()
  
  lookup <- read_csv("NFL_Odds/Data/NFL_Totals_Lookup_Stratified_By_Spread.csv", 
                     show_col_types = FALSE) |> 
    janitor::clean_names()
  
  model_raw <- read_csv("Week 1 model pred_updated.csv", 
                        show_col_types = FALSE) |> 
    janitor::clean_names()
  
  # implied_odds <- 0.523805237
  
  calc_implied_odds <- function(odds) {
    ifelse(odds < 0,
           abs(odds) / (abs(odds) + 100),  # Negative odds (favorites)
           100 / (odds + 100))             # Positive odds (underdogs)
  }
  
  nfl_odds <- model_raw |> 
    left_join(select(api_odds, week, team, game, median_spread, median_spread_price,
                     median_total, median_total_price, last_update_api), 
              by = c("week", "home_team" = "team")) |> 
    mutate(true_spread = ((model_prediction * 0.35) + (median_spread * 0.65)),  
           .after = median_spread) |> 
    mutate(true_spread = round(true_spread * 2) / 2) |> 
    mutate(median_spread = round(median_spread * 2) / 2) |> 
    left_join(margin, by = 
                c("median_spread" = "market_line", "true_spread" = "true_line")) |> 
    rename(spread_cover_probability = cover_probability, 
           spread_push_probability = push_probability, 
           spread_loss_probability = loss_probability
    ) |> 
    mutate(implied_odds_spread = calc_implied_odds(median_spread_price)) |> 
    mutate(spread_edge = spread_cover_probability - implied_odds_spread) |> 
    mutate(bin_cat = case_when(
      abs(median_spread) <= 3 ~ 1, 
      abs(median_spread) <= 7 ~ 2, 
      abs(median_spread) > 7 ~ 3
    )) |> 
    mutate(true_total = (raw_model * 0.35) + (median_total * 0.65)) |> 
    mutate(true_total = round(true_total * 2) / 2) |> 
    mutate(median_total = round(median_total * 2) / 2) |> 
    left_join(lookup, 
              by = c("bin_cat" = "spread_bin", 
                     "median_total" = "market_total", 
                     "true_total")) |> 
    mutate(implied_odds_total = calc_implied_odds(median_total_price)) |> 
    mutate(total_over_probability = over_probability - implied_odds_total) |> 
    mutate(total_under_probability = under_probability - implied_odds_total) |> 
    select(week, team = home_team, game, true_spread, median_spread, 
           spread_cover_probability, spread_edge, bin_cat, 
           true_total, median_total, over_probability,
           total_over_probability, under_probability, total_under_probability, 
           last_update_api
    ) 
  
  return(nfl_odds)
  
}


nfl_odds_btb <- update_nfl_odds()

write_csv(nfl_odds_btb, "NFL_Odds/Data/Odds API.csv")



