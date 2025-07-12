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
  
  # sport <- "americanfootball_nfl"
  # apiKey <- Sys.getenv("ODDS_API_KEY")
  # regions <- "us"
  # markets <- "spreads,totals"
  # year <- nflreadr::get_current_season(roster = TRUE)
  # oddsFormat <- "american"
  
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
  
  teams <- nflreadr::load_teams() |> 
    select(team_abbr, team_name)
  
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
    left_join(teams, by = c("home_team" = "team_name")) |> 
    rename(home_abbr = team_abbr) |> 
    left_join(teams, by = c("away_team" = "team_name")) |> 
    rename(away_abbr = team_abbr) |> 
    mutate(week = case_when(
      commence_ny < week_one_wednesday ~ 0,  # Pre-season or invalid
      TRUE ~ as.numeric(floor((commence_ny - week_one_wednesday) / 7) + 1)
    )) |> 
    mutate(week = if_else(week == 23, 22, week)) |> 
    select(week, commence_time, commence_ny, bookmaker_id, 
           bookmaker, last_update_api, last_update_markets, market, 
           home_team, home_abbr, away_team, away_abbr, name, price, point)
  
  return(api_unnested)
}

api_data <- get_odds_api()

  margin <- read_csv("interpolated_2d_margin_probs.csv",
                     show_col_types = FALSE) |>
    janitor::clean_names()
  
  lookup <- read_csv("NFL_Totals_Lookup_Stratified_By_Spread.csv",
                     show_col_types = FALSE) |>
    janitor::clean_names()
  
  model_raw <- read_csv("Week 1 model pred_updated.csv",
                        show_col_types = FALSE) |>
    janitor::clean_names() |> 
    select(-c(offensive_expected_points_season:proe))
  
  teams <- nflreadr::load_teams() |> 
    select(team_abbr, team_logo_espn, team_name)
  
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
    filter(market == "spreads") |> 
    left_join(teams, by = c("name" = "team_name")) |> 
    rename(team = team_abbr) |> 
    mutate(game = paste0(away_abbr, "@", home_abbr)) |> 
    select(week, game, last_update_api, bookmaker, home_abbr, away_abbr, name, 
           spread = point, spread_price = price, team, team_logo_espn)
  
  api_totals <- api_data |> 
    filter(market == "totals") |> 
    mutate(game = paste0(away_abbr, "@", home_abbr)) |> 
    select(week, game, bookmaker, name, total_price = price, total = point)
    
  odds_calculated <- model_raw |> 
    mutate(home_team = clean_team_abbrs(home_team)) |> 
    left_join(select(api_spreads, week, team, game, spread, spread_price,
                     last_update_api, team_logo_espn, bookmaker), 
              by = c("week", "home_team" = "team")) |> 
    mutate(true_spread = ((model_prediction * 0.35) + (spread * 0.65)),  
           .after = spread) |> 
    mutate(true_spread = round(true_spread * 2) / 2) |> 
    mutate(spread = round(spread * 2) / 2) |> 
    left_join(margin, by = 
                c("spread" = "market_line", "true_spread" = "true_line")) |> 
    rename(spread_cover_probability = cover_probability, 
           spread_push_probability = push_probability, 
           spread_loss_probability = loss_probability
    ) |> 
    mutate(implied_odds_spread = calc_implied_odds(spread_price)) |> 
    mutate(spread_edge = spread_cover_probability - implied_odds_spread) |> 
    select(week, team = home_team, game, team_logo_espn, true_spread, spread, 
           spread_cover_probability, spread_edge, bookmaker, raw_model, last_update_api) |> 
    group_by(week, game, team) |> 
    mutate(
      median_edge_row = row_number(-spread_edge) == ceiling(n()/2),
      highest_edge_row = row_number(-spread_edge) == 1
    ) |> 
    ungroup() |> 
    arrange(week, game, team, -spread_edge)
  
  
  spread_summary <- odds_calculated |> 
    summarise(
      last_update_api = max(last_update_api, na.rm = TRUE), 
      model_prediction = true_spread[median_edge_row],
      market_line = spread[median_edge_row],
      cover_probability = spread_cover_probability[median_edge_row],
      edge = spread_edge[median_edge_row],
      best_book = bookmaker[highest_edge_row],
      best_line = spread[highest_edge_row],
      best_cover_probability = spread_cover_probability[highest_edge_row], 
      best_edge = spread_edge[highest_edge_row],
      .by = c(week, game, team)
    )

   
  totals_odds <- odds_calculated |> 
    # select(week, game, bookmaker, spread, raw_model, last_update_api) |> 
    left_join(api_totals, by = c("week", "game", "bookmaker"), 
              relationship = "many-to-many") |> 
    mutate(bin_cat = case_when(
      abs(spread) <= 3 ~ 1, 
      abs(spread) <= 7 ~ 2, 
      abs(spread) > 7 ~ 3
    )) |> 
    mutate(true_total = (raw_model * 0.35) + (total * 0.65)) |> 
    mutate(true_total = round(true_total * 2) / 2) |> 
    mutate(total = round(total * 2) / 2) |>
    left_join(lookup, 
              by = c("bin_cat" = "spread_bin", 
                     "total" = "market_total", 
                     "true_total")) |> 
    mutate(implied_odds_total = calc_implied_odds(total_price)) |> 
    mutate(probability = case_when(
      name == "Over" ~ over_probability, 
      name == "Under" ~ under_probability, 
      TRUE ~ over_probability)) |> 
    mutate(total_probability = probability - implied_odds_total) |> 
    # select(week, team_logo_espn, team, game, true_total, name, total, probability,
    #        edge = total_probability, last_update_api, bookmaker
    # ) 
    select(week, team, game, name, true_total, total, total_price, probability,
           edge = total_probability, last_update_api, bookmaker
    ) |> 
    arrange(week, team, game, name, -edge) |> 
    group_by(week, team, game, name) |> 
    mutate(
      median_edge_row = row_number(-edge) == ceiling(n()/2),
      highest_edge_row = row_number(-edge) == 1
    ) |> 
    ungroup()
  
  
  total_summary <- totals_odds |> 
    group_by(week, game) |> 
    summarise(
      model_prediction = first(true_total),
      
      # Median values - taking median across all bets regardless of Over/Under
      median_over_line = first(total[median_edge_row & name == "Over"]),
      median_over_price = first(total_price[median_edge_row & name == "Over"]),
      median_over_cover_probability = first(probability[median_edge_row & name == "Over"]),
      median_over_edge = first(edge[median_edge_row & name == "Over"]),
      median_under_line = first(total[median_edge_row & name == "Under"]),
      median_under_price = first(total_price[median_edge_row & name == "Under"]),
      median_under_cover_probability = first(probability[median_edge_row & name == "Under"]),
      median_under_edge = first(edge[median_edge_row & name == "Under"]),
      
      # Best values by bet type
      best_over_book = first(bookmaker[highest_edge_row & name == "Over"]),
      best_over_line = first(total[highest_edge_row & name == "Over"]),
      best_over_price = first(total_price[highest_edge_row & name == "Over"]),
      best_over_cover_probability = first(probability[highest_edge_row & name == "Over"]),
      best_over_edge = first(edge[highest_edge_row & name == "Over"]),
      
      best_under_book = first(bookmaker[highest_edge_row & name == "Under"]),
      best_under_line = first(total[highest_edge_row & name == "Under"]),
      best_under_price = first(total_price[highest_edge_row & name == "Under"]),
      best_under_cover_probability = first(probability[highest_edge_row & name == "Under"]),
      best_under_edge = first(edge[highest_edge_row & name == "Under"]),
      .groups = "drop"
    ) 
  



write_csv(total_summary, "NFL_Odds/Data/totals_odds.csv")
write_csv(spread_summary, "NFL_Odds/Data/spreads_odds.csv")











