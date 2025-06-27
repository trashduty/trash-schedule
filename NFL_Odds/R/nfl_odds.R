library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(nflreadr)
library(httr)
library(jsonlite)
library(glue)
library(lubridate)

sport = "americanfootball_nfl"
apiKey = Sys.getenv("ODDS_API_KEY")
regions = "us"
markets = "spreads,totals"
year = nflreadr::get_current_season(roster = TRUE)

# URL
url <- glue::glue("https://api.the-odds-api.com/v4/sports/{sport}/odds/?apiKey={apiKey}&regions={regions}&markets={markets}")

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
  select(commence_time, commence_ny, bookmaker_id, 
         bookmaker, last_update_api, last_update_markets, market, 
         home_team, home_abbr, away_team, away_abbr, name, price, point)

write_csv(api_unnested, "NFL_Odds/Data/Odds API.csv")




