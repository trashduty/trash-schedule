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

# Build a many-to-one lookup that maps every known name variant for a team
# (short name, full btb name, cfbfastR name, api name) to that team's team_id.
# This is used as a failsafe wherever we need to match a team name coming
# from an external/raw source (model output, odds API) back to a team_id,
# regardless of which name format that source happens to use.
create_team_name_lookup <- function(cfb_crosswalk) {
  name_lookup <- cfb_crosswalk |>
    select(team_id, btb_team_short, btb_team, cfbfastr_team, api_team) |>
    pivot_longer(
      cols = c(btb_team_short, btb_team, cfbfastr_team, api_team),
      names_to = "name_type",
      values_to = "team_name"
    ) |>
    filter(!is.na(team_name)) |>
    select(team_id, team_name) |>
    distinct()

  ambiguous_names <- name_lookup |>
    distinct(team_id, team_name) |>
    summarise(n_teams = n_distinct(team_id), .by = team_name) |>
    filter(n_teams > 1) |>
    pull(team_name)

  if (length(ambiguous_names) > 0) {
    warning(glue(
      "Team name variant(s) map to more than one team_id in the crosswalk: {paste(ambiguous_names, collapse = ', ')}"
    ))
  }

  name_lookup
}

team_name_lookup <- create_team_name_lookup(cfb_crosswalk)

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
blended_model_weight <- 0.20
blended_market_weight <- 0.80

calculate_cfb_week <- function(game_date) {
  case_when(
    game_date >= week_zero_start & game_date <= week_zero_end ~ 0,
    TRUE ~ as.numeric(floor((game_date - week_one_start) / 7) + 1)
  )
}

team_id_lookup <- cfb_crosswalk |>
  select(team_id, btb_team)
