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
max_preview_games <- 10
blended_model_weight <- 0.35
blended_market_weight <- 0.65
# Deterministic sportsbook order used to break ties when multiple totals rows
# are otherwise equally good candidates for the representative or best line.
bookmaker_priority <- c("BetMGM", "BetRivers", "DraftKings", "FanDuel",
                        "ESPN BET", "Fanatics", "Caesars")

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

required_model_columns <- c("model_prediction", "home_team_id", "away_team_id", "team", "opponent", "week")
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

unmatched_output_path <- "CFB_Odds/Data/unmatched_totals.csv"
spreads_all_teams <- unique(c(spreads_games$away_team, spreads_games$home_team))

unmatched_diagnostics <- missing_game_rows |>
  mutate(
    game_description = paste0(opponent, " @ ", team),
    reason = case_when(
      is.na(model_team) & is.na(model_opponent) ~ "Both team IDs not found in crosswalk",
      is.na(model_team) ~ "Home team ID not found in crosswalk",
      is.na(model_opponent) ~ "Away team ID not found in crosswalk",
      model_team %in% spreads_all_teams & model_opponent %in% spreads_all_teams ~
        "Both teams in spreads_odds but this matchup is not present (game may not be in model output_new.csv)",
      !(model_team %in% spreads_all_teams) & !(model_opponent %in% spreads_all_teams) ~
        "Neither team found in spreads_odds.csv",
      !(model_team %in% spreads_all_teams) ~
        paste0("Home team '", model_team, "' not found in spreads_odds.csv"),
      TRUE ~
        paste0("Away team '", model_opponent, "' not found in spreads_odds.csv")
    )
  ) |>
  select(
    game_description,
    cfb_total_output_team = team,
    cfb_total_output_opponent = opponent,
    spreads_matched_team = model_team,
    spreads_matched_opponent = model_opponent,
    reason
  )

# Always overwrite so the file reflects the current run (0 rows = all games matched)
write_csv(unmatched_diagnostics, unmatched_output_path)

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
  warning(glue::glue(
    "{nrow(missing_game_rows)} game(s) in CFB Total Output could not be matched to spreads_odds.csv",
    " - see {unmatched_output_path}{overflow_note}: ",
    "{paste(head(preview_games, max_preview_games), collapse = '; ')}"
  ))
}

model_with_game <- model_with_game |>
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

# Select one totals row per game.
# `data` must include `week` and `game` columns, which define each grouped
# matchup, plus a `bookmaker` column. The `...` argument accepts the primary
# arrange expressions for choosing the row within each week/game group.
# The `...` argument accepts the primary arrange expressions for that
# selection, after which bookmaker rank and bookmaker name are used as
# deterministic tiebreakers. Returns one row per week/game.
select_totals_row <- function(data, ...) {
  ranked_data <- data

  if (!"bookmaker_rank" %in% names(ranked_data)) {
    bookmaker_rank_raw <- match(ranked_data$bookmaker, bookmaker_priority)
    ranked_data$bookmaker_rank <- ifelse(
      is.na(bookmaker_rank_raw),
      length(bookmaker_priority) + 1L,
      bookmaker_rank_raw
    )
  }

  ranked_data |>
    arrange(week, game, ..., bookmaker_rank, bookmaker) |>
    slice_head(n = 1, by = c(week, game))
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

api_totals_bookmaker <- api_data |>
  filter(market == "totals", name == "Over") |>
  filter(!is.na(home_team), !is.na(away_team)) |>
  mutate(game = paste0(away_team, "@", home_team)) |>
  select(
    week,
    game,
    last_update_api,
    bookmaker,
    total = point,
    total_price = price
  )

missing_total_keys <- model_with_game |>
  filter(!is.na(model_prediction)) |>
  distinct(game) |>
  anti_join(
    api_totals_bookmaker |> distinct(game),
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
      api_totals_bookmaker,
      game,
      api_week = week,
      total,
      total_price,
      last_update_api,
      bookmaker
    ),
    by = "game",
    relationship = "many-to-many"
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
    true_total = ((model_prediction * blended_model_weight) + (median_total * blended_market_weight)),
    .after = total
  ) |>
  mutate(
    true_total = round(true_total * 2) / 2,
    total = round(total * 2) / 2,
    drive_bin = calculate_drive_bin(true_total),
    implied_odds_total = calc_implied_odds(total_price)
  ) |>
  left_join(
    lookup,
    by = c("drive_bin", "total" = "market_total", "true_total"),
    relationship = "many-to-one"
  )

missing_lookup_rows <- totals_lookup_joined |>
  filter(is.na(over_probability) | is.na(under_probability)) |>
  distinct(week, game, drive_bin, total, true_total)

if (nrow(missing_lookup_rows) > 0) {
  overflow_note <- if_else(
    nrow(missing_lookup_rows) > max_preview_games,
    glue::glue(" (showing first {max_preview_games} of {nrow(missing_lookup_rows)})"),
    ""
  )
  preview_rows <- missing_lookup_rows |>
    mutate(
      lookup_key = glue::glue(
        "{game} drive_bin={drive_bin}, market_total={total}, true_total={true_total}"
      )
    ) |>
    pull(lookup_key)
  stop(glue::glue(
    "Totals pricing lookup failed for {nrow(missing_lookup_rows)} row(s){overflow_note}: ",
    "{paste(head(preview_rows, max_preview_games), collapse = '; ')}"
  ))
}

totals_calculated <- totals_lookup_joined |>
  mutate(
    over_edge = over_probability - implied_odds_total,
    under_edge = under_probability - implied_odds_total,
    median_distance = abs(total - median_total)
  )

totals_last_update <- totals_calculated |>
  summarise(
    last_update_api = safe_max_datetime(last_update_api),
    .by = c(week, game)
  )

totals_median_summary <- totals_calculated |>
  select_totals_row(median_distance, desc(over_edge)) |>
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
    under_edge
  )

totals_best_summary <- totals_calculated |>
  select_totals_row(desc(over_edge)) |>
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
