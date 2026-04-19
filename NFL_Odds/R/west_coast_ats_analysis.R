library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(openxlsx)
library(nflreadr)
library(lubridate)

SEASONS <- 2016:2025
WEST_COAST_AWAY <- c("SEA", "LAR", "RAM", "LAC", "SDG", "SF", "LV", "OAK")
WEST_TEAM_MAP <- c("RAM" = "LAR", "SDG" = "LAC", "OAK" = "LV")
ET_HOME_TEAMS <- c(
  "BUF", "MIA", "NE", "NYJ", "BAL", "CIN", "CLE", "PIT",
  "IND", "JAX", "NYG", "PHI", "WAS", "DET", "ATL", "CAR", "TB"
)

PLOTS_DIR <- "NFL_Odds/Plots/WestCoastATS"
DATA_DIR <- "NFL_Odds/Data"
EXCEL_FILE <- file.path(DATA_DIR, "WestCoast_ATS_Analysis.xlsx")
OVERALL_PLOT_FILE <- file.path(PLOTS_DIR, "west_coast_ats_overall_fav_vs_dog.png")
TEAM_PLOT_FILE <- file.path(PLOTS_DIR, "west_coast_ats_by_team_fav_vs_dog.png")

normalize_team <- function(team_abbr) {
  recode(team_abbr, !!!WEST_TEAM_MAP, .default = team_abbr)
}

extract_kickoff_time <- function(schedule_df) {
  kickoff_candidates <- c(
    "start_time",
    "gametime",
    "gametime_et",
    "gametime_eastern",
    "game_time_eastern"
  )

  existing <- kickoff_candidates[kickoff_candidates %in% names(schedule_df)]
  if (length(existing) == 0) {
    return(rep(NA_character_, nrow(schedule_df)))
  }

  kickoff <- as.character(schedule_df[[existing[1]]])
  if (length(existing) > 1) {
    for (i in 2:length(existing)) {
      kickoff <- dplyr::coalesce(kickoff, as.character(schedule_df[[existing[i]]]))
    }
  }

  kickoff
}

is_one_pm_et <- function(kickoff) {
  parsed <- kickoff |>
    as.character() |>
    str_to_upper() |>
    str_squish() |>
    str_extract("\\d{1,2}:\\d{2}(:\\d{2})?\\s*(AM|PM)?")

  str_detect(parsed, "^(13:00(:00)?|1:00(:00)?\\s*PM|01:00(:00)?\\s*PM)$")
}

if (!dir.exists(PLOTS_DIR)) dir.create(PLOTS_DIR, recursive = TRUE)
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

message("Loading schedule data...")
schedules <- nflreadr::load_schedules(seasons = SEASONS)

kickoff_time <- extract_kickoff_time(schedules)

west_games <- schedules |>
  mutate(
    away_team_norm = normalize_team(away_team),
    home_team_norm = normalize_team(home_team),
    kickoff_time = kickoff_time,
    is_one_pm_et = is_one_pm_et(kickoff_time)
  ) |>
  filter(
    season %in% SEASONS,
    game_type == "REG",
    away_team %in% WEST_COAST_AWAY,
    home_team %in% ET_HOME_TEAMS,
    is_one_pm_et,
    !is.na(spread_line),
    !is.na(away_score),
    !is.na(home_score)
  ) |>
  mutate(
    west_team = away_team_norm,
    west_spread = -spread_line,
    score_margin = away_score - home_score,
    ats_margin = score_margin + west_spread,
    result_indicator = case_when(
      ats_margin > 0 ~ "Covered",
      ats_margin < 0 ~ "Not Covered",
      TRUE ~ "Push"
    ),
    role = case_when(
      west_spread < 0 ~ "Favorite",
      west_spread > 0 ~ "Underdog",
      TRUE ~ "Pick'em"
    ),
    matchup = paste0(away_team, " @ ", home_team),
    score = paste0(away_score, "-", home_score),
    game_date = as.Date(gameday),
    spread = sprintf("%+.1f", west_spread)
  )

if (nrow(west_games) == 0) {
  stop("No games matched the requested filters. Check kickoff-time column names/data format.")
}

excel_output <- west_games |>
  transmute(
    Date = game_date,
    Matchup = matchup,
    Score = score,
    Spread = spread,
    `Result Indicator` = result_indicator
  ) |>
  arrange(Date)

openxlsx::write.xlsx(list(WestCoast_ATS_Games = excel_output), EXCEL_FILE, overwrite = TRUE)

plot_base <- west_games |>
  filter(role %in% c("Favorite", "Underdog"), result_indicator != "Push")

overall_summary <- plot_base |>
  group_by(season, role) |>
  summarise(
    wins = sum(result_indicator == "Covered"),
    losses = sum(result_indicator == "Not Covered"),
    games = wins + losses,
    cover_pct = if_else(games > 0, wins / games, NA_real_),
    record = paste0(wins, "-", losses),
    .groups = "drop"
  )

team_summary <- plot_base |>
  group_by(west_team, season, role) |>
  summarise(
    wins = sum(result_indicator == "Covered"),
    losses = sum(result_indicator == "Not Covered"),
    games = wins + losses,
    cover_pct = if_else(games > 0, wins / games, NA_real_),
    record = paste0(wins, "-", losses),
    .groups = "drop"
  ) |>
  mutate(west_team = factor(west_team, levels = c("SEA", "LAR", "LAC", "SF", "LV")))

overall_plot <- ggplot(overall_summary, aes(x = factor(season), y = cover_pct, fill = role)) +
  geom_col(width = 0.75, show.legend = FALSE) +
  geom_text(aes(label = record), vjust = -0.25, size = 3.4) +
  facet_wrap(~role, nrow = 1) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "West Coast Teams at ET 1:00 PM - ATS by Role",
    subtitle = "Regular Season, 2016-2025",
    x = "Season",
    y = "Cover Percentage"
  ) +
  theme_minimal(base_size = 12)

team_plot <- ggplot(team_summary, aes(x = factor(season), y = cover_pct, fill = role)) +
  geom_col(width = 0.75, show.legend = FALSE) +
  geom_text(aes(label = record), vjust = -0.25, size = 2.8) +
  facet_grid(west_team ~ role) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "West Coast Teams at ET 1:00 PM - ATS by Team and Role",
    subtitle = "Regular Season, 2016-2025",
    x = "Season",
    y = "Cover Percentage"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text.y = element_text(angle = 0))

ggsave(OVERALL_PLOT_FILE, overall_plot, width = 12, height = 6, dpi = 300)
ggsave(TEAM_PLOT_FILE, team_plot, width = 14, height = 12, dpi = 300)

message("Saved plot: ", OVERALL_PLOT_FILE)
message("Saved plot: ", TEAM_PLOT_FILE)
message("Saved data: ", EXCEL_FILE)
