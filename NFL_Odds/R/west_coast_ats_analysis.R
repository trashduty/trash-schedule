library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(openxlsx)
library(nflreadr)
library(nflplotR)
library(lubridate)

SEASONS <- 2016:2025
WEST_COAST_AWAY <- c("SEA", "LAR", "RAM", "LAC", "SDG", "SF", "LV", "OAK")
WEST_TEAM_MAP <- c("RAM" = "LAR", "SDG" = "LAC", "OAK" = "LV")
WEST_COAST_TEAMS <- c("SEA", "LAR", "LAC", "SF", "LV")
ET_HOME_TEAMS <- c(
  "BUF", "MIA", "NE", "NYJ", "BAL", "CIN", "CLE", "PIT",
  "IND", "JAX", "NYG", "PHI", "WAS", "DET", "ATL", "CAR", "TB"
)

PLOTS_DIR <- "NFL_Odds/Plots/WestCoastATS"
DATA_DIR <- "NFL_Odds/Data"
EXCEL_FILE <- file.path(DATA_DIR, "WestCoast_ATS_Analysis.xlsx")
CSV_FILE <- file.path(DATA_DIR, "WestCoast_ATS_Analysis.csv")
OVERALL_PLOT_FILE <- file.path(PLOTS_DIR, "west_coast_ats_overall_fav_vs_dog.png")
TEAM_PLOT_FILES <- c(
  "SEA" = file.path(PLOTS_DIR, "SEA_ats_1pm_et.png"),
  "LAR" = file.path(PLOTS_DIR, "LAR_ats_1pm_et.png"),
  "LAC" = file.path(PLOTS_DIR, "LAC_ats_1pm_et.png"),
  "SF" = file.path(PLOTS_DIR, "SF_ats_1pm_et.png"),
  "LV" = file.path(PLOTS_DIR, "LV_ats_1pm_et.png")
)
ROLE_COLORS <- c("Favorite" = "#FF6F61", "Underdog" = "#00CFCF")
TEXT_COLOR <- "white"
ANNOTATION_X_OFFSET <- 0.45
ANNOTATION_Y_POSITION <- 1.08
LOGO_RIGHT_MARGIN_X_OFFSET <- 1.05
LOGO_TOP_MARGIN_Y_POSITION <- 1.17
LOGO_FACET_ROLE <- "Underdog"
Y_AXIS_UPPER_LIMIT <- 1.18

DARK_THEME <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = scales::alpha(TEXT_COLOR, 0.35), linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = TEXT_COLOR),
    axis.title = element_text(color = TEXT_COLOR),
    plot.title = element_text(color = TEXT_COLOR, face = "bold"),
    plot.subtitle = element_text(color = TEXT_COLOR),
    strip.background = element_rect(fill = "black", color = TEXT_COLOR),
    strip.text = element_text(color = TEXT_COLOR),
    plot.margin = margin(18, 120, 10, 10)
  )

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

resolve_west_spread <- function(schedule_df) {
  oak_bal_reference <- schedule_df |>
    filter(
      as.Date(gameday) == as.Date("2016-10-02"),
      away_team == "OAK",
      home_team == "BAL",
      !is.na(spread_line)
    ) |>
    slice(1)

  if (nrow(oak_bal_reference) == 1) {
    ref_line <- oak_bal_reference$spread_line[1]
    if (dplyr::near(ref_line, -3.5)) {
      message("Detected home-team spread_line perspective; using west_spread = -spread_line")
      return(-schedule_df$spread_line)
    }
    if (dplyr::near(ref_line, 3.5)) {
      message("Detected away-team spread_line perspective; using west_spread = spread_line")
      return(schedule_df$spread_line)
    }
  }

  message("Could not infer spread_line orientation from OAK @ BAL reference (game not found or spread_line missing); defaulting to west_spread = -spread_line")
  -schedule_df$spread_line
}

if (!dir.exists(PLOTS_DIR)) dir.create(PLOTS_DIR, recursive = TRUE)
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

message("Loading schedule data...")
schedules <- nflreadr::load_schedules(seasons = SEASONS)

kickoff_time <- extract_kickoff_time(schedules)
west_spread <- resolve_west_spread(schedules)

west_games <- schedules |>
  mutate(
    away_team_norm = normalize_team(away_team),
    home_team_norm = normalize_team(home_team),
    kickoff_time = kickoff_time,
    is_one_pm_et = is_one_pm_et(kickoff_time),
    west_spread = west_spread
  ) |>
  filter(
    season %in% SEASONS,
    game_type == "REG",
    away_team_norm %in% WEST_COAST_TEAMS,
    home_team %in% ET_HOME_TEAMS,
    is_one_pm_et,
    !is.na(spread_line),
    !is.na(away_score),
    !is.na(home_score)
  ) |>
  mutate(
    west_team = away_team_norm,
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
    game_date = as.Date(gameday),
    spread = sprintf("%+.1f", west_spread)
  )

if (nrow(west_games) == 0) {
  stop("No games matched the requested filters. Check kickoff-time column names/data format.")
}

excel_output <- west_games |>
  transmute(
    Date = game_date,
    Team = west_team,
    Opponent = home_team_norm,
    Matchup = matchup,
    `Team Score` = away_score,
    `Opponent Score` = home_score,
    Spread = spread,
    `Result Indicator` = result_indicator
  ) |>
  arrange(Date)

openxlsx::write.xlsx(list(WestCoast_ATS_Games = excel_output), EXCEL_FILE, overwrite = TRUE)
utils::write.csv(excel_output, CSV_FILE, row.names = FALSE)

oak_bal_verification <- west_games |>
  filter(
    game_date == as.Date("2016-10-02"),
    away_team == "OAK",
    home_team == "BAL"
  ) |>
  slice(1)

if (nrow(oak_bal_verification) == 1 &&
    (!dplyr::near(oak_bal_verification$west_spread[[1]], 3.5) ||
      oak_bal_verification$away_score[[1]] != 28 ||
      oak_bal_verification$home_score[[1]] != 27 ||
      oak_bal_verification$result_indicator[[1]] != "Covered")) {
  warning("Reference check failed for 2016-10-02 OAK @ BAL (expected spread +3.5, score 28-27, Covered).")
}

plot_base <- west_games |>
  filter(role %in% c("Favorite", "Underdog"), result_indicator != "Push")

overall_summary <- plot_base |>
  group_by(season, role) |>
  summarise(
    wins = sum(result_indicator == "Covered"),
    losses = sum(result_indicator == "Not Covered"),
    games = wins + losses,
    cover_pct = if_else(games > 0, wins / games, NA_real_),
    .groups = "drop"
  ) |>
  mutate(record = paste0(wins, "-", losses))

overall_totals <- plot_base |>
  summarise(
    wins = sum(result_indicator == "Covered"),
    losses = sum(result_indicator == "Not Covered")
  ) |>
  mutate(record = paste0(wins, "-", losses))

role_totals <- plot_base |>
  group_by(role) |>
  summarise(
    wins = sum(result_indicator == "Covered"),
    losses = sum(result_indicator == "Not Covered"),
    .groups = "drop"
  ) |>
  mutate(role_record = paste0(role, ": ", wins, "-", losses))

team_summary <- plot_base |>
  group_by(west_team, season, role) |>
  summarise(
    wins = sum(result_indicator == "Covered"),
    losses = sum(result_indicator == "Not Covered"),
    games = wins + losses,
    cover_pct = if_else(games > 0, wins / games, NA_real_),
    .groups = "drop"
  ) |>
  mutate(
    record = paste0(wins, "-", losses),
    west_team = factor(west_team, levels = WEST_COAST_TEAMS)
  )

team_totals <- plot_base |>
  group_by(west_team) |>
  summarise(
    wins = sum(result_indicator == "Covered"),
    losses = sum(result_indicator == "Not Covered"),
    .groups = "drop"
  ) |>
  mutate(record = paste0(wins, "-", losses))

team_role_totals <- plot_base |>
  group_by(west_team, role) |>
  summarise(
    wins = sum(result_indicator == "Covered"),
    losses = sum(result_indicator == "Not Covered"),
    .groups = "drop"
  ) |>
  mutate(role_record = paste0(role, ": ", wins, "-", losses))

can_render_logos <- requireNamespace("magick", quietly = TRUE)
if (!can_render_logos) {
  message("Package 'magick' is unavailable; generating plots without team logos.")
}

overall_role_annotations <- role_totals |>
  mutate(
    season = max(SEASONS) + ANNOTATION_X_OFFSET,
    cover_pct = ANNOTATION_Y_POSITION
  )

overall_plot <- ggplot(overall_summary, aes(x = season, y = cover_pct, fill = role)) +
  geom_col(width = 0.75, show.legend = FALSE) +
  geom_text(aes(label = record), vjust = -0.35, size = 3.4, color = TEXT_COLOR) +
  geom_text(
    data = overall_role_annotations,
    aes(x = season, y = cover_pct, label = role_record),
    inherit.aes = FALSE,
    hjust = 1,
    size = 3.6,
    color = TEXT_COLOR,
    fontface = "bold"
  ) +
  facet_wrap(~role, nrow = 1) +
  scale_fill_manual(values = ROLE_COLORS) +
  scale_x_continuous(breaks = SEASONS, labels = SEASONS) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, Y_AXIS_UPPER_LIMIT),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "West Coast Teams at ET 1:00 PM - ATS by Role",
    subtitle = paste0("Aggregate ATS Record: ", overall_totals$record[[1]], " | Regular Season, 2016-2025"),
    x = "Year",
    y = "Cover Percentage"
  ) +
  DARK_THEME +
  coord_cartesian(clip = "off")

team_title_map <- c(
  "SEA" = "Seattle ATS on East Coast at 1PM ET",
  "LAR" = "LA Rams ATS on East Coast at 1PM ET",
  "LAC" = "LA Chargers ATS on East Coast at 1PM ET",
  "SF" = "San Francisco ATS on East Coast at 1PM ET",
  "LV" = "Las Vegas ATS on East Coast at 1PM ET"
)

ggsave(OVERALL_PLOT_FILE, overall_plot, width = 12, height = 6, dpi = 300)

logo_template <- data.frame(
  role = LOGO_FACET_ROLE,
  season = max(SEASONS) + LOGO_RIGHT_MARGIN_X_OFFSET,
  cover_pct = LOGO_TOP_MARGIN_Y_POSITION,
  stringsAsFactors = FALSE
)

team_summary_norm <- team_summary |>
  mutate(west_team = normalize_team(as.character(west_team)))
team_totals_norm <- team_totals |>
  mutate(west_team = normalize_team(as.character(west_team)))
team_role_totals_norm <- team_role_totals |>
  mutate(west_team = normalize_team(as.character(west_team)))

for (team in names(TEAM_PLOT_FILES)) {
  team_data <- team_summary_norm |>
    filter(west_team == team)

  team_total <- team_totals_norm |>
    filter(west_team == team) |>
    pull(record)

  if (nrow(team_data) == 0 || length(team_total) == 0) {
    warning("No ATS summary data available for team: ", team, ". Skipping plot generation for this team.")
    next
  }

  team_role_annotation <- team_role_totals_norm |>
    filter(west_team == team) |>
    mutate(
      season = max(SEASONS) + ANNOTATION_X_OFFSET,
      cover_pct = ANNOTATION_Y_POSITION
    )

  logo_data <- logo_template |>
    mutate(team_abbr = team)

  team_plot <- ggplot(team_data, aes(x = season, y = cover_pct, fill = role)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = record), vjust = -0.35, size = 3.0, color = TEXT_COLOR) +
    geom_text(
      data = team_role_annotation,
      aes(x = season, y = cover_pct, label = role_record),
      inherit.aes = FALSE,
      hjust = 1,
      size = 3.4,
      color = TEXT_COLOR,
      fontface = "bold"
    ) +
    facet_wrap(~role, nrow = 1) +
    scale_fill_manual(values = ROLE_COLORS) +
    scale_x_continuous(breaks = SEASONS, labels = SEASONS) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, Y_AXIS_UPPER_LIMIT),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = team_title_map[[team]],
      subtitle = paste0("Total ATS Record: ", team_total[[1]], " | Regular Season, 2016-2025"),
      x = "Year",
      y = "Cover Percentage"
    ) +
    DARK_THEME +
    coord_cartesian(
      xlim = c(min(SEASONS) - 0.5, max(SEASONS) + 0.5),
      clip = "off"
    )

  if (can_render_logos) {
    team_plot <- team_plot +
      nflplotR::geom_nfl_logos(
        data = logo_data,
        aes(x = season, y = cover_pct, team_abbr = team_abbr),
        inherit.aes = FALSE,
        width = 0.18
      )
  }

  ggsave(TEAM_PLOT_FILES[[team]], team_plot, width = 12, height = 6, dpi = 300)
}

message("Saved plot: ", OVERALL_PLOT_FILE)
for (plot_file in TEAM_PLOT_FILES) {
  message("Saved plot: ", plot_file)
}
message("Saved data: ", EXCEL_FILE)
message("Saved data: ", CSV_FILE)
