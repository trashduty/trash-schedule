library(tidyverse)
library(nflfastR)
library(nflplotR)
library(scales)

# Timestamp for captions
timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M UTC")

# ── 1. Load 2025 NFL Play-by-Play Data ────────────────────────────────────────
pbp <- nflfastR::load_pbp(2025)

# ── 2. 4th Down Aggression vs. Efficiency ─────────────────────────────────────
# "Sensible" 4th down situations: ydstogo <= 5, exclude missing posteam /
# fourth_down_converted values.
fourth_down_stats <- pbp |>
  filter(
    down == 4,
    ydstogo <= 5,
    !is.na(posteam),
    !is.na(fourth_down_converted)
  ) |>
  group_by(team = posteam) |>
  summarize(
    total_situations = n(),
    actual_attempts  = sum(play_type %in% c("run", "pass"), na.rm = TRUE),
    go_rate          = actual_attempts / total_situations,
    conv_rate        = sum(fourth_down_converted == 1, na.rm = TRUE) / actual_attempts,
    .groups = "drop"
  ) |>
  filter(actual_attempts >= 3)  # remove teams with very few attempts to avoid noise

# ── 3. 2-Point Conversion Aggression vs. Success ──────────────────────────────
two_pt_stats <- pbp |>
  filter(!is.na(posteam)) |>
  group_by(team = posteam) |>
  summarize(
    total_tds      = sum(touchdown == 1 & play_type %in% c("run", "pass"), na.rm = TRUE),
    two_pt_attempts = sum(two_point_attempt == 1, na.rm = TRUE),
    attempt_rate   = two_pt_attempts / total_tds,
    conv_rate      = sum(two_point_conv_result == "success", na.rm = TRUE) / two_pt_attempts,
    .groups = "drop"
  ) |>
  filter(two_pt_attempts > 0)

# ── 4. Generate Plots ─────────────────────────────────────────────────────────
dir.create("docs/plots/nfl_2025", showWarnings = FALSE, recursive = TRUE)

# Plot 1: 4th Down Aggression vs. Efficiency
plot_4th <- ggplot(fourth_down_stats, aes(x = go_rate, y = conv_rate)) +
  nflplotR::geom_mean_lines(aes(x0 = go_rate, y0 = conv_rate), color = "gray40") +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.055, alpha = 0.85) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    plot.caption  = element_text(size = 9,  hjust = 1,   color = "gray50"),
    axis.title    = element_text(size = 13),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title    = "NFL 2025: 4th Down Aggression vs. Efficiency",
    subtitle = "4th & 5 or less | Are teams that go for it more also more successful?",
    x        = "Go-for-it Rate (% of sensible situations)",
    y        = "Conversion Rate (% of actual attempts)",
    caption  = paste0("Data: nflfastR | Plot: @trashduty | ", timestamp)
  )

ggsave(
  "docs/plots/nfl_2025/nfl_4th_down_aggression.png",
  plot_4th,
  width = 12, height = 8, dpi = "retina"
)

# Plot 2: 2-Point Conversion Aggression vs. Success
plot_2pt <- ggplot(two_pt_stats, aes(x = attempt_rate, y = conv_rate)) +
  nflplotR::geom_mean_lines(aes(x0 = attempt_rate, y0 = conv_rate), color = "gray40") +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.055, alpha = 0.85) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    plot.caption  = element_text(size = 9,  hjust = 1,   color = "gray50"),
    axis.title    = element_text(size = 13),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title    = "NFL 2025: 2-Point Conversion Aggression vs. Success",
    subtitle = "Attempt Rate (per offensive TD) vs. Conversion Rate",
    x        = "Attempt Rate (2-pt attempts per offensive TD)",
    y        = "Conversion Rate (% of 2-pt attempts)",
    caption  = paste0("Data: nflfastR | Plot: @trashduty | ", timestamp)
  )

ggsave(
  "docs/plots/nfl_2025/nfl_2pt_aggression.png",
  plot_2pt,
  width = 12, height = 8, dpi = "retina"
)

cat("\u2705 NFL aggression plots saved to docs/plots/nfl_2025/\n")
