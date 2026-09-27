    logo,
    model_prediction = true_total,
    market_line = total,
    market_price = total_price,
    market_under_price,
    over_probability,
    under_probability,
    over_edge,
    under_edge,
    drive_bin
  )

totals_best_summary <- totals_calculated |>
  filter(side == "Over") |>
  arrange(week, game, desc(over_edge), bookmaker) |>
  slice_head(n = 1, by = c(week, game)) |>
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

totals_best_under_summary <- totals_calculated |>
  filter(side == "Under") |>
  arrange(week, game, desc(under_edge), bookmaker) |>
  slice_head(n = 1, by = c(week, game)) |>
  transmute(
    week,
    game,
    best_under_book = bookmaker,
    best_under_line = total,
    best_under_price = total_price,
    best_under_cover_probability = under_probability,
    best_under_valid_edge = under_edge
  )

totals_summary <- totals_last_update |>
  left_join(totals_median_summary, by = c("week", "game")) |>
  left_join(totals_best_summary, by = c("week", "game")) |>
  left_join(totals_best_under_summary, by = c("week", "game"))

message("\n===== CFB TOTALS PIPELINE COUNTS =====")
message("model_raw: ", nrow(model_raw))
message("spreads_games: ", nrow(spreads_games))
message("model_joined: ", nrow(model_joined))
message("model_with_game: ", nrow(model_with_game))
message("api_data: ", nrow(api_data))
message("api_totals_bookmaker: ", nrow(api_totals_bookmaker))
message("spreads_predictions: ", nrow(spreads_predictions))
message("totals_lookup_joined: ", nrow(totals_lookup_joined))
message("totals_calculated: ", nrow(totals_calculated))
message("totals_last_update: ", nrow(totals_last_update))
message("totals_median_summary: ", nrow(totals_median_summary))
message("totals_best_summary: ", nrow(totals_best_summary))
message("totals_summary: ", nrow(totals_summary))

if (nrow(totals_summary) == 0) {
  message("\nModel matchups not found in spreads data:")

  unmatched_model_games <- model_joined |>
    anti_join(
      select(spreads_games, matchup_key),
      by = "matchup_key"
    ) |>
    distinct(
      week,
      model_team,
      model_opponent,
      matchup_key
    )

  print(unmatched_model_games, n = Inf)

  message("\nAPI totals games not found in matched model games:")

  unmatched_api_games <- api_totals_bookmaker |>
    anti_join(
      distinct(model_with_game, game),
      by = "game"
    ) |>
    distinct(week, game)

  print(unmatched_api_games, n = Inf)

  warning(
    paste0(
      "CFB totals pipeline produced zero rows. ",
      "The totals model may not yet be updated for the current week. ",
      "Existing totals_odds.csv was preserved."
    ),
    call. = FALSE,
    immediate. = TRUE
  )

} else {
  write_csv(
    totals_summary,
    "CFB_Odds/Data/totals_odds.csv"
  )

  message(
    "Successfully wrote ",
    nrow(totals_summary),
    " games to CFB_Odds/Data/totals_odds.csv."
  )
}
