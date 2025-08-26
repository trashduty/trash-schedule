cfb_fastR <- cfbfastR::espn_cfb_schedule(year = 2025) |> 
    filter(type == "regular") |> 
    mutate(game_date = as_date(with_tz(ymd_hm(game_date), "America/New_York")))
  
readr::write_csv(cfb_fastR, "CFB_Odds/Data/cfb_schedule.csv")
