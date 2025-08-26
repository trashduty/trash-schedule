cfb_fastR <- cfbfastR::espn_cfb_schedule(year = 2025)
  
readr::write_csv(cfb_fastR, "CFB_Odds/Data/cfb_schedule.csv")
