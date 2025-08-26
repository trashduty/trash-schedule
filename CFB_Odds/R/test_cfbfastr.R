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

cfb_fastR <- cfbfastR::espn_cfb_schedule(year = 2025)

# Debug: Check what game_date looks like
print("Raw game_date column:")
print(head(cfb_fastR$game_date))
print("Class of game_date:")
print(class(cfb_fastR$game_date))

cfb_fastR_clean <- cfb_fastR |> 
  filter(type == "regular") |> 
  mutate(game_date = as_date(with_tz(ymd_hm(game_date), "America/New_York")))

readr::write_csv(cfb_fastR_clean, "CFB_Odds/Data/cfb_schedule.csv")
