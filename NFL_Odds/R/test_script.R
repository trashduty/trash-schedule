library(readr)

my_csv <- read_csv("NFL_Odds/Data/interpolated_2d_margin_probs.csv")

write_csv(my_csv, "NFL_Odds/Data/test_write.csv")
