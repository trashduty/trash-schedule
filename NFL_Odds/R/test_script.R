library(readr)

my_csv <- read_csv("Data/interpolated_2d_margin_probs.csv")

write_csv(my_csv, "Data/test_write.csv")
