## code to prepare `DATASET` dataset goes here

library(readr)
library(devtools)
accident_2015.csv.bz2 <- read.csv("./data-raw/accident_2015.csv.bz2")
write_csv(accident_2015.csv.bz2,"accident_2015.csv.bz2")
usethis::use_data(accident_2015.csv.bz2,overwrite = TRUE)

