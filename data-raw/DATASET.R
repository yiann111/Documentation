## code to prepare `DATASET` dataset goes here
accident_2013.csv.bz2 <- read_csv("./data-raw/accident_2013.csv.bz2") %>% write_csv(file = "accident_2013.csv.bz2")
accident_2014.csv.bz2 <-read_csv("./data-raw/accident_2014.csv.bz2") %>% write_csv(file = "accident_2014.csv.bz2")
accident_2015.csv.bz2 <-read_csv("./data-raw/accident_2015.csv.bz2") %>% write_csv(file = "accident_2015.csv.bz2")


usethis::use_data(accident_2013.csv.bz2, overwrite = TRUE)
usethis::use_data(accident_2014.csv.bz2, overwrite = TRUE)
usethis::use_data(accident_2015.csv.bz2, overwrite = TRUE)


