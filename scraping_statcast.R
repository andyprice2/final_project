# libraries

library(tidyverse)
library(baseballr)
library(janitor)
library(broom)


# Coming up with list of needed dates

list_of_dates_2018 <- seq(as.Date("2018-03-29"), as.Date("2018-10-01"), by="days")

# Function to scrape one day of statcast data 
statcast_one_date <- function(date) {
  scrape_statcast_savant(start_date = date, end_date = date, player_type = 'pitcher')
}

# This line pulls out all the statcast data from 2019
# statcast_data_2018 <- map_dfr(list_of_dates_2018, statcast_one_date)

with_res <- run_expectancy_code(statcast_data_2018, level = "pitch")


# write_csv(with_res, path = "/Users/andrewprice/Desktop/Final Project/with_res.csv")
