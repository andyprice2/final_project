
# Loading libraries -------------------------------------------------------



# Use to force install of Baseballr package:
# devtools::install_github("BillPetti/baseballr", force = TRUE)

library(baseballr)
library(tidyverse)

# For scraping pitch fx data (mostly unneccessary, as baseballr can do most of
# this)

library(pitchRx)

# For storing pitch fx data

library(RSQLite)

# For limiting date range

library(lubridate)

library(dplyr)

# Defining time saving function to convert factors to numerics

as.numeric.factor <- function(x) {
  as.numeric(levels(x))[x]
}



# Loading in pitch data ---------------------------------------------------



# Reading in full data set, should add colclasses since it's treating everything
# as a factor.

full_pitch_fx_data <- read.csv("gd_savant_new.csv")

full_pitch_fx_data <- full_pitch_fx_data %>%
  filter(pitch_type != "Error")

# Changing variables into numerics

full_pitch_fx_data$pfx_x <- as.numeric.factor(full_pitch_fx_data$pfx_x)
full_pitch_fx_data$pfx_z <- as.numeric.factor(full_pitch_fx_data$pfx_z)

# Creating training set. Use sample frac to give percentage of rows in original
# dataframe.

full_pitch_fx_data$game_date <- parse_date(as.character(full_pitch_fx_data$game_date))
pitch_data_2018 <- full_pitch_fx_data %>%
  filter(game_date > "2018-03-28" & game_date < "2018-10-29")

set.seed(19)
training_set_2018 <- sample_frac(pitch_data_2018, size = .8)
training_set_2018$release_speed <- as.numeric.factor(training_set_2018$release_speed)




# Graphing speed x movement (not nec.) ------------------------------------


training_set_2018 %>%
  filter(pitch_type == "FF") %>%
  ggplot(aes(x = pfx_z, y = release_speed)) +
  geom_point() +
  geom_smooth()



# Functions to find run environment ---------------------------------------

# I need to create a function that takes two dates and calculates the standings
# from 15 days before and 15 days after, and then subtract the total runs and
# games to come up with standings over that period. Then, come up with run
# environment.

# Calculate RPG from season start to inputed date

rpg_from_standings <- function(date) {
  AL_standings_date <- as.data.frame(standings_on_date_bref(date, "AL Overall"))
  colnames(AL_standings_date) <- c("team", "wins", "losses", "winning_percentage", "games_back", "runs_scored", "runs_against", "pyth_win_percentage")

  AL_standings_date <- AL_standings_date %>%
    summarise(
      games = sum(wins),
      runs = sum(runs_scored)
    )

  NL_standings_date <- as.data.frame(standings_on_date_bref(date, "NL Overall"))
  colnames(NL_standings_date) <- c("team", "wins", "losses", "winning_percentage", "games_back", "runs_scored", "runs_against", "pyth_win_percentage")

  NL_standings_date <- NL_standings_date %>%
    summarise(
      games = sum(wins),
      runs = sum(runs_scored)
    )

  combined_standings <- bind_rows(NL_standings_date, AL_standings_date)
  combined_standings <- combined_standings %>%
    summarise(
      games = sum(games),
      runs = sum(runs)
    )

  return(combined_standings)
}

# Calculate RPG over a given period (within one year)

subtract_then_rpg <- function(start_date, end_date) {
  date1 <- rpg_from_standings(start_date)
  date2 <- rpg_from_standings(end_date)

  both_dates <- bind_rows(date2, date1)

  diff <- both_dates %>%
    summarise(
      games_since = .[1, 1] - .[2, 1],
      runs_since = .[1, 2] - .[2, 2]
    )

  rpg_since <- diff %>%
    summarise(rpg = (runs_since / games_since) / 2)

  return(rpg_since$rpg)
}

# Teach it how to deal with dates in the baseball season (if march/april, just
# run environment from start of season to end of april, if between may 1 and
# august 31, then simply the run environment from the 25 days on either side of
# it (50 total), and if in Sept/August, then the run environment from september
# first to october first.)

calculate_run_env <- function(date) {
 
   if (month(date) %in% c(3, 4) & year(date) == 2018) {
    march_start_date <- "2018-03-29"
    april_end_date <- "2018-04-30"
    subtract_then_rpg(march_start_date, april_end_date)
  } else if (month(date) > 8 & year(date) == 2018) {
    september_start_date <- "2018-09-01"
    october_end_date <- "2018-10-01"
    subtract_then_rpg(september_start_date, october_end_date)
  } else {
    start_date <- date - 25
    end_date <- date + 25
    subtract_then_rpg(start_date, end_date)
  }
}




# Joining run envs with pitch data ----------------------------------------



# Run game environment on every date in 2018 season (will be faster than running
# the function on 700,000 games). Then join with pitch data. Created using the
# text below, but don't run unless I have to, because it takes so long. Instead,
# I wrote it to a csv and simply read that in every time.

# dates_with_run_envs <- unique(training_set_2018$game_date)
# dates_with_run_envs2 <- map_dbl(dates_with_run_envs, calculate_run_env)
# full_dates_w_run_envs <- tibble(dates_with_run_envs, dates_with_run_envs2)
# colnames(full_dates_w_run_envs) <- c("game_date", "run_env")
# full_dates_w_run_envs <- full_dates_w_run_envs %>% 
#   arrange(game_date)

# write_csv(full_dates_w_run_envs, path = "/Users/andrewprice/Desktop/Final Project/run_env.csv")
# write_csv(training_set_2018, path = "/Users/andrewprice/Desktop/Final Project/training_data_2018.csv")

# Read these back in (much quicker than running the whole script).

run_environments_2018 <- read.csv("run_env.csv")
training_set_2018 <- read.csv("training_data_2018.csv")


# Join with 2018 pitch data
