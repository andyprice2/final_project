# Load libraries and read-in data -----------------------------------------


# libraries

library(tidyverse)
library(baseballr)
library(janitor)
library(broom)
library(ranger)
library(rsample)
library(fitdistrplus)
library(fs)

# function that negates %in%

`%nin%` = Negate(`%in%`)

# read in data from csv written in last step

both_years <- read.csv("both_years.csv")

# Test with two days of data ----------------------------------------------


# test random forest


# test_for_random_forest <- data_2018 %>%
#   filter(game_date %in% c("2018-05-21", "2018-05-22")) %>%
#   filter(!is.na(pfx_x)) %>%
#   filter(!is.na(pfx_z)) %>% 
#   filter(!is.na(release_speed)) %>%
#   filter(!is.na(description)) %>%
#   mutate(new_event = ifelse(!is.na(events), paste(events), paste(description))) %>%
#   mutate(new_event = ifelse(new_event == "strikeout", paste(description), paste(new_event))) %>%
#   mutate(new_event = ifelse(new_event == "walk", paste(description), paste(new_event))) 
# 
# test_model <- ranger(formula = new_event ~ pfx_x + pfx_z + release_speed + vx0 + vy0 + vz0 + ax + ay + az, 
#                      data = test_for_random_forest, 
#                      num.trees = 100, seed = 42, probability = T) 
# 
# predicted <- data_2018 %>%
#   filter(game_date %in% c("2018-06-21", "2018-06-22")) %>%
#   filter(!is.na(pfx_x)) %>%
#   filter(!is.na(pfx_z)) %>% 
#   filter(!is.na(release_speed)) %>%
#   filter(!is.na(description)) %>%
#   select(pfx_x, pfx_z, release_speed)
# 
# predict(test_model, repeated)$predictions



# gerrit cole test
# repeated <- tibble(pfx_x = -1.0553,
# pfx_z = 1.2258,
# release_speed = 97.3)




# Building the larger model ------------------------------------------------

# Make list of pitch types with more than a thousand pitches

# pitch_types_with_many_pitches <- data_2018 %>%
#   count(pitch_name) %>%
#   filter(n > 10000) %>%
#   select(pitch_name) %>%
#   filter(pitch_name != "") %>%
#   as.vector()

# Clean the data

lots_of_pitches <- c("2-Seam Fastball", "4-Seam Fastball", "Changeup", "Curveball", "Cutter", "Knuckle Curve", "Sinker", "Slider", "Split Finger")

groomed_data <- both_years %>%
  filter(pitch_name %in% lots_of_pitches) %>%
  filter(!is.na(pfx_x)) %>%
  filter(!is.na(pfx_z)) %>% 
  filter(!is.na(release_speed)) %>%
  filter(!is.na(vx0)) %>%
  filter(!is.na(vy0)) %>% 
  filter(!is.na(vz0)) %>%
  filter(!is.na(ax)) %>%
  filter(!is.na(ay)) %>%
  filter(!is.na(az)) %>%
  mutate(new_event = ifelse(!is.na(events), paste(events), paste(description))) %>%
  mutate(new_event = ifelse(new_event == "strikeout", paste(description), paste(new_event))) %>%
  mutate(new_event = ifelse(new_event == "walk", paste(description), paste(new_event))) %>%
  filter(new_event %nin% c("sac_fly_double_play", "pickoff_2b", "sac_bunt_double_play", 
                           "pickoff_caught_stealing_home", "pickoff_3b", 
                           "pickoff_caught_stealing_3b", "run", "batter_interference", "pitchout", 
                           "bunt_foul_tip", "catcher_interf", "caught_stealing_home", 
                           "caught_stealing_3b", "pickoff_1b", "other_out", "missed_bunt", 
                           "caught_stealing_2b", "foul_bunt", "sac_bunt", "pickoff_caught_stealing_2b")) %>%
  mutate(newer_event = recode(new_event, 
                              swinging_strike = "strike",
                              swinging_strike_blocked = "strike",
                              called_strike = "strike",
                              strikeout_double_play = "strike",
                              blocked_ball = "ball",
                              foul_tip = "foul",
                              grounded_into_double_play = "in_play_out",
                              field_out = "in_play_out",
                              field_error = "in_play_out",
                              force_out = "in_play_out",
                              sac_fly = "in_play_out",
                              fielders_choice_out = "in_play_out",
                              fielders_choice = "in_play_out",
                              triple_play = "in_play_out",
                              double_play = "in_play_out"))
  



  
# # Splitting into test and train
# 
# data_split <- initial_split(groomed_data, prop = .75)
# 
# # Extract the training dataframe
# 
# training_data <- training(data_split)
# 
# # Extract the testing dataframe
# 
# testing_data <- testing(data_split)
# 
# # Creating list column structure

nested_training_data <- training_data %>%
  group_by(pitch_name) %>%
  nest()

# Set seed

set.seed(42)

# cross-validation --------------------------------------------------------

# Deprecated, because random forests hold out some data to test against (OOB),
# but useful in case I switch model types.

# # 
# cv_split <- nested_training_data %>% 
#   mutate(stuff = map(data, ~ vfold_cv(.x, v = 5))) 
# 
# full_nested_training_set <- cv_split %>% 
#   unnest(stuff) 
# 
# splitted <- full_nested_training_set %>%
#   mutate(
#     
#     # Extract the train dataframe for each split
  #   
  #   train = map(splits, ~training(.x)), 
  #   
  #   # Extract the validate dataframe for each split
  #   
  #   validate = map(splits, ~testing(.x))
  # )


# running model -----------------------------------------------------------



models_only_velo <- nested_training_data %>% 
  mutate(model = map(data, ~ranger(formula = new_event ~ pfx_x + pfx_z + release_speed, 
                                    data = .x, 
                                    num.trees = 100, seed = 42, probability = T)))


# How to name rows -- not needed now but still useful

# .rowNamesDF(models_only_velo, make.names=FALSE) <- c("4-Seam Fastball", "Changeup", "2-Seam Fastball", "Slider", "Curveball", "Split Finger", "Sinker", "Cutter", "Knuckle Curve")

# Deprecated test on Gerrit Cole, but a useful example to keep that shows
# individual pitcher workflow, as well as how to sample from a pitcher's
# velocity distribution if I decide to go that route.

# models_only_velo$row.names
# cole <- testing_data %>%
#   filter(player_name == "Gerrit Cole") %>%
#   filter(pitch_name == "4-Seam Fastball")
# 
# cole %>% ggplot() +
#   geom_point(aes(x = release_speed, y = pfx_z))
# 
# cole %>%
#   summarise(mean = mean(release_speed),
#             sd = sd(release_speed))

# rnorm(10, 96.53244, 1.220829)

# How to prepare weights

# weighted <- cole %>%
#   mutate(raw_weights = sample(c(1, 2), size = 413, replace = TRUE),
#          weights = as.double(raw_weights))

# weights_for_test <- as.vector(weighted$weights)
# 
# ranger(formula = new_event ~ pfx_x + pfx_z + release_speed, 
#        data = weighted,  case.weights = weights_for_test,
#        num.trees = 100, seed = 42, probability = T)


# By pitcher --------------------------------------------------------------


# Function to use on all players (with more than 25 of a given pitch)

grouping_function <- function(data) {
  
  data %>%
  group_by(player_name) %>%
  summarise(n = n(),
            release_speed = mean(release_speed),
            pfx_x = mean(pfx_x),
            pfx_z = mean(pfx_z)) %>%
  arrange(desc(n)) %>%
  slice(1:4)

}
  
# Run the grouping function on every row using purrr.

with_all <- models_only_velo %>%
  mutate(pitcher = map(data, ~grouping_function(.x)))

# MAP 2 should do the trick. Also you need to make it into a dataframe so unnest
# will work (doesn't work on matrices, only dataframes).

with_preds <- with_all %>% 
  mutate(predict = map2(model, pitcher, ~as.data.frame(predict(.x, .y)$predictions)))


# Unnesting for each pitch type -------------------------------------------

# Unnest for each pitch type and save as individual object.

fastball_four_seam <- with_preds %>% 
  filter(pitch_name == "4-Seam Fastball") %>%
  unnest(c(pitcher, predict))

fastball_four_seam <- fastball_four_seam[, -c(2,3)]

changeup <- with_preds %>% 
  filter(pitch_name == "Changeup") %>%
  unnest(c(pitcher, predict))

changeup <- changeup[, -c(2,3)]

fastball_two_seam <- with_preds %>% 
  filter(pitch_name == "2-Seam Fastball") %>%
  unnest(c(pitcher, predict))

fastball_two_seam <- fastball_two_seam[, -c(2,3)]

slider <- with_preds %>% 
  filter(pitch_name == "Slider") %>%
  unnest(c(pitcher, predict))

slider <- slider[, -c(2,3)]

curveball <- with_preds %>% 
  filter(pitch_name == "Curveball") %>%
  unnest(c(pitcher, predict))

curveball <- curveball[, -c(2,3)]

split_finger <- with_preds %>% 
  filter(pitch_name == "Split Finger") %>%
  unnest(c(pitcher, predict))

split_finger <- split_finger[, -c(2,3)]

sinker <- with_preds %>% 
  filter(pitch_name == "Sinker") %>%
  unnest(c(pitcher, predict))

sinker <- sinker[, -c(2,3)]

cutter <- with_preds %>% 
  filter(pitch_name == "Cutter") %>%
  unnest(c(pitcher, predict))

cutter <- cutter[, -c(2,3)]

knuckle_curve <- with_preds %>% 
  filter(pitch_name == "Knuckle Curve") %>%
  unnest(c(pitcher, predict))

knuckle_curve <- knuckle_curve[, -c(2,3)]

# Save as RDSs ------------------------------------------------------------

# Save all of these as RDSs to the shiny folder, as they will be used to create
# graphs in the shiny.

write_rds(fastball_four_seam, "final_shiny/fastball_four_seam.rds")
write_rds(changeup, "final_shiny/changeup.rds")
write_rds(slider, "final_shiny/slider.rds")
write_rds(curveball, "final_shiny/curveball.rds")



