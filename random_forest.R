# Load libraries and read-in data -----------------------------------------


# libraries

library(tidyverse)
library(baseballr)
library(janitor)
library(broom)
library(ranger)
library(rsample)


# read in data from csv written in last step

# data_2018 <- read.csv("with_res.csv")

# Test with two days of data ----------------------------------------------


# test random forest


test_for_random_forest <- data_2018 %>%
  filter(game_date %in% c("2018-05-21", "2018-05-22")) %>%
  filter(!is.na(pfx_x)) %>%
  filter(!is.na(pfx_z)) %>% 
  filter(!is.na(release_speed)) %>%
  filter(!is.na(description)) %>%
  mutate(new_event = ifelse(!is.na(events), paste(events), paste(description))) %>%
  mutate(new_event = ifelse(new_event == "strikeout", paste(description), paste(new_event))) %>%
  mutate(new_event = ifelse(new_event == "walk", paste(description), paste(new_event))) 

test_model <- ranger(formula = new_event ~ pfx_x + pfx_z + release_speed + vx0 + vy0 + vz0 + ax + ay + az, 
                     data = test_for_random_forest, 
                     num.trees = 100, seed = 42, probability = T) 

predicted <- data_2018 %>%
  filter(game_date %in% c("2018-06-21", "2018-06-22")) %>%
  filter(!is.na(pfx_x)) %>%
  filter(!is.na(pfx_z)) %>% 
  filter(!is.na(release_speed)) %>%
  filter(!is.na(description)) %>%
  select(pfx_x, pfx_z, release_speed)

predict(test_model, repeated)$predictions



# gerrit cole test
# repeated <- tibble(pfx_x = -1.0553,
# pfx_z = 1.2258,
# release_speed = 97.3)




# Building the larger model ------------------------------------------------

# Make list of pitch types with more than a thousand pitches

pitch_types_with_many_pitches <- data_2018 %>%
  count(pitch_name) %>%
  filter(n > 10000) %>%
  select(pitch_name) %>%
  filter(pitch_name != "") %>%
  as.vector()

# Clean the data

lots_of_pitches <- c("2-Seam Fastball", "4-Seam Fastball", "Changeup", "Curveball", "Cutter", "Knuckle Curve", "Sinker", "Slider", "Split Finger")

groomed_data <- data_2018 %>%
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
  mutate(new_event = ifelse(new_event == "walk", paste(description), paste(new_event))) 
  
# Splitting into test and train

data_split <- initial_split(groomed_data, prop = .75)

# Extract the training dataframe

training_data <- training(data_split)

# Extract the testing dataframe

testing_data <- testing(data_split)

# Creating list column structure

nested_training_data <- training_data %>%
  group_by(pitch_name) %>%
  nest()


# Set seed

set.seed(42)

# 
cv_split <- nested_training_data %>% 
  mutate(stuff = map(data, ~ vfold_cv(.x, v = 5))) 

full_nested_training_set <- cv_split %>% 
  unnest(stuff) 

splitted <- full_nested_training_set %>%
  mutate(
    
    # Extract the train dataframe for each split
    
    train = map(splits, ~training(.x)), 
    
    # Extract the validate dataframe for each split
    
    validate = map(splits, ~testing(.x))
  )

test_model <- ranger(formula = new_event ~ pfx_x + pfx_z + release_speed + vx0 + vy0 + vz0 + ax + ay + az, 
                     data = test_for_random_forest, 
                     num.trees = 100, seed = 42, probability = T) 

