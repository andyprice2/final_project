# final_project


reading_links.R contains a list of useful readings on the topics.

scraping_statcast.R creates a statcast database using purrr to scrape for every specific date in 2018 (and using scraping function from baseballR). Writes a CSV for this data so doesn't need to be run every time -- can simply be read in when needed. 

data_scraping.R is a deprecated script that reads in data from an external csv (as opposed to scraping) and includes a created function that calculates run environments for specific intervals -- not part of current workflow but parts of it will be necessary in the future. 

random_forest.R is where most of the important work is done. It cleans the data, splits it into a training and testing set, and then uses purrr to run a random forest analysis for every type of pitch.