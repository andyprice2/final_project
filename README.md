# final_project

reading_links.R contains a list of useful readings on the topic. 

scraping_statcast.R scrapes data directly from Statcast, building a database by using Purrr to get around the site's request limit and scrape data for each day and then binding them together.

data_scraping.R is a depricated script that reads in an external csv (as opposed to scraping) of pitch data. It includes a created function that calculates run environment for any given date interval. Not currently a part of the workflow but parts will be useful down the road. 

random_forest.R is the most important script, and it's where I clean the data, split it into training and testing sets, and then use Purrr to run a random forest analysis on every type of pitch.

milestone5.Rmd contains an about page for the project.
