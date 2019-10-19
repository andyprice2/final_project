# Use to force install of Baseballr package: 
# devtools::install_github("BillPetti/baseballr", force = TRUE)

library(baseballr)
library(tidyverse)

# For scraping pitch fx data (mostly unneccessary, as baseballr can do most of this)

library(pitchRx)

# For storing pitch fx data 

library(RSQLite)



scrape_pitch <- function(pitch, start_date = "2018-03-01", end_date = Sys.Date()) {
  name1 <<- scrape_statcast_savant(start_date = start_date, end_date = end_date, player_type='pitcher',
      envir = .GlobalEnv)
  paste(pitch, "_", start_date, "_to_",  end_date, sep = "") <<- name1

# %>%
  # filter(pitch_type == pitch) %>% 
  # group_by(player_name) %>% 
  # summarize(mean_pfx_z = mean(pfx_z)) %>%
  # arrange(desc(mean_pfx_z))
}

scrape_pitch(pitch = "CU", start_date = "2018-04-02", end_date = "2018-04-04")
