# Imports an NFL stadium CSV database that I created and
# creates a row for each team / season combination.


# Load Packages -----------------------------------------------------------

library(tidyverse)

source("userDefinedValues.R")


# Import Stadium Data -----------------------------------------------------

stadiums_raw <- 
  read_csv("data/stadiums.csv")

# creates a row for each team / season combination
stadiums <-
  stadiums_raw %>%
  mutate(last_season = replace_na(last_season, this_season)) %>%
  mutate(seasons = pmap(., 
                        function(first_season, last_season, ...) {
                          seq(from = first_season, to = last_season, by = 1)
                        }
  )) %>%
  select(-first_season, -last_season) %>%
  unnest(c(seasons)) %>%
  distinct(team, season = seasons, stadium_name, has_dome, lat, lon, altitude)

# exports the resulting data frame to an .rds file
write_rds(stadiums, "data/stadiums.rds")
