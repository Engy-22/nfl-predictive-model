# Imports an NFL coaching history database that I created and
# creates a row for each coach / team-season combination.


# Load Packages -----------------------------------------------------------

library(tidyverse)

source("userDefinedValues.R")


# Import Coach Data -----------------------------------------------------

coaches_raw <- 
  read_csv("data/coaches.csv")

# creates a row for each coach / team-season combination
coaches <-
  coaches_raw %>%
  mutate(last_season = replace_na(last_season, this_season)) %>%
  mutate(seasons = pmap(., 
                        function(first_season, last_season, ...) {
                          seq(from = first_season, to = last_season, by = 1)
                        }
  )) %>%
  select(-first_season, -last_season) %>%
  unnest(c(seasons)) %>%
  distinct(coach, season = seasons, team) %>%
  arrange(coach, season)

# exports the resulting data frame to an .rds file
write_rds(coaches, "data/coaches.rds")
