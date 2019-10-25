# For each game, downloads weather information using the Dark Sky weather API.


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(darksky)

source("userDefinedValues.R")


# Import Dark Sky Weather Data ----------------------------------------------------

games <- read_rds("data/games.rds")
stadiums <- read_rds("data/stadiums.rds")

# combine game data and stadium data to get the
# date, time, longitude, and latitude of each game
games_stadiums <- 
  left_join(games, stadiums, by = c("home_team" = "team", "season")) %>%
  rename(dome_game = has_dome)

# calls Darksky weather API and puts the data into a data frame column
darksky_raw <-
  games_stadiums %>%
  mutate(timestamp = str_c(game_date, "T", time)) %>%
  rename(latitude = lat, longitude = lon) %>%
  mutate(weather = pmap(., get_forecast_for)) %>%
  select(game_id, weather)

# each Dark Sky function call costs $0.0001, so I'm storing the
# raw output in a separate .rds file
# darksky_raw <- write_rds(darksky_raw, "data/darksky_raw.rds")


# Process Dark Sky Weather Data ---------------------------------------------------

darksky_raw <- read_rds("data/darksky_raw.rds")

# extracts information from the Darksky weather data object
# and puts it into columns of a data frame
weather <-
  darksky_raw %>%
  mutate(current_weather = map(weather, function(x) '[['(x, "currently")),
         temperature = map_dbl(current_weather, function(x) '[['(x, "temperature")),
         wind_speed = map_dbl(current_weather, function(x) '[['(x, "windSpeed")),
         precip_probability = map(current_weather, function(x) '[['(x, "precipProbability")),
         precip_probability = as.double(replace_na(precip_probability, NA)),
         ) %>%
  select(-weather, -current_weather)

write_rds(weather, "data/weather.rds")
