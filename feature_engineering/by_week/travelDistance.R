# For each game, calculates the away team's travel distance.


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(geosphere)


# Calculations ------------------------------------------------------------

games <- read_rds("data/games.rds")
stadiums <- read_rds("data/stadiums.rds")

home_games <-
  games %>%
  distinct(game_id, team = home_team, season, week, location = home_team)

away_games <-
  games %>%
  distinct(game_id, team = away_team, season, week, location = home_team)

# for each game, finds the location each team traveled from
lagged_locations <-
  bind_rows(home_games, away_games) %>%
  arrange(team, season, week) %>%
  complete(team, season, week) %>% # adds bye weeks
  mutate(location = pmap_chr(., function(location, team, ...) replace_na(location, team))
         ) %>% # sets the location of bye weeks to the home location
  group_by(team, season) %>%
  mutate(lagged_location = lag(location)) %>%
  ungroup() %>%
  mutate(lagged_location = pmap_chr(., function(lagged_location, team, week, ...) if_else(week == 1, team, lagged_location))
         ) %>% # sets the lagged location of week 1 games to the home location
  filter(!is.na(game_id)) %>% # removes bye weeks
  select(game_id, team, lagged_location)

# keep only away team lagged locations
away_lagged_locations <-
  semi_join(lagged_locations, away_games, by = c("game_id", "team"))

# gets latitude and longitude data for the home team's location
# and the away team's lagged location
games_lat_long <-
  left_join(games, stadiums, by = c("home_team" = "team", "season")) %>%
  select(game_id, season, lat, lon) %>%
  left_join(away_lagged_locations, by = c("game_id")) %>%
  left_join(stadiums, by = c("lagged_location" = "team", "season")) %>%
  select(game_id, lat = lat.x, lon = lon.x, lagged_lat = lat.y, lagged_lon = lon.y)

# calculates the distance the away team traveled
travel_distance <-
  games_lat_long %>%
  mutate(travel_distance = pmap_dbl(., function(lat, lon, lagged_lat, lagged_lon, ...) distm(c(lon, lat), c(lagged_lon, lagged_lat), fun = distHaversine)),
         travel_distance = travel_distance / 1000) %>%
  select(game_id, travel_distance)

# exports the resulting data frame to an .rds file
write_rds(travel_distance, "data/travel_distance.rds")


