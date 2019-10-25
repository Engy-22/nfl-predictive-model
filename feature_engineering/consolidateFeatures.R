# Consolidates all model features calculated in previous steps into a
# single data frame and saves that data frame to file.

# Load Packages -----------------------------------------------------------

library(tidyverse)

source("userDefinedValues.R")

# Consolidate Data --------------------------------------------------------

games <- read_rds("data/games.rds")
vegas_odds <- read_rds("data/vegas_odds.rds")
stadiums <- 
  read_rds("data/stadiums.rds") %>%
  select(-stadium_name, -lat, -lon)
coaches <- read_rds("data/coaches.rds")
homeaway_qbs <- read_rds("data/homeaway_qbs.rds")
qb_projected_efficiency <- read_rds("data/quarterbacks_projected_efficiency.rds")
teams_projected_efficiency <- read_rds("data/teams_projected_efficiency.rds")
coaches_projected_scores <- read_rds("data/coach_scores_projected.rds")
travel_distance <- read_rds("data/travel_distance.rds")
byes <- read_rds("data/byes.rds")
weather <- read_rds("data/weather.rds")


# limits games to seasons used in the final training/test sets
games <- filter(games, season >= first_usable)

# for each game, joins all of the data created in previous steps
final_data <-
  games %>%
  left_join(vegas_odds, by = "game_id") %>%
  left_join(homeaway_qbs, by = "game_id") %>%
  left_join(coaches, by = c("season", "home_team" = "team")) %>%
  rename("home_coach" = "coach") %>%
  left_join(coaches, by = c("season", "away_team" = "team")) %>%
  rename("away_coach" = "coach") %>%
  left_join(qb_projected_efficiency, 
          by = c("season" = "this_season",
                 "week" = "coming_into_week",
                 "home_qb" = "quarterback")) %>%
  rename(home_qb_projected_dropback_epa = projected_dropback_epa) %>%
  left_join(qb_projected_efficiency, 
          by = c("season" = "this_season",
                 "week" = "coming_into_week",
                 "away_qb" = "quarterback")) %>%
  rename(away_qb_projected_dropback_epa = projected_dropback_epa) %>%
  left_join(teams_projected_efficiency, 
            by = c("season",
                   "week" = "coming_into_week",
                   "home_team" = "team")) %>%
  rename_at(vars(
    projected_offensive_passing_epa,
    projected_offensive_rushing_epa,
    projected_defensive_passing_epa,
    projected_defensive_rushing_epa,
    projected_special_teams_epa
    ), ~ str_c("home_", .)) %>%
  left_join(teams_projected_efficiency, 
            by = c("season",
                   "week" = "coming_into_week",
                   "away_team" = "team")) %>%
  rename_at(vars(
    projected_offensive_passing_epa,
    projected_offensive_rushing_epa,
    projected_defensive_passing_epa,
    projected_defensive_rushing_epa,
    projected_special_teams_epa
  ), ~ str_c("away_", .)) %>%
  left_join(coaches_projected_scores,
            by = c("season" = "this_season",
                   "week" = "coming_into_week",
                   "home_coach" = "coach")) %>%
  rename_at(vars(
    projected_fourthdown_score,
    projected_rushpass_score
    ), ~ str_c("home_", .)) %>%
  left_join(coaches_projected_scores,
            by = c("season" = "this_season",
                   "week" = "coming_into_week",
                   "away_coach" = "coach")) %>%
  rename_at(vars(
    projected_fourthdown_score,
    projected_rushpass_score
    ), ~ str_c("away_", .)) %>%
  left_join(travel_distance, by = "game_id") %>%
  left_join(byes, by = "game_id") %>%
  left_join(stadiums, by = c("home_team" = "team", "season")) %>%
  left_join(weather, by = "game_id") %>%
  mutate_at(vars(temperature, wind_speed, precip_probability),
            # if it's a dome game, set the weather data to NA
            ~ ifelse(has_dome == TRUE, NA, .)) 

write_rds(final_data, "data/final_data.rds")
