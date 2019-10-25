
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(gbm) 
# limiting myself to a boosting model for now; will do a true
# training/test/hold-out set process to determine the best model
# for this data when I have more time
library(darksky)

source("userDefinedValues.R")


# Train Model -------------------------------------------------------------

final_data <- 
  read_rds("data/final_data.rds") %>%
  mutate(spread = home_score - away_score)

# drops games with one-off data issues
# (ie, MIA vs TB week one rescheduled because of a hurricane)
# will come back to fix some of these
final_data <-
  final_data %>%
  filter(!(game_id %in% c(2011121807,
                          2011121808,
                          2011122404,
                          2011122500,
                          2012010108,
                          2012010114,
                          2017091707,
                          2017091708,
                          2018091600,
                          2018091608,
                          2018091606,
                          2018091605,
                          2018091604,
                          2018091601)))

select_predictors <- function(data_frame) {
  data_frame %>%
    select(-game_id, -game_date, -season,
           -home_score, -away_score, -time, 
           -home_team, -away_team, 
           -home_qb, -away_qb, 
           -home_coach, -away_coach, -has_dome) %>% # weather data captures dome/non-dome
    # the gbm function cannot use logical and character types
    mutate_if(is.logical, as.factor) %>%
    mutate_if(is.character, as.factor)
}

boosting_data <- select_predictors(final_data)

boost <- gbm(spread ~ ., data = boosting_data, 
             distribution = "gaussian", n.trees = trees,
             interaction.depth = tree_depth)


# Make Predictions --------------------------------------------------------
# will eventually make this step automatic; code is sloppy and I should not
# need to type in any information in to make predictions. but it works!!

select_predictors(final_data)

home_team_pred <- "SF"
away_team_pred <- "CAR"

home_qb_pred <- "J.Garoppolo"
away_qb_pred <- "K.Allen"

teams_projected <- read_rds("data/teams_projected_efficiency.rds")
qbs_projected <- read_rds("data/quarterbacks_projected_efficiency.rds")
coach_scores_projected <- read_rds("data/coach_scores_projected.rds")
stadiums <- read_rds("data/stadiums.rds")
coaches <- read_rds("data/coaches.rds")

home_team_efficiency <-
  teams_projected %>%
  filter(team == home_team_pred,
         season == this_season,
         coming_into_week == this_week) %>%
  select(-team, -season, -coming_into_week) %>%
  rename_all(~ paste0("home_", .))

away_team_efficiency <-
  teams_projected %>%
  filter(team == away_team_pred,
         season == this_season,
         coming_into_week == this_week) %>%
  select(-team, -season, -coming_into_week) %>%
  rename_all(~ paste0("away_", .))


home_qb_efficiency <-
  qbs_projected %>%
  rename(season = this_season) %>%
  filter(quarterback == home_qb_pred,
         season == this_season,
         coming_into_week == this_week) %>%
  select(-quarterback, -season, -coming_into_week) %>%
  rename_all(~ paste0("home_qb_", .))

away_qb_efficiency <-
  qbs_projected %>%
  rename(season = this_season) %>%
  filter(quarterback == away_qb_pred,
         season == this_season,
         coming_into_week == this_week) %>%
  select(-quarterback, -season, -coming_into_week) %>%
  rename_all(~ paste0("away_qb_", .))

home_coach_pred <- 
  coaches %>%
  filter(season == this_season,
         team == home_team) %>%
  pull(coach)

away_coach_pred <- 
  coaches %>%
  filter(season == this_season,
         team == away_team) %>%
  pull(coach)

home_coach_scores <-
  coach_scores_projected %>%
  rename(season = this_season) %>%
  filter(coach == home_coach_pred,
         season == this_season,
         coming_into_week == this_week) %>%
  select(projected_fourthdown_score, projected_rushpass_score) %>%
  rename_all(~ paste0("home_", .))

away_coach_scores <-
  coach_scores_projected %>%
  rename(season = this_season) %>%
  filter(coach == away_coach_pred,
         season == this_season,
         coming_into_week == this_week) %>%
  select(projected_fourthdown_score, projected_rushpass_score) %>%
  rename_all(~ paste0("away_", .))

travel_distance <-
  final_data %>%
  filter(home_team == home_team_pred,
         away_team == away_team_pred) %>%
  tail(1) %>%
  pull(travel_distance)

stadium_data <-
  stadiums %>%
  filter(season == this_season,
         team == home_team_pred)

altitude <- stadium_data$altitude
has_dome <- stadium_data$has_dome

lat <- stadium_data$lat
lon <- stadium_data$lon

home_odds <- -256
away_odds <- +214

home_had_bye <- FALSE
away_had_bye <- TRUE

temperature <- 67
wind_speed <- 11
precip_probability <- 0

week <- this_week
day <- "Sunday"

predictors <-
  data.frame(home_team_efficiency, away_team_efficiency, home_qb_efficiency, away_qb_efficiency, 
           home_coach_scores, away_coach_scores, travel_distance, altitude, has_dome, temperature, 
           wind_speed, home_had_bye, away_had_bye, home_odds, away_odds, week)

predictors

predict(boost, newdata = predictors, n.trees = trees)
