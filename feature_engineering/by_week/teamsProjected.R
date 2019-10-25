# For each week in each season, weighs average team efficiency stats up 
# up to that point in the season against an average of that team's season 
# by season efficiency stats, where the weight on the current season is 
# based on the number of plays of a given type so far that season.


# Load Packages -----------------------------------------------------------

library(tidyverse)

source("userDefinedValues.R")


# Calculations ------------------------------------------------------------

games <- read_rds("data/games.rds")
teams_weightedavg_byseason <- read_rds("data/teams_weightedavg_byseason.rds")
teams_historical_byweek <- read_rds("data/teams_historical_byweek.rds")

# vector of stats to be projected
chronological_stats <- 
  c("passes",
    "offensive_passing_epa",
    "defensive_passing_epa_faced",
    "rushes",
    "offensive_rushing_epa",
    "defensive_rushing_epa_faced",
    "passes_against",
    "defensive_passing_epa",
    "offensive_passing_epa_faced",
    "rushes_against",
    "defensive_rushing_epa",
    "offensive_rushing_epa_faced",
    "special_teams_plays",
    "special_teams_epa")

nvar <- length(chronological_stats)

# for each week in each season, calculates each team's efficiency stats
# and number of plays of each type made up until that point in the season
data_list <- list()
i <- 1
for (yr in (first_usable:this_season)) {
  
  # creates rows for each team coming into week one
  temp1 <- data.frame(season = yr, coming_into_week = 1, team = teams)
  temp2 <- data.frame(matrix(rep(0, nvar * 32), ncol = nvar, nrow = 32))
  names(temp2) <- str_c(chronological_stats, "_this_season")
  
  week_one_rows <- data.frame(temp1, temp2)
  data_list[[i]] <- week_one_rows
  i <- i + 1
  
  for (wk in 2:17) {
    if (yr != this_season | (yr == this_season & wk <= this_week)) {
      teams_coming_into_this_week <-
        teams_historical_byweek %>%
        filter(season == yr,
               week < wk) %>%
        mutate(coming_into_week = wk,
               total_offensive_passing_epa = offensive_passing_epa * passes,
               total_offensive_rushing_epa = offensive_rushing_epa * rushes,
               total_defensive_passing_epa = defensive_passing_epa * passes_against,
               total_defensive_rushing_epa = defensive_rushing_epa * rushes_against,
               total_special_teams_epa = special_teams_epa * special_teams_plays
        ) %>%
        group_by(season, coming_into_week, team) %>%
        summarize(
          passes_this_season = sum(passes),
          offensive_passing_epa_this_season = sum(total_offensive_passing_epa) / sum(passes),
          rushes_this_season = sum(rushes),
          offensive_rushing_epa_this_season = sum(total_offensive_rushing_epa) / sum(rushes),
          passes_against_this_season = sum(passes_against),
          defensive_passing_epa_this_season = sum(total_defensive_passing_epa) / sum(passes_against),
          rushes_against_this_season = sum(rushes_against),
          defensive_rushing_epa_this_season = sum(total_defensive_rushing_epa) / sum(rushes_against),
          special_teams_plays_this_season = sum(special_teams_plays),
          special_teams_epa_this_season = sum(total_special_teams_epa) / sum(special_teams_plays)
          ) %>%
        ungroup()
      data_list[[i]] <- teams_coming_into_this_week
      i <- i + 1     
    }
  }
}

teams_chronological_byweek <- bind_rows(data_list)

# for each week in each season, weighs each team's efficiency stats 
# that season against an exponential moving average of prior season-by-season 
# efficiency stats, where the weight on the current season is based on 
# the number of plays of a given type so far that season
prior_weighted_performance <-
  left_join(teams_chronological_byweek, teams_weightedavg_byseason, 
            by = c("season" = "coming_into_season", "team")) %>%
  mutate(this_season_offensive_passing_weight = (passes_this_season / full_passing_credibility) ^ learning_factor,
         this_season_offensive_passing_weight = pmin(this_season_offensive_passing_weight, 1),
         prior_passing_weight = 1 - this_season_offensive_passing_weight,
         projected_offensive_passing_epa = this_season_offensive_passing_weight * offensive_passing_epa_this_season +
                                           prior_passing_weight * weightedavg_offensive_passing_epa,
         this_season_offensive_rushing_weight = (rushes_this_season / full_rushing_credibility) ^ learning_factor,
         this_season_offensive_rushing_weight = pmin(this_season_offensive_rushing_weight, 1),
         prior_rushing_weight = 1 - this_season_offensive_rushing_weight,
         projected_offensive_rushing_epa = this_season_offensive_rushing_weight * offensive_rushing_epa_this_season +
                                           prior_rushing_weight * weightedavg_offensive_rushing_epa,
         this_season_defensive_passing_weight = (passes_against_this_season / full_defense_credibility) ^ learning_factor,
         this_season_defensive_passing_weight = pmin(this_season_defensive_passing_weight, 1),
         prior_defensive_passing_weight = 1 - this_season_defensive_passing_weight,
         projected_defensive_passing_epa = this_season_defensive_passing_weight * defensive_passing_epa_this_season +
                                           prior_defensive_passing_weight * weightedavg_defensive_passing_epa,
         this_season_defensive_rushing_weight = (rushes_against_this_season / full_defense_credibility) ^ learning_factor,
         this_season_defensive_rushing_weight = pmin(this_season_defensive_rushing_weight, 1),
         prior_defensive_rushing_weight = 1 - this_season_defensive_rushing_weight,
         projected_defensive_rushing_epa = this_season_defensive_rushing_weight * defensive_rushing_epa_this_season +
                                           prior_defensive_rushing_weight * weightedavg_defensive_rushing_epa,
         this_season_special_teams_weight = (special_teams_plays_this_season / full_special_teams_credibility) ^ learning_factor,
         this_season_special_teams_weight = pmin(this_season_special_teams_weight, 1),
         prior_special_teams_weight = 1 - this_season_special_teams_weight,
         projected_special_teams_epa = this_season_special_teams_weight * special_teams_epa_this_season +
                                       prior_special_teams_weight * weightedavg_special_teams_epa
         ) %>%
  select(season, team, coming_into_week, projected_offensive_passing_epa, 
         projected_offensive_rushing_epa, projected_defensive_passing_epa,
         projected_defensive_rushing_epa, projected_special_teams_epa) %>%
  as_tibble()

write_rds(prior_weighted_performance, "data/teams_projected_efficiency.rds")
