# Calculates an exponential moving average of each team's offensive and defensive
# season by season performance.


# Load Packages -----------------------------------------------------------

library(tidyverse)

source("userDefinedValues.R")


# Calculations ------------------------------------------------------------

teams_historical_byseason <- 
  read_rds("data/teams_historical_byseason.rds") %>%
  arrange(season, team)

offensive_stats <- c("offensive_passing_epa",
                     "offensive_rushing_epa",
                     "defensive_passing_epa_faced",
                     "defensive_rushing_epa_faced")

defensive_stats <- c("defensive_passing_epa",
                     "defensive_rushing_epa",
                     "offensive_passing_epa_faced",
                     "offensive_rushing_epa_faced",
                     "special_teams_epa")

all_stats <- c(offensive_stats, defensive_stats)

# exponentially smoothed average of offensive and defensive performance
# over every prior season
data_list <- list()
i <- first_usable - first
for (yr in first_usable:this_season) {
  nvar_offense <- length(offensive_stats)
  nvar_defense <- length(defensive_stats)
  nvar <- nvar_offense + nvar_defense
  
  weighted_stats <-
    matrix(rep(0, nvar * 32), ncol = nvar, nrow = 32) %>%
    as.data.frame()
  names(weighted_stats) <- all_stats
  
  # assigns a different smoothing weight to offensive and defensive stats
  weight_vector <- c(rep(offensive_persistency_weight, nvar_offense), 
                     rep(defensive_persistency_weight, nvar_defense))
  
  for (j in 1:i) {
    prior_yr_stats <-
      teams_historical_byseason %>%
      filter(season == yr - j) %>%
      select(all_stats)
    
    weighted_stats <-
      weighted_stats + prior_yr_stats * weight_vector ^ (j - 1)
  }
  weighted_stats <- weighted_stats * (1 - weight_vector)
  
  data_list[[yr]] <- data.frame(coming_into_season = yr, team = teams, weighted_stats)
  i <- i + 1
}

teams_weightedavg_byseason <-
  bind_rows(data_list) %>%
  as_tibble() %>%
  rename_at(vars(-coming_into_season, -team),
            ~ str_c("weightedavg_", .))

write_rds(teams_weightedavg_byseason, "data/teams_weightedavg_byseason.rds")
