# For each week in each season, weighs each quarterback's average EPA / dropback 
# up to that point in the season against an average of that quarterback's
# season by season efficiency stats, where the weight on the current season is 
# based on the number of dropbacks so far that season.


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(zoo)

source("userDefinedValues.R")


# Calculations ------------------------------------------------------------

games <- read_rds("data/games.rds")
qbs_weightedavg_byseason <- read_rds("data/quarterbacks_weightedavg_byseason.rds")
qbs_historical_byweek <- read_rds("data/quarterbacks_historical_byweek.rds")

# for each week in each season, calculates each quarterback's EPA / dropback
# and number of dropbacks up until that point in the season
data_list <- list()
i <- 1
for (yr in (first_usable:this_season)) {

  # gets vector of quarterbacks who played in the "yr" season
  quarterbacks_yr <-
    qbs_historical_byweek %>%
    filter(season == yr) %>%
    distinct(quarterback) %>%
    pull()

  # adds a row for week one
  data_list[[i]] <- data.frame(this_season = yr, 
                               quarterback = quarterbacks_yr,
                               coming_into_week = 1, 
                               dropbacks_this_season = 0,
                               epa_per_dropback_this_season = 0)
  i <- i + 1

  for (wk in 2:17) {
    if (yr != this_season | (yr == this_season & wk <= this_week)) {
      stats_coming_into_this_week <-
        qbs_historical_byweek %>%
        filter(season == yr,
               week < wk) %>%
        mutate(coming_into_week = wk,
               total_dropback_epa = dropback_epa * dropbacks) %>%
        group_by(this_season = season, quarterback, coming_into_week) %>%
        summarize(
          dropbacks_this_season = sum(dropbacks),
          dropback_epa_this_season = sum(total_dropback_epa) / sum(dropbacks)
        ) %>%
        ungroup()
      data_list[[i]] <- stats_coming_into_this_week
      i <- i + 1     
    }
  }
}

# for quarterbacks who don't play week one, the week one dropbacks_this_season
# and dropback_epa variables are assigned to be zero
qbs_chronological_byweek <- 
  bind_rows(data_list) %>%
  complete(this_season, coming_into_week, quarterback) %>%
  arrange(this_season, quarterback, coming_into_week) %>%
  mutate(dropbacks_this_season = if_else((this_season == first_usable &
                                         coming_into_week == 1 &
                                         is.na(dropbacks_this_season)),
                                         0, dropbacks_this_season),
         dropback_epa_this_season = if_else((this_season == first_usable &
                                                coming_into_week == 1 &
                                                is.na(dropback_epa_this_season)),
                                                0, dropback_epa_this_season))

# the below code sets the season dropbacks_this_season and dropback_epa variables of 
# quarterbacks who don't play in a given week to be the value of those variables 
# when they played their most recent game that season, or, in the case of zero
# snaps that season, zeros

filled_dropbacks_this_season <-
  qbs_chronological_byweek %>%
  pull(dropbacks_this_season) %>%
  na.locf()

qbs_chronological_byweek$dropbacks_this_season <- 
  filled_dropbacks_this_season

filled_dropback_epa_this_season <-
  qbs_chronological_byweek %>%
  pull(dropback_epa_this_season) %>%
  na.locf()

qbs_chronological_byweek$dropback_epa_this_season <- 
  filled_dropback_epa_this_season

# for each week in each season, weighs each quarterback's efficiency stats 
# that season against an exponential moving average of prior season-by-season 
# efficiency stats, where the weight on the current season is based on 
# the number of dropbacks so far that season
prior_weighted_performance <-
  left_join(qbs_chronological_byweek, qbs_weightedavg_byseason, 
            by = c("this_season" = "coming_into_season", "quarterback")) %>%
  mutate(this_season_weight = (dropbacks_this_season / full_passing_credibility) ^ learning_factor,
         this_season_weight = pmin(this_season_weight, 1),
         prior_weight = 1 - this_season_weight,
         projected_dropback_epa = this_season_weight * dropback_epa_this_season +
                                     prior_weight * career_dropback_epa
         ) %>%
  select(this_season, coming_into_week, quarterback, projected_dropback_epa)

write_rds(prior_weighted_performance, "data/quarterbacks_projected_efficiency.rds")
