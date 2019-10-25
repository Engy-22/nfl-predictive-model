# Calculates a weighted average of quarterback season by season data
# based on both sample size (dropbacks in a season) and recency.
# Assigns quarterbacks with little or no experience a replacement-level 
# EPA / dropback.


# Load Packages -----------------------------------------------------------

library(tidyverse)

source("userDefinedValues.R")


# Calculations ------------------------------------------------------------

qbs_historical_byseason <- read_rds("data/quarterbacks_historical_byseason.rds")

# fills out data frame with zeros for quarterbacks with no play in a given season
qbs_historical_byseason_complete <-
  qbs_historical_byseason %>%
  complete(season, quarterback) %>%
  mutate_at(vars(dropbacks, dropback_epa), ~ replace_na(., 0)) %>%
  select(quarterback, season, dropbacks, dropback_epa)

# for each season, calculates a volume-weighted average of each
# quarterback's career performance up to that point
data_list <- list()
for (yr in first_usable:this_season) {
  volume_weighted_avg <-
    qbs_historical_byseason_complete %>%
    filter(season < yr) %>%
    mutate(avg_term = dropbacks * dropback_epa) %>%
    group_by(quarterback) %>%
    summarise(
      volume_weighted_avg_dropback_epa = sum(avg_term) / sum(dropbacks)
      ) %>%
    mutate(volume_weighted_avg_dropback_epa = replace_na(volume_weighted_avg_dropback_epa, 0))
  data_list[[yr]] <- data.frame(coming_into_season = yr, volume_weighted_avg)
}

qbs_volume_weighted_avg_byseason <- 
  bind_rows(data_list) %>%
  select(quarterback, everything())

# for each quarterback in a season, calculates an exponential moving
# average of the volume-weighted career averages that were calculated 
# in the previous step
data_list <- list()
for (yr in first_usable:this_season) {
  exponential_moving_avg <-
    qbs_volume_weighted_avg_byseason %>%
    filter(coming_into_season <= yr) %>%
    mutate(years_back = yr - coming_into_season,
           avg_term = volume_weighted_avg_dropback_epa * 
                      qb_persistency_weight ^ years_back
    ) %>%
    group_by(quarterback) %>%
    summarise(
      exponential_moving_avg_dropback_epa = sum(avg_term)
      ) %>%
    mutate(exponential_moving_avg_dropback_epa = exponential_moving_avg_dropback_epa  * (1 - qb_persistency_weight))
  data_list[[yr]] <- data.frame(coming_into_season = yr, exponential_moving_avg)
}

qbs_exponential_moving_avg_byseason <-
  bind_rows(data_list) %>%
  select(quarterback, everything())

# for each season, calculates the number of dropbacks
# in each quarterbacks career up to that point
data_list = list()
for (yr in first_usable:this_season) {
  cumulative_dropbacks <-
    qbs_historical_byseason_complete %>%
    filter(season < yr) %>%
    group_by(quarterback) %>%
    summarise(career_dropbacks = sum(dropbacks))
  data_list[[yr]] <- data.frame(coming_into_season = yr, cumulative_dropbacks)
}

career_dropbacks <- bind_rows(data_list)

# quarterbacks that have no or little experience are assumed to be
# replacement-level players; that is, players with an EPA / dropback
# of approximately 0. as the sample size of career dropbacks increases,
# additional weight is given to a quarterback's observed EPA / dropback
career_performance <-
  left_join(qbs_exponential_moving_avg_byseason, career_dropbacks,
            by = c("quarterback", "coming_into_season")) %>%
  mutate(career_weight = (career_dropbacks / full_qb_career_credibility) ^ learning_factor,
         career_weight = pmin(career_weight, 1),
         replacement_level_weight = 1 - career_weight,
         career_dropback_epa = 
           career_weight * exponential_moving_avg_dropback_epa + replacement_level_weight * 0) %>%
  select(quarterback, coming_into_season, career_dropback_epa) %>%
  arrange(quarterback, coming_into_season) %>%
  as_tibble()

write_rds(career_performance, "data/quarterbacks_weightedavg_byseason.rds")

