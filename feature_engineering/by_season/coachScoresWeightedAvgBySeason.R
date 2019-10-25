# For each season, calculates an exponential moving average of 
# past coach decisionmaking scores.


# Load Packages -----------------------------------------------------------

library(tidyverse)

source("userDefinedValues.R")


# Calculations ------------------------------------------------------------

coaches <- read_rds("data/coaches.rds")
fourthdown_scores_byseason <- read_rds("data/fourthdown_scores_historical_byseason.rds")
rushpass_scores_byseason <- read_rds("data/rushpass_scores_historical_byseason.rds")

coach_scores_byseason <-
  coaches %>%
  left_join(fourthdown_scores_byseason, by = c("season", "team")) %>%
  left_join(rushpass_scores_byseason, by = c("season", "team")) %>%
  select(-team) %>%
  arrange(coach)

# exponential moving average of coach season fourth-down 
# and rush-pass decisionmaking scores
data_list <- list()
i <- 2
for (yr in first_usable:this_season) {
  coach_names_yr <-
    coaches %>%
    filter(season == yr) %>%
    distinct(coach) %>%
    arrange(coach)

  weighted_scores <- data.frame(fourthdown_score = rep(0, 32),
                                rushpass_score = rep(0, 32))
  
  for (j in 1:i) {
    prior_yr_scores <-
      coach_scores_byseason %>%
      filter(season == yr - j)
    
    # for coaches with few seasons of experience (or no head coaching experience),
    # sets the prior years' decisionmaking scores to zero, regressing their scores
    # back to the mean decisionmaking score
    prior_yr_scores_NA <-
      left_join(data.frame(coach = coach_names_yr), 
                prior_yr_scores, by = c("coach")) %>%
      mutate(fourthdown_score = replace_na(fourthdown_score, 0),
             rushpass_score = replace_na(rushpass_score, 0))

    weighted_scores <-
      weighted_scores +
      coaching_persistency_weight ^ (j - 1) *
      prior_yr_scores_NA[, c("fourthdown_score", 
                            "rushpass_score")]
  }
  weighted_scores <- weighted_scores * (1 - coaching_persistency_weight)
  
  data_list[[yr]] <- data.frame(coach = coach_names_yr, 
                                coming_into_season = yr, 
                                weighted_scores)
  i <- i + 1
}

weighted_avg_coach_scores <-
  bind_rows(data_list) %>%
  rename_at(vars(fourthdown_score, rushpass_score),
            ~ str_c("career_", .)) %>%
  as_tibble()

write_rds(weighted_avg_coach_scores, "data/coach_scores_weightedavg_byseason.rds")
