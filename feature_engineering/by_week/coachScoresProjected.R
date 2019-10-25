# For each week in each season, weighs average decisionmaking scores 
# up until that point in the season against an average of that coach's 
# career decisionmaking scores, where the weight on the current season
# is based on the number of decisions made so far that season.

# Load Packages -----------------------------------------------------------

library(tidyverse)

source("userDefinedValues.R")


# Calculations ------------------------------------------------------------

coaches <- read_rds("data/coaches.rds")
coach_scores_weightedavg_byseason <- read_rds("data/coach_scores_weightedavg_byseason.rds")
fourthdown_scores_byweek <- read_rds("data/fourthdown_scores_historical_byweek.rds")
rushpass_scores_byweek <- read_rds("data/rushpass_scores_historical_byweek.rds")

coach_scores_byweek <-
  coaches %>%
  left_join(fourthdown_scores_byweek, by = c("season", "team")) %>%
  left_join(rushpass_scores_byweek, by = c("season", "team", "week")) %>%
  select(-team) %>%
  arrange(coach, season, week)

# for each week in each season, calculates each coach's average
# decisionmaking score and number of decisions made up until
# that point in the season
data_list <- list()
i <- 1
for (yr in (first_usable:this_season)) {
  coach_names_yr <-
    coach_scores_byweek %>%
    filter(season == yr) %>%
    distinct(coach)
  
  # adds a row for week one
  data_list[[i]] <- data.frame(this_season = yr, 
                               coming_into_week = 1, 
                               coach = coach_names_yr,
                               fourthdowns_this_season = 0,
                               fourthdown_score_this_season = 0,
                               plays_this_season = 0,
                               rushpass_score_this_season = 0)
  i <- i + 1

  for (wk in 2:17) {
    if (yr != this_season | (yr == this_season & wk <= this_week)) {
      scores_coming_into_this_week <-
        coach_scores_byweek %>%
        filter(season == yr,
               week < wk) %>%
        mutate(coming_into_week = wk,
               weighted_fourthdown_score = fourthdown_score * fourthdowns,
               weighted_rushpass_score = rushpass_score * plays) %>%
        group_by(this_season = season, coming_into_week, coach) %>%
        summarize(
          fourthdowns_this_season = sum(fourthdowns),
          fourthdown_score_this_season = sum(weighted_fourthdown_score) / sum(fourthdowns),
          plays_this_season = sum(plays),
          rushpass_score_this_season = sum(weighted_rushpass_score) / sum(plays)
        ) %>%
        ungroup()
      data_list[[i]] <- scores_coming_into_this_week
      i <- i + 1     
    }
  }
}

coach_scores_chronological_byweek <- 
  bind_rows(data_list)

# for each week in each season, weighs current-season average decisionmaking
# against an exponential moving average of prior season-by-season decisionmaking 
# scores, wherethe weight on the current season is based on the number of decisions 
# made so far that season
prior_weighted_scores <-
  left_join(coach_scores_chronological_byweek, coach_scores_weightedavg_byseason, 
            by = c("this_season" = "coming_into_season", "coach")) %>%
  mutate(this_season_fourthdown_weight = (fourthdowns_this_season / full_fourthdown_credibility) ^ learning_factor,
         this_season_fourthdown_weight = pmin(this_season_fourthdown_weight, 1),
         prior_fourthdown_weight = 1 - this_season_fourthdown_weight,
         projected_fourthdown_score = this_season_fourthdown_weight * fourthdown_score_this_season +
                                      prior_fourthdown_weight * career_fourthdown_score,
         this_season_rushpass_weight = (plays_this_season / full_rushpass_credibility) ^ learning_factor,
         this_season_rushpass_weight = pmin(this_season_rushpass_weight, 1),
         prior_rushpass_weight = 1 - this_season_rushpass_weight,
         projected_rushpass_score = this_season_rushpass_weight * rushpass_score_this_season +
                                    prior_rushpass_weight * career_rushpass_score
  ) %>%
  select(this_season, coming_into_week, coach,
         projected_fourthdown_score, projected_rushpass_score
  ) %>%
  as_tibble()

write_rds(prior_weighted_scores, "data/coach_scores_projected.rds")
