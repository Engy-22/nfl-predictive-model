# Calculates a rush / pass decisionmaking "score" for each team
# that measures how often it made the optimal rush / pass decision 
# relative to the average NFL coach based on historical EPA per 
# play in similar game situations.


# Load Packages -----------------------------------------------------------

library(tidyverse)


# Functions ---------------------------------------------------------------

# finds the optimal rush/pass decision for each play
# based on historical EPA in similar situations
optimalDecision <- function(down, ydstogo, yardline_100, ...) {
  pbp_threedowns %>%
    rename(down_number = down, 
           yds_to_firstdown = ydstogo, 
           yds_to_goal = yardline_100) %>%
    filter(between(yds_to_goal, yardline_100 - 2, yardline_100 + 2),
           between(yds_to_firstdown, ydstogo - 1, ydstogo + 1),
           down_number == down) %>%
    group_by(pass) %>%
    summarize(
      mean_epa = mean(epa)
    ) %>%
    filter(mean_epa == max(mean_epa)) %>%
    pull(pass) %>%
    return()
}

# calculates the percent of times for a given situation that coaches
# made the optimal rush/pass decision in similar situations
avgDecision <- function(down, ydstogo, yardline_100, ...) {
  consolidated_decisions %>%
    rename(down_number = down, 
           yds_to_firstdown = ydstogo, 
           yds_to_goal = yardline_100) %>%
    filter(between(yds_to_goal, yardline_100 - 2, yardline_100 + 2),
           between(yds_to_firstdown, ydstogo - 1, ydstogo + 1),
           down_number == down) %>%
    summarize(
      n = sum(n),
      sum_of_decisions = sum(sum_of_decisions),
      mean_decision = sum_of_decisions / n
    ) %>%
    pull(mean_decision) %>%
    return()
}


# Calculations ------------------------------------------------------------

pbp <- read_rds("data/pbp_rushpass.rds")

pbp_threedowns <-
  pbp %>%
  filter(pass == TRUE | rush == TRUE,
         down != 4,
         qtr != 4,
         abs(score_differential) <= 14,
         !is.na(epa)) %>%
  select(posteam, season, week, down, ydstogo, yardline_100, pass, epa)
  
# creates a data frame containing the optimal rush/pass decision for 
# each game situation based on historical EPA in similar situations
optimal_decisions <-
  pbp_threedowns %>%
  distinct(down, ydstogo, yardline_100) %>%
  mutate(optimal_decision = pmap_lgl(., optimalDecision))

# for each play, determines whether coach made optimal rush/pass decision
decisionmaking <-
  pbp_threedowns %>%
  left_join(optimal_decisions, by = c("down", "ydstogo", "yardline_100")) %>%
  mutate(made_correct_decision = pass == optimal_decision)

# improves computation time by aggregating situation/decision combinations
consolidated_decisions <-
  decisionmaking %>%
  group_by(down, ydstogo, yardline_100) %>%
  summarize(
    n = n(),
    sum_of_decisions = sum(made_correct_decision)
  ) %>%
  ungroup()

# creates a field containing the percent of times for a given situation
# that coaches made the optimal rush/pass decision in similar situations
avg_decisions <-
  pbp_threedowns %>%
  distinct(down, ydstogo, yardline_100) %>%
  mutate(avg_decision = pmap_dbl(., avgDecision))

# for each team season, scores the coach's rush / pass decisionmaking
# relative to the average NFL coach
season_rushpass_scores <-
  decisionmaking %>%
  left_join(avg_decisions, by = c("down", "ydstogo", "yardline_100")) %>%
  mutate(decisionmaking_above_expectation = made_correct_decision - avg_decision) %>%
  group_by(season, team = posteam) %>%
  summarize(
    rushpass_score = mean(decisionmaking_above_expectation)
  ) %>%  
  arrange(desc(rushpass_score))

# for each game, scores the coach's rush / pass decisionmaking
# relative to the average NFL coach
weekly_rushpass_scores <-
  decisionmaking %>%
  left_join(avg_decisions, by = c("down", "ydstogo", "yardline_100")) %>%
  mutate(decisionmaking_above_expectation = made_correct_decision - avg_decision) %>%
  group_by(season, team = posteam, week) %>%
  summarize(
    plays = n(),
    rushpass_score = mean(decisionmaking_above_expectation)
  ) %>%  
  arrange(desc(rushpass_score))

# exports the resulting data frame to an .rds file
write_rds(season_rushpass_scores, "data/rushpass_scores_historical_byseason.rds")
write_rds(weekly_rushpass_scores, "data/rushpass_scores_historical_byweek.rds")
