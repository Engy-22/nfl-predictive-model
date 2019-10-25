# Using the New York Times fourth-down decisionmaking model,
# calculates a fourth-down decisionmaking "score" for each team
# that compares its decisions to those made by NFL coaches in
# similar situations. This avoids penalizing teams for not going
# for it in situations in which NFL coaches would never go for it.


# Load Packages -----------------------------------------------------------

library(tidyverse)


# Functions ---------------------------------------------------------------

# based on the New York Times model, determines whether a given
# fourth-down decision was optimal, given the game situation
optimDecision <- function(ydstogo, yardline_100, play_type, ...) {
  goforit <- play_type %in% c("run", "pass")
  punt <- play_type == "punt"
  fieldgoal <- play_type == "field_goal"
  
  case_when(
    ydstogo == 1 & yardline_100 <= 91 & goforit |
      ydstogo == 1 & yardline_100 >  91 & punt ~ TRUE,
    
    ydstogo == 2 & yardline_100 <= 72 & goforit |
      ydstogo == 2 & yardline_100 >  72 & punt ~ TRUE,
    
    ydstogo == 3 & yardline_100 <  5  & goforit |
      ydstogo == 3 & between(yardline_100, 5, 20) & fieldgoal |
      ydstogo == 3 & between(yardline_100, 21, 59) & goforit |
      ydstogo == 3 & yardline_100 >  59 & punt ~ TRUE,
    
    ydstogo == 4 & yardline_100 <  4  & goforit |
      ydstogo == 4 & between(yardline_100, 4, 29) & fieldgoal |
      ydstogo == 4 & between(yardline_100, 29, 46) & goforit |
      ydstogo == 4 & yardline_100 >  46 & punt ~ TRUE,
    
    ydstogo == 5 & yardline_100 <  5  & goforit |
      ydstogo == 5 & between(yardline_100, 5, 33) & fieldgoal |
      ydstogo == 5 & between(yardline_100, 33, 50) & goforit |
      ydstogo == 5 & yardline_100 >  50 & punt ~ TRUE,
    
    ydstogo == 6 & yardline_100 <  6  & goforit |
      ydstogo == 6 & between(yardline_100, 6, 34) & fieldgoal |
      ydstogo == 6 & between(yardline_100, 34, 48) & goforit |
      ydstogo == 6 & yardline_100 >  48 & punt ~ TRUE,
    
    ydstogo == 7 & yardline_100 <  7  & goforit |
      ydstogo == 7 & between(yardline_100, 7, 36) & fieldgoal |
      ydstogo == 7 & between(yardline_100, 36, 44) & goforit |
      ydstogo == 7 & yardline_100 >  44 & punt ~ TRUE,
    
    ydstogo == 8 & yardline_100 <  8  & goforit |
      ydstogo == 8 & between(yardline_100, 8, 37) & fieldgoal |
      ydstogo == 8 & between(yardline_100, 37, 41) & goforit |
      ydstogo == 8 & yardline_100 >  41 & punt ~ TRUE,
    
    ydstogo == 9 & yardline_100 <  9  & goforit |
      ydstogo == 9 & between(yardline_100, 9, 38) & fieldgoal |
      ydstogo == 9 & between(yardline_100, 38, 39) & goforit |
      ydstogo == 9 & yardline_100 >  39 & punt ~ TRUE,
    
    ydstogo >  9 & yardline_100 < ydstogo & goforit |
      ydstogo >  9 & between(yardline_100, ydstogo, 37) & fieldgoal |
      ydstogo >  9 & yardline_100 > 37 & punt ~ TRUE,
    
    TRUE ~ FALSE
  ) %>%
    return()
}

# calculates the percent of times for a given situation that coaches
# made the optimal fourth-down decision in similar situations
avgDecision <- function(yds_to_firstdown, yds_to_goal) {
  consolidated_decisions %>%
    filter(between(yardline_100, yds_to_goal - 2, yds_to_goal + 2),
           ydstogo == yds_to_firstdown) %>%
    summarize(
      n = sum(n),
      sum_of_decisions = sum(sum_of_decisions),
      mean_decision = sum_of_decisions / n
    ) %>%
    pull(mean_decision) %>%
    return()
}


# Calculations ------------------------------------------------------------

pbp_all <- read_rds("data/pbp_all.rds")

pbp_fourthdown <- 
  pbp_all %>%
  filter(play_type == "pass" |
           play_type == "run" |
           play_type == "field_goal" |
           play_type == "punt",
         down == 4,
         qtr != 4) %>%
  select(posteam, season, week, ydstogo, yardline_100, play_type)

# for each play, determines whether coach made optimal fourth-down decision
decisionmaking <-
  pbp_fourthdown %>%
  mutate(made_correct_decision = pmap_dbl(., optimDecision))

# improves computation time by aggregating situation/decision combinations
consolidated_decisions <- 
  decisionmaking %>%
  group_by(ydstogo, yardline_100) %>%
  summarize(
    n = n(),
    sum_of_decisions = sum(made_correct_decision)
  )

# creates a field containing the percent of times for a given situation
# that coaches made the optimal fourth-down decision in similar situations
avg_decisions <-
  pbp_fourthdown %>%
  distinct(ydstogo, yardline_100) %>%
  mutate(avg_decision = map2_dbl(ydstogo, yardline_100, avgDecision))

# for each team season, scores the coach's fourth-down decisionmaking
# relative to the average NFL coach
season_fourthdown_scores <-
  decisionmaking %>%
  left_join(avg_decisions, by = c("yardline_100", "ydstogo")) %>%
  mutate(decisionmaking_above_expectation = made_correct_decision - avg_decision) %>%
  group_by(season, team =  posteam) %>%
  summarize(
    fourthdown_score = mean(decisionmaking_above_expectation)
  ) %>%  
  arrange(desc(fourthdown_score))

# for each game, scores the coach's fourth-down decisionmaking
# relative to the average NFL coach
weekly_fourthdown_scores <-
  decisionmaking %>%
  left_join(avg_decisions, by = c("yardline_100", "ydstogo")) %>%
  mutate(decisionmaking_above_expectation = made_correct_decision - avg_decision) %>%
  group_by(season, team =  posteam, week) %>%
  summarize(
    fourthdowns = n(),
    fourthdown_score = mean(decisionmaking_above_expectation)
  ) %>%  
  arrange(desc(fourthdown_score))

# exports the resulting data frames to .rds files
write_rds(season_fourthdown_scores, "data/fourthdown_scores_historical_byseason.rds")
write_rds(weekly_fourthdown_scores, "data/fourthdown_scores_historical_byweek.rds")
