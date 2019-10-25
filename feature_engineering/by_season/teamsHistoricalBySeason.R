# Calculates offensive and defensive efficiency stats by season, as well as
# the average efficiency stats of each team's opponents.


# Load Packages -----------------------------------------------------------

library(tidyverse)


# Team Stat Calculations --------------------------------------------------

pbp <- read_rds("data/pbp_rushpass.rds")
pbp_all <- read_rds("data/pbp_all.rds")
games <- read_rds("data/games.rds")

teams <-  
  games %>%
  distinct(season, team = home_team)

offensive_passing_epa <-
  pbp %>%
  filter(pass == TRUE) %>%
  group_by(season, team = posteam) %>%
  summarize(
    passes = n(),
    offensive_passing_epa = sum(epa) / passes
  ) %>%
  ungroup() %>%
  arrange(season, team)

offensive_rushing_epa <-
  pbp %>%
  filter(rush == TRUE) %>%
  group_by(season, team = posteam) %>%
  summarize(
    rushes = n(),
    offensive_rushing_epa = sum(epa) / rushes
  ) %>%
  ungroup() %>%
  arrange(season, team)

defensive_passing_epa <-
  pbp %>%
  filter(pass == TRUE) %>%
  group_by(season, team = defteam) %>%
  summarize(
    defensive_passing_epa = sum(epa) / n(),
  ) %>%
  ungroup() %>%
  arrange(season, team)

defensive_rushing_epa <-
  pbp %>%
  filter(rush == TRUE) %>%
  group_by(season, team = defteam) %>%
  summarize(
    defensive_rushing_epa = sum(epa) / n(),
  ) %>%
  ungroup() %>%
  arrange(season, team)

special_teams_epa <-
  pbp_all %>%
  filter(play_type %in% c("kickoff", "punt", "field_goal", "extra_point"),
         !is.na(epa)) %>%
  group_by(season, team = posteam) %>%
  summarize(
    special_teams_plays = n(),
    special_teams_epa = sum(epa) / n()
  ) %>%
  ungroup() %>%
  arrange(season, team)

team_stats <-
  teams %>%
  left_join(offensive_passing_epa, by = c("season", "team")) %>%
  left_join(offensive_rushing_epa, by = c("season", "team")) %>%
  left_join(defensive_passing_epa, by = c("season", "team")) %>%
  left_join(defensive_rushing_epa, by = c("season", "team")) %>%
  left_join(special_teams_epa, by = c("season", "team")) %>%
  mutate(plays = passes + rushes,
         pass_rate = passes / plays,
         rush_rate = rushes / plays) %>%
  select(season, team, plays, passes, pass_rate, offensive_passing_epa,
         rushes, rush_rate, offensive_rushing_epa, defensive_passing_epa,
         defensive_rushing_epa, special_teams_plays, special_teams_epa) %>%
  arrange(season, team)


# Opponent Stat Calculations ----------------------------------------------

opponents_home <-
  games %>%
  distinct(season, team = home_team, opponent = away_team)

opponents_away <-
  games %>%
  distinct(season, team = away_team, opponent = home_team)

opponents <-
  bind_rows(opponents_home, opponents_away) %>%
  arrange(season, team, opponent)

epa_faced <-
  opponents %>%
  left_join(team_stats, by = c("season", "opponent" = "team")) %>%
  group_by(season, team) %>%
  summarize(offensive_passing_epa_faced = mean(offensive_passing_epa),
            offensive_rushing_epa_faced = mean(offensive_rushing_epa),
            defensive_passing_epa_faced = mean(defensive_passing_epa),
            defensive_rushing_epa_faced = mean(defensive_rushing_epa)
  ) %>%
  arrange(season, team)


# Merge Data Frames -------------------------------------------------------

teams_historical_byseason <- 
  left_join(team_stats, epa_faced, by = c("season", "team"))

write_rds(teams_historical_byseason, "data/teams_historical_byseason.rds")
