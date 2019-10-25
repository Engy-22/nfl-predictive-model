# Calculates team offensive and defensive efficiency stats by week.


# Load Packages -----------------------------------------------------------

library(tidyverse)


# Team Stat Calculations --------------------------------------------------

pbp <- read_rds("data/pbp_rushpass.rds")
pbp_all <- read_rds("data/pbp_all.rds")
games <- read_rds("data/games.rds")

teams_home <- distinct(games, season, team = home_team, week)
teams_away <- distinct(games, season, team = away_team, week)
teams <-
  bind_rows(teams_home, teams_away) %>%
  arrange(season, team, week)

offensive_passing_epa <-
  pbp %>%
  filter(pass == TRUE) %>%
  group_by(season, team = posteam, week) %>%
  summarize(
    passes = n(),
    offensive_passing_epa = sum(epa) / passes
  ) %>%
  ungroup() %>%
  arrange(season, team, week)

offensive_rushing_epa <-
  pbp %>%
  filter(rush == TRUE) %>%
  group_by(season, team = posteam, week) %>%
  summarize(
    rushes = n(),
    offensive_rushing_epa = sum(epa) / rushes
  ) %>%
  ungroup() %>%
  arrange(season, team, week)

defensive_passing_epa <-
  pbp %>%
  filter(pass == TRUE) %>%
  group_by(season, team = defteam, week) %>%
  summarize(
    defensive_passing_epa = sum(epa) / n(),
  ) %>%
  ungroup() %>%
  arrange(season, team, week)

defensive_rushing_epa <-
  pbp %>%
  filter(rush == TRUE) %>%
  group_by(season, team = defteam, week) %>%
  summarize(
    defensive_rushing_epa = sum(epa) / n(),
  ) %>%
  ungroup() %>%
  arrange(season, team, week)

special_teams_epa <-
  pbp_all %>%
  filter(play_type %in% c("kickoff", "punt", "field_goal", "extra_point"),
         !is.na(epa)) %>%
  group_by(season, team = posteam, week) %>%
  summarize(
    special_teams_plays = n(),
    special_teams_epa = sum(epa) / n()
  ) %>%
  ungroup() %>%
  arrange(season, team)

team_stats <-
  teams %>%
  left_join(offensive_passing_epa, by = c("season", "team", "week")) %>%
  left_join(offensive_rushing_epa, by = c("season", "team", "week")) %>%
  left_join(defensive_passing_epa, by = c("season", "team", "week")) %>%
  left_join(defensive_rushing_epa, by = c("season", "team", "week")) %>%
  left_join(special_teams_epa, by = c("season", "team", "week")) %>%
  mutate(plays = passes + rushes,
         pass_rate = passes / plays,
         rush_rate = rushes / plays) %>%
  select(season, team, week, plays, passes, pass_rate, offensive_passing_epa,
         rushes, rush_rate, offensive_rushing_epa, defensive_passing_epa,
         defensive_rushing_epa, special_teams_plays, special_teams_epa)


# Opponent Stat Calculations ----------------------------------------------

opponents_home <-
  games %>%
  distinct(season, team = home_team, week, opponent = away_team)

opponents_away <-
  games %>%
  distinct(season, team = away_team, week, opponent = home_team)

opponents <-
  bind_rows(opponents_home, opponents_away) %>%
  arrange(season, team, week, opponent)

epa_faced <-
  opponents %>%
  left_join(team_stats, by = c("season", "week", "opponent" = "team")) %>%
  group_by(season, team, week) %>%
  summarize(offensive_passing_epa_faced = mean(offensive_passing_epa),
            passes_against = sum(passes),
            offensive_rushing_epa_faced = mean(offensive_rushing_epa),
            rushes_against = sum(rushes),
            defensive_passing_epa_faced = mean(defensive_passing_epa),
            defensive_rushing_epa_faced = mean(defensive_rushing_epa)
  ) %>%
  arrange(season, team, week)


# Merge Data Frames -------------------------------------------------------

teams_historical_byweek <- 
  left_join(team_stats, epa_faced, by = c("season", "team", "week"))

write_rds(teams_historical_byweek, "data/teams_historical_byweek.rds")
