# For each game, finds whether the home and away teams had a bye the previous week.


# Load Packages -----------------------------------------------------------

library(tidyverse)


# Calculations ------------------------------------------------------------

games <- read_rds("data/games.rds")
stadiums <- read_rds("data/stadiums.rds")

home_games <-
  games %>%
  distinct(game_id, team = home_team, season, week)

away_games <-
  games %>%
  distinct(game_id, team = away_team, season, week)

# for each team / game combination, finds whether the team had a bye
# the previous week
byes <-
  bind_rows(home_games, away_games) %>%
  arrange(team, season, week) %>%
  mutate(bye = FALSE) %>%
  complete(team, season, week) %>% # adds bye weeks
  group_by(team, season) %>%
  mutate(bye = replace_na(bye, TRUE),
         had_bye = lag(bye),
         had_bye = if_else(week == 1, FALSE, had_bye)) %>%
  ungroup() %>%
  filter(bye == FALSE) %>% # removes bye weeks
  select(game_id, team, had_bye)

# for each game, creates fields signifying whether the home or away
# teams had a bye the previous week
games_with_byes <-
  games %>%
  left_join(byes, 
            by = c("game_id", "home_team" = "team")) %>%
  rename(home_had_bye = had_bye) %>%
  left_join(byes,
            by = c("game_id", "away_team" = "team")) %>%
  rename(away_had_bye = had_bye) %>%
  select(game_id, home_had_bye, away_had_bye)

# exports the resulting data frame to an .rds file
write_rds(games_with_byes, "data/byes.rds")
