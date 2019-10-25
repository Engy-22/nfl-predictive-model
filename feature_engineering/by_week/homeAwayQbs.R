# Creates a data frame containing the quarterbacks with the most snaps for
# the home and away teams.


# Load Packages -----------------------------------------------------------

library(tidyverse)


# Calculations ------------------------------------------------------------

pbp <- read_rds("data/pbp_rushpass.rds")
games <- read_rds("data/games.rds")

qb_passes <-
  pbp %>%
  filter(pass == TRUE,
         !is.na(passer_player_name)) %>%
  group_by(game_id, posteam, passer_player_name) %>%
  summarise(passes = n()) %>%
  ungroup %>%
  rename(team = posteam,
         quarterback = passer_player_name)

qb_scrambles <- 
  pbp %>%
  filter(pass == TRUE,
         qb_scramble == TRUE,
         !is.na(rusher_player_name)) %>%
  group_by(game_id, posteam, rusher_player_name) %>%
  summarise(scrambles = n()) %>%
  ungroup() %>%
  rename(team = posteam,
         quarterback = rusher_player_name)

qb_dropbacks <-
  left_join(qb_passes, qb_scrambles, 
            by = c("game_id", "quarterback", "team")) %>%
  mutate(passes = replace_na(passes, 0),
         scrambles = replace_na(scrambles, 0),
         dropbacks = passes + scrambles) %>%
  select(game_id, team, quarterback, dropbacks)

main_qbs <-
  qb_dropbacks %>%
  group_by(game_id, team) %>%
  slice(which.max(dropbacks)) %>%
  select(-dropbacks)

homeaway_qbs <-
  games %>%
  select(game_id, home_team, away_team) %>%
  left_join(main_qbs, by = c("game_id", "home_team" = "team")) %>%
  rename(home_qb = quarterback) %>%
  left_join(main_qbs, by = c("game_id", "away_team" = "team")) %>%
  rename(away_qb = quarterback) %>%
  select(game_id, home_qb, away_qb)

write_rds(homeaway_qbs, "data/homeaway_qbs.rds")
