# Calculates quarterback efficiency stats by season.


# Load Packages -----------------------------------------------------------

library(tidyverse)

source("userDefinedValues.R")


# Calculations ------------------------------------------------------------

pbp <- read_rds("data/pbp_rushpass.rds")

air_yards <-
  pbp %>%
  filter(pass == TRUE,
         sack == FALSE,
         qb_scramble == FALSE,
         !is.na(air_yards),
         !is.na(air_epa)) %>%
  group_by(quarterback = passer_player_name, season) %>%
  summarize(
    air_yards_per_pass = sum(air_yards) / n(),
    air_yards_passing_epa = sum(air_epa) / n()
  ) %>%
  ungroup() %>%
  arrange(quarterback, season)

passing_epa <-
  pbp %>%
  filter(pass == TRUE,
         qb_scramble == FALSE
  ) %>%
  group_by(quarterback = passer_player_name, season) %>%
  summarize(
    passes = n(),
    total_passing_epa = sum(epa),
    passing_epa = total_passing_epa / passes,
  ) %>%
  ungroup() %>%
  arrange(quarterback, season)

scramble_epa <-
  pbp %>%
  filter(pass == TRUE,
         qb_scramble == TRUE
  ) %>%
  group_by(quarterback = rusher_player_name, season) %>%
  summarize(
    scrambles = n(),
    total_scramble_epa = sum(epa),
    scramble_epa = total_scramble_epa / scrambles
  ) %>%
  ungroup() %>%
  arrange(quarterback, season)

consolidated_epa <-
  left_join(passing_epa, scramble_epa,
            by = c("quarterback", "season")) %>%
  # replaces NAs with zeros for these variables, allowing dropback EPA
  # to be calculated for passers with no scrambles or scramblers with no passes
  mutate_at(vars(passes, scrambles, total_passing_epa, total_scramble_epa), 
            ~ replace_na(., replace = 0)) %>% 
  mutate(dropbacks = passes + scrambles,
         dropback_epa = (total_passing_epa + total_scramble_epa) / dropbacks
         ) %>%
  select(quarterback, season, dropbacks, dropback_epa, 
         passes, passing_epa, scrambles, scramble_epa) %>%
  arrange(quarterback, season)

qbs_historical_by_season <-
  left_join(consolidated_epa, air_yards,
            by = c("quarterback", "season"))

write_rds(qbs_historical_by_season, "data/quarterbacks_historical_byseason.rds")
