# Imports nflscrapR and Pro Football Reference game data, fixes data issues, 
# and combines the databases. The Pro Football Reference data was included 
# so that I could include the scores and start times for each game (necessary 
# for pulling the weather data).


# Load Packages -----------------------------------------------------------

library(qdap)
library(tidyverse)
library(janitor)
library(nflscrapR)
library(lubridate)
library(RCurl)
library(XML)

source("userDefinedValues.R")


# Import nflscrapR Data ---------------------------------------------------

pbp <- read_rds("data/temp/pbp_rushpass_temp.rds")

games_nflscrapR <-
  pbp %>%
  distinct(game_id, home_team, away_team)

# add game dates
games_nflscrapR <- 
  games_nflscrapR %>%
  mutate(game_date = substring(game_id, 1, 8),
         game_date = ymd(game_date))

# one game date is incorrect in this data set 
games_nflscrapR <-
  games_nflscrapR %>%
  mutate(game_date = if_else(game_id == 2014112301, as.Date("2014-11-24"), game_date))


# Import Pro Football Reference Data --------------------------------------

# creates data frame of team name abbreviations
abbr <-
  nflteams %>%
  select(team, abbr) %>%
  mutate(team = recode(team, "St.Louis Rams" = "St. Louis Rams"))

# scrapes Pro Football Reference game data
data_list = list()
for (yr in first:this_season) {
  cat(yr, "\n")
  url <- paste0("https://www.pro-football-reference.com/years/", yr, "/games.htm")
  urlData <- getURL(url)
  games_raw <- readHTMLTable(urlData, stringsAsFactors = FALSE)$games
  colnames(games_raw)[c(6, 8)] <- c("at", "boxscore_field")
  data_list[[yr]] <- games_raw %>% mutate(season = yr)
}

# removes playoff games and extraneous rows, and standardizes column names
games_PFR_raw <-
  bind_rows(data_list) %>%
  as_tibble() %>%
  clean_names() %>%
  filter(!(week %in% c("Week", "", "WildCard", "Division", 
                       "ConfChamp", "SuperBowl"))) %>%
  mutate(week = as.integer(week)) %>%
  select(-boxscore_field)

# abbreviates team names, fixes data issues, and converts
# winning and losing team data to home and away team data
games_PFR <- 
  games_PFR_raw %>%
  mutate_at(vars(winner_tie, loser_tie), 
            ~ replace_abbreviation(., abbreviation = abbr)) %>%
  mutate_at(vars(winner_tie, loser_tie),
            ~ recode(., "JAX" = "JAC", "STL" = "LA", "SD" = "LAC")) %>%
  mutate(home_team = if_else(at == "", winner_tie, loser_tie),
         home_score = if_else(at == "", pts_w, pts_l),
         home_score = as.integer(home_score),
         away_team = if_else(home_team == winner_tie, loser_tie, winner_tie),
         away_score = if_else(home_team == winner_tie, pts_l, pts_w),
         away_score = as.integer(away_score),
         year = if_else(startsWith(date, "January"), as.integer(season + 1), as.integer(season)),
         date = str_c(date, ", ", year),
         date = mdy(date),
         time = format(strptime(time, "%I:%M%p"), "%H:%M:%S") # convert AM/PM to 24-hour
         ) %>%
  select(season, week, day, date, time, home_team, away_team, home_score, away_score)

# Combine Databases -------------------------------------------------------

# adds nflscrapR game IDs to Pro Football Reference game data
games <-
  left_join(games_nflscrapR, games_PFR, 
            by = c("game_date" = "date", "home_team", "away_team")
            ) %>%
  select(game_id, season, week, game_date, time, day, 
         home_team, away_team, home_score, away_score)

# exports the resulting data frame to an .rds file
write_rds(games, "data/games.rds")


# Add Season/Week Data to PBP Data Frames ---------------------------------

pbp_all <- read_rds("data/temp/pbp_all_temp.rds")
pbp_rushpass <- read_rds("data/temp/pbp_rushpass_temp.rds")

season_week <-
  games %>%
  select(game_id, season, week)

pbp_all <- left_join(season_week, pbp_all, by = "game_id")
pbp_rushpass <- left_join(season_week, pbp_rushpass, by = "game_id")

write_rds(pbp_all, "data/pbp_all.rds")
write_rds(pbp_rushpass, "data/pbp_rushpass.rds")
