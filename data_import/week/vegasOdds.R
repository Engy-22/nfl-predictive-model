# Scrapes an aggregated average of home and away odds for NFL games
# from a dynamic webpage using the Splash web scraping tool.

# Load Packages and Load Data -----------------------------------------------------------

library(qdap)
library(tidyverse)
library(rvest)
library(splashr)
library(lubridate)
library(nflscrapR)

source("userDefinedValues.R")

games <- read_rds("data/games.rds")

# creates data frame of team name abbreviations
abbr <- 
  nflteams %>%
  mutate(abbr = recode(abbr, "JAX" = "JAC")) %>%
  select(team, abbr)

# wait time for Splash webpage scraping calls
wait_time <- 2


# Calculations ------------------------------------------------------------

# initialize Splash
splash_container <- start_splash()

# scrapes for all seasons that will be used as training/test data
data_list <- list()
for (season in first_usable:this_season) {
  data_list[[season]] <- getMoneylineOdds(season)
}
odds <- bind_rows(data_list)

# combines home team / away team data with odds data
# and abbreviates team names for matching with the 
# nflscrapR game IDs
odds_with_gameids <- 
  odds %>%
  mutate_at(vars(home_team, away_team), 
            ~ replace_abbreviation(., abbreviation = abbr)) %>%
  left_join(games, by = c("season", "home_team", "away_team")) %>%
  select(game_id, home_odds, away_odds) %>%
  as_tibble()

# commenting to prevent accidental overwriting
write_rds(odds_with_gameids, "data/vegas_odds.rds")

stop_splash(splash_container)

# Functions ---------------------------------------------------------------

getMoneylineOdds <- function(season) {
  page_count <- getPageCount(season)
  
  data_list <- list()
  for (page in 1:page_count) {
    cat(season, page, "/", page_count, "\n")
    result <- scrapePage(season, page)
    if (!is.null(result)) {
      data_list[[page]] <- result
    }
  }
  return(bind_rows(data_list))
}

getPageCount <- function(season) {
  this_year <- season
  next_year <- season + 1
  
  if (season == this_season) {
    url <- "https://www.oddsportal.com/american-football/usa/nfl/results/"
  } else {
    url <- paste0("https://www.oddsportal.com/american-football/usa/nfl-", 
                  this_year, "-", next_year, "/results/")
  }

  # scrape dynamic web page
  webpage <-
    splash("localhost") %>%
    splash_user_agent(ua_macos_chrome) %>%
    splash_go(url) %>%
    splash_wait(wait_time) %>%
    splash_html()
  
  # finds the number of pages that need to be scraped for the given season
  page_text <-
    html_nodes(webpage, '#pagination') %>%
    html_text()
  page_text_split <- strsplit(page_text, split = "")[[1]]
  last_page <-
    page_text_split %>%
    as.integer() %>%
    max(na.rm = TRUE)
  
  return(last_page)
}

scrapePage <- function(season, page) {
  this_year <- as.character(season)
  next_year <- as.character(season + 1)
  
  if (season == this_season) {
    url <- paste0("https://www.oddsportal.com/american-football/usa/nfl/results/#/page/",
                  page, "/")
                  
  } else {
    url <- paste0("https://www.oddsportal.com/american-football/usa/nfl-", 
                  this_year, "-", next_year, "/results/#/page/", page, "/")
  }  
  url
  
  # scrapes html content from dynamic web page
  webpage <-
    splash("localhost") %>%
    splash_user_agent(ua_macos_chrome) %>%
    splash_go(url) %>%
    splash_wait(wait_time) %>%
    splash_html()
  
  # extract game date and team home/away text from html
  teams_text <-
    html_nodes(webpage, '.table-participant , .nob-border .tl') %>%
    html_text()
  
  # extract only the regular season games from the html text
  teams_text_regular_season <- getRegularSeasonGames(teams_text, 
                                                     this_year,
                                                     next_year)
  
  # if all games on a page are playoff or preseason games,
  # return NULL
  if (is.null(teams_text_regular_season)) {
    return(NULL)
  }
  
  # removes dates and splits teams text into home / away columns
  teams <-
    data.frame(text = teams_text_regular_season) %>%
    mutate(text = as.character(text)) %>%
    filter(!endsWith(text, this_year),
           !endsWith(text, next_year)) %>%
    separate(text, into = c("home_team", "away_team"), sep = " - ")

  # extract game date and home/away moneyline odds text from web page
  odds_text <-
    html_nodes(webpage, '.odds-nowrp, .nob-border .tl') %>%
    html_text()

  # extract only the regular season games from the html text
  odds_text_regular_season <- getRegularSeasonGames(odds_text,
                                                    this_year,
                                                    next_year)
  
  # removes dates
  odds_text_regular_season <-
    subset(odds_text_regular_season, 
           !str_detect(as.character(odds_text_regular_season), " "))

  # splits data frame into home and away odds by position (alternating)
  odds <- seq(from = 1, to = length(odds_text_regular_season), by = 2)
  evens <- seq(from = 2, to = length(odds_text_regular_season), by = 2)
  home_odds <- 
    odds_text_regular_season[odds] %>%
    as.integer()
  away_odds <- 
    odds_text_regular_season[evens] %>%
    as.integer()
  
  to_return <- data.frame(season = season, teams, home_odds, away_odds)

  return(to_return)
}


getRegularSeasonGames <- function(html_text, this_year, next_year) {
  dates <- subset(html_text, (str_detect(html_text, this_year) |
                              str_detect(html_text, next_year)))
  if (all(endsWith(dates, "Pre-season")) |
      all(endsWith(dates, "Play Offs") | endsWith(dates, "All Stars"))) {
    return(NULL)
  }
  
  # identifies the position of the last playoff game date, and then
  # removes all rows up to that position. this removes the Pro Bowl,
  # allowing me to use an endsWith(this_year) or endsWith(next_year)
  # on the game date rows to identify the first non-playoff game
  temp <- which((endsWith(html_text, "Play Offs")))
  last_playoff_game_position <- ifelse(is_empty(temp), 1, temp[[length(temp)]])
  html_text <- html_text[last_playoff_game_position:length(html_text)]
  
  # identifies the position of the first non-playoff game date
  cutoff_point_playoffs <-
    which((endsWith(html_text, this_year) | endsWith(html_text, next_year)) &
            !(endsWith(html_text, "Play Offs")))[[1]]
  
  # identifies the position of the first preseason game date
  temp <- which((endsWith(html_text, "Pre-season")))
  cutoff_point_preseason <- ifelse(is_empty(temp), length(html_text), temp[[1]] - 1)
  
  # removes playoff and preseason games
  html_text_regular_season <- html_text[cutoff_point_playoffs:cutoff_point_preseason]

  return(html_text_regular_season)
}

