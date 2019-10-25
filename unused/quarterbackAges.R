# Scrapes player birthday data from Pro Football Reference and calculates
# the age of each quarterback at the beginning of the current season.
# Will be used in the future to develop a quarterback age curve that
# updates preseason expectations based on a quarterback's age relative
# his previous age (22 -> 23, 30 -> 31, etc).

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(RCurl)
library(XML)

source("userDefinedValues.R")


# Calculations ------------------------------------------------------------

daysInMonth <- function(month) {
  if (month %in% c(4, 6, 9, 11)) {
    return(30)
  } else if (month == 2) {
    return(29)
  } else {
    return(31)
  }
}

# scrapes player birthday data from Pro Football Reference
data_list = list()
i <- 1
for (month in 1:12) {
  for (day in 1:daysInMonth(month)) {
    cat(month, day, "\n")
    url <- paste0("https://www.pro-football-reference.com/friv/birthdays.cgi?month=", month, "&day=", day)
    urlData <- getURL(url)
    players_raw <- readHTMLTable(urlData, stringsAsFactors = FALSE)$birthdays
    players_raw <- players_raw[, c("Player", "Pos", "Born", "From", "To")]
    players_raw$born_month <- month
    players_raw$born_day <- day
    data_list[[i]] <- players_raw
    i <- i + 1
  }
}

# sets the season start to September 1 of the current season
season_start <- ymd(str_c(this_season, "09", "01"))

# transforms a one digit integer into a two-digit string (9 to "09")
# in order to make a date string for lubridate
makeTwoDigits <- function(x) {
  ifelse(x < 10, paste0("0", x), as.character(x))
}

# selects for quarterbacks who played since 2009
# and calculates their ages
qb_ages <- 
  bind_rows(data_list) %>%
  clean_names() %>%
  rename(position = pos,
         first_season_in_league = to,
         last_season_in_league = from,
         born_year = born) %>%
  arrange(player) %>%
  filter(last_season_in_league %in% first:this_season,
         str_detect(pos, "QB")) %>%
  mutate(born_month = map_chr(born_month, makeTwoDigits),
         born_day = map_chr(born_day, makeTwoDigits),
         birthday = str_c(born_year, born_month, born_day),
         birthday = ymd(birthday),
         age = interval(birthday, season_start) / years(1),
         age = round(age))

# changes the format of quarterback names to the one used by nflscrapR
# ("Aaron Rodgers" -> "A.Rodgers")
qb_ages <-
  ages %>%
  separate(player, into = c("first_name", "last_name"), sep = " ", extra = "merge") %>%
  mutate(first_name = substring(first_name, 1, 1),
         quarterback = str_c(first_name, ".", last_name)) %>%
  select(quarterback, age) %>%
  arrange(quarterback)

# if two (or more) quarterbacks have the same abbreviated name, choose the youngest
qb_ages <-
  qb_ages %>%
  arrange(age) %>%
  distinct(quarterback, .keep_all = TRUE) %>%
  arrange(quarterback) %>%
  as_tibble()

# commented so as to prevent accidental overwriting; scraping the data takes a while
# write_rds(ages, "data/quarterbacks_ages.rds")
