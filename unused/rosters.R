# Based on Ben Baldwin's Github code, imports the nflscrapR roster
# database and fixes some data issues.


# Load Packages -----------------------------------------------------------

library(tidyverse)

source("userDefinedValues.R")


# Import Roster Data ------------------------------------------------------

# download nflscrapR data
data_list = list()
for (yr in first:this_season) {
  if (yr <= 2017) {
    roster <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/legacy_data/team_rosters/team_", yr, "_rosters.csv")))
    roster <- 
      roster %>% 
      select(
        season = Season,
        full_player_name = Player,
        abbr_player_name = name,
        position = Pos,
        team = Team,
        gsis_id = GSIS_ID
      )
  }
  else {
    roster <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_", yr, ".csv")))
    roster <- 
      roster %>% 
      select(-season_type)
  }
  
  data_list[[yr]] <- roster
}

rosters_all <- bind_rows(data_list)

# fix team name discontinuities
rosters_all <- 
  rosters_all %>% 
  mutate_at(vars(team), 
            ~ case_when(
              . %in% "JAX" ~ "JAC",
              . %in% "STL" ~ "LA",
              . %in% "SD" ~ "LAC",
              TRUE ~ .))

rosters_all %>%
  glimpse


# exports the resulting data frame to an .rds file
write_rds(rosters_all, path = "data/rosters.rds")
