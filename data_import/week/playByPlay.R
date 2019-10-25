# Based on Ben Baldwin's Github code, imports the nflscrapR play-by-play
# database, fixes some data issues, and creates a better encoding of 
# rush / pass than what the NFL's database provides.


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(na.tools)

source("userDefinedValues.R")


# Import Play-by-play Data ------------------------------------------------

# download nflscrapR data
data_list = list()
for (yr in first:this_season) {
  cat(yr, "\n")
  pbp <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv")))
  # removes columns that caused a bind_rows error (not at all important for prediction)
  pbp <-
    pbp %>%
    select(-fumble_recovery_2_yards, 
           -blocked_player_id, 
           -fumble_recovery_2_player_id)
  data_list[[yr]] <- pbp
}
pbp_all <- bind_rows(data_list)

# fix team name discontinuities
pbp_all <- 
  pbp_all %>% 
  mutate_at(vars(home_team, away_team, posteam, defteam), 
            ~ case_when(
              . %in% "JAX" ~ "JAC",
              . %in% "STL" ~ "LA",
              . %in% "SD" ~ "LAC",
              TRUE ~ .))

# remove two-point conversions and penalties
pbp_all <-
  pbp_all %>%
  filter(two_point_attempt == FALSE,
         penalty == FALSE)

# one game date is incorrect in this data set 
pbp_all <-
  pbp_all %>%
  mutate(game_date = if_else(game_id == 2014112301, as.Date("2014-11-24"), game_date))

# creates a data frame only encompassing rush / pass plays (including penalties),
# providing a better encoding of rush / pass than what the NFL's database provides
pbp_rushpass <- 
  pbp_all %>%
  filter(!is_na(epa), !is_na(posteam), play_type == "no_play" | play_type == "pass" | play_type == "run") %>%
  mutate(
    pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa > 0, 1 , 0),
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, 
                                              "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name),
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    yards_gained = ifelse(play_type == "no_play", NA, yards_gained),
    play = 1
  ) %>%
  filter(pass == 1 | rush == 1)

# translate 0's and 1's to FALSE and TRUE
pbp_rushpass <-
  pbp_rushpass %>%
  mutate_if(~ all(. %in% c(0, 1)), 
            ~ as.logical(.))

# exports the resulting data frames to .rds files
write_rds(pbp_all, path = "data/temp/pbp_all_temp.rds")
write_rds(pbp_rushpass, path = "data/temp/pbp_rushpass_temp.rds")
