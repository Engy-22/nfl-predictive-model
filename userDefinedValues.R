teams <- readRDS("data/teams.rds")

first <- 2009 # first available season
first_usable <- 2011 # first season for which games are used as training observations
last_complete <- 2018 # most recent complete season
this_season <- last_complete + 1 # current season

this_week <- 8 # upcoming week of the current season



# Tuning Parameters -------------------------------------------------------
# haven't had time to tune these yet, will write a script that
# saves test set RSE over combinations of these parameters

full_passing_credibility <- 1000
full_rushing_credibility <- full_passing_credibility

full_qb_career_credibility <- 1000

full_defense_credibility <- 1000
full_special_teams_credibility <- full_defense_credibility / 2

full_coaching_credibility <- 1500
full_rushpass_credibility <- full_coaching_credibility
full_fourthdown_credibility <- full_coaching_credibility / 10

learning_factor <- 1 / 2

qb_persistency_weight <- .15
coaching_persistency_weight <- .3
offensive_persistency_weight <- .2
defensive_persistency_weight <- .1

trees <- 5000
tree_depth <- 10
shrinkage <- .01

# used for checking whether tuning parameters have changed in the
# model tuning process. this only needs to be saved to file
# one time before the initial tuning run; the 
# "tuning_parameters_snapshot.rds" file is automatically updated 
# in the tuning parameters process
parameters <-
  data.frame(full_passing_credibility, full_rushing_credibility,
             full_qb_career_credibility, full_defense_credibility,
             full_special_teams_credibility, full_rushpass_credibility,
             full_coaching_credibility, qb_persistency_weight,
             coaching_persistency_weight, offensive_persistency_weight,
             defensive_persistency_weight, trees, tree_depth, learning_factor)
# write_rds(parameters, "tuning/tuning_parameters_snapshot.rds")
