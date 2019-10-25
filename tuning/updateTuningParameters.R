# Checks which (if any) tuning parameters were changed, runs the 
# files that rely on the changed tuning parameters, runs the
# model on the training set (2011-2016), and prints the RSE 
# on the test set (2017-2019).
#
# In the future, I plan to refactor this code to loop through many
# combinations of tuning parameters so as to identify the most
# predictive combination. Unfortunately, because each model
# run is somewhat time-intensive (about one minute), this will
# take a good amount of time, especially considering the number
# of tuning parameters involved. Will consider reducing the number
# of parameters to reduce computation time, or (perhaps) use a
# an alternative (probably more predictive) method of projecting 
# player/team/coach performance.


# Load Package ------------------------------------------------------------

library(tidyverse)

source("userDefinedValues.R")


# Rerun Feature Calculations ----------------------------------------------

# gets the list of parameters used the last time this script was run.
# then, saves the current list of parameters to the same file (for
# future reference by this script)
parameters_prior <- read_rds("tuning/tuning_parameters_snapshot.rds")
write_rds(parameters, "tuning/tuning_parameters_snapshot.rds")

# for each tuning parameter, checks if a change was made
change <- parameters != parameters_prior
change <- as.list(change)
names(change) <- names(parameters)

# if these tuning parameters are changed, these files need to be run:
#
# weighted averages:
# teams: offensive persistency, defensive persistency
# coach scores: coaching persistency
# quarterbacks: qb persistency, career credibility, learning factor

# projected:
# teams: passing credibility, rushing credibility, defense credibility, learning factor
# coach scores: coaching credibility, learning factor
# quarterbacks: passing credibility, learning factors

update_teams_weightedavg <- 
  change$offensive_persistency_weight | change$defensive_persistency_weight
update_coachscores_weightedavg <-
  change$coaching_persistency_weight
update_qbs_weightedavg <-
  change$qb_persistency_weight | change$full_qb_career_credibility | 
  change$learning_factor

update_teams_projected <-
  update_teams_weightedavg | change$full_passing_credibility |
  change$full_rushing_credibility | change$full_defense_credibility |
  change$learning_factor
update_coachscores_projected <-
  update_coachscores_weightedavg | change$full_coaching_credibility |
  change$learning_factor
update_qbs_projected <-
  update_qbs_weightedavg | change$full_passing_credibility |
  change$learning_factor

if (update_teams_weightedavg) {
  source("feature_engineering/by_season/teamsWeightedAvgBySeason.R")
}
if (update_teams_projected) {
  source("feature_engineering/by_week/teamsProjected.R")
}


if (update_coachscores_weightedavg) {
  source("feature_engineering/by_season/coachScoresWeightedAvgBySeason.R")
}
if (update_coachscores_projected) {
  source("feature_engineering/by_week/coachScoresProjected.R")
}


if (update_qbs_weightedavg) {
  source("feature_engineering/by_season/quarterbacksWeightedAvgBySeason.R")
}
if (update_qbs_projected) {
  source("feature_engineering/by_week/quarterbacksProjected.R")
}


if (update_teams_projected | 
    update_coachscores_projected | 
    update_qbs_projected) {
  source("feature_engineering/consolidateFeatures.R")
}

# trains the model on 2011-2016 data and prints test set RSE
# for 2017-2019 data
source("tuning/testSetRSE.R")

