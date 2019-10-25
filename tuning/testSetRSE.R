# Trains a gradient boosting machine on 2011-2016 data (training set),
# then calculates the RSE on 2017-2019 (test set) spreads.

# Limiting myself to a boosting model for now; will do a true
# training/test/hold-out set process to determine the best model
# for this data when I have more time.


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(gbm) 

source("userDefinedValues.R")


# Train Model -------------------------------------------------------------

final_data <- 
  read_rds("data/final_data.rds") %>%
  mutate(spread = home_score - away_score)

# drops games with one-off data issues
# (ie, MIA vs TB week one rescheduled because of a hurricane)
# will come back to fix some of these
final_data <-
  final_data %>%
  filter(!(game_id %in% c(2011121807,
                          2011121808,
                          2011122404,
                          2011122500,
                          2012010108,
                          2012010114,
                          2017091707,
                          2017091708,
                          2018091600,
                          2018091608,
                          2018091606,
                          2018091605,
                          2018091604,
                          2018091601)))

select_predictors <- function(data_frame) {
  data_frame %>%
    select(-game_id, -game_date, -season,
           -home_score, -away_score, -time, 
           -home_team, -away_team, 
           -home_qb, -away_qb, 
           -home_coach, -away_coach, -has_dome) %>% # weather data captures dome/non-dome
    # the gbm function cannot use logical and character types
    mutate_if(is.logical, as.factor) %>%
    mutate_if(is.character, as.factor)
}

training_full <- filter(final_data, season <= 2016)
training <- select_predictors(training_full)

boost <- gbm(spread ~ ., data = training, 
             distribution = "gaussian", n.trees = trees,
             interaction.depth = tree_depth)


# Calculate RSE -----------------------------------------------------------

test_full <- filter(final_data, season >= 2017)
test <- select_predictors(test_full)
test_spreads <- test_full$spread

test_predictions <- predict(boost, newdata = test, n.trees = trees)

rse <- sqrt(mean((test_spreads - test_predictions) ^ 2))
cat(rse, "\n")
