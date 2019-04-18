library(tidyverse)
library(magrittr)
library(gameofthrones)

source("./dataCleaning.R")
source("./dataPlotting.R")
source("./createLeaderboard.R")

got_data <- read_csv("./got_submissions.csv") 
got_data_cleaned <- clean_got_submission_data(got_data)

## Create Vegas Implied Probabilities
american_odds <- read_csv("./got_odds.csv")
death_pool_vegas <- american_odds %>% 
  mutate(Vegas_Implied_Probabilities = purrr::map_dbl(will_die, convert_american_odds_to_prob)) %>% 
  select(name, Vegas_Implied_Probabilities)

dies_first_vegas <- american_odds %>% 
  mutate(Vegas_Implied_Probabilities = purrr::map_dbl(die_first, convert_american_odds_to_prob)) %>% 
  select(name, Vegas_Implied_Probabilities)

take_the_throne_vegas <- american_odds %>% 
  mutate(Vegas_Implied_Probabilities = purrr::map_dbl(take_the_throne, convert_american_odds_to_prob)) %>% 
  select(name, Vegas_Implied_Probabilities)

#----------------------------------------------------------------------------------------
## Death Pool Vegas vs. Average
death_pool_vs_vegas_avg <- combine_death_pool_data(got_data_cleaned, death_pool_vegas) 
generate_death_pool_plot(death_pool_vs_vegas_avg)

#----------------------------------------------------------------------------------------
## Take the Throne Vegas vs Average
take_the_throne_combined <- combine_take_the_throne_data(got_data_cleaned, take_the_throne_vegas)
generate_take_the_throne_plot(take_the_throne_combined)

#----------------------------------------------------------------------------------------
# First Death Odds
dies_first_combined <- combine_die_first_data(got_data_cleaned, dies_first_vegas)
generate_die_first_plot(dies_first_combined)

#----------------------------------------------------------------------------------------
## Death Pool Vegas vs. You
# 7 points for throne, 4 for first death, 1 for each death, -1 for each wrong death
leaderboard <- create_leaderboard(got_data_cleaned)


  