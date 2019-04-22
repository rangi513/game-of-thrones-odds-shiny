# Create Leaderboard functions

## Throne Pool Functions
create_throne_points <- function(data, vegas_data){
  data %>% 
    select(full_name, Pool, who_takes_the_throne) %>% 
    left_join(vegas_data, by = c("who_takes_the_throne" = "name")) %>% 
    mutate(Vegas_Implied_Probabilities = if_else(is.na(Vegas_Implied_Probabilities), 0, Vegas_Implied_Probabilities)) %>% 
    mutate(expected_throne_points = Vegas_Implied_Probabilities * 7 / 100) %>% 
    select(full_name, Pool, who_takes_the_throne, expected_throne_points)
}

create_throne_points_with_ppr <- function(data, take_the_throne_vegas, results_with_ppr){
  the_throne_points <- create_throne_points(data, take_the_throne_vegas)
  throne_ppr <- the_throne_points %>%
    left_join(results_with_ppr %>%
                filter(Key == "takes_the_throne_actual") %>%
                select(-Key),
              by = c("who_takes_the_throne"="name")) %>% 
    mutate(actual = actual * 7.0,
           expected_throne_points = if_else(PPR == 0, actual, expected_throne_points)) %>% 
    rename(throne_actual = actual,
           throne_ppr = PPR)
  return(throne_ppr)
}




## Death Pool functions
sum_death_pool_per_person <- function(this_person, this_pool, data, death_pool_vegas){
  expected_death_pool_points <- data %>% 
    filter(full_name == this_person, Pool == this_pool) %>% 
    gather("name", "Dies", -full_name, -Pool) %>% 
    left_join(death_pool_vegas, by = "name") %>% 
    mutate(expected_positive_points = Dies * Vegas_Implied_Probabilities/100,
           expected_negative_points = Dies * (100 - Vegas_Implied_Probabilities)/100) %>% 
    summarise(total_expected_death_pool_points = sum(expected_positive_points - expected_negative_points, na.rm = TRUE)) %$%
    total_expected_death_pool_points
  return(expected_death_pool_points)
}

create_death_pool_points <- function(data, death_pool_vegas){
  data %>% 
    select(-who_takes_the_throne, -dies_first, -dies_last) %>% 
    mutate(expected_death_pool_points = purrr::map2_dbl(full_name, Pool, sum_death_pool_per_person, ., death_pool_vegas)) %>% 
    select(full_name, Pool, expected_death_pool_points)
}

create_death_pool_points_with_ppr <- function(death_pool_by_episode, death_pool_vegas, results){
  max_episode <- 2 # max(results$dead_actual)
  ratio_of_episodes_left <- 1/(6-max_episode)
  
  expected_death_points_by_character <- death_pool_by_episode %>% 
    gather("name", "episode_of_death", -full_name, -Pool) %>% 
    left_join(results %>% select(name, dead_actual), by = "name") %>% 
    mutate(death_pool_actual_points = case_when(episode_of_death == 0              ~ 0, # Predicted Survives equals 0 Points
                                                episode_of_death > 0 & 
                                                  dead_actual ==  episode_of_death ~ 3, # Correct Episode of Death equals 3 Points
                                                episode_of_death > 0 & 
                                                  dead_actual > 0 & 
                                                  dead_actual !=  episode_of_death ~ 1, # Correct Prediction of Death equals 1 Point
                                                episode_of_death > 0 &
                                                  dead_actual == -1                ~-1, # Incorrect Prediction of Death (This person survives after last episode)
                                                TRUE                               ~ 0  # Else 0 (They are still alive so we will use Vegas Estimates)
    )) %>% 
    mutate(PPR = case_when(episode_of_death == 0             ~ 0, # Predicted Survives equals 0 Points
                           episode_of_death > 0 & 
                             episode_of_death > max_episode  ~ 3, # Have a chance to guess correct death episode
                           episode_of_death > 0 & 
                             episode_of_death <= max_episode ~ 1, # Guessed someone died in a past episode
                           TRUE                              ~ 0  
                           
    )) %>% 
    left_join(death_pool_vegas, by = c("name")) %>% 
    mutate(prob = Vegas_Implied_Probabilities/100,
           # If you predicted someone is going to die, possible positive points - possible negative points, otherwise 0 (survive)
           expected_future_death_points = case_when(episode_of_death > 0 & episode_of_death > max_episode  ~ (PPR*ratio_of_episodes_left + (PPR/3)* 1-ratio_of_episodes_left) * prob - (1 - prob),
                                                    episode_of_death > 0 & episode_of_death <= max_episode ~ 1 * prob - 1 * (1 - prob),
                                                    TRUE                                                   ~ 0))
  
  expected_death_points_by_submission <- expected_death_points_by_character %>% 
    group_by(full_name, Pool) %>% 
    summarise(death_pool_ppr = sum(PPR),
              expected_death_pool_points = sum(expected_future_death_points),
              death_pool_actual = sum(death_pool_actual_points))
  return(expected_death_points_by_submission)
}




## First Death Functions
create_first_death_points <- function(data, dies_first_vegas){
  data %>% 
    select(full_name, Pool, dies_first) %>% 
    left_join(dies_first_vegas, by = c("dies_first" = "name")) %>% 
    mutate(expected_first_death_points = Vegas_Implied_Probabilities * 4 /100) %>% 
    select(full_name, Pool, dies_first, expected_first_death_points)
}

create_first_death_points_with_ppr <- function(data, dies_first_vegas, results_with_ppr){
  first_death_points <- create_first_death_points(data, dies_first_vegas)
  first_death_ppr <- first_death_points %>%
    left_join(results_with_ppr %>%
                filter(Key == "dies_first_actual") %>%
                select(-Key),
              by = c("dies_first"="name")) %>% 
    mutate(actual = actual * 4.0,
           expected_first_death_points = if_else(PPR == 0, actual, expected_first_death_points)) %>% 
    rename(dies_first_actual = actual,
           dies_first_ppr = PPR)
  return(first_death_ppr)
}

## Last Death Functions
create_last_death_points_with_ppr <- function(data, results_with_ppr){
  last_death_ppr <- data %>%
    select(full_name, Pool, dies_last) %>%
    mutate(expected_last_death_points = 0) %>% 
    left_join(results_with_ppr %>%
                filter(Key == "dies_last_actual") %>%
                select(-Key),
              by = c("dies_last"="name")) %>% 
    mutate(actual = actual * 4.0,
           expected_last_death_points = if_else(PPR == 0, actual, expected_last_death_points)) %>% 
    rename(dies_last_actual = actual,
           dies_last_ppr = PPR)
  return(last_death_ppr)
}



create_leaderboard <- function(data, take_the_throne_vegas, death_pool_vegas, death_pool_by_episode, dies_first_vegas, results, this_full_name = "All", pool = "All"){
  results_with_ppr <- append_ppr_to_results(results)
  the_throne_points_ppr <- create_throne_points_with_ppr(data, take_the_throne_vegas, results_with_ppr)
  death_pool_points_ppr <- create_death_pool_points_with_ppr(death_pool_by_episode, death_pool_vegas, results)
  first_death_points_ppr <- create_first_death_points_with_ppr(data, dies_first_vegas, results_with_ppr)
  last_death_points_ppr <- create_last_death_points_with_ppr(data, results_with_ppr)
  
  
  leaderboard <- the_throne_points_ppr %>% 
    left_join(death_pool_points_ppr, by = c("full_name","Pool")) %>% 
    left_join(first_death_points_ppr, by = c("full_name","Pool")) %>% 
    left_join(last_death_points_ppr, by = c("full_name","Pool")) %>% 
    mutate(`Total Points` = throne_actual + dies_first_actual + dies_last_actual + death_pool_actual,
           `Expected Total Points` = expected_throne_points + expected_first_death_points + expected_death_pool_points + `Total Points`,
           PPR = throne_ppr + dies_first_ppr + dies_last_ppr + death_pool_ppr) %>% 
    arrange(desc(`Expected Total Points`))
  
  if(this_full_name != "All"){
    leaderboard <- leaderboard %>% filter(full_name == this_full_name)
  }
  if(pool != "All"){
    leaderboard <- leaderboard %>% filter(Pool == pool)
  }
  
  leaderboard_renamed <- leaderboard %>% 
    select(full_name,
            Pool,
            `Total Points`,
            `Expected Total Points`,
            PPR,
            who_takes_the_throne,
            dies_first,
            dies_last,
            death_pool_actual,
            expected_death_pool_points,
            death_pool_ppr,
            throne_actual,
            expected_throne_points,
            throne_ppr,
            dies_first_actual,
            expected_first_death_points,
            dies_first_ppr,
            dies_last_actual) %>% 
    rename(`Full Name` = full_name,
           `Takes The Throne` = who_takes_the_throne, 
           `Dies First` = dies_first, 
           `Dies Last` = dies_last,
           `Death Pool Actual` = death_pool_actual,
           `Death Pool PPR` = death_pool_ppr,
           `Throne Actual` = throne_actual, 
           `Throne PPR` = throne_ppr,
           `Dies First Actual` = dies_first_actual,
           `Dies First PPR` = dies_first_ppr,
           `Expected Throne` = expected_throne_points,
           `Expected First Death` = expected_first_death_points, 
           `Expected Death Pool` = expected_death_pool_points,
           `Dies Last Actual` = dies_last_actual)
  return(leaderboard_renamed)
}

