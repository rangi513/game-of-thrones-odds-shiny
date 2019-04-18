## Create Leaderboard functions
create_throne_points <- function(data, vegas_data){
  data %>% 
    select(full_name, Pool, who_takes_the_throne) %>% 
    left_join(vegas_data, by = c("who_takes_the_throne" = "name")) %>% 
    mutate(Vegas_Implied_Probabilities = if_else(is.na(Vegas_Implied_Probabilities), 0, Vegas_Implied_Probabilities)) %>% 
    mutate(expected_throne_points = Vegas_Implied_Probabilities * 7 / 100) %>% 
    select(full_name, Pool, who_takes_the_throne, expected_throne_points)
}


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
    select(-who_takes_the_throne, -dies_first) %>% 
    mutate(expected_death_pool_points = purrr::map2_dbl(full_name, Pool, sum_death_pool_per_person, ., death_pool_vegas)) %>% 
    select(full_name, Pool, expected_death_pool_points)
}


create_first_death_points <- function(data, dies_first_vegas){
  data %>% 
    select(full_name, Pool, dies_first) %>% 
    left_join(dies_first_vegas, by = c("dies_first" = "name")) %>% 
    mutate(expected_first_death_points = Vegas_Implied_Probabilities * 4 /100) %>% 
    select(full_name, Pool, dies_first, expected_first_death_points)
}

create_leaderboard <- function(data, take_the_throne_vegas, death_pool_vegas, dies_first_vegas, this_full_name = "All", pool = "All"){
  the_throne_points <- create_throne_points(data, take_the_throne_vegas)
  death_pool_points <- create_death_pool_points(data, death_pool_vegas)
  first_death_points <- create_first_death_points(data, dies_first_vegas)
  leaderboard <- the_throne_points %>% 
    left_join(death_pool_points, by = c("full_name","Pool")) %>% 
    left_join(first_death_points, by = c("full_name","Pool")) %>% 
    mutate(total_expected_points = expected_throne_points + expected_first_death_points + expected_death_pool_points) %>% 
    select(full_name, Pool, total_expected_points, who_takes_the_throne, dies_first, expected_throne_points, expected_first_death_points, expected_death_pool_points) %>% 
    arrange(desc(total_expected_points))
  
  if(this_full_name != "All"){
    leaderboard <- leaderboard %>% filter(full_name == this_full_name)
  }
  if(pool != "All"){
    leaderboard <- leaderboard %>% filter(Pool == pool)
  }
  
  leaderboard_renamed <- leaderboard %>% 
    rename(`Full Name` = full_name,
           `Expected Points` = total_expected_points, 
           `Takes The Throne` = who_takes_the_throne, 
           `Dies First` = dies_first, 
           `Expected Throne` = expected_throne_points,
           `Expected First Death` = expected_first_death_points, 
           `Expected Death Pool` = expected_death_pool_points)
  return(leaderboard_renamed)
}
