## Data Cleaning Functions
clean_got_submission_data <- function(data){
  cleaned_data <- data %>% 
    rename(who_takes_the_throne = `WHO TAKES THE THRONE?`,
           full_name = `Full Name`,
           dies_first = `WHO DIES FIRST?`,
           dies_last = `WHO DIES LAST?`) %>% 
    mutate_at(c("who_takes_the_throne", "dies_first", "dies_last"), impute_misspellings) %>% 
    mutate_at(vars(contains('DEATH')), function(x){if_else(str_detect(x, "Survives"), 0, 1)}) 
  # Rename Columns
  colnames(cleaned_data) <- str_replace(str_replace(colnames(cleaned_data), "DEATH POOL \\[", ""), "\\]","")
  return(cleaned_data)
}

impute_misspellings <- function(x){
  case_when(x == "Daenerys Targaryen" ~ "Daenerys Targeryen",
            x == "Samwell Tarly"      ~ "Samwell Tarley",
            TRUE                      ~ x)
}

clean_death_data_by_episode <- function(got_data){
  cleaned_data <- got_data %>% 
    rename(full_name = `Full Name`) %>% 
    select_at(vars( "full_name", "Pool", contains('DEATH'))) %>% 
    mutate_at(vars(contains('DEATH')), function(x){if_else(str_detect(x, "Survives"), 0, as.double(str_extract(x, "[0-9]")))}) 
  # Rename Columns
  colnames(cleaned_data) <- str_replace(str_replace(colnames(cleaned_data), "DEATH POOL \\[", ""), "\\]","")
  return(cleaned_data)
}

convert_american_odds_to_prob <- function(american_odds_value){
  if(american_odds_value > 0){
    implied_prob <- 100 / (100 + american_odds_value) * 100
  }else{
    negative_odds <- -1 * american_odds_value
    implied_prob <- negative_odds /  (100 + negative_odds) * 100
  }
  return(round(implied_prob,2))
}

## Combine Data
combine_death_pool_data <- function(submission_data, vegas_data){
  submission_data %>% 
    select(-full_name, -Pool, -who_takes_the_throne, -dies_first, -dies_last) %>% 
    gather("name", "death") %>% 
    group_by(name) %>% 
    summarise(`RV Percentage` = sum(death)/n() * 100) %>% 
    left_join(vegas_data, by = "name") 
}

combine_take_the_throne_data <- function(submission_data, vegas_data){
  take_the_throne_rv <- submission_data %>% 
    select(full_name, who_takes_the_throne) %>% 
    group_by(who_takes_the_throne) %>% 
    summarise(`RV Percentage` = n()/nrow(.)* 100) %>% 
    rename(name = who_takes_the_throne) 
  take_the_throne_combined <- vegas_data %>% full_join(take_the_throne_rv, by = "name") 
}

combine_die_first_data <- function(submission_data, vegas_data){
  die_first_rv <- submission_data %>% 
    select(full_name, dies_first) %>% 
    group_by(dies_first) %>% 
    summarise(`RV Percentage` = n()/nrow(.)* 100) %>% 
    rename(name = dies_first) 
  
  dies_first_combined <- vegas_data %>% full_join(die_first_rv, by = "name") 
}


## PPR Functions
create_ppr <- function(results){
  results %>%
    summarise(dead_actual = sum(1 - dead_actual),
              dies_first_actual = (1 - ifelse(sum(dies_first_actual) > 0, 1, sum(dies_first_actual))) * 4,
              dies_last_actual = (1 - ifelse(sum(dies_last_actual) > 0, 1, sum(dies_last_actual))) * 4,
              takes_the_throne_actual = (1 - ifelse(sum(takes_the_throne_actual) > 0, 1, sum(takes_the_throne_actual))) * 7) %>% 
    gather("Key", "PPR")
}

append_ppr_to_results <- function(results){
  possible_points_remaining <- create_ppr(results)
  results_with_ppr <- results %>% 
    select(-dead_actual) %>% 
    gather("Key", "actual", -name) %>% 
    left_join(possible_points_remaining, by = "Key")
}
