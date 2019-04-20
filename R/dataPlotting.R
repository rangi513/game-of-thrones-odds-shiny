### Plotting Functions
generate_death_pool_plot <- function(data, submission_data, nameSelection = "All", selectedSortDirection = "RV Percentage", houseSelection = "daenerys"){
  person_selections <- create_death_pool_selection_highlighting_df(submission_data, nameSelection)
  
  data %>% 
    mutate(name = forcats::fct_reorder(name, .[[selectedSortDirection]], .desc = TRUE)) %>% 
    gather("Source", "Probability", -name) %>% 
    ggplot(aes(x = name, y = Probability)) + 
    geom_col(aes(fill = Source), position = "dodge") + 
    geom_col(data = person_selections, alpha = 0.5, color = "black") +
    xlab(label = "Name") +
    geom_hline(yintercept = 50, alpha = 0.5, linetype = 2) +
    ggtitle("Death Pool - RV Average vs. Vegas Odds (Implied Probabilities)") + 
    scale_fill_got_d(option = houseSelection) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="top") 
}

generate_take_the_throne_plot <- function(data, submission_data, nameSelection = "All", selectedSortDirection = "RV Percentage", houseSelection = "jon_snow"){
  person_selections <- create_plot_selection_highlighting_df(data, submission_data, nameSelection, "who_takes_the_throne")
  
  data %>%
    mutate(name = forcats::fct_reorder(name, .[[selectedSortDirection]], .desc = TRUE)) %>% 
    rename(`Vegas Implied Probabilities` = Vegas_Implied_Probabilities) %>% 
    gather("Source", "Probability", -name) %>% 
    ggplot(aes(x = name, y = Probability)) + 
    geom_col(aes(fill = Source), position = "dodge") + 
    geom_col(data = person_selections, alpha = 0.5, color = "black") +
    scale_fill_got_d(option = houseSelection) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="top") + 
    xlab(label = "Name") +
    ggtitle("Take The Throne - RV Average vs. Vegas Odds (Implied Probabilities)")
}


generate_die_first_plot <- function(data, submission_data, nameSelection = "All", selectedSortDirection = "RV Percentage", houseSelection = "lannister"){
  person_selections <- create_plot_selection_highlighting_df(data, submission_data, nameSelection, "dies_first")
  
  data %>% 
    mutate(name = forcats::fct_reorder(name, .[[selectedSortDirection]], .desc = TRUE)) %>% 
    rename(`Vegas Implied Probabilities` = Vegas_Implied_Probabilities) %>% 
    gather("Source", "Probability", -name) %>% 
    ggplot(aes(x = name, y = Probability)) + 
    geom_col(aes(fill = Source), position = "dodge") + 
    geom_col(data = person_selections, alpha = 0.5, color = "black") +
    scale_fill_got_d(option = houseSelection, direction = -1) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="top") + 
    xlab(label = "Name") +
    ggtitle("First Death - RV Average vs. Vegas Odds (Implied Probabilities)")
}

create_death_pool_selection_highlighting_df <- function(data, nameSelection){
  data %>% 
    filter(full_name == nameSelection) %>% 
    select(-full_name, -Pool, -who_takes_the_throne, -dies_first) %>% 
    gather("name", "Probability") %>% 
    mutate(Probability = Probability * 100)
}

create_plot_selection_highlighting_df <- function(data, submission_data, nameSelection, columnFilter){
  submission_data %>% 
    filter(full_name == nameSelection) %>% 
    select(!!columnFilter) %>% 
    rename(name = !!columnFilter) %>% 
    mutate(Probability = max_plot_probability(data))
}

max_plot_probability <- function(data){
  max(c(data$Vegas_Implied_Probabilities,data$`RV Percentage`), na.rm = TRUE)
}
