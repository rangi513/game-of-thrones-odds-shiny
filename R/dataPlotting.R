### Plotting Functions
generate_death_pool_plot <- function(data, selectedSortDirection = "RV Percentage", houseSelection = "daenerys"){
  data %>% 
    mutate(name = forcats::fct_reorder(name, .[[selectedSortDirection]], .desc = TRUE)) %>% 
    gather("Source", "Probability", -name) %>% 
    ggplot(aes(x = name, y = Probability)) + 
    geom_col(aes(fill = Source), position = "dodge") + 
    xlab(label = "Name") +
    geom_hline(yintercept = 50, alpha = 0.5, linetype = 2) +
    ggtitle("Death Pool - RV Average vs. Vegas Odds (Implied Probabilities)") + 
    scale_fill_got_d(option = houseSelection) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
}

generate_take_the_throne_plot <- function(data, selectedSortDirection = "RV Percentage", houseSelection = "jon_snow"){
  data %>%
    mutate(name = forcats::fct_reorder(name, .[[selectedSortDirection]], .desc = TRUE)) %>% 
    rename(`Vegas Implied Probabilities` = Vegas_Implied_Probabilities) %>% 
    gather("Source", "Probability", -name) %>% 
    ggplot(aes(x = name, y = Probability)) + 
    geom_col(aes(fill = Source), position = "dodge") + 
    scale_fill_got_d(option = houseSelection) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    xlab(label = "Name") +
    ggtitle("Take The Throne - RV Average vs. Vegas Odds (Implied Probabilities)")
}


generate_die_first_plot <- function(data, selectedSortDirection = "RV Percentage", houseSelection = "lannister"){
  data %>% 
    mutate(name = forcats::fct_reorder(name, .[[selectedSortDirection]], .desc = TRUE)) %>% 
    rename(`Vegas Implied Probabilities` = Vegas_Implied_Probabilities) %>% 
    gather("Source", "Probability", -name) %>% 
    ggplot(aes(x = name, y = Probability)) + 
    geom_col(aes(fill = Source), position = "dodge") + 
    scale_fill_got_d(option = houseSelection, direction = -1) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    xlab(label = "Name") +
    ggtitle("First Death - RV Average vs. Vegas Odds (Implied Probabilities)")
}
