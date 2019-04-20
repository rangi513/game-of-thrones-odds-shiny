# Game of Thrones Death Pool Shiny Web App
# Ryan Angi
# 4/18/19

library(shiny)
library(tidyverse)
library(magrittr)
library(gameofthrones)
library(DT)
library(shinythemes)

# Params
options(DT.options = list(pageLength = 100))
plotHeight <- 750
  
source("./R/dataCleaning.R")
source("./R/dataPlotting.R")
source("./R/createLeaderboard.R")
source("./R/createHouseIconDivs.R")

# Create Icons and selections for houses
selectableHouses <- unique(gameofthrones::got.map$house)
houseIcons <- createHouseIconDivs(selectableHouses)

## Data Read and Setup
got_data <- read_csv("./csv/got_submissions.csv") 
got_data_cleaned <- clean_got_submission_data(got_data)
death_pool_by_episode <- clean_death_data_by_episode(got_data)

## Results
results <- read_csv("./csv/current_results.csv")


## Create Vegas Implied Probabilities
american_odds <- read_csv("./csv/got_odds.csv")
death_pool_vegas <- american_odds %>% 
  mutate(Vegas_Implied_Probabilities = purrr::map_dbl(will_die, convert_american_odds_to_prob)) %>% 
  select(name, Vegas_Implied_Probabilities)

dies_first_vegas <- american_odds %>% 
  mutate(Vegas_Implied_Probabilities = purrr::map_dbl(die_first, convert_american_odds_to_prob)) %>% 
  select(name, Vegas_Implied_Probabilities)

take_the_throne_vegas <- american_odds %>% 
  mutate(Vegas_Implied_Probabilities = purrr::map_dbl(take_the_throne, convert_american_odds_to_prob)) %>% 
  select(name, Vegas_Implied_Probabilities)

# Base Plotting DataFrames
death_pool_vs_vegas_avg <- combine_death_pool_data(got_data_cleaned, death_pool_vegas) 
take_the_throne_combined <- combine_take_the_throne_data(got_data_cleaned, take_the_throne_vegas)
dies_first_combined <- combine_die_first_data(got_data_cleaned, dies_first_vegas)

# Define UI for application that draws a histogram
ui <- navbarPage(
                 id = "tabs",
                 theme = shinytheme("cosmo"),
                 title = "Game of Thrones Death Pool",
                 tabPanel('House',
                          fluidRow(column(12, align="center",
                          img(src='got-logo.jpg', align = "center", width="70%"),
                          titlePanel("House Selection"),
                          radioButtons('house',
                                       "Select Your House/Character Colors:",
                                       choiceNames = houseIcons,
                                       choiceValues = selectableHouses,
                                       selected = character(0)
                                       )))),
                 tabPanel('Graphics',
                  fluidRow(column(12, align="center",
                                  img(src='got-logo.jpg', align = "center", width="70%"))),
                          
                  fluidRow(
                    column(3, 
                       selectizeInput("full_name_plots",
                                      "Submission Name:",
                                      choices = c("All", sort(unique(got_data_cleaned$full_name))))),
                    column(3,radioButtons("pool_plots", 
                                          "Pool Filter:",
                                          choices = c("All Data" = "All",
                                                      "Paid" = "Paid",
                                                      "Free" = "Free"))),
                    column(3,
                           radioButtons("sortdirection",
                                        "Sort Plots By:",
                                        c("RV Percentage" = "RV Percentage",
                                          "Vegas Implied Probabilities" = "Vegas_Implied_Probabilities")))),
                  fluidRow(
                    tabsetPanel(type = "tabs",
                                tabPanel("Death Pool", plotOutput("death_pool_plot", height = paste0(plotHeight, "px"))),
                                tabPanel("Take The Throne", plotOutput("take_the_throne_plot", height = paste0(plotHeight, "px"))),
                                tabPanel("First to Die", plotOutput("first_death_plot", height = paste0(plotHeight, "px")))
                                )
                  )
                 ),
                 tabPanel(title = "Leaderboard",
                          fluidRow(column(12, align="center",
                          img(src='got-logo.jpg', align = "center", width="70%"))),
                          fluidRow(column(3, 
                                          selectizeInput("full_name",
                                                         "Submission Name:",
                                                         choices = c("All", sort(unique(got_data_cleaned$full_name))))),
                                   column(3,radioButtons("pool", 
                                                         "Pool Filter:",
                                                         choices = c("All Data" = "All",
                                                                     "Paid" = "Paid",
                                                                     "Free" = "Free")))),
                          titlePanel('Leaderboard'),
                          dataTableOutput("leaderboard")),
                 tabPanel(title = "Odds and Data",
                          
                          fluidRow(
                            column(5, 
                                   titlePanel('American Odds'),
                                   dataTableOutput("raw_odds")),
                            column(1),
                            column(6,
                                   titlePanel('Raw Submissions'),
                                   dataTableOutput("raw_submissions"))
                          )
                  ),
                 tabPanel(title = "Methodology",
                          fluidRow(
                            column(12,align = "center",
                          img(src='got-logo.jpg',  width="70%"))),
                          fluidRow(column(12,
                                          withMathJax(includeMarkdown("R/generateMethodology.Rmd"))
                            ))
                          
                          )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ## Render Plots
    output$death_pool_plot <- renderCachedPlot({
      generate_death_pool_plot(death_pool_vs_vegas_avg, got_data_cleaned, input$full_name_plots, input$sortdirection, input$house)
    }, cacheKeyExpr = list(input$full_name_plots, input$sortdirection, input$house))
  
   output$take_the_throne_plot <- renderCachedPlot({
     generate_take_the_throne_plot(take_the_throne_combined, got_data_cleaned, input$full_name_plots, input$sortdirection, input$house)
   }, cacheKeyExpr = list(input$full_name_plots, input$sortdirection, input$house))
   
   output$first_death_plot <- renderCachedPlot({
     generate_die_first_plot(dies_first_combined, got_data_cleaned, input$full_name_plots, input$sortdirection, input$house)
   }, cacheKeyExpr = list(input$full_name_plots, input$sortdirection, input$house))
   
   
   ## Leaderboard
   output$leaderboard <- renderDataTable({
     create_leaderboard(got_data_cleaned, 
                        take_the_throne_vegas, 
                        death_pool_vegas, 
                        death_pool_by_episode,
                        dies_first_vegas, 
                        results,
                        input$full_name, 
                        input$pool)
   }, style = 'bootstrap', 
      class = 'table-bordered table-condensed')
   
   observeEvent(input$house,
                updateNavbarPage(session, "tabs",
                                  selected = "Graphics"))
   
   
   ## Raw Data Tables
   output$raw_submissions <- renderDataTable(got_data)
   output$raw_odds <- renderDataTable({american_odds},
                                      caption = "Odds Last Updated on April 15th")
}

# Run the application 
shinyApp(ui = ui, server = server)

