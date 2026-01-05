#Packages
library(shiny)
library(bslib)
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(glue)

#Helper files
source("helpers.R")

#Set ggplot theme
theme_set(theme_clean())

#Load in datasets, one per individual playtest
entries1 <- read_in_data(fileName = "data/v0_14_1_d01_04_2026.csv", playtestDate = "01/04/2026", playtestVersion = "0.14.*", hasHeaders = TRUE)

#Merge obs. into one set of data
dflist <- list(entries1)
cgada <- merge_playtests(dflist)
cgada <- generate_seqID(cgada) |>
  filter(Is_Complete == TRUE)

# UI

ui <- fluidPage(
  
  titlePanel("Cash Grab Arena Data Viewer"),
  
  navset_pill(
    
    #Background info on CGA and CGADA
    nav_panel("Home",
              div(a("Cash Grab Arena", href = "https://github.com/HazilTheNut/cashgrab/wiki"), "is a free-for-all, PvP arena game created within Minecraft using datapacks created by HazilTheNut."),
              div("The Cash Grab Arena Data Viewer is a tool built to visualize and analyze data collected from playtests of Cash Grab Arena created by Makse."),
              br(),
              div("The data used here is recorded under the following parameters:"),
              div("- Time Limit: 15 minutes"),
              div("- Coin Goal: 100 coins"),
              div("- 4+ players each game"),
              br(),
              div(glue("A total of {length(cgada$ID)} lives were played across {length(dflist)} playtests. The total playtime (in active games) on this version of Cash Grab was {round(sum(cgada$Lifetime) / 60 / 60, 2)} hours. Thank you to all who participated!"))
              ),
    
    nav_panel("Bar Charts"),
    
    nav_panel("Other Plots"),
    
    nav_panel("Fun Facts")
    
  )
  
)

# Server

server <- function(input, output, session) {
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)