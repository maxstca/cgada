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

#Grab a list of filenames
folder_path <- "data"
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

df_list <- list()

for (f in file_list) {
  
  #Get necessary args
  date_arg <- gsub("_", "/", substring(f,7,12))
  version_arg <- sub(".$", "*", gsub("_", ".", substring(f,7,12)))
  
  #Read in file
  df <- read_in_data(fileName = f, playtestDate = date_arg, playtestVersion = version_arg, hasHeaders = TRUE)
  
  #Add it to the list
  fname <- basename(f)
  df_list[[fname]] <- df
}

cgada <- merge_playtests(df_list)
cgada <- generate_seqID(cgada) |>
  filter(Is_Complete == TRUE)

#Grab distinct observations for categorical variables
classes <- unique(cgada$Class)
trinkets <- unique(cgada$Trinket)
maps <- unique(cgada$Map)
versions <- unique(cgada$Version)

# UI

ui <- fluidPage(
  
  titlePanel("Cash Grab Arena Data Viewer"),
  
  navset_pill(
    
    #Background info on CGA and CGADA
    nav_panel("Home",
              div(a("Cash Grab Arena", href = "https://github.com/HazilTheNut/cashgrab/wiki"), "is a free-for-all, PvP arena game within Minecraft created by HazilTheNut."),
              div("The Cash Grab Arena Data Viewer is a tool built to visualize and analyze data collected from playtests of Cash Grab Arena created by Makse."),
              br(),
              div("The data used here is recorded under the following parameters:"),
              div("- Time Limit: 15 minutes"),
              div("- Coin Goal: 100 coins"),
              div("- Version: v0.14.0-current"),
              div("- 4+ players each game"),
              div("- Only observations where the player died (referred to as `Complete` observations) are kept"),
              br(),
              div(glue("A total of {length(cgada$ID)} lives were played across {length(df_list)} playtests. The total playtime (in active games) across these Cash Grab playtests was {round(sum(cgada$Lifetime) / 60 / 60, 2)} hours. Thank you to all who participated!"))
              ),
    
    nav_panel("Bar Charts"),
    
    nav_panel("Other Plots",
              
              sidebarPanel(
                selectInput("other.versionfilter", "Filter by Version (Kills vs. Coins): ", choices = c("-- NONE --" = "", versions)),
              ),
              
              mainPanel(
                plotOutput("killsversuscoins"),
                plotOutput("coinsovertime")
              )
              
              ),
    
    nav_panel("Fun Facts"),
    
    nav_panel("Raw Data",
              
              sidebarPanel(
                selectInput("raw.versionfilter", "Filter by Version: ", choices = c("-- NONE --" = "", versions), multiple = TRUE),
                selectInput("raw.classfilter", "Filter by Class: ", choices = c("-- NONE --" = "", classes), multiple = TRUE),
                selectInput("raw.trinketfilter", "Filter by Trinket: ", choices = c("-- NONE --" = "", trinkets), multiple = TRUE),
                selectInput("raw.mapfilter", "Filter by Map: ", choices = c("-- NONE --" = "", maps), multiple = TRUE),
                downloadButton("savedata", "Save current output")
              ),
              
              mainPanel(
                tableOutput("summary_table"),
                tableOutput("table")
              )
              
              )
    
  )
  
)

# Server

server <- function(input, output, session) {
  
  # Reactive objects
  
  tableData <- reactive({
    cgada |>
      filter(is.null(input$raw.versionfilter) | Version %in% input$raw.versionfilter,
             is.null(input$raw.classfilter) | Class %in% input$raw.classfilter,
             is.null(input$raw.trinketfilter) | Trinket %in% input$raw.trinketfilter,
             is.null(input$raw.mapfilter) | Map %in% input$raw.mapfilter
      )
  })
  
  # Outputs
  
  output$killsversuscoins <- renderPlot({
    cgada |>
      filter(!nzchar(input$other.versionfilter) | Version %in% input$other.versionfilter) |>
      group_by(Class) |>
      summarise(Entry = n(),
                KPE = sum(Kills) / sum(Entry),
                CPE = sum(Coins) / sum(Entry)) |>
      ggplot(aes(x = KPE, y = CPE, color = Class, label = Class)) +
      geom_point(position =) + geom_text_repel() +
      xlab("Kills per Entry") + ylab("Coins per Entry") +
      labs(title = "Class Specializations: Kills per Entry vs. Coins per Entry") +
      geom_hline(yintercept = sum(cgada$Coins) / nrow(cgada), color = "gray") +
      geom_vline(xintercept = sum(cgada$Kills) / nrow(cgada), color = "gray") +
      theme(legend.position = "none") +
      annotate(geom = "text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.1, label = "Coin-specialized", color = "black", alpha = 0.4) +
      annotate(geom = "text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, label = "High Event", color = "black", alpha = 0.4) +
      annotate(geom = "text", x = -Inf, y = -Inf, hjust = -0.1, vjust = -0.2, label = "Low Event", color = "black", alpha = 0.4) +
      annotate(geom = "text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.2, label = "Kill-specialized", color = "black", alpha = 0.4) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank())
    })
  
  output$coinsovertime <- renderPlot(cgada |>
    group_by(Game_ID) |>
    ggplot(aes(x = seqID, y = Coins, color = Map)) +
    geom_point() +
    geom_line() +
    stat_summary(aes(group = 1), geom = "line", fun.y = mean, size = 1.5, color = "black") +
    stat_summary(geom = "ribbon", fun.data = 'mean_sdl', mult = 1, color = "gray", alpha = 0.05) +
    labs(title = "Coins Over Time (All Observations)",
         caption = "Bold line is the average coins per life across all games played for the given sequential ID.
       Ribbon indicates error bounds of one standard deviaton above and below the average coins per life.") + xlab("Sequential Entry ID")
    )
  
  output$table <- renderTable(tableData())
  
  output$summary_table <- renderTable({
    tableData() |>
      summarise(
        'Lives Played' = n(),
        'Avg. Lifetime (seconds)' = mean(Lifetime),
        'Std. Deviation in Lifetime' = sd(Lifetime),
        'Avg. Kills per Life' = mean(Kills),
        'Std. Deviation in Kills per Life' = sd(Kills),
        'Avg. Coins per Life' = mean(Coins),
        'Std. Deviation in Coins per Life' = sd(Coins)
      )
  })
  
  output$savedata <- downloadHandler(
    filename = "Cash Grab Arena data.csv",
    content = function(file) {
      write.csv(tableData(), file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)