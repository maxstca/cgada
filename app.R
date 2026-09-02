#Packages
library(shiny)
library(bslib)
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(glue)
library(DT)

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
  date_arg <- gsub("_", "/", substring(f,15,24))
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
    
    nav_panel("Bar Charts",
              sidebarPanel(
                selectInput("bar.versionfilter", "Filter by Version: ", choices = c("-- NONE --" = "", versions)),
                div(tags$b(tags$u("Entry-based Measurements"))),
                br(),
                selectInput("entry.x","Choose Category:", choices = c("Class", "Trinket", "Map")),
                selectInput("entry.y","Choose Measurement:", choices = c("Kills", "Coins", "Lifetime")),
                div("NOTE: Lifetime is measured in seconds."),
                br(),
                div(tags$b(tags$u("Popularity Measurements"))),
                br(),
                selectInput("popularity.x","Choose Category:", choices = c("Class", "Trinket", "Map")),
                br(),
              ),
              
              mainPanel(
                plotOutput("entry"),
                br(),
                plotOutput("popularity")
              )
              ),
    
    nav_panel("Other Plots",
              
              sidebarPanel(
                selectInput("other.versionfilter", "Filter by Version (Kills vs. Coins): ", choices = c("-- NONE --" = "", versions))
              ),
              
              mainPanel(
                plotOutput("killsversuscoins"),
                br(),
                plotOutput("coinsovertime")
              )
              
              ),
    
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
                br(),
                DT::dataTableOutput("table")
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
  
  output$entry <- renderPlot({
    cgada |>
      filter(!nzchar(input$bar.versionfilter) | Version %in% input$bar.versionfilter) |>
      group_by(!!sym(input$entry.x)) |>
        summarize(Entry = n(),
                  Ratio = sum(!!sym(input$entry.y)) / sum(Entry),
                  sd = sd(!!sym(input$entry.y)),
                  se = sd / sqrt(Entry)) |>
      ggplot(aes(x = !!sym(input$entry.x), y = Ratio)) +
      geom_bar(stat = "identity", color = "black", fill = "lightblue") +
      theme(legend.position = "none") +
      xlab(input$entry.x) + ylab(input$entry.y) +
      labs(title = glue("{input$entry.x} {input$entry.y} per Entry"),
           caption = glue("Red line indicates the average {input$entry.y} per Entry, regardless of {input$entry.x}.
                          Error bars indicate values which we could likely observe on repeated playtests, accounting for sample size.")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      geom_errorbar(aes(ymin = Ratio - se,
                        ymax = Ratio + se),
                    width = 0.5) +
      geom_hline(yintercept = sum(cgada |>
                   filter(!nzchar(input$bar.versionfilter) | Version %in% input$bar.versionfilter) |>
                   select(!!sym(input$entry.y))) 
                   / nrow(cgada |>
                     filter(!nzchar(input$bar.versionfilter) | Version %in% input$bar.versionfilter)), color = "red")
                  
  })
  
  output$popularity <- renderPlot({
    cgada |>
      filter(!nzchar(input$bar.versionfilter) | Version %in% input$bar.versionfilter) |>
      group_by(!!sym(input$popularity.x), Date) |>
      summarize(Pop = sum(Lifetime) / 60) |>
      ungroup() |>
      group_by(!!sym(input$popularity.x)) |>
      summarize(sd = sd(Pop),
                PopTotal = sum(Pop),
                .groups = "drop") |>
      ggplot(aes(x = !!sym(input$popularity.x), y = PopTotal)) +
      geom_bar(stat = "identity", color = "black", fill = "pink2") +
      theme(legend.position = "none") +
      xlab(input$popularity.x) + ylab("Time Played (minutes)") +
      labs(title = glue("{input$popularity.x} Playtime"),
           caption = glue("Red line indicates expected popularity, assuming each {input$popularity.x} is equally played.
                          Error bars indicate values which we could likely observe if we repeated a sample of the same number of playtests.")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      geom_errorbar(aes(ymin = PopTotal - sd,
                        ymax = PopTotal + sd),
                    width = 0.5) +
      geom_hline(yintercept = (sum(cgada |>
                                     filter(!nzchar(input$bar.versionfilter) | Version %in% input$bar.versionfilter) |>
                                     select(Lifetime)) / 60) / nrow(distinct(cgada, !!(sym(input$popularity.x)))), color = "red")
  })
  
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
  
  output$table <- DT::renderDataTable({
    datatable(tableData(),options = list(ordering = TRUE))
  })
  
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