# Load in relevant packages
library(ggplot2)
library(glue)
library(purrr)
library(tidyverse)
library(ggforce)
library(ggthemes)
library(ggrepel)

#Read input data as a dataframe
read_in_data <- function(fileName, playtestDate = "99/99/9999", playtestVersion = "0.0.*", hasHeaders = FALSE) {
  out <- read.csv(fileName, hasHeaders)
  colnames(out) <- c("ID", "Class", "Trinket", "Map", "Game_ID", "Kills", "Coins", "Is_Complete", "Lifetime")
  out <- out |>
    mutate(Date = playtestDate,
           Version = playtestVersion,
           Lifetime = Lifetime / 20)
  return(out)
}

#Merge data from individual playtests
merge_playtests <- function(dataframes) {
  out <- dataframes[[1]]
  
  if (length(dataframes) == 1) {
    return(out)
  }
  
  for (i in 2:length(dataframes)) {
    dataframes[[i]]$Game_ID <- dataframes[[i]]$Game_ID + max(dataframes[[i - 1]]$Game_ID)
    dataframes[[i]]$ID <- dataframes[[i]]$ID + max(dataframes[[i - 1]]$ID)
    out <- rbind(out, dataframes[[i]])
  }
  
  return(out)
}

#Create seqID variable for use in Coins over Time display
generate_seqID <- function(x) {
  
  x |>
    mutate(ID = ID + 1) |>
    group_by(Game_ID) |>
    mutate(seqID = 1:n()) |>
    ungroup()
  
}

