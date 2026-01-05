#Create a function that generates dummy data to use for testing purposes
generate_test_data <- function(fileName = "test_data", n_obs = 10000) {
  
  base_class <- c("Alchemist", "Artillery", "Battlesmith", "Champion", "Chronomancer", 
                  "Cloudbreaker", "Dancer", "Evincer", "Frost Knight", "Griefer", 
                  "Hexblade", "Ranger", "Skirmisher", "Spectre", "Stargazer", 
                  "Titan", "Vampire")
  base_trinket <- c("Accelerator", "Bandage", "Echo Bolt", "Flame Trap", "Hedge Seed", 
                    "Lava Cake", "Nox Bomb", "Risk-it-Biscuit", "Smoke Screen", "Spring Plume", 
                    "Vigor Flask")
  base_map <- c("Fissure", "Crystal", "Sunken", "Dustworks", "Polaris", 
                "Scald", "Golden", "Bogpit", "Deepgrove", "Skullway", 
                "Graphite")
  random_nums <- runif(n_obs)
  random_ticks <- 
  
  
  Index <- seq(1, n_obs, by = 1)
  Class <- sample(base_class, size = n_obs, replace = TRUE)
  Trinket <- sample(base_trinket, size = n_obs, replace = TRUE)
  Map <- sample(base_map, size = n_obs, replace = TRUE)
  Game_ID <- sample(c(1:50), size = n_obs, replace = TRUE)
  Kills <- sample(c(0:5), size = n_obs, replace = TRUE)
  Coins <- sample(c(1:40), size = n_obs, replace = TRUE)
  Complete <- ifelse(random_nums <= 0.05, FALSE, TRUE)
  Lifetime <- round(rnorm(500, mean = 60 * 20, sd = 20 * 20))
  
  
  out <- data.frame(Index, Class, Trinket, Map, Game_ID, Kills, Coins, Complete, Lifetime)
  write.csv(out, paste0("./data/", fileName, ".csv"), row.names = FALSE)
  
}

generate_test_data("test_data", 500)
generate_test_data("test_data2", 500)

