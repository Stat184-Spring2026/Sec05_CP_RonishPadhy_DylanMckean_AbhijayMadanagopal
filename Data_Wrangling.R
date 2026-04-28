# Step 1: Install and Load Packages ----

install.packages("nflreadr")
install.packages("nflreadr", repos = c("https://nflverse.r-universe.dev", getOption("repos")))
remotes::install_github("nflverse/nflreadr")

library(tidyverse)
library(nflreadr)
library(ggplot2)

# Step 2: Load Data ----

Combine_2021 <- load_combine(2021)
Combine_2022 <- load_combine(2022)
Combine_2023 <- load_combine(2023)
Combine_2024 <- load_combine(2024)
Combine_2025 <- load_combine(2025)

Player_Stats_2021 <- load_player_stats(2021)
Player_Stats_2022 <- load_player_stats(2022)
Player_Stats_2023 <- load_player_stats(2023)
Player_Stats_2024 <- load_player_stats(2024)
Player_Stats_2025 <- load_player_stats(2025)

# Step 3: Tidy Combine Data ----

# Stack all combine years into one data frame
Combine_All <- bind_rows(
  Combine_2021,
  Combine_2022,
  Combine_2023,
  Combine_2024,
  Combine_2025
)

# Contextual note: Lower three cone time means player is
# faster and more agile so a declining average means improvement.
Combine_RB_Avg <- Combine_All |>
  # Filter out all other positions besides running back
  filter(pos == "RB") |> 
  # Drop rows with no three cone performance  
  filter(!is.na(cone)) |> 
  group_by(season) |>
  summarise(
    # Find average time for three cone drill
    avg_cone = mean(cone, na.rm = TRUE)
  )
