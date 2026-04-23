# Step 1: Install and Load Packages ----
install.packages("nflreadr")
install.packages("nflreadr", repos = c("https://nflverse.r-universe.dev", getOption("repos")))
remotes::install_github("nflverse/nflreadr")

library(tidyverse)
library(nflreadr)

# Step 2: Load Data ----
# Combine_Performance <- read.csv("combine_official.csv")
Player_Stats_2021 <- load_player_stats(2021)
Player_Stats_2022 <- load_player_stats(2022)
Player_Stats_2023 <- load_player_stats(2023)
Player_Stats_2024 <- load_player_stats(2024)
Player_Stats_2025 <- load_player_stats(2025)