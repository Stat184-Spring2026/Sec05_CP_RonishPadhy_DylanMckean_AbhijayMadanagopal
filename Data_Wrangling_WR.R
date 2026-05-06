# Step 1: Install and Load Packages ----

install.packages("nflreadr")
install.packages("nflreadr", repos = c("https://nflverse.r-universe.dev",
                                       getOption("repos")))
remotes::install_github("nflverse/nflreadr")

# load everything we need to run this WR analysis
library(tidyverse)
library(dplyr)
library(nflreadr)
library(ggplot2)


# Step 2: Load Data ----

# pulling in each combine year one at a time for WR analysis
Combine_2016 <- load_combine(2016)
Combine_2017 <- load_combine(2017)
Combine_2018 <- load_combine(2018)
Combine_2019 <- load_combine(2019)
Combine_2020 <- load_combine(2020)
Combine_2021 <- load_combine(2021)
Combine_2022 <- load_combine(2022)
Combine_2023 <- load_combine(2023)
Combine_2024 <- load_combine(2024)
Combine_2025 <- load_combine(2025)

# grabbing the matching player stats for each season
Player_Stats_2016 <- load_player_stats(2016)
Player_Stats_2017 <- load_player_stats(2017)
Player_Stats_2018 <- load_player_stats(2018)
Player_Stats_2019 <- load_player_stats(2019)
Player_Stats_2020 <- load_player_stats(2020)
Player_Stats_2021 <- load_player_stats(2021)
Player_Stats_2022 <- load_player_stats(2022)
Player_Stats_2023 <- load_player_stats(2023)
Player_Stats_2024 <- load_player_stats(2024)
Player_Stats_2025 <- load_player_stats(2025)


# Step 3: Tidy Combine Data ----

# stack every combine year together into one big data frame
Combine_All <- bind_rows(
  Combine_2016,
  Combine_2017,
  Combine_2018,
  Combine_2019,
  Combine_2020,
  Combine_2021,
  Combine_2022,
  Combine_2023,
  Combine_2024,
  Combine_2025
)

# reminder: a lower three cone time = faster and more agile WR
# so if the average drops over time that is actually a good thing
Combine_WR_Avg <- Combine_All |>
  
  # we only want wide receivers here, everyone else gets dropped
  filter(pos == "WR") |>
  
  # remove any WR who didnt run the three cone at the combine
  filter(!is.na(cone)) |>
  
  group_by(season) |>
  summarise(
    
    # average three cone time across all WRs for that year
    avg_cone = mean(cone, na.rm = TRUE)
  )


# Step 4: Get WR Receiving Stats from Player Data ----

# stack all the player stats years into one big table
Player_Stats_All <- bind_rows(
  Player_Stats_2016,
  Player_Stats_2017,
  Player_Stats_2018,
  Player_Stats_2019,
  Player_Stats_2020,
  Player_Stats_2021,
  Player_Stats_2022,
  Player_Stats_2023,
  Player_Stats_2024,
  Player_Stats_2025
)

# filter down to just wide receivers and get their yards per game
# data is weekly so we have to build YPG ourselves from receiving_yards
WR_YPG_Avg <- Player_Stats_All |>
  filter(position == "WR") |>
  filter(season_type == "REG") |>
  
  # group by season and player to get individual totals first
  group_by(season, player_id, player_name) |>
  summarise(
    games_played = n_distinct(week),
    total_rec_yards = sum(receiving_yards, na.rm = TRUE),
    .groups = "drop"
  ) |>
  
  # drop WRs who barely played to avoid inflating the average
  filter(games_played >= 3) |>
  
  # now calculate each WRs individual yards per game
  mutate(player_ypg = total_rec_yards / games_played) |>
  group_by(season) |>
  summarise(
    
    # average receiving YPG across all qualifying WRs
    avg_wr_ypg = mean(player_ypg, na.rm = TRUE)
  )

# now we join the combine averages with the receiving averages
TCD_WRYPG_Data <- inner_join(Combine_WR_Avg,
                             WR_YPG_Avg, by = "season")


# Step 5: Build the WR 3-Cone Plot ----

# the 3 cone times and receiving YPG live on totally different scales
# so we have to do a linear transformation to get them on the same axis

cone_range   <- range(TCD_WRYPG_Data$avg_cone)
ypg_range  <- range(TCD_WRYPG_Data$avg_wr_ypg)

# figure out how to stretch the YPG values to match the cone scale
scale_factor <- diff(cone_range) / diff(ypg_range)
shift <- cone_range[1] - ypg_range[1] * scale_factor

# add the scaled YPG column so we can plot it on the left axis
TCD_WRYPG_Data <- TCD_WRYPG_Data |>
  mutate(
    ypg_scaled = avg_wr_ypg * scale_factor + shift
  )

# actually build the plot now
TCD_WRYPG_Plot <- ggplot(TCD_WRYPG_Data, aes(x = season)) +
  
  # solid line for the 3 cone drill times
  geom_line(aes(y = avg_cone, color = "Avg 3-Cone (sec)"),
            linewidth = 1.2) +
  geom_point(aes(y = avg_cone, color = "Avg 3-Cone (sec)"),
             size = 3) +
  
  # dashed line for the receiving yards per game
  geom_line(aes(y = ypg_scaled, color = "Avg WR Rec YPG"),
            linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = ypg_scaled, color = "Avg WR Rec YPG"),
             size = 3) +
  
  # left axis shows cone times, right axis converts back to actual YPG
  scale_y_continuous(
    name = "Avg 3-Cone Drill Time (seconds) — lower is faster",
    sec.axis = sec_axis(
      transform = ~ (. - shift) / scale_factor,
      name = "Avg WR Receiving Yards Per Game"
    )
  ) +
  
  scale_x_continuous(breaks = 2016:2025) +
  
  # keep the colors consistent across both lines
  scale_color_manual(
    values = c("Avg 3-Cone (sec)" = "#003f5c",
               "Avg WR Rec YPG"   = "#ff6361")
  ) +
  
  labs(
    title = "NFL Combine WR 3-Cone Drill vs. WR Receiving Yards Per Game",
    subtitle = "Combine classes 2016–2025 | Regular season stats 2016–2025",
    x = "Season / Combine Year",
    color = NULL,
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    legend.position = "bottom",
    axis.title.y.left  = element_text(color = "#003f5c"),
    axis.title.y.right = element_text(color = "#ff6361")
  )

TCD_WRYPG_Plot


# Step 6: Tidy Data for Average WR 40-Yard Dash Per Year ----

# grab the forty yard dash averages for WRs only
Combine_WR_Forty_Avg <- Combine_All |>
  filter(pos == "WR") |>
  filter(!is.na(forty)) |>
  
  group_by(season) |>
  summarise(
    avg_forty = mean(forty, na.rm = TRUE)
  )

# join the forty averages with the receiving YPG we already built
Forty_WRYPG_Data <- inner_join(Combine_WR_Forty_Avg,
                               WR_YPG_Avg, by = "season")


# Step 7: Build the WR 40-Yard Dash Plot ----

# same dual axis problem here, forty times and YPG need to be rescaled
forty_range  <- range(Forty_WRYPG_Data$avg_forty)
ypg_range  <- range(Forty_WRYPG_Data$avg_wr_ypg)

# stretch the YPG to sit on the same axis as the forty times
scale_factor <- diff(forty_range) / diff(ypg_range)
shift <- forty_range[1] - ypg_range[1] * scale_factor

# add the scaled column to the data frame before plotting
Forty_WRYPG_Data <- Forty_WRYPG_Data |>
  mutate(
    ypg_scaled = avg_wr_ypg * scale_factor + shift
  )

# build the forty yard dash plot
Forty_WRYPG_Plot <- ggplot(Forty_WRYPG_Data, aes(x = season)) +
  
  # solid line for the forty yard dash times
  geom_line(aes(y = avg_forty, color = "Avg 40-Yard Dash (sec)"),
            linewidth = 1.2) +
  geom_point(aes(y = avg_forty, color = "Avg 40-Yard Dash (sec)"),
             size = 3) +
  
  # dashed line for receiving yards per game
  geom_line(aes(y = ypg_scaled, color = "Avg WR Rec YPG"),
            linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = ypg_scaled, color = "Avg WR Rec YPG"),
             size = 3) +
  
  # left axis is forty times, right axis converts back to actual YPG
  scale_y_continuous(
    name = "Avg 40-Yard Dash Time (seconds) — lower is faster",
    sec.axis = sec_axis(
      transform = ~ (. - shift) / scale_factor,
      name = "Avg WR Receiving Yards Per Game"
    )
  ) +
  
  scale_x_continuous(breaks = 2016:2025) +
  
  # matching colors to the three cone plot for consistency
  scale_color_manual(
    values = c("Avg 40-Yard Dash (sec)" = "#003f5c",
               "Avg WR Rec YPG"         = "#ff6361")
  ) +
  
  labs(
    title = "NFL Combine WR 40-Yard Dash vs. WR Receiving Yards Per Game",
    subtitle = "Combine classes 2016–2025 | Regular season stats 2016–2025",
    x = "Season / Combine Year",
    color = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    legend.position = "bottom",
    axis.title.y.left  = element_text(color = "#003f5c"),
    axis.title.y.right = element_text(color = "#ff6361")
  )

Forty_WRYPG_Plot