# Step 1: Install and Load Packages ----

install.packages("nflreadr")
install.packages("nflreadr", repos = c("https://nflverse.r-universe.dev", getOption("repos")))
remotes::install_github("nflverse/nflreadr")

library(tidyverse)
library(nflreadr)
library(ggplot2)

# Step 2: Load Data ----

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

# Stack all combine years into one data frame
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

# Step 4: Tidy Player Stats ----

# Combine all player stats data frames into one
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

RB_YPG_Avg <- Player_Stats_All |>
  # Keep only data from regular season and for running backs
  filter(season_type == "REG", position == "RB") |>
  group_by(season, player_id, player_name) |>
  summarise(
    games_played  = n_distinct(week),
    total_rushing = sum(rushing_yards, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Exclude players with almost no carries
  # This avoids inflating YPG with backups who played 1 snap
  filter(games_played >= 3) |>
  # Calculate yard per game
  mutate(ypg = total_rushing / games_played) |>
  group_by(season) |>
  summarise(
    # Find average yards per game
    avg_rb_ypg = mean(ypg, na.rm = TRUE)
  )

# Join the two summary tables
TCD_RBYPG_Data <- inner_join(Combine_RB_Avg, RB_YPG_Avg, by = "season")

# Step 5: Create the plot ----

# Because the three cone drill and rushing YPG are on different scales,
# we will use a secondary axis. We need to use a linear transformation
# that maps one scale onto the other

# Scaling parameters — adjust if your data range changes
cone_range <- range(TCD_RBYPG_Data$avg_cone)
ypg_range <- range(TCD_RBYPG_Data$avg_rb_ypg)

scale_factor <- diff(cone_range) / diff(ypg_range)
shift <- cone_range[1] - ypg_range[1] * scale_factor

# Transform YPG onto the three cone axis for plotting
TCD_RBYPG_Data <- TCD_RBYPG_Data |>
  mutate(ypg_scaled = avg_rb_ypg * scale_factor + shift)

# Create the plot
TCD_RBYPG_Plot <- ggplot(TCD_RBYPG_Data, aes(x = season)) +
  geom_line(aes(y = avg_cone, color = "Avg 3-Cone (sec)"),
            linewidth = 1.2) +
  geom_point(aes(y = avg_cone, color = "Avg 3-Cone (sec)"),
             size = 3) +
  geom_line(aes(y = ypg_scaled, color = "Avg RB Rush YPG"),
            linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = ypg_scaled, color = "Avg RB Rush YPG"),
             size = 3) +
  scale_y_continuous(
    name = "Avg 3-Cone Drill Time (seconds)",
    sec.axis = sec_axis(
      transform = ~ (. - shift) / scale_factor,
      name = "Avg RB Rushing Yards Per Game"
    )
  ) +
  scale_x_continuous(breaks = 2016:2025) +
  scale_color_manual(
    values = c("Avg 3-Cone (sec)" = "#003f5c",
               "Avg RB Rush YPG" = "#ff6361")
  ) +
  labs(
    title = "NFL Combine RB 3-Cone Drill vs. RB Rushing Yards Per Game",
    subtitle = "Combine classes 2016–2025 | Regular season stats 2016–2025",
    x = "Season / Combine Year",
    color = NULL,
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    legend.position = "bottom",
    axis.title.y.left  = element_text(color = "#003f5c"),
    axis.title.y.right = element_text(color = "#ff6361")
  )

TCD_RBYPG_Plot

# Step 6: Tidy data for average RB 40 yard dash per year ----

Combine_RB_Forty_Avg <- Combine_All |>
  filter(pos == "RB") |>
  filter(!is.na(forty)) |>
  group_by(season) |>
  summarise(
    avg_forty = mean(forty, na.rm = TRUE)
  )

Forty_RBYPG_Data <- inner_join(Combine_RB_Forty_Avg, RB_YPG_Avg, by = "season")

# Step 7: Create the plot ----

# Because the forty yard dash and rushing YPG are on different scales,
# we will use a secondary axis. We need to use a linear transformation
# that maps one scale onto the other

# Scaling parameters
forty_range <- range(Forty_RBYPG_Data$avg_forty)
ypg_range <- range(Forty_RBYPG_Data$avg_rb_ypg)

scale_factor <- diff(forty_range) / diff(ypg_range)
shift <- forty_range[1] - ypg_range[1] * scale_factor

# Transform YPG onto the forty yard dash axis for plotting
Forty_RBYPG_Data <- Forty_RBYPG_Data |>
  mutate(ypg_scaled = avg_rb_ypg * scale_factor + shift)

# Create the plot
Forty_RBYPG_Plot <- ggplot(Forty_RBYPG_Data, aes(x = season)) +
  geom_line(aes(y = avg_forty, color = "Avg 40-Yard Dash (sec)"),
            linewidth = 1.2) +
  geom_point(aes(y = avg_forty, color = "Avg 40-Yard Dash (sec)"),
             size = 3) +
  geom_line(aes(y = ypg_scaled, color = "Avg RB Rush YPG"),
            linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = ypg_scaled, color = "Avg RB Rush YPG"),
             size = 3) +
  scale_y_continuous(
    name = "Avg 40-Yard Dash Time (seconds)",
    sec.axis = sec_axis(
      transform = ~ (. - shift) / scale_factor,
      name = "Avg RB Rushing Yards Per Game"
    )
  ) +
  scale_x_continuous(breaks = 2016:2025) +
  scale_color_manual(
    values = c("Avg 40-Yard Dash (sec)" = "#003f5c",
               "Avg RB Rush YPG" = "#ff6361")
  ) +
  labs(
    title = "NFL Combine RB 40-Yard Dash vs. RB Rushing Yards Per Game",
    subtitle = "Combine classes 2016–2025 | Regular season stats 2016–2025",
    x = "Season / Combine Year",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    legend.position = "bottom",
    axis.title.y.left = element_text(color = "#003f5c"),
    axis.title.y.right = element_text(color = "#ff6361")
  )

Forty_RBYPG_Plot

# Simple Combine_All Table for Running Backs ----

library(knitr)
library(dplyr)
library(stringr)

Combine_All_RB_Table <- Combine_All |>
  filter(pos == "RB") |>
  mutate(
    height_inches = case_when(
      # If ht looks like "5-11", convert to inches
      str_detect(as.character(ht), "^\\d-\\d{1,2}$") ~
        as.numeric(str_extract(as.character(ht), "^\\d")) * 12 +
        as.numeric(str_extract(as.character(ht), "\\d{1,2}$")),
      
      # If ht is already numeric, keep it
      suppressWarnings(!is.na(as.numeric(ht))) ~ as.numeric(ht),
      
      TRUE ~ NA_real_
    )
  ) |>
  group_by(season) |>
  summarise(
    rb_count = n(),
    avg_height_inches = round(mean(height_inches, na.rm = TRUE), 1),
    avg_weight = round(mean(wt, na.rm = TRUE), 1),
    avg_forty = round(mean(forty, na.rm = TRUE), 2),
    avg_vertical = round(mean(vertical, na.rm = TRUE), 1),
    avg_bench = round(mean(bench, na.rm = TRUE), 1),
    avg_cone = round(mean(cone, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(season)

kable(
  Combine_All_RB_Table,
  caption = "Table 1. Average NFL Combine Measurements for Running Backs by Season",
  col.names = c(
    "Season",
    "RB Count",
    "Avg Height (inches)",
    "Avg Weight",
    "Avg 40-Yard Dash",
    "Avg Vertical",
    "Avg Bench Reps",
    "Avg 3-Cone"
  )
)

