# NFL Combine Performance and Player Success

This project explores NFL Combine data from 2016 to 2026 to examine patterns in player athletic testing. The goal is to see whether Combine performance is related to position and actual NFL performance.

## Overview

This project analyzes NFL Combine results and NFL player performance data to better understand how Combine metrics connect to real league outcomes. We are interested in whether certain Combine events, such as the 40-yard dash, bench press, vertical jump, broad jump, shuttle, and three-cone drill, show meaningful differences by position.

We also want to explore whether strong Combine performance translates into better NFL performance. Since the Combine is often used to evaluate draft prospects, this project looks at whether those measurements actually reveal useful information about future player success.

### Interesting Insight

One interesting insight from this project is that Combine performance is highly dependent on player position. For example, speed-based events may matter more for wide receivers, running backs, and defensive backs, while strength-based events may be more relevant for linemen.

Including a visualization comparing Combine event performance by position would help show these differences more clearly than summary statistics alone.

## Data Sources and Acknowledgements

The data for this project comes from publicly available NFL Combine and NFL performance datasets.

Sources used include:

- NFL Draft Combine data from Stathead/Score Network:  
  https://data.scorenetwork.org/football/nfl-draft-combine.html

- NFL Combine official dataset from GitHub:  
  https://github.com/array-carpenter/nfl-draft-data/blob/master/data/combine_official.csv

- NFL player performance data from nflverse:  
  https://github.com/nflverse

We acknowledge the creators and maintainers of these public datasets for making the data available for analysis.

## Current Plan

Our current plan is to clean and organize the NFL Combine data, then separate the data into useful groups based on position and event type. After that, we will create summary statistics and visualizations to compare Combine performance across positions and years.

We also plan to connect Combine data with NFL performance data to explore whether Combine results have any relationship with actual league performance. This plan may change as we continue working with the data and identify which variables are most useful.

Our main steps are:

1. Create and organize the GitHub repository.
2. Create the RStudio project.
3. Import the NFL Combine and NFL performance datasets.
4. Tidy and clean the datasets.
5. Split or organize the data by position, event type, and year.
6. Create summary statistics for Combine events.
7. Build visualizations comparing Combine performance by position.
8. Explore possible relationships between Combine performance and NFL performance.
9. Write the final report and update the README as the project develops.

## Repo Structure

The repository is organized to make the project easy to follow.

```text
project-folder/
│
├── data/
│   ├── raw/
│   └── cleaned/
│
├── plots/
│
├── scripts/
│
├── report/
│
├── README.md
└── project-plan.md