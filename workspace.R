library(tidyverse)
library(hoopR)
#devtools::install_github("lbenz730/ncaahoopR")
library(ncaahoopR)


# read in schedule for current men's college basketball season
schedule <- hoopR::load_mbb_schedule()

# view variable names in the schedule data frame
schedule |> names()

# read in .csv file of data from Kenpom (download on Kenpom homepage)
kenpom_data <- read_csv("summary26.csv")

# create data frame to specifically view names in kenpom_data data frame
kenpom_names <- kenpom_data |> 
  select(TeamName) |> 
  arrange(TeamName)

View(kenpom_names)

# create data frame to specifically view names in schedule data frame
schedule_names <- schedule |> 
  select(home_short_display_name) |> 
  filter(home_short_display_name != "TBD",
         home_short_display_name != "Chaminade") |> 
  arrange(home_short_display_name) |> 
  distinct(home_short_display_name) |> 
  rename(TeamName = home_short_display_name)

View(schedule_names)

# check if any team names in schedule data have a period
schedule_names |> filter(grepl(TeamName, "."))

# change Kenpom names to not include periods
kenpom_names <- kenpom_names |> 
  mutate(TeamName = str_replace_all(TeamName, "\\.", ""))

# change Kenpom names to not include periods (for the actual data)
kenpom_data <- kenpom_data |> 
  mutate(TeamName = str_replace_all(TeamName, "\\.", ""))

# define differences in team names between KenPom and ESPN
team_aliases <- tribble(
  ~kenpom_name,                           ~espn_name,
  "Arkansas Pine Bluff",                  "AR-Pine Bluff",
  "Abilene Christian",                    "Abilene Chrstn",
  "Albany",                               "UAlbany",
  "Appalachian St",                       "App State",
  "Ball St",                              "Ball State",
  "Boston University",                    "Boston U",
  "Bethune Cookman",                      "Bethune",
  "Central Arkansas",                     "C Arkansas",
  "Central Michigan",                     "C Michigan",
  "Central Connecticut",                  "C Connecticut",
  "Cal St Fullerton",                     "Fullerton",
  "Cal St Bakersfield",                    "Bakersfield",
  "Cal Baptist",                          "CA Baptist",
  "Texas A&M Corpus Chris",               "Texas A&M-CC",
  "North Carolina Central",               "NC Central",
  "North Carolina A&T",                   "NC A&T",
  "Northern Arizona",                     "N Arizona",
  "North Dakota St",                      "N Dakota St",
  "Northern Illinois",                    "N Illinois",
  "Northwestern St",                      "N'Western St",
  "Northern Colorado",                    "N Colorado",
  "Northern Kentucky",                    "N Kentucky",
  "Mississippi",                          "Ole Miss",
  "Massachusetts",                        "UMass",
  "CSUN",                                 "CSU Northridge",
  "Charleston Southern",                  "Charleston So",
  "Coastal Carolina",                     "Coastal",
  "Connecticut",                          "UConn",
  "East Tennessee St",                     "ETSU",
  "Eastern Illinois",                     "E Illinois",
  "Eastern Kentucky",                     "E Kentucky",
  "Eastern Michigan",                     "E Michigan",
  "East Texas A&M",                       "E Texas A&M",
  "Eastern Washington",                   "E Washington",
  "Florida Gulf Coast",                   "FGCU",
  "Fairleigh Dickinson",                  "FDU",
  "Florida Atlantic",                     "FAU",
  "Jacksonville St",                      "Jax State",
  "Maryland Eastern Shore",               "MD Eastern",
  "Purdue Fort Wayne",                    "Purdue FW",
  "Gardner Webb",                         "Gardner-Webb",
  "George Washington",                    "G Washington",
  "Georgia Southern",                     "GA Southern",
  "Grambling St",                         "Grambling",
  "Hawaii",                               "Hawai’i",
  "Houston Christian",                    "Hou Christian",
  "Iowa St",                              "Iowa State",
  "Illinois Chicago",                     "UIC",
  "Kent St",                              "Kent State",
  "Loyola Marymount",                     "LMU",
  "Middle Tennessee",                     "MTSU",
  "LIU",                                  "Long Island",
  "Louisiana Monroe",                     "UL Monroe",
  "Miami FL",                             "Miami",
  "Mississippi Valley St",                "Miss Valley St",
  "Nebraska Omaha",                       "Omaha",
  "Ohio St",                              "Ohio State",
  "Penn St",                              "Penn State",
  "Pittsburgh",                           "Pitt",
  "Prairie View A&M",                     "Prairie View",
  "Southern Illinois",                    "S Illinois",
  "South Carolina St",                    "SC State",
  "USC Upstate",                          "SC Upstate",
  "South Dakota St",                      "S Dakota St",
  "Southeastern Louisiana",               "SE Louisiana",
  "Southeast Missouri",                   "SE Missouri",
  "Stephen F Austin",                     "SF Austin",
  "Sam Houston St",                       "Sam Houston",
  "San Jose St",                          "San José St",
  "UC Santa Barbara",                     "Santa Barbara",
  "Seattle",                              "Seattle U",
  "Southern Indiana",                     "So Indiana",
  "St Thomas",                            "St Thomas (MN)",
  "Tennessee Martin",                     "UT Martin",
  "UT Rio Grande Valley",                 "UT Rio Grande",
  "Utah St",                              "Utah State",
  "Western Carolina",                     "W Carolina",
  "Western Illinois",                     "W Illinois",
  "Western Michigan",                     "W Michigan",
  "Western Kentucky",                     "Western KY",
  "Mount St Mary's",                      "Mount St Marys"
)

# clean the team names for the home team data
home_clean <- schedule |>
  filter(home_short_display_name != "TBD",
         home_short_display_name != "Chaminade",
         home_score > 0) |> 
  left_join(team_aliases, by = c("home_short_display_name" = "espn_name")) |>
  mutate(home_short_display_name = coalesce(kenpom_name, home_short_display_name)) |>
  select(game_id, date, home_short_display_name, home_score, conference_competition)

# combine the home team stats with their Kenpom adjusted tempo, offensive efficiency, defensive efficiency, and efficiency margin
home_combined_stats <- home_clean |> 
  left_join(kenpom_data, by = c("home_short_display_name" = "TeamName")) |> 
  rename(
    "home_adj_tempo" = "AdjTempo",
    "home_adj_off" = "AdjOE",
    "home_adj_def" = "AdjDE",
    "home_adj_em" = "AdjEM"
  ) |> 
  select(game_id, home_short_display_name, home_score, home_adj_tempo, home_adj_off, home_adj_def, home_adj_em)
  
# clean the team names for the away team data
away_clean <- schedule |>
  filter(away_short_display_name != "TBD",
         away_short_display_name != "Chaminade",
        away_score > 0) |> 
  left_join(team_aliases, by = c("away_short_display_name" = "espn_name")) |>
  mutate(away_short_display_name = coalesce(kenpom_name, away_short_display_name)) |>
  select(game_id, date, away_short_display_name, away_score, conference_competition)

# combine the away team stats with their Kenpom adjusted tempo, offensive efficiency, defensive efficiency, and efficiency margin
away_combined_stats <- away_clean |> 
  left_join(kenpom_data, by = c("away_short_display_name" = "TeamName")) |> 
  rename(
    "away_adj_tempo" = "AdjTempo",
    "away_adj_off" = "AdjOE",
    "away_adj_def" = "AdjDE",
    "away_adj_em" = "AdjEM"
  ) |> 
  select(game_id, away_short_display_name, away_score, away_adj_tempo, away_adj_off, away_adj_def, away_adj_em)

# combine the home and away stats to create the final data set for analysis
combined_stats <- home_combined_stats |> 
  left_join(away_combined_stats, by = "game_id")

# create point spread variable (will be the response variable)
combined_stats <- combined_stats |> 
  mutate(
    point_spread = home_score - away_score
  )

# view the final data
View(combined_stats)
