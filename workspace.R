library(tidyverse)
library(hoopR)
#devtools::install_github("lbenz730/ncaahoopR")
library(ncaahoopR)
library(tidymodels)
library(glmnet)
library(rpart)
library(baguette)
library(ranger)

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
  left_join(away_combined_stats, by = "game_id") |> 
  # remove observations that did not include kenpom ratings (means one of the schools was non-DI)
  filter(
    !is.na(home_adj_em), 
    !is.na(away_adj_em)
  )

# create point spread variable (will be the response variable)
combined_stats <- combined_stats |> 
  mutate(
    point_spread = home_score - away_score
  )

# view the final data
View(combined_stats)



# split the data into training and test sets
# set seed so the results are replicable
set.seed(2824)

# split the data into a 75/25 training/test split, stratify the split by seasons
data_split <- initial_split(combined_stats, prop = 0.75)

# save the 75% split as training
train_data <- training(data_split)

# save the 25% split as testing
test_data <- testing(data_split)

# view the training and testing sets
train_data
test_data


View(train_data)

# want to create a 10 fold CV split on our training data
# create a function to do so (same as in notes from github pages)
get_cv_splits <- function(data, num_folds){
  #get fold size
  size_fold <- floor(nrow(data)/num_folds)
  #get random indices to subset the data with
  random_indices <- sample(1:nrow(data), size = nrow(data), replace = FALSE)
  #create a list to save our folds in
  folds <- list()
  #now cycle through our random indices vector and take the appropriate observations to each fold
  for(i in 1:num_folds){
    if (i < num_folds) {
      fold_index <- seq(from = (i-1)*size_fold +1, to = i*size_fold, by = 1)
      folds[[i]] <- data[random_indices[fold_index], ]
    } else {
      fold_index <- seq(from = (i-1)*size_fold +1, to = length(random_indices), by = 1)
      folds[[i]] <- data[random_indices[fold_index], ]
    }
  }
  return(folds)
}

# save the results in folds
folds <- get_cv_splits(train_data, 10)

# view the 10 fold CV
folds


# fit MLR models
# create some recipes with tidymodels
# first recipe: 
mlr_recipe_1 <- recipe(point_spread ~  home_adj_off + home_adj_def + away_adj_off + away_adj_def + home_adj_tempo + away_adj_tempo, data = train_data) |> 
  # normalize all numeric variables
  step_normalize(all_numeric_predictors())

# view results
mlr_recipe_1 |> 
  prep(training = train_data) |> 
  bake(train_data)

# names of variables in recipe 1
mlr_recipe_1  |> 
  prep(training = train_data) |> 
  bake(train_data) |> 
  names()


# second recipe (no paces included): 
mlr_recipe_2 <- recipe(point_spread ~  home_adj_off + home_adj_def + away_adj_off + away_adj_def, data = train_data) |> 
  # normalize all numeric variables
  step_normalize(all_numeric_predictors())

# view results
mlr_recipe_2 |> 
  prep(training = train_data) |> 
  bake(train_data)

# names of variables in recipe 1
mlr_recipe_2  |> 
  prep(training = train_data) |> 
  bake(train_data) |> 
  names()

# third recipe (use efficiency margins instead of offensive and defensive efficiencies): 
mlr_recipe_3 <- recipe(point_spread ~  home_adj_em + away_adj_em + home_adj_tempo + away_adj_tempo, data = train_data) |> 
  # normalize all numeric variables
  step_normalize(all_numeric_predictors())

# view results
mlr_recipe_3 |> 
  prep(training = train_data) |> 
  bake(train_data)

# names of variables in recipe 1
mlr_recipe_3  |> 
  prep(training = train_data) |> 
  bake(train_data) |> 
  names()

# fourth recipe (don't normalize): 
mlr_recipe_4 <- recipe(point_spread ~  home_adj_off + home_adj_def + away_adj_off + away_adj_def + home_adj_tempo + away_adj_tempo, data = train_data)

# view results
mlr_recipe_4 |> 
  prep(training = train_data) |> 
  bake(train_data)

# names of variables in recipe 1
mlr_recipe_4  |> 
  prep(training = train_data) |> 
  bake(train_data) |> 
  names()



# fit and compare the MLR models
# set the engine for our models to be "lm"
mlr_mod <- linear_reg() |> 
  set_engine("lm")

# create a workflow that adds our recipe and our model with corresponding function
mlr_workflow_1 <- workflow() |>
  add_recipe(mlr_recipe_1) |>
  add_model(mlr_mod)
mlr_workflow_1

mlr_workflow_2 <- workflow() |>
  add_recipe(mlr_recipe_2) |>
  add_model(mlr_mod)
mlr_workflow_2

mlr_workflow_3 <- workflow() |>
  add_recipe(mlr_recipe_3) |>
  add_model(mlr_mod)
mlr_workflow_3

mlr_workflow_4 <- workflow() |>
  add_recipe(mlr_recipe_4) |>
  add_model(mlr_mod)
mlr_workflow_4

# use a 10 fold CV in the training set
data_10_fold <- vfold_cv(train_data, 10)

# get the metrics for the 10 CV fit on recipe 1
mlr_CV_fits_1 <- mlr_workflow_1 |>
  fit_resamples(data_10_fold)

# get the metrics for the 10 CV fit on recipe 2
mlr_CV_fits_2 <- mlr_workflow_2 |>
  fit_resamples(data_10_fold)

# get the metrics for the 10 CV fit on recipe 3
mlr_CV_fits_3 <- mlr_workflow_3 |>
  fit_resamples(data_10_fold)

# get the metrics for the 10 CV fit on recipe 4
mlr_CV_fits_4 <- mlr_workflow_4 |>
  fit_resamples(data_10_fold)

# view the metrics from the three models
rbind(mlr_CV_fits_1 |> collect_metrics(), 
      mlr_CV_fits_2 |> collect_metrics(), 
      mlr_CV_fits_3 |> collect_metrics(),
      mlr_CV_fits_4 |> collect_metrics())


# fit the model from recipe 1
mlr_fit <- mlr_workflow_1 |>
  fit(train_data)

# see how the model performs on the test data
mlr_workflow_1 |>
  last_fit(data_split) |>
  collect_metrics()

# view the final model coefficient table
mlr_fit |> 
  extract_fit_parsnip() |> 
  tidy()


# LASSO model
# specify the LASSO model
LASSO_spec <- linear_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet")

# create the workflow for our LASSO model
LASSO_wkf <- workflow() |>
  add_recipe(mlr_recipe_1) |>
  add_model(LASSO_spec)

LASSO_wkf

# fit the model with tune_grid() and grid_regular()
# a warning will occur for one value of the tuning parameter, safe to ignore
LASSO_grid <- LASSO_wkf |>
  tune_grid(resamples = data_10_fold,
            grid = grid_regular(penalty(), levels = 200),
            metrics = metric_set(mae)) 

# view what LASSO_grid gives us
LASSO_grid

# view the mae of each of our 200 fit LASSO models 
LASSO_grid[1, ".metrics"][[1]]

# pick the LASSO model with the best MAE
lowest_rmse_LASSO <- LASSO_grid |>
  select_best(metric = "mae")

lowest_rmse_LASSO

# finalize workflow with the best model and fit it to the training set
LASSO_final_wkf <- LASSO_wkf |>
  finalize_workflow(lowest_rmse_LASSO) 

LASSO_final <- LASSO_final_wkf |>
  last_fit(data_split, metrics = metric_set(mae))


# regression trees
# define our regression tree's model and engine
reg_tree_mod <- decision_tree(tree_depth = tune(),
                              min_n = 20,
                              cost_complexity = tune()) |>
  set_engine("rpart") |>
  set_mode("regression")

# create the workflow for our regression tree
reg_tree_wkf <- workflow() |>
  add_recipe(mlr_recipe_1) |>
  add_model(reg_tree_mod)

reg_tree_wkf

# tune our regression tree with 10 fold CV (using default levels)
reg_tree_fits <- reg_tree_wkf |> 
  tune_grid(resamples = data_10_fold,
            metrics = metric_set(mae))

# view the metrics of our tree with different tunings
reg_tree_fits |> 
  collect_metrics()

# sort the trees by how they perform in terms of MAE
reg_tree_fits |>
  collect_metrics() |>
  filter(.metric == "mae") |>
  arrange(mean)

# select the best tree in terms of MAE
reg_tree_best_params <- select_best(reg_tree_fits, metric = "mae")

# view the best tree's parameters
reg_tree_best_params

# finalize our workflow and fit our best regression tree to the training data
reg_tree_final_wkf <- reg_tree_wkf |>
  finalize_workflow(reg_tree_best_params)

reg_tree_final_fit <- reg_tree_final_wkf |>
  last_fit(data_split, metrics = metric_set(mae))

reg_tree_final_fit


# Bagged tree model
# set up our model type and engine for our bagged tree model, tune on cost complexity
bag_spec <- bag_tree(tree_depth = 5, min_n = 10, cost_complexity = tune()) |>
  set_engine("rpart") |>
  set_mode("regression")

# create a workflow for the bagged tree (uses baguette library)
bag_wkf <- workflow() |>
  add_recipe(mlr_recipe_1) |>
  add_model(bag_spec)

# fit our bagged tree models
bag_fit <- bag_wkf |>
  tune_grid(
    resamples = data_10_fold,
    grid = grid_regular(cost_complexity(),
                        levels = 15),
    metrics = metric_set(mae)
  )

bag_fit

# sort the fitted bagged trees by their MAE
bag_fit |>
  collect_metrics() |>
  filter(.metric == "mae") |>
  arrange(mean)

# get our bagged tree model with the best tuning parameter
bag_best_params <- select_best(bag_fit, metric = "mae")

bag_best_params

# update the workflow and fit our best bagged tree model on the training set
bag_final_wkf <- bag_wkf |>
  finalize_workflow(bag_best_params)

bag_final_fit <- bag_final_wkf |>
  last_fit(data_split, metrics = metric_set(mae))


# Random Forest Model
# set up our model type and engine for our random forest model, tune on number of parameters to split on
rf_spec <- rand_forest(mtry = tune()) |>
  set_engine("ranger") |>
  set_mode("regression")

# create the workflow for our random forest model
rf_wkf <- workflow() |>
  add_recipe(mlr_recipe_1) |>
  add_model(rf_spec)

# fit our random forest models
rf_fit <- rf_wkf |>
  tune_grid(
    resamples = data_10_fold,
    grid = 7,
    metrics = metric_set(mae)
  )

# view which of our random forest models work best based on MAE
rf_fit |>
  collect_metrics() |>
  filter(.metric == "mae") |>
  arrange(mean)

# save our best tuning parameters
rf_best_params <- select_best(rf_fit, metric = "mae")
rf_best_params

# update the workflow and fit our best random forest model on the training set
rf_final_wkf <- rf_wkf |>
  finalize_workflow(rf_best_params)

rf_final_fit <- rf_final_wkf |>
  last_fit(data_split, metrics = metric_set(mae))


set.seed(2824)
# compare metrics of each of the final models above
rbind(
  # MLR
  mlr_workflow_1 |>
    last_fit(data_split, metrics = metric_set(mae)) |>
    collect_metrics() |>
    mutate(Model = "MLR", .before = ".metric"),
  # LASSO
  LASSO_final |> 
    collect_metrics() |> 
    mutate(Model = "LASSO", .before = ".metric"),
  # regression tree
  reg_tree_final_fit |> 
    collect_metrics() |> 
    mutate(Model = "REG TREE", .before = ".metric"),
  # bagged tree
  bag_final_fit |> 
    collect_metrics() |> 
    mutate(Model = "BAG TREE", .before = ".metric"),
  # random forest
  rf_final_fit |> 
    collect_metrics() |> 
    mutate(Model = "RAND FOR", .before = ".metric")
)


# fit our MLR model to the entire data set
final_model <- mlr_workflow_1 |> 
  fit(combined_stats)

# view our entire final model
tidy(final_model) |> print(n = 100)

0.418*126.7 - 0.36*123.7 - 0.397*91.1 + 0.331*102.1 + 0.0172*65.9 + 0.0109*68.1

# numbers for UNC @ Duke
unc_vs_duke <- tibble(
  home_adj_off   = 126.7,
  home_adj_def   = 123.7,
  away_adj_off   = 91.1,
  away_adj_def   = 102.1,
  home_adj_tempo = 65.9,
  away_adj_tempo = 68.1,
  home_adj_em = 35.64,
  away_adj_em = 21.61
)

# numbers for Duke @ UNC
duke_vs_unc <- tibble(
  away_adj_off   = 126.7,
  away_adj_def   = 123.7,
  home_adj_off   = 91.1,
  home_adj_def   = 102.1,
  away_adj_tempo = 65.9,
  home_adj_tempo = 68.1,
  away_adj_em = 35.64,
  home_adj_em = 21.61
)

# tibble where you can read in the kenpom value for teams to then put in the model to predict
game_prediction <- tibble(
  home_adj_off   = combined_stats |> 
    filter(home_short_display_name == "NC State") |> 
    select(home_adj_off) |> 
    slice(1) |> 
    pull(),
  home_adj_def   = combined_stats |> 
    filter(home_short_display_name == "NC State") |> 
    select(home_adj_def) |> 
    slice(1) |> 
    pull(),
  home_adj_tempo = combined_stats |> 
    filter(home_short_display_name == "NC State") |> 
    select(home_adj_tempo) |> 
    slice(1) |> 
    pull(),
  home_adj_em = combined_stats |> 
    filter(home_short_display_name == "NC State") |> 
    select(home_adj_em) |> 
    slice(1) |> 
    pull(),
  away_adj_off   = combined_stats |> 
    filter(away_short_display_name == "Virginia Tech") |> 
    select(away_adj_off) |> 
    slice(1) |> 
    pull(),
  away_adj_def   =  combined_stats |> 
    filter(away_short_display_name == "Virginia Tech") |> 
    select(away_adj_def) |> 
    slice(1) |> 
    pull(),
  away_adj_tempo =  combined_stats |> 
    filter(away_short_display_name == "Virginia Tech") |> 
    select(away_adj_tempo) |> 
    slice(1) |> 
    pull(),
  away_adj_em =  combined_stats |> 
    filter(away_short_display_name == "Virginia Tech") |> 
    select(away_adj_em) |> 
    slice(1) |> 
    pull()
)

# predict the final point spread(home team - away team) of games
predict(final_model, new_data = game_prediction)
