library(nblR)
library(dplyr)
library(zoo)
library(xgboost)
library(caret)

# Get the team box scores data
team_box <- nbl_box_team()
results_wide <- nbl_results(wide_or_long = "wide")

team_box <- team_box %>%
  left_join(
    results_wide %>%
      select(match_id, home_team_name, away_team_name, match_time), 
    by = "match_id"
  )

# Combine the data for home and away teams, and sort by team and match date
combined_team_df <- team_box %>%
  mutate(
    team = name,
    opponent = opp_name,
    match_date = as.Date(match_time, format = "%Y-%m-%d")  # Adjust date format if needed
  ) %>%
  select(match_id, season, match_date, team, opponent, everything()) %>%
  arrange(team, match_date)

# Function to calculate rolling averages for each team over the last 5 games (home and away)
calculate_rolling_averages <- function(df) {
  df %>%
    group_by(team) %>%
    mutate(across(
      where(is.numeric), 
      ~ lag(rollapply(.x, width = 5, FUN = mean, fill = NA, align = "right")),
      .names = "avg_{col}"
    )) %>%
    ungroup()
}

# Apply the function to calculate the averages over the last 5 games (home and away combined)
team_rolling_avg_df <- calculate_rolling_averages(combined_team_df)

if ("home_away" %in% colnames(team_rolling_avg_df)) {
  home_df <- team_rolling_avg_df[team_rolling_avg_df$home_away == "home", ]
  away_df <- team_rolling_avg_df[team_rolling_avg_df$home_away == "away", ]
  

  names(home_df)[3:ncol(home_df)] <- paste0("home.", names(home_df)[3:ncol(home_df)])

  names(away_df)[3:ncol(away_df)] <- paste0("away.", names(away_df)[3:ncol(away_df)])
  

  combined_df <- merge(home_df, away_df, by = "match_id")
  
  # View the result
  head(combined_df)
} else {
  message("Column 'home_away' is missing. Please check your data structure.")
}

# Now, split the data into home and away teams for further analysis
home_df <- team_rolling_avg_df[team_rolling_avg_df$home_away == "home", ]
away_df <- team_rolling_avg_df[team_rolling_avg_df$home_away == "away", ]
# Rename columns for home team
names(home_df)[3:ncol(home_df)] <- paste0("home.", names(home_df)[3:ncol(home_df)])
# Rename columns for away team
names(away_df)[3:ncol(away_df)] <- paste0("away.", names(away_df)[3:ncol(away_df)])
# Merge the home and away data on match_id
combined_df <- merge(home_df, away_df, by = "match_id")

# Function to calculate new ELO ratings
calculate_elo <- function(elo_team1, elo_team2, result_team1, k = 20) {
  expected_team1 <- 1 / (1 + 10 ^ ((elo_team2 - elo_team1) / 400))
  new_elo_team1 <- elo_team1 + k * (result_team1 - expected_team1)
  return(new_elo_team1)
}

# Combine home and away data into a long format where each row represents a single team for the match
combine_elo_df <- function(df) {
  home_df <- df %>%
    mutate(team_name = home_team_name,
           team_score = as.numeric(home_score_string),
           opponent_name = away_team_name,
           opponent_score = as.numeric(away_score_string),
           result = ifelse(home_score_string > away_score_string, 1, 0)) %>%
    select(match_id, season, team_name, opponent_name, team_score, opponent_score, result)
  
  away_df <- df %>%
    mutate(team_name = away_team_name,
           team_score = as.numeric(away_score_string),
           opponent_name = home_team_name,
           opponent_score = as.numeric(home_score_string),
           result = ifelse(away_score_string > home_score_string, 1, 0)) %>%
    select(match_id, season, team_name, opponent_name, team_score, opponent_score, result)
  
  combined_elo_df <- bind_rows(home_df, away_df)
  
  return(combined_elo_df)
}

# Initialize ELO ratings for all teams
initialize_elo <- function(teams, initial_elo = 1500) {
  tibble(team_name = unique(teams), elo = initial_elo)
}


update_elo_ratings <- function(df, elo_df) {
  df <- df %>%
    rowwise() %>%
    mutate(
      elo_team = elo_df$elo[elo_df$team_name == team_name],
      elo_opponent = elo_df$elo[elo_df$team_name == opponent_name],
      new_elo_team = calculate_elo(elo_team, elo_opponent, result),
      new_elo_opponent = calculate_elo(elo_opponent, elo_team, 1 - result)
    ) %>%
    ungroup()
  

  elo_df <- elo_df %>%
    mutate(elo = case_when(
      team_name == df$team_name ~ df$new_elo_team,
      team_name == df$opponent_name ~ df$new_elo_opponent,
      TRUE ~ elo
    ))
  
  return(list(elo_df = elo_df, pre_match_elo = df))
}


calculate_elo_rankings <- function(df, initial_elo = 1500, k = 20) {
  # Combine home and away games into combined_elo_df
  combined_elo_df <- combine_elo_df(df)
  
  # Initialize the ELO ratings for all teams
  teams <- unique(c(df$home_team_name, df$away_team_name))
  elo_df <- initialize_elo(teams, initial_elo)
  

  df <- df %>%
    mutate(
      home_team_elo_before = NA_real_,
      away_team_elo_before = NA_real_
    )
  
  # Loop through each match to update ELO rankings and store pre-match ELOs
  for (i in seq_len(nrow(df))) {
    # Get the home and away team names
    home_team <- df$home_team_name[i]
    away_team <- df$away_team_name[i]
    
    home_team_elo <- elo_df$elo[elo_df$team_name == home_team]
    away_team_elo <- elo_df$elo[elo_df$team_name == away_team]
    

    df$home_team_elo_before[i] <- home_team_elo
    df$away_team_elo_before[i] <- away_team_elo
    

    temp_match <- tibble(
      match_id = df$match_id[i],
      season = df$season[i],
      team_name = home_team,
      opponent_name = away_team,
      team_score = as.numeric(df$home_score_string[i]),
      opponent_score = as.numeric(df$away_score_string[i]),
      result = ifelse(df$home_score_string[i] > df$away_score_string[i], 1, 0)
    )
    
    # Update the ELO ratings for both teams
    result <- update_elo_ratings(temp_match, elo_df)
    elo_df <- result$elo_df
  }
  
  return(df)
}

# Apply the function on your results_wide dataframe
results_with_elo <- calculate_elo_rankings(results_wide)


# Convert 'match_time' to Date format 
results_with_elo$match_time <- as.Date(results_with_elo$match_time)
results_with_elo <- results_with_elo[order(results_with_elo$match_time), ]


last_game_dates <- list()
home_last_game_days <- numeric(nrow(results_with_elo))
away_last_game_days <- numeric(nrow(results_with_elo))


for (i in 1:nrow(results_with_elo)) {
  home_team <- results_with_elo$home_team_name[i]
  away_team <- results_with_elo$away_team_name[i]
  match_date <- results_with_elo$match_time[i]
  
  # Check if the home team has played before and calculate days since their last game
  if (home_team %in% names(last_game_dates)) {
    home_last_game_days[i] <- as.numeric(difftime(match_date, last_game_dates[[home_team]], units = "days"))
  } else {
    home_last_game_days[i] <- NA  # No previous game found
  }
  
  # Check if the away team has played before and calculate days since their last game
  if (away_team %in% names(last_game_dates)) {
    away_last_game_days[i] <- as.numeric(difftime(match_date, last_game_dates[[away_team]], units = "days"))
  } else {
    away_last_game_days[i] <- NA  # No previous game found
  }
  
  # Update the last game date for both the home and away teams
  last_game_dates[[home_team]] <- match_date
  last_game_dates[[away_team]] <- match_date
}

results_with_elo$home_last_game <- home_last_game_days
results_with_elo$away_last_game <- away_last_game_days


results_with_elo %>%
  select(match_id, home_team_name, away_team_name, home_team_elo_before, away_team_elo_before, home_score_string, away_score_string, match_time, home_last_game, away_last_game) %>%
  head()

combined_df <- combined_df %>%
  left_join(
    results_with_elo %>%
      select(match_id, home_team_name, away_team_name, home_team_elo_before, away_team_elo_before,  home_last_game, away_last_game, match_time), 
    by = c("home.team" = "home_team_name", "away.team" = "away_team_name", "match_id" = "match_id")
  )

combined_df %>%
  select(match_id, home.team, away.team, home_team_elo_before, away_team_elo_before) %>%
  head()

combined_df <- combined_df %>%
  filter(!is.na(away.avg_score) & !is.na(home.avg_score))

combined_df <- combined_df %>%
  mutate(home.result = ifelse(home.score > away.score, 1, 0))


combined_df <- combined_df %>%
  filter(!is.na(home.score))



#ADD NBL ODDS
# First, make sure the Date columns are in the same format if they aren't already
nbl_betting_odds$Date <- as.Date(nbl_betting_odds$Date, format="%Y-%m-%d")

combined_data <- merge(combined_df, 
                     nbl_betting_odds[, c("Date", "Home Team", "Away Team", "Home Odds", "Away Odds", "Home Odds Open", "Away Odds Open", "Home Odds Close", "Away Odds Close")],
                     by.x = c("match_time", "home.opponent", "away.opponent"),
                     by.y = c("Date", "Away Team", "Home Team"),
                     all.x = TRUE)

combined_df <- combined_data


#DELETE ANY NULL ODDS ROWS
combined_df <- combined_df[!is.na(combined_df$`Home Odds`) & !is.na(combined_df$`Away Odds`), ]

combined_df$home_prob <- (1/combined_df$`Home Odds`)/((1/combined_df$`Home Odds`) + (1/combined_df$`Away Odds`))


#Exclude current season on training data
train_data <- combined_df %>% filter(!season.x %in% c("2024-2025"))


# Filter prediction data (only 2024-2025)
predict_data <- combined_df %>% filter(season.x == "2024-2025")
predict_data$match_time <- as.Date(predict_data$match_time, format="%Y-%m-%d")
# Check column names to ensure correct features
print(colnames(train_data))

# Features for model to take into account
features <- c("home_team_elo_before", "away_team_elo_before", 
              "home.avg_score", "away.avg_score", "home.team", "away.team", "home.avg_fouls", "away.avg_fouls",
              "home.avg_three_pointers_made", "away.avg_three_pointers_made","away.avg_three_pointers_percentage", "home.avg_three_pointers_percentage",
              "home.avg_field_goals_attempted", "away.avg_field_goals_attempted","home.avg_turnovers", "away.avg_turnovers","home.avg_rebounds_defensive", "away.avg_rebounds_defensive",
              "home.avg_rebounds_offensive", "away.avg_rebounds_offensive","home.avg_blocks", "away.avg_blocks", "home.avg_assists", "away.avg_assists", 
              "home.avg_steals", "away.avg_steals", "home_last_game", "away_last_game"
)

# Encoding team names are numerical ID
train_data$home.team <- as.numeric(as.factor(train_data$home.team))
train_data$away.team <- as.numeric(as.factor(train_data$away.team))

predict_data$home.team <- as.numeric(as.factor(predict_data$home.team))
predict_data$away.team <- as.numeric(as.factor(predict_data$away.team))

set.seed(42)

# Training setup column
X_train <- as.matrix(train_data[, features])
y_train <- train_data$home_prob
X_predict <- as.matrix(predict_data[, features])
dtrain <- xgb.DMatrix(data = X_train, label = y_train)

# XGBoost Setup
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",  
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  eval_metric = "logloss"
)


xgb_model <- xgboost(params = params, data = dtrain, nrounds = 100, verbose = 1)
dpredict <- xgb.DMatrix(data = X_predict)

# Set up future predictions based on model
predictions <- predict(xgb_model, dpredict)

#This Column will store match predictions
predict_data$predicted_home_prob <- predictions

# Show Predictions
head(predict_data[, c("match_id", "home.team", "away.team", "predicted_home_prob")])
