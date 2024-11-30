library(dplyr)
remaining_fixtures <- read.csv("remaining fixtures.csv", header = TRUE)
team_stats <- read.csv("Stats_for_PL.csv", header = TRUE)
str(team_stats$Teams)

simulate_game <- function(home_team, away_team, team_stats, monte_carlo = 500) {
  
  home_stats <- team_stats %>% filter(Teams == home_team)
  away_stats <- team_stats %>% filter(Teams == away_team)
  # gets values in allign with variable names
  home_xG_90 <- as.numeric(home_stats$xG.p90)
  home_xGA_90 <- as.numeric(home_stats$xGA..p90)
  away_xG_90 <- as.numeric(away_stats$xG.p90)
  away_xGA_90 <- as.numeric(away_stats$xGA..p90)
  
  home_goals_p90 <- as.numeric((home_stats$G..p90))
  away_goals_p90 <- as.numeric((away_stats$G..p90))
  
  home_goals_against_p90 <- as.numeric((home_stats$GA..p90))
  away_goals_against_p90 <- as.numeric((away_stats$GA..p90))
  #averages goals and xG
  home_xG_90 <- (home_goals_p90 + home_xG_90) / 2
  away_xG_90 <- (away_goals_p90 + away_xG_90) / 2
  # averages goals against xGA
  home_xGA_90 <- (home_goals_against_p90 + home_xGA_90) / 2
  away_xGA_90 <- (away_goals_against_p90 + away_xGA_90) / 2
  # finds average of goals for and other teams against
  home_xG_real <- (home_xG_90 + away_xGA_90) / 2
  away_xG_real <- (away_xG_90 + home_xGA_90) / 2
  
  home_shots <- as.numeric((home_stats$shots.p90))
  away_shots <- as.numeric((away_stats$shots.p90))
  
  home_convRate <- as.numeric((home_stats$conversion.rate))
  away_convRate <- as.numeric((away_stats$conversion.rate))
  
  home_xG <- 0.5 * (home_xG_real) + 0.5 * (home_shots * home_convRate) 
  away_xG <- 0.5 * (away_xG_real) + 0.5 * (away_shots * away_convRate) 
  
  
  # adjust based on home advantage
  home_xG <- home_xG * 1.2
  away_xG <- away_xG * 0.8
  
  
  
  #home_xG <- home_xG * runif(1, 0.9, 1.1)
  #away_xG <- away_xG * runif(1, 0.9, 1.1)
  
  home_wins <- 0
  away_wins <- 0
  draws <- 0
  
  # Initialize result counters
  home_wins <- 0
  away_wins <- 0
  draws <- 0
  
  for (i in 1:monte_carlo) {
    home_goals <- rpois(1, lambda = home_xG)
    away_goals <- rpois(1, lambda = away_xG)
    
    if (home_goals > away_goals) {
      home_wins <- home_wins + 1
    } else if (home_goals < away_goals) {
      away_wins <- away_wins + 1
    } else {
      draws <- draws + 1
    }
  }
  
  # Determine the most likely outcome
  if (home_wins > away_wins && home_wins > draws) {
    home_result <- "Win"
  } else if (away_wins > home_wins && away_wins > draws) {
    home_result <- "Loss"
  } else {
    home_result <- "Draw"
  }
  
  return(list(
    HomeTeam = home_team,
    AwayTeam = away_team,
    HomeResult = home_result,
    HomeWins = home_wins,
    AwayWins = away_wins,
    Draws = draws,
    ExpectedGoals = list(Home = home_xG, Away = away_xG)
  ))
}


simulate_game("Liverpool", "Man Utd", team_stats)

simulate_reamining_season <- function(remaining_fixtures, team_stats) {
  # Create a data frame to track teams, points, wins, draws, losses
  teams <- data.frame(
    Teams = team_stats$Teams,
    Pts = team_stats$Pts,
    Wins = rep(0, nrow(team_stats)),
    Draws = rep(0, nrow(team_stats)),
    Losses = rep(0, nrow(team_stats))
  )
  
  # Loop through each remaining fixture
  for (i in 1:nrow(remaining_fixtures)) {
    # Extract Home Team and Away Team for each row
    home_team <- remaining_fixtures$Home.Team[i]
    away_team <- remaining_fixtures$Away.Team[i]
    
    # Simulate the game
    result <- simulate_game(home_team, away_team, team_stats)
    
    # Update the points and result counters
    if (result$HomeResult == "Win") {
      teams <- teams %>%
        mutate(
          Pts = ifelse(Teams == home_team, Pts + 3, Pts),
          Wins = ifelse(Teams == home_team, Wins + 1, Wins),
          Losses = ifelse(Teams == away_team, Losses + 1, Losses)
        )
    } else if (result$HomeResult == "Loss") {
      teams <- teams %>%
        mutate(
          Pts = ifelse(Teams == away_team, Pts + 3, Pts),
          Wins = ifelse(Teams == away_team, Wins + 1, Wins),
          Losses = ifelse(Teams == home_team, Losses + 1, Losses)
        )
    } else { # Draw
      teams <- teams %>%
        mutate(
          Pts = ifelse(Teams == home_team, Pts + 1, Pts),
          Pts = ifelse(Teams == away_team, Pts + 1, Pts),
          Draws = ifelse(Teams == home_team, Draws + 1, Draws),
          Draws = ifelse(Teams == away_team, Draws + 1, Draws)
        )
    }
  }
  
  # Sort teams by points, wins, goal difference (if needed for a tiebreaker)
  teams <- teams %>%
    arrange(desc(Pts), desc(Wins), desc(Draws))
  
  # Assign ranks to the teams
  rownames(teams) <- seq(1, nrow(teams))
  
  # Print the final table with all details
  print(teams)
}


simulate_reamining_season(remaining_fixtures, team_stats)

