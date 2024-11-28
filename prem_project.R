library(dplyr)
remaining_fixtures <- read.csv("remaining fixtures.csv", header = TRUE)
team_stats <- read.csv("Stats_for_PL.csv", header = TRUE)
str(team_stats$Teams)

simulate_game <- function(home_team, away_team, team_stats, monte_carlo = 500){
  
  home_stats <- team_stats %>% filter(Teams == home_team)
  away_stats <- team_stats %>% filter(Teams == away_team)
  
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
  
  # adjust based on home advantage
  home_xG <- home_xG_90 * 1.2
  away_xG <- away_xG_90 * 0.8
  
  home_xG_real <- (home_xG + away_xGA_90) / 2
  away_xG_real <- (away_xG + home_xGA_90) / 2
  
  home_xGD <- as.numeric(home_stats$xGD.p90)
  away_xGD <- as.numeric(away_stats$xGD.p90)
  
  home_xG_real <- home_xG_real + max(0, home_xGD * 0.1)
  away_xG_real <- away_xG_real + max(0, away_xGD * 0.1)
  
  home_position <- as.numeric(home_stats$position)
  away_position <- as.numeric(away_stats$position)
  
  home_xG_real <- home_xG_real *(2 * ((21 - home_position) / 20))
  away_xG_real <- away_xG_real * (2 * ((21 - away_position) / 20))
  
  home_wins <- 0
  away_wins <- 0
  draws <- 0
  
  for (i in 1:monte_carlo) {
    home_goals <- rpois(1, lambda = home_xG_real)
    away_goals <- rpois(1, lambda = away_xG_real)
    if (home_goals > away_goals) {
      home_wins <- home_wins + 1
    } else if (home_goals < away_goals) {
      away_wins <- away_wins + 1
    } else {
      draws <- draws + 1
    }
  }

  
  if (home_wins > away_wins && home_wins > draws) {
    home_result <- "Win"
  } else if (home_wins < away_wins && draws < away_wins) {
    home_result <- "Loss"
  } else {
    home_result <- "Draw"
  }
  
  return(list(
    HomeTeam = home_team,
    AwayTeam = away_team,
    HomeResult = home_result,
    HomeGoals = home_goals,
    AwayGoals = away_goals,
    ExpectedGoals = list(Home = home_xG_real, Away = away_xG_real)
  ))
  
}

simulate_game("Man Utd", "Ipswich", team_stats)

simulate_reamining_season <- function(remaining_fixtures, team_stats){
  teams <- matrix(nrow = 20, ncol = 2) #create table 
  for (i in 1:nrow(team_stats)) {
    teams[i,1] <- team_stats$Teams[i]  #populate teams
    teams[i,2] <- team_stats$Pts[i]    # populate thier current points
  }
  #print(teams)
  #loops through each remaining fixture
  for (i in 1:nrow(remaining_fixtures)) {
    # Extract Home Team and Away Team for each row
    home_team <- remaining_fixtures$Home.Team[i]
    away_team <- remaining_fixtures$Away.Team[i]
    
    # Print or store the result (for demonstration purposes, we'll print)
    #print(paste("Row", i, "- Home Team:", home_team, "Away Team:", away_team))
    result <- simulate_game(home_team, away_team, team_stats)  # simulated game
    #print(result$HomeResult)
    
    home_stats <- team_stats %>% filter(Teams == home_team)
    home_startPos <- as.numeric(home_stats$position)          # find original postion in the table
    away_stats <- team_stats %>% filter(Teams == away_team)
    away_startPos <- as.numeric(home_stats$position)
    # following section adds points to the respective teams 
    if (result$HomeResult == "Win"){
      points <- as.numeric(teams[home_startPos,2])
      teams[home_startPos,2] <- points + 3
    }
    else if (result$HomeResult == "Loss"){
      points <- as.numeric(teams[away_startPos,2])
      teams[away_startPos,2] <- points + 3
    }
    else{
      points <- as.numeric(teams[home_startPos,2])
      teams[home_startPos,2] <- points + 1
      points <- as.numeric(teams[away_startPos,2])
      teams[away_startPos,2] <- points + 1
    }
  }
  teams <- teams[order(teams[, 2], decreasing = TRUE),]
  my_table <- as.table(teams)
  colnames(my_table) <- c("Teams", "Pts")
  rownames(my_table) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
  print(my_table)
}

simulate_reamining_season(remaining_fixtures, team_stats)

