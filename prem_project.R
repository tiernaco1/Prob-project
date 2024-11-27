library(dplyr)
remaining_fixtures <- read.csv("remaining fixtures.csv")
team_stats <- read.csv("Stats_for_PL.csv")
str(team_stats$Teams)

simulate_game <- function(home_team, away_team, team_stats){

home_stats <- team_stats %>% filter(Teams == home_team)
away_stats <- team_stats %>% filter(Teams == away_team)

home_xG_90 <- as.numeric(home_stats$xG.p90)
home_xGA_90 <- as.numeric(home_stats$xGA..p90)
away_xG_90 <- as.numeric(away_stats$xG.p90)
away_xGA_90 <- as.numeric(away_stats$xGA..p90)

home_xG <- home_xG_90 * 1.1
away_xG <- away_xG_90 * 0.9

home_xG_real <- (home_xG + away_xGA_90) / 2
away_xG_real <- (away_xG + home_xGA_90) / 2

home_goals <- rpois(1, lambda = home_xG_real)
away_goals <- rpois(1, lambda = away_xG_real)

if (home_goals > away_goals) {
  home_result <- "Win"
} else if (home_goals < away_goals) {
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
