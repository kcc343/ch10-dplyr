# Exercise 7: using dplyr on external data

# Load the dplyr library

library(dplyr)

# Use the `read.csv()` function to read in the included data set. Remember to
# save it as a variable.

team_data <- read.csv("data/nba_teams_2016.csv", stringsAsFactors = FALSE)

# View the data frame you loaded, and get some basic information about the 
# number of rows/columns. 
# Note the "X" preceding some of the column titles as well as the "*" following
# the names of teams that made it to the playoffs that year.

View(team_data)

# Add a column that gives the turnovers to steals ratio (TOV / STL) for each team

team_data <- team_data %>% 
  mutate(turn_steal_ratio = TOV / STL)

# Sort the teams from lowest turnover/steal ratio to highest
# Which team has the lowest turnover/steal ratio?

least_ratio <- team_data %>% 
  filter(turn_steal_ratio == min(turn_steal_ratio)) %>% 
  select(Team)

# Using the pipe operator, create a new column of assists per game (AST / G) 
# AND sort the data.frame by this new column in descending order.

team_data <- team_data %>% 
  mutate(assists_per_game = AST / G) %>% 
  arrange(desc(assists_per_game))

# Create a data frame called `good_offense` of teams that scored more than 
# 8700 points (PTS) in the season

good_offense <- team_data %>% 
  filter(PTS >= 8700)
  
# Create a data frame called `good_defense` of teams that had more than 
# 470 blocks (BLK)

good_defense <- team_data %>% 
  filter(BLK > 470)

# Create a data frame called `offense_stats` that only shows offensive 
# rebounds (ORB), field-goal % (FG.), and assists (AST) along with the team name.

offense_stats <- team_data %>% 
  select(Team, ORB, FG., AST)

# Create a data frame called `defense_stats` that only shows defensive 
# rebounds (DRB), steals (STL), and blocks (BLK) along with the team name.

defense_stats <- team_data %>% 
  select(Team, DRB, STL, BLK)

# Create a function called `better_shooters` that takes in two teams and returns
# a data frame of the team with the better field-goal percentage. Include the 
# team name, field-goal percentage, and total points in your resulting data frame

better_shooters <- function(team_one, team_two) {
  better_team <- team_data %>% 
    filter(Team %in% c(team_one, team_two)) %>% 
    filter(FG. == max(FG.)) %>% 
    select(Team, FG., PTS)
}

# Call the function on two teams to compare them (remember the `*` if needed)

sackings_vs_washwiz <- better_shooters("Sacramento Kings", "Washington Wizards")

