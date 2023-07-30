## immaculate grid 07-27-2023

# set up 
library(Lahman)
library(tidyverse)

# data 
pitch = Lahman::Pitching
awards = Lahman::AwardsPlayers
guide = Lahman::Teams %>% 
  select(teamID, name)
names = Lahman::People %>% 
  select(playerID, nameFirst, nameLast)

bat = names %>% 
  left_join(Lahman::Batting, by = "playerID")
pitch = names %>% 
  left_join(Lahman::Pitching, by = "playerID")

# create function to do this 
get_team_square = function(team1, team1_name, team2, team2_name, filter_condition = "> 0"){
  x = bat %>% 
    filter(teamID == team1 | teamID == team2) %>% 
    group_by(playerID, nameFirst, nameLast) %>% 
    summarize({{ team1_name }} := sum(teamID == team1), {{ team2_name }} := sum(teamID == team2)) %>% 
    filter(.data[[team1_name]] > 0 & .data[[team2_name]] > 0) %>% 
    arrange(.data[[team1_name]], .data[[team2_name]]) %>% 
    select(nameFirst, nameLast) %>% 
    head(1)
  
  print(x)
}

# get_team_stat = 
get_pitch_stat = function(team, stat, value){
x = pitch %>% 
  filter(teamID == {{ team }}) %>% 
  filter({{ stat }} >= value) %>% 
  arrange(yearID) %>% 
  select(nameFirst, nameLast) %>% 
  head(1)

print(x)
}

# square 1 - red sox and white sox 
s1 = get_team_square(team1 = "CHA", "white_sox", "BOS", "red_sox")

# square 2 - white sox and braves ]
s2 = get_team_square("CHA", "white_sox", "ATL", "braves")

s2 = bat %>% 
  filter(teamID == "CHA" | teamID == "ATL") %>% 
  group_by(playerID, nameFirst, nameLast) %>% 
  summarize(white_sox = sum(teamID == "CHA"), braves = sum(teamID == "ATL")) %>% 
  filter(white_sox > 0 & braves  > 0)
s2_filter = s2$playerID

s2_players = bat %>% 
  filter(teamID == "CHA" | teamID == "ATL") %>% 
  filter(playerID %in% s2_filter) %>% 
  group_by(nameFirst, nameLast, teamID) %>% 
  summarize(games = sum(G)) %>% 
  pivot_wider(names_from = teamID, values_from = games) %>% 
  arrange(CHA, descending = FALSE) %>% 
  arrange(ATL, descending = F)

# square 3 - White Sox Cy Young
s3 = Lahman::AwardsPlayers %>% 
  filter(awardID == "Cy Young Award") %>% 
  select(playerID, yearID) %>% 
  left_join(pitch, by = c("playerID", "yearID")) %>% 
  filter(teamID == "CHA")

# s4 
s4 = bat %>% 
  filter(teamID == "BOS" | teamID == "BAL") %>% 
  group_by(playerID, nameFirst, nameLast) %>% 
  summarize(red_sox = sum(teamID == "BOS"), orioles = sum(teamID == "BAL")) %>% 
  filter(red_sox > 0 & orioles  > 0)

s4_filter = s4$playerID

s4_players = bat %>% 
  filter(teamID == "BOS" | teamID == "BAL") %>% 
  filter(playerID %in% s4_filter) %>% 
  group_by(nameFirst, nameLast, teamID) %>% 
  summarize(games = sum(G)) %>% 
  pivot_wider(names_from = teamID, values_from = games) %>% 
  arrange(BOS, descending = F) %>% 
  arrange(BAL, descending = F)

# s5 - orioles and braves 
s5 = bat %>% 
  filter(teamID == "ATL" | teamID == "BAL") %>% 
  group_by(playerID, nameFirst, nameLast) %>% 
  summarize(braves = sum(teamID == "ATL"), orioles = sum(teamID == "BAL")) %>% 
  filter(braves > 0 & orioles  > 0)

s5_filter = s5$playerID

s5_players = bat %>% 
  filter(teamID == "ATL" | teamID == "BAL") %>% 
  filter(playerID %in% s5_filter) %>% 
  group_by(nameFirst, nameLast, teamID) %>% 
  summarize(games = sum(G)) %>% 
  pivot_wider(names_from = teamID, values_from = games) %>% 
  arrange(ATL, descending = F) %>% 
  arrange(BAL, descending = F)

# s6 - orioles cy young 
s6 = Lahman::AwardsPlayers %>% 
  filter(awardID == "Cy Young Award") %>% 
  select(playerID, yearID) %>% 
  left_join(pitch, by = c("playerID", "yearID")) %>% 
  filter(teamID == "BAL")

# s7 - boston 20 game winner 
s7 = pitch %>% 
  filter(W >= 20) %>% 
  filter(teamID == "BOS") %>% 
  arrange(yearID, descending = F)

s8 = pitch %>% 
  filter(W >= 20) %>% 
  filter(teamID == "ATL") %>% 
  arrange(yearID, descending = F)

s9 = Lahman::AwardsPlayers %>% 
  filter(awardID == "Cy Young Award") %>% 
  select(playerID, yearID) %>% 
  left_join(pitch, by = c("playerID", "yearID")) %>% 
  filter(W >= 20)

