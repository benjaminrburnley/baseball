## Acuna 40-40 Tracker 

# libraries 
library(tidyverse)
library(baseballr)
library(patchwork)

# get data 
data = fg_batter_game_logs(playerid = 18401, year = 2023) %>% 
  select(Date, HR, SB) %>% 
  rowid_to_column() %>% 
  mutate(Game = max(rowid) + 1 - rowid) %>% 
  arrange(Game) %>% 
  mutate(hr_total = cumsum(HR),
         sb_total = cumsum(SB))

pace = tibble(Game = 1:162, Stat = seq(0, 40, 0.2469136))

# plot 
plot1 = ggplot()+
  geom_line(data = data, aes(x = Game, y = hr_total), color = "#13274f")+
  geom_line(data = pace, aes(x = Game, y = Stat), color = "#ce1141")+
  theme_minimal()+
  annotate(geom = "text", x = 75, y = 20, label = "40-40 Pace",angle = "45", color = "#ce1141", fontface = "italic")+
  labs(
    y = "Total Home Runs",
    title = "Ronald Acuña Jr. 40-40 Tracker",
    subtitle = "Home Runs"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic")
  )

plot2 = ggplot()+
  geom_line(data = data, aes(x = Game, y = sb_total), color = "#13274f")+
  geom_line(data = pace, aes(x = Game, y = Stat), color = "#ce1141")+
  theme_minimal()+
  annotate(geom = "text", x = 75, y = 20, label = "40-40 Pace",angle = "45", color = "#ce1141", fontface = "italic") +
  labs(
    y = "Total Stolen Bases",
    subtitle = "Stolen Bases",
    caption = "Source: FanGraphs"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic")
  )

plot1+plot2




## function 
plot_4040 = function(playerid, player_name, year, color1, color2){
  pull = fg_batter_game_logs(playerid = playerid, year = year) %>% 
    select(Date, HR, SB) %>% 
    rowid_to_column() %>% 
    mutate(Game = max(rowid) + 1 - rowid) %>% 
    arrange(Game) %>% 
    mutate(hr_total = cumsum(HR),
           sb_total = cumsum(SB))
  
  pace = tibble(Game = 1:162, Stat = seq(0, 40, 0.2469136))
  
  # plot 
  p1 = ggplot()+
    geom_line(data = pull, aes(x = Game, y = hr_total), color = color1)+
    geom_line(data = pace, aes(x = Game, y = Stat), color = color2)+
    theme_minimal()+
    annotate(geom = "text", x = 75, y = 20, label = "40-40 Pace",angle = "45", color = color2, fontface = "italic")+
    labs(
      title = paste(player_name,"40 - 40 Tracker", "-", as.character(year), sep = " "),
      y = "Total Home Runs",
      subtitle = "Home Runs"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "italic")
    )
  
  p2 = ggplot()+
    geom_line(data = pull, aes(x = Game, y = sb_total), color = color1)+
    geom_line(data = pace, aes(x = Game, y = Stat), color = color2)+
    theme_minimal()+
    annotate(geom = "text", x = 75, y = 20, label = "40-40 Pace",angle = "45", color = color2, fontface = "italic") +
    labs(
      y = "Total Stolen Bases",
      subtitle = "Stolen Bases",
      caption = "Source: FanGraphs"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "italic")
    )
  
  p1+p2
}

# historical plots
plot_4040(playerid = 1001918, player_name = "Jose Canseco", year = 1988, color1 = "#003831", color2 = "#EFB21E")
plot_4040(playerid = 1109, player_name = "Barry Bonds", year = 1996, color1 = "#FD5A1E", color2 = "#27251F")
plot_4040(playerid = 1274, player_name = "Alex Rodriguez", year = 1998, color1 = "#0C2C56", color2 = "#005C5C")
plot_4040(playerid = 847, player_name = "Alfonso Soriano", year = 2006, color1 = "#AB0003", color2 = "#14225A")
plot_4040(playerid = 18401, player_name = "Ronald Acuña Jr.", year = 2023, color1 = "#ce1141", color2 = "#13274f")
plot_4040(playerid = 18401, player_name = "Ronald Acuña Jr.", year = 2019, color1 = "#ce1141", color2 = "#13274f")

