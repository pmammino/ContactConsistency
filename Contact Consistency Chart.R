install.packages("roll")
library(roll)
library(dplyr)
library(grid)
library(ggplot2)

all_play_by_play <- read.csv("All Play By Play.csv")
all_play_by_play$launch_angle <- as.numeric(as.character(all_play_by_play$launch_angle))
all_play_by_play$woba_denom <- as.numeric(as.character(all_play_by_play$woba_denom))
all_play_by_play$launch_speed <- as.numeric(as.character(all_play_by_play$launch_speed))



contact_consistency_chart <- function(player, num)
{
  test <- filter(all_play_by_play,player_name==player)
  test$roll_wOBA_num <- roll_sum(test$woba_value,num)
  test$roll_wOBA_denom <- roll_sum(test$woba_denom,num)
  test$wOBA <- test$roll_wOBA_num/test$roll_wOBA_denom
  test$contact_consistency <- roll_sd(test$launch_angle,num)
  test$aLA <- roll_mean(test$launch_angle,num)
  test$aEV <- roll_mean(test$launch_speed,num)
  test$BIP <- 1:length(test$batter)

  test <- test[!is.na(test$contact_consistency),]

  p1 <- ggplot(data = test, aes(x = BIP, y = contact_consistency)) + geom_line(color="red")
  p2 <- ggplot(data = test, aes(x = BIP, y = wOBA)) + geom_line(color="blue")


  grid.newpage()
  grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
}

contact_consistency_chart("Ketel Marte",30)
