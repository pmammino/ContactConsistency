all_play_by_play <- read.csv("data/All Play By Play.csv")
whiffs <- read.csv("data/2019whiff.csv")
whiffs <- whiffs[,c("player_id", "wrate")]
all_play_by_play$launch_angle <- as.numeric(as.character(all_play_by_play$launch_angle))
all_play_by_play$woba_denom <- as.numeric(as.character(all_play_by_play$woba_denom))
all_play_by_play$launch_speed <- as.numeric(as.character(all_play_by_play$launch_speed))
all_play_by_play$estimated_woba_using_speedangle <- as.numeric(as.character(all_play_by_play$estimated_woba_using_speedangle))



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


all_play_by_play_clean <- all_play_by_play[,c("player_name",
                                              "batter",
                                              "launch_angle",
                                              "launch_speed",
                                              "woba_value",
                                              "woba_denom",
                                              "estimated_woba_using_speedangle")]

all_play_by_play_clean <- all_play_by_play_clean %>% 
  filter(woba_denom != 0) %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(estimated_woba_using_speedangle))

summary <- all_play_by_play_clean %>%
  group_by(player_name,batter) %>%
  arrange(player_name,batter) %>% 
  summarise(BIPs = n(), 
            sdLA = sd(launch_angle),
            aLA = mean(launch_angle),
            aEV = mean(launch_speed),
            xwOBACon = round(sum(estimated_woba_using_speedangle)/sum(woba_denom),3),
            wOBACon = round(sum(woba_value)/sum(woba_denom),3))
summary <- filter(summary, BIPs >= 50)

summary <- merge(summary, whiffs, by.x = "batter",by.y="player_id",all.x=TRUE)
summary$wrate <- as.numeric(as.character(summary$wrate))

summary <- summary%>% rename(
  Whiff.Rate = wrate)

summary$sdLA <- round(summary$sdLA,1)
summary$aLA <- round(summary$aLA,1)
summary$aEV <- round(summary$aEV,1)
summary$Whiff.Rate <- round(summary$Whiff.Rate,2)
summary <- arrange(summary,sdLA)

plot <- ggplot(summary, aes(x=wOBACon, y=sdLA, color=Whiff.Rate)) + 
  geom_point() +
  scale_color_gradient2(midpoint=mean(summary$Whiff.Rate), low="blue", mid="white",
                        high="red", space ="Lab") +
  labs(y = "sdLA",
       x = "wOBACon",
       caption = "Data from Baseball Savant",
       title = "sdLA Versus wOBACon",
       subtitle = "2019") +
  theme_dark() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

