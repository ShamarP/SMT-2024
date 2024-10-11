library(arrow)
library(dplyr)
library(bit64)
library(tidyr)
library(ggplot2)
ball_location <- read.csv("Data_csvs/ball_location_in_steal_plays.csv")

zone_entry <- read.csv("Data_csvs/zone_entry.csv")
data_directory <- '2024_SMT_Data_Challenge'
game_events <- arrow::open_csv_dataset(paste0(data_directory,"/game_events"),
                                       partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"),
                                       hive_style = F,
                                       unify_schemas = T,
                                       na = c("", "NA", "NULL", NA, "\\N")) |> collect()


game_info <- arrow::open_csv_dataset(paste0(data_directory,"/game_info"),
                                     partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"),
                                     hive_style = F,
                                     unify_schemas = T,
                                     na = c("", "NA", "NULL", NA, "\\N"))
play_id_and_per_game <- game_events |> collect() %>% select("game_str","play_per_game","play_id")
game_info_with_id <- merge((game_info |> collect()),play_id_and_per_game,by = c("game_str","play_per_game"),all.x = TRUE)

find_fielder_ball_collection_time <- function(game,play){
  curr_play <- game_events %>% filter(event_code == 2  & (player_position == 4 | player_position == 6 |player_position == 5)) %>%
    filter(game_str == game & play_id == play)
  curr_play$timestamp[1]
}

ball_caught_time <- c()

for(i in 1:nrow(zone_entry)){
  time <- find_fielder_ball_collection_time(zone_entry$game_str[i],zone_entry$play_id[i])
  ball_caught_time <- c(ball_caught_time,time)
  print(time)
}



zone_entry$fielder_catch_time <- ball_caught_time


fielder_catch_timing <- zone_entry[!is.na(zone_entry$fielder_catch_time),]

expected_caught_stealing <- c()

for(i in 1:nrow(fielder_catch_timing)){
  if(!is.na(fielder_catch_timing$tb_entry_time[i]) & (fielder_catch_timing$tb_entry_time[i] < fielder_catch_timing$fielder_catch_time[i])){
    expected_caught_stealing <- c(expected_caught_stealing,0)
  }else if(!is.na(fielder_catch_timing$sb_entry_time[i]) & (fielder_catch_timing$sb_entry_time[i] < fielder_catch_timing$fielder_catch_time[i])){
    expected_caught_stealing <- c(expected_caught_stealing,0)
  }
  else{
    expected_caught_stealing <- c(expected_caught_stealing,1)
  }
}

fielder_catch_timing$expected_cs <- expected_caught_stealing

plays_cse <- fielder_catch_timing %>% select(game_str,play_id,diagonalpitch_time,diagonalexchange_time,
                                             diagonalpop_time,valid_steal,expected_cs,sb_entry_time,tb_entry_time,fielder_catch_time) %>% filter(diagonalpop_time <= 2.5)

pitchers_and_batters_in_play <- game_info_with_id %>% select(game_str,play_id,pitcher,catcher)

pitchers_and_batters_in_play

plays_cse <- left_join(plays_cse,pitchers_and_batters_in_play,by = c("game_str","play_id")) %>% unique()

plays_cse <- plays_cse %>% filter(!is.na(sb_entry_time) | !is.na(tb_entry_time))

colnames(plays_cse)[3] <- "pitch_time"
colnames(plays_cse)[4] <- "exchange_time"
colnames(plays_cse)[5] <- "pop_time"


pitchers <- plays_cse %>% group_by(pitcher) %>% summarize (total_plays = n(),
                                                           caught_steals = sum(valid_steal),
                                                           expected_caught_steals = sum(expected_cs),
                                                           average_pitch_time = mean(pitch_time)
)
catchers <- plays_cse %>% group_by(catcher) %>% summarize (total_plays = n(),
                                                           caught_steals = sum(valid_steal),
                                                           expected_caught_steals = sum(expected_cs),
                                                           average_pop_time = mean(pop_time)
)



## found using baseball savant
average_pop_time <- mean(plays_cse$pop_time) * 1000

## plays with average catcher
plays_with_average_catcher <- c()

for(i in 1:nrow(plays_cse)){
  difference_in_pop_time <- plays_cse$pop_time[i] - average_pop_time
  if(!is.na(plays_cse$sb_entry_time[i]) & (plays_cse$fielder_catch_time[i] + difference_in_pop_time <=
                                           plays_cse$sb_entry_time[i])){
    plays_with_average_catcher <- c(plays_with_average_catcher,1)
  }else if (!is.na(plays_cse$tb_entry_time[i]) & (plays_cse$fielder_catch_time[i] + difference_in_pop_time <=
                                                  plays_cse$tb_entry_time[i])){
    plays_with_average_catcher <- c(plays_with_average_catcher,1)
  }else{
    plays_with_average_catcher <- c(plays_with_average_catcher,0)
  }
}

plays_cse$steal_with_average_catcher <- plays_with_average_catcher

## plays with average pitcher
plays_with_average_pitcher <- c()

average_pitch_time <- mean(plays_cse$pitch_time) * 1000

for(i in 1:nrow(plays_cse)){
  difference_in_pitch_time <- plays_cse$pitch_time[i] - average_pitch_time
  if(!is.na(plays_cse$sb_entry_time[i]) & (plays_cse$fielder_catch_time[i] + difference_in_pitch_time <=
                                           plays_cse$sb_entry_time[i])){
    plays_with_average_pitcher <- c(plays_with_average_pitcher,1)
  }else if (!is.na(plays_cse$tb_entry_time[i]) & (plays_cse$fielder_catch_time[i] + difference_in_pitch_time <=
                                                  plays_cse$tb_entry_time[i])){
    plays_with_average_pitcher <- c(plays_with_average_pitcher,1)
  }else{
    plays_with_average_pitcher <- c(plays_with_average_pitcher,0)
  }
}

plays_cse$steals_with_average_pitcher <- plays_with_average_pitcher

pitchers <- plays_cse %>% group_by(pitcher) %>% summarize (total_plays = n(),
                                                           caught_steals = sum(valid_steal),
                                                           expected_caught_steals = sum(steal_with_average_catcher),
                                                           average_pitch_time = mean(pitch_time)
)
catchers <- plays_cse %>% group_by(catcher) %>% summarize (total_plays = n(),
                                                           caught_steals = sum(valid_steal),
                                                           expected_caught_steals = sum(steals_with_average_pitcher),
                                                           average_pop_time = mean(pop_time)
)

write.csv(pitchers,"Data_csvs/pitchers.csv")
write.csv(catchers,"Data_csvs/catchers.csv")
write.csv(plays_cse, "Data_csvs/expected_steals_df.csv")

pitchers

#############################################################################################################################

visual_plays <- plays_cse %>% select(game_str,play_id,valid_steal)

steal_location_plays <- left_join(visual_plays,ball_location,by = c("game_str","play_id")) %>%
  mutate(level = substr(game_str,nchar(game_str)- 1, nchar(game_str)))



write.csv(steal_location_plays,"Data_csvs/steal_location.csv")

steal_location_plays

steal_location_plays

x_bin_size <- 1
y_bin_size <- 1

# Create breaks for x and y
x_breaks <- seq(-2.5, 2.5, by = x_bin_size)
y_breaks <- seq(0, 5, by = y_bin_size)

zones <- steal_location_plays %>% mutate (
  ball_x_pos = cut(ball_position_x, breaks = x_breaks, include.lowest = TRUE, right = FALSE, labels = x_breaks[-1]),
  ball_y_pos = cut(ball_position_z, breaks = y_breaks, include.lowest = TRUE, right = FALSE, labels = y_breaks[-1]),
  level = substr(game_str,nchar(game_str)- 1, nchar(game_str))
) %>% filter(!is.na(ball_x_pos) & !is.na(ball_y_pos))


zones <- zones %>%
  mutate(
    ball_x_pos = as.numeric(as.character(ball_x_pos)),
    ball_y_pos = as.numeric(as.character(ball_y_pos))
  )

bin_combinations <- expand.grid(
  ball_x_pos = x_breaks[-1],
  ball_y_pos = y_breaks[-1]
)

zones <- zones %>%
  group_by(ball_x_pos, ball_y_pos) %>%
  summarise(zone_value = n() - sum(valid_steal),total_zone = n())  %>%
  ungroup()

zones <- bin_combinations %>%
  left_join(zones, by = c("ball_x_pos", "ball_y_pos")) %>%
  replace_na(list(zone_value = 0))

zones <- zones %>%
  group_by(ball_x_pos, ball_y_pos) %>%
  mutate(percentage = ifelse(zone_value > 0, zone_value / total_zone * 100, 0))

zones <- zones %>% mutate(highlight =ifelse(ball_x_pos >= -0.5 & ball_x_pos <= 2 & ball_y_pos >= 2 & ball_y_pos <= 4, TRUE, FALSE))

zones
write.csv(zones,"Data_csvs/zones.csv")

zones


plot_colour <- "#D55E00"
border_colour <- "steelblue"

ggplot(zones, aes(x = ball_x_pos, y = ball_y_pos, fill = percentage)) +
  geom_tile(color = "black") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), color = "black") +
  scale_fill_gradient(low = "white", high = plot_colour) +
  geom_rect(data = subset(zones, highlight),
            aes(xmin = ball_x_pos - 0.5, xmax = ball_x_pos + 0.5, ymin = ball_y_pos - 0.5, ymax = ball_y_pos + 0.5),
            fill = NA, color =  border_colour, size = 1.5) +
  theme_minimal() +
  labs(title = "Percentage Plot by Zone", x = "X", y = "Z", fill = "Caught Stealing Percentage") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )




## heat map

x_hm_bin_size <- 0.05
y_hm_bin_size <- 0.01

# Create breaks for x and y
x_hm_breaks <- seq(1.5,2.2,by = x_hm_bin_size)
y_hm_breaks <- seq(0.45,0.6, by = y_hm_bin_size)

plays_cse$level <- plays_cse %>% mutate(level = substr(game_str,nchar(game_str)- 1, nchar(game_str)))

plays_cse

zones_hm <- plays_cse %>% mutate (
  pop_time = cut(pop_time, breaks = x_hm_breaks, include.lowest = TRUE, right = FALSE, labels = x_hm_breaks[-1]),
  pitch_time = cut(pitch_time, breaks = y_hm_breaks, include.lowest = TRUE, right = FALSE, labels = y_hm_breaks[-1])
)

zones_hm <- zones_hm %>%
  mutate(
    pop_time = as.numeric(as.character(pop_time)),
    pitch_time = as.numeric(as.character(pitch_time))
  )

bin_combinations <- expand.grid(
  pop_time = x_hm_breaks[-1],
  pitch_time = y_hm_breaks[-1]
)

zones_hm <- zones_hm %>%
  group_by(pop_time, pitch_time) %>%
  summarise(Successful_steals = n() - sum(valid_steal),Steal_Attempts = n())  %>%
  ungroup()


zones_hm <- bin_combinations %>%
  left_join(zones_hm, by = c("pop_time", "pitch_time")) %>%
  replace_na(list(zone_value = 0))

zones_hm <- zones_hm %>%
  group_by(pop_time, pitch_time) %>%
  mutate(percentage = ifelse(Successful_steals > 0, Successful_steals /Steal_Attempts * 100,NA))

g <- ggplot(zones_hm, aes(x = pop_time, y = pitch_time, fill = percentage)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = border_colour, high = plot_colour,na.value = "white") +
  theme_minimal() +
  labs(title = "Pop time vs Pitch time Heat Map", x = "Pop Time", y = "Pitch Time", fill = "Caught Stealing Percentage") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

g

