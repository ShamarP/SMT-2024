library(arrow)
library(dplyr)
library(bit64)

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


plays_cse <- plays_cse %>% select (-steal_with_average)



write.csv(pitchers,"Data_csvs/pitchers.csv")
write.csv(catchers,"Data_csvs/catchers.csv")
write.csv(plays_cse, "Data_csvs/expected_steals_df.csv")

pitchers


