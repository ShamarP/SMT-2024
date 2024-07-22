library(dplyr)
library(arrow)

pitching_numbers <- read.csv("pitch_timings.csv")
ball_pos <- read_parquet("Potential_Steals/ball_pos.parquet") %>% as.data.frame()
game_events <- read_parquet("Potential_Steals/game_events.parquet")
View(game_events)

times_of_first_ball_acquisition <- game_events %>% filter(event_code == 2,player_position == 2) %>%
  group_by(game_str,play_id) %>% arrange(timestamp) %>% slice(1) %>% ungroup() %>% select(game_str,play_id,timestamp)

plays_and_catcher_catch_time <- merge(pitching_numbers,times_of_first_ball_acquisition,by.x = c("game_str","play_id"))

plays <- plays_and_catcher_catch_time %>% select(game_str,play_id,timestamp)

ball_pos_in_steal_plays <- merge(ball_pos, plays,by = c("game_str","play_id","timestamp"))

ball_pos_in_steal_plays

