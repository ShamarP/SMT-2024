## Andy Hutchison
## SMT 2024

## I'm gonna try and filter to have only stolen bases
## filtered_game_events from Shamar's Data_prepping
library(tidyverse)
library(dplyr)
library(arrow)
library(gt)
library(webshot2)
## creating a vector of all play_id that have a steal occur and then using this
## to filter out. Also need game_str since same play_id can occur in different games

source("SMT_Data_starter.R")
source("Data_prepping.R")

filtered_game_events <- read_parquet("Potential_Steals/game_events.parquet")
filtered_game_info <- read_parquet("Potential_Steals/game_info.parquet")

# Most common steal sequences without many bounces
steal_sequences <- list(c(1, 2, 2, 4), c(1, 2, 2, 5), c(1, 2, 2, 6),
                        c(1, 255, 2, 2, 4), c(1, 255, 2, 2, 5), c(1, 255, 2, 2, 6),
                        c(1, 2, 2, 255, 255, 4), c(1, 2, 2, 255, 255, 5), c(1, 2, 2, 255, 255, 6),
                        c(1, 255, 2, 2, 255, 4), c(1, 255, 2, 2, 255, 5), c(1, 255, 2, 2, 255, 6))


steal_sequence_check <- function(events, steal_sequences){
  any(sapply(steal_sequences, function(sequen) all(sequen %in% events)))
}

filter_steals <- filtered_game_events %>%
  group_by(game_str, play_id) %>%
  summarise(event_sequence = list(player_position), .groups = "drop") %>%
  mutate(sequence_match = sapply(event_sequence, function(events) steal_sequence_check(events, steal_sequences))) %>%
  filter(sequence_match) %>%
  select(game_str, play_id)

# At this point there are some 10s in here (batter plays) and if the batter does
# something it's not a steal so we need to take that out.
# But almost steals is plays where one of steal_sequences shows up, could be with
# other stuff in there
almost_steals <- filtered_game_events %>%
  inner_join(filter_steals, by = c("game_str", "play_id"))

# steals is just all the steal plays that occur
steals <- almost_steals %>%
  group_by(game_str, play_id) %>%
  filter(!any(player_position == 10)) %>%
  ungroup()

## grabbing all plays that contain steals
plays_with_steals <- steals %>% select(game_str,play_id) %>% distinct()

## we are now creating a dataframe that will grab the following play so we can
## examine whos on base next
following_play_with_steals <- plays_with_steals
following_play_with_steals$play_id <- following_play_with_steals$play_id + 1

## will now merge our steal play and the following play

steal_att <- semi_join(filtered_game_info,plays_with_steals,by = c("game_str","play_id"))
game_info <- game_info %>% as_tibble()
post_steal <- semi_join(game_info_with_id,following_play_with_steals,by=c("game_str","play_id")) %>% distinct()
post_steal
steal <- c()

post_steal <- post_steal %>%  mutate(play_id = play_id - 1)
during_after_steal <- steal_att %>% inner_join(post_steal, by = c("game_str","play_id"))

## will now check if a steal has occured in our plays
for(i in 1:nrow(during_after_steal)){
  steal_val <- 0
  if(!is.na(during_after_steal[i,]$first_baserunner.x)){
    if(!is.na(during_after_steal[i,]$second_baserunner.y)){
      steal_val <- 1
    }
  }
  if(!is.na(during_after_steal[i,]$second_baserunner.x)){
    if(!is.na(during_after_steal[i,]$third_baserunner.y)){
      steal_val <- 1
    }
  }
  steal <- c(steal,steal_val)
}

during_after_steal$valid_steal <- steal

## calculating pitch , exchange and pop times

no_bounce_steals <- steals %>%
  group_by(game_str, play_id) %>%
  filter(!any(player_position == 255)) %>%
  ungroup()

no_bounce_steals <- no_bounce_steals %>%
  group_by(game_str, play_id)%>%
  mutate(pitch_time =  (timestamp - lag(timestamp))/1000)%>%
  mutate(exchange_time =  (timestamp - lag(timestamp))/1000)%>%
  mutate(pop_time =  (timestamp - lag(lag(timestamp)))/1000)%>%
  ungroup() %>% filter(!is.na(pitch_time))

extract_diagonal <- function(df) {
  n <- nrow(df)
  diag_elements <- c(df[1, "pitch_time"], df[2, "exchange_time"], df[3, "pop_time"])
  return(diag_elements)
}

timings_df <- no_bounce_steals %>% group_by(game_str,play_id) %>%
  summarize (diagonal = list(extract_diagonal(cur_data())),
             .groups = 'drop') %>% unnest_wider(col = diagonal, names_sep = "")

timings_steals <- left_join(timings_df,during_after_steal,by = c("game_str","play_id"))

dir.create("Data_csvs")

write.csv(timings_steals,"pitch_timings_and_valid_steal.csv")



