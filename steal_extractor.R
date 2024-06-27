## Andy Hutchison
## SMT 2024

## I'm gonna try and filter to have only stolen bases
## filtered_game_events from Shamar's Data_prepping
library(tidyverse)
library(dplyr)
## creating a vector of all play_id that have a steal occur and then using this
## to filter out. Also need game_str since same play_id can occur in different games

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
