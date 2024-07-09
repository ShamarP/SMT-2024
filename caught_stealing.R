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

steal_plays <- steals %>% distinct(game_str,play_id)
colnames(steal_plays)[2] <- "play_per_game"
steal_plays
following_play <- steal_plays
following_play$play_per_game <- following_play$play_per_game + 1


steal_att <- semi_join(filtered_game_info,steal_plays,by=c("game_str","play_per_game"))
game_info <- game_info %>% as_tibble()
post_steal <- semi_join(game_info,following_play,by=c("game_str","play_per_game"))
steal <- c()

post_steal <- post_steal %>%  mutate(play_per_game = play_per_game - 1)
pre_steal <- steal_att %>% inner_join(post_steal, by = c("game_str","play_per_game"))
pre_steal

for(i in 1:nrow(pre_steal)){
  steal_val <- 0
  if(!is.na(pre_steal[i,]$first_baserunner.x)){
    if(!is.na(pre_steal[i,]$second_baserunner.y)){
      steal_val <- 1
    }
  }
  if(!is.na(pre_steal[i,]$second_baserunner.x)){
    if(!is.na(pre_steal[i,]$third_baserunner.y)){
      steal_val <- 1
    }
  }
  steal <- c(steal,steal_val)
}

differences <- steals %>% group_by(game_str, play_id) %>% filter(!player_pos == 255) %>%
  for (i in 2:(nrow(steals) - 1)) {
    differences[i] <- steals$value(timestamp[i]) - steals$value(timestamp[i-1])
  } %>%
  mutate(differences = differences[i]) %>%
  ungroup()


pitch_time <- steals %>% group_by(game_str, play_id) %>% 
  mutate(pitcher_pitch = min(timestamp)) %>%
  ungroup()
pitch_time <- pitch_time %>% group_by(game_str, play_id) %>% filter(event_code == 2) %>%
  mutate(pitch_time = min(timestamp)) %>%
  ungroup()

steal
pre_steal$valid_steal <- steal

catcher_allowed_steals <- pre_steal %>% group_by(catcher.x) %>%
  summarize(
    steal_allowed = sum(valid_steal),
    steal_attempts = n(),
    caught_stealing_percentage = (n() - sum(valid_steal))/ (n())
    )

ten_catchers_who_allowed_most_steals <- catcher_allowed_steals %>% arrange(desc(steal_attempts)) %>%
  slice_head(n=10)

ten_catchers_who_allowed_most_steals

battery_allowed_steals <- pre_steal %>% group_by(catcher.x,pitcher.x) %>%
  summarize(
    steal_allowed = sum(valid_steal),
    steal_attempts = n(),
    caught_stealing_percentage = (n() - sum(valid_steal))/ (n())
  ) %>% ungroup()

batteries_who_allowed_most_steals <- battery_allowed_steals %>% arrange(desc(steal_attempts)) %>% slice_head(n=10)

battery_allowed_steals
batteries_who_allowed_most_steals

gt_table <- ten_catchers_who_allowed_most_steals %>%
  gt() %>%
  tab_header(
    title = "Ten Catchers Who Allowed Most Steals",
    subtitle = "Steals Attempted and Allowed"
  ) %>%
  cols_label(
    catcher.x = "Catcher ID",
    steal_allowed = "Total Steals Allowed",
    steal_attempts = "Total Attempt",
    caught_stealing_percentage = "CS%"
  ) %>%
  fmt_number(
    columns = c(steal_allowed, steal_attempts,caught_stealing_percentage),
    decimals = 2
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightblue"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = steal_allowed > 10
    )
  ) %>%
  tab_footnote(
    footnote = "Steals greater than 10 are highlighted.",
    locations = cells_body(
      columns = steal_allowed,
      rows = steal_allowed > 10
    )
  ) %>%
  tab_source_note(
    source_note = "SMT Data"
  )

battery_table <- batteries_who_allowed_most_steals %>%
  gt() %>%
  tab_header(
    title = "Ten Catcher and pitcher pairings Who Allowed Most Steals",
    subtitle = "Steals Attempted and Allowed"
  ) %>%
  cols_label(
    catcher.x = "Catcher ID",
    pitcher.x = "Pitcher ID",
    steal_allowed = "Total Steals Allowed",
    steal_attempts = "Total Attempt",
    caught_stealing_percentage = "CS%"
  ) %>%
  fmt_number(
    columns = c(steal_allowed, steal_attempts,caught_stealing_percentage),
    decimals = 2
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightblue"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = steal_allowed > 10
    )
  ) %>%
  tab_footnote(
    footnote = "Steals greater than 10 are highlighted.",
    locations = cells_body(
      columns = steal_allowed,
      rows = steal_allowed > 5
    )
  ) %>%
  tab_source_note(
    source_note = "SMT Data"
  )

# Display the table
#gtsave(gt_table, "table.png")
#gtsave(battery_table, "battery.png")
#webshot2::webshot("table.pdf")
#webshot2::webshot("battery.pdf")
