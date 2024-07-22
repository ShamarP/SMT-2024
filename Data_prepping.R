# Welcome to the 2024 SMT Data Challenge! Here are some functions to help you get
# started. After you unzip the dataset, copy the name of the directory you saved
# it to into the 'data_directory` field below. After making sure you have the
# `arrow` package installed, you may call this file at the top of your work file(s)
# by calling `source("SMT_Data_starter.R"). Then, you may apply functions and
# operations to the table names below as you would any other table and load them
# into your working environment by calling `collect()`. For an example of this
# process, un-comment and run the lines below the starter code.
#
# WARNING: The data subsets are large, especially `player_pos`. Reading the
#   entire subset at once without filtering may incur performance issues on your
#   machine or even crash your R session. It is recommended that you filter
#   data subsets wisely before calling `collect()`.

data_directory <- '2024_SMT_Data_Challenge'

###############################################################################
################## STARTER CODE: DO NOT MODIFY ################################
###############################################################################

library(arrow)
library(dplyr)
library(bit64)

game_info <- arrow::open_csv_dataset(paste0(data_directory,"/game_info"),
                                     partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"),
                                     hive_style = F,
                                     unify_schemas = T,
                                     na = c("", "NA", "NULL", NA, "\\N"))

ball_pos <- arrow::open_csv_dataset(paste0(data_directory,"/ball_pos"),
                                    partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"),
                                    hive_style = F,
                                    unify_schemas = T,
                                    na = c("", "NA", "NULL", NA, "\\N"))

game_events <- arrow::open_csv_dataset(paste0(data_directory,"/game_events"),
                                       partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"),
                                       hive_style = F,
                                       unify_schemas = T,
                                       na = c("", "NA", "NULL", NA, "\\N"))

player_pos <- arrow::open_csv_dataset(paste0(data_directory,"/player_pos"),
                                      partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"),
                                      hive_style = F,
                                      unify_schemas = T,
                                      na = c("", "NA", "NULL", NA, "\\N"))

team_info <- arrow::open_csv_dataset(paste0(data_directory,"/team_info.csv"),
                                     hive_style = F,
                                     unify_schemas = T,
                                     na = c("", "NA", "NULL", NA, "\\N"))

###############################################################################
########################## END STARTER CODE ###################################
###############################################################################
## this is where i will add play id to game_info
play_id_and_per_game <- game_events |> collect() %>% select("game_str","play_per_game","play_id")
game_info_with_id <- merge((game_info |> collect()),play_id_and_per_game,by = c("game_str","play_per_game"),all.x = TRUE)

## the following functions filter the data based on if there is a baserunner on not on a specific base
# "|>" is called a pipe it takes the arguments to the left and puts it into the right function
# so we take our initial dataframe and filter it based on the requirements
first <- game_info_with_id |> filter(!is.na(first_baserunner)&  is.na(second_baserunner)& is.na(third_baserunner))

second <- game_info_with_id|> filter(is.na(first_baserunner)&  !is.na(second_baserunner)& is.na(third_baserunner))

first_second <- game_info_with_id |> filter(!is.na(first_baserunner) &  !is.na(second_baserunner) & is.na(third_baserunner))

first_third <- game_info_with_id |> filter(!is.na(first_baserunner)&  is.na(second_baserunner) & !is.na(third_baserunner))

## we filter out all the distinct gamestr and play_ids where we can expect a steal
filtered_plays <- bind_rows(select(first,game_str,play_id),select(second,game_str,play_id),
                            select(first_third,game_str,play_id),select(first_second,game_str,play_id))

## something weird was happening so i had to convert types not really important
filtered_plays$play_id <- as.integer64(filtered_plays$play_id)


## here we have all of the player, ball positions and game events that correspond to our list of plays
## join is pretty much ensuring that the pairwise matchup of id and game_str holds think of it like a primary key
filtered_game_events <- game_events %>% semi_join(filtered_plays, by = c("game_str", "play_id")) %>% collect()
filtered_ball_pos <- ball_pos %>% semi_join(filtered_plays, by = c("game_str", "play_id")) %>% collect()
filtered_player_pos <- player_pos %>% semi_join(filtered_plays, by = c("game_str", "play_id")) %>% collect()

## storing as a parquet file its an efficient way to store such large files
all_plays <- bind_rows(first,second,first_second,first_third)

dir.create("Potential_Steals")

write_parquet(all_plays, "Potential_Steals/game_info.parquet")
write_parquet(filtered_game_events, "Potential_Steals/game_events.parquet")
write_parquet(filtered_ball_pos, "Potential_Steals/ball_pos.parquet")
write_parquet(filtered_player_pos, "Potential_Steals/player_pos.parquet")





View(filtered_game_events)
