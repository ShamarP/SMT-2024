## Andy Hutchison
## The KPIs: Pitch speed (and time), pitcher windup time, and pitch position (in the strike zone)
## SMT Data Challenge 2024


## Pitch speed first:
# First filter out the steal plays we need:
steal_filtered_ball_pos <- inner_join(filtered_ball_pos, steal_plays, by = c("game_str" = "game_str", "play_id" = "play_per_game"))

# Take ball_pos_y at timestamp 1 and at timestamp 8 (time diff of 350 ms)
# just take y not x and z since we don't care about the absolute velo, only
# velo in the y axis (so no hypotenuse measured)

pitch_speed_diffs <- steal_filtered_ball_pos %>%
  group_by(game_str, play_id) %>%
  filter(n() >= 8) %>%
  filter(row_number() %in% c(1,8)) %>%
  dplyr::summarise(loc_diff = diff(ball_position_y), .groups = "drop") %>%
  filter(!is.na(loc_diff))

avg_pitch_velo <- data.frame(game_str = pitch_speed_diffs$game_str,
                             play_id = pitch_speed_diffs$play_id,
                             pitch_velo = pitch_speed_diffs$loc_diff * -1.9473942599)
  
# most of the pitches in avg_pitch_velo are good and usable, but some of them need
# a second look or other method because the slow windup is messing with the 
# velo

# mention to Meredith

## This is the final one:
avg_pitch_velo_adj <- avg_pitch_velo %>%
  filter(pitch_velo >= 68)
