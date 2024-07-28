library(dplyr)
library(ggplot2)
library(sf)
library(sportyR)
library(viridis)


## second base zone
steals <- read.csv("Data_csvs/pitch_timings_and_valid_steal.csv")
pos <- read_parquet("Potential_Steals/player_pos.parquet")
steals

second_base_center_x <- 0
second_base_center_y <- 127.28

right_vertex <- c(second_base_center_x + 0.75,second_base_center_y)
bottom_vertex <- c(second_base_center_x,second_base_center_y - 0.75)

width <- 1.25

# Calculate the southeast direction vector
northeast_vector <- c(1,1)
southeast_vector <- c(1, -1)
northwest_vector <- c(-1,1)
southwest_vector <- c(-1,-1)

# Normalize the southeast vector
southeast_vector <- southeast_vector / sqrt(sum(southeast_vector^2))
southwest_vector <- southwest_vector / sqrt(sum(southwest_vector^2))
norteast_vector <- northeast_vector / sqrt(sum(northeast_vector^2))
northwest_vector <- northwest_vector / sqrt(sum(northwest_vector^2))

right_vertex <- right_vertex + width * northeast_vector
bottom_vertex <- bottom_vertex + width * southwest_vector

# Calculate the other two points 15 units away
distance <- 15
point3 <- right_vertex + distance * southeast_vector
point4 <- bottom_vertex + distance * southeast_vector


# Define the points in a matrix
rectangle_points <- matrix(c(
  right_vertex[1], right_vertex[2],
  point3[1], point3[2],
  point4[1], point4[2],
  bottom_vertex[1], bottom_vertex[2],
  right_vertex[1], right_vertex[2]  # Closing the polygon
), ncol = 2, byrow = TRUE)

# Create the sf polygon object
polygon <- st_sfc(st_polygon(list(rectangle_points)))
polygon_sf <- st_sf(geometry = polygon)

# Create a data frame for ggplot
polygon_df <- st_as_sf(data.frame(id = 1), geometry = polygon)

# Create a baseball diamond plot
baseball_diamond <- geom_baseball(league = "MLB", display_range = "full")

# Plot the baseball diamond and overlay the rectangle

baseball_diamond +
  geom_sf(data = polygon_df, fill = "lightblue", color = "black") +
  ggtitle("Rectangle on Baseball Diamond") +
  theme_minimal()


zone_entry_time_first_b <- function(game,play,position = 11){
  curr_play <- pos %>% filter(game_str == game,play_id == play,player_position == position)
  entry_time <- NA
  entry_x <- NA
  entry_y <- NA
  for(i in 1:nrow(curr_play)){
    x <- curr_play$field_x[i]
    y <- curr_play$field_y[i]
    point <- st_point(c(x, y))
    point_sf <- st_sfc(point)
    point_sf <- st_sf(geometry = point_sf)
    contains <- st_contains(polygon_sf, point_sf, sparse = FALSE)
    if(contains){
      entry_time <- curr_play$timestamp[i]
      entry_x <- x
      entry_y <- y
      break
    }
  }
  return(list(sb_entry_time = entry_time, sb_entry_x = entry_x,eb_entry_y = entry_y))
}
steals
steal_stats <- steals %>%
  select(game_str,play_id,valid_steal,first_baserunner.x,second_baserunner.x,diagonalpitch_time,diagonalexchange_time,diagonalpop_time) %>%
  filter(!is.na(play_id))
steal_stats

sb_entry <- data.frame()
for(i in 1:nrow(steal_stats)){
  if(is.na(steal_stats$first_baserunner.x[i])){
    sb_entry <- rbind(sb_entry,NA)
    next
  }
  time <- zone_entry_time_first_b(steal_stats$game_str[i],steal_stats$play_id[i])
  sb_entry <- rbind(sb_entry,time)
  print(i)
}

sb_entry

steal_stats <- cbind(steal_stats,sb_entry)


## third base zone
third_base_center <- c(-62.58,63.64)

tb_right_vertex <- c(third_base_center[1] + 0.75,third_base_center[2])
tb_top_vertex <- c(third_base_center[1],third_base_center[2] + 0.75)

tb_right_vertex <- tb_right_vertex + width * southeast_vector
tb_top_vertex <- tb_top_vertex + width * northwest_vector



pointtbright <- tb_right_vertex + distance *northeast_vector
pointtbtop <- tb_top_vertex + distance * northeast_vector

tb_points <- matrix(c(
  tb_right_vertex[1], tb_right_vertex[2],
  pointtbright[1], pointtbright[2],
  pointtbtop[1], pointtbtop[2],
  tb_top_vertex[1], tb_top_vertex[2],
  tb_right_vertex[1], tb_right_vertex[2]  # Closing the polygon
), ncol = 2, byrow = TRUE)


# Create the sf polygon object
tb_polygon <- st_sfc(st_polygon(list(tb_points)))
tb_polygon_sf <- st_sf(geometry = tb_polygon)

# Create a data frame for ggplot
tb_polygon_df <- st_as_sf(data.frame(id = 1), geometry = tb_polygon)

baseball_diamond +
  geom_sf(data = polygon_df, fill = "lightblue", color = "black") +
  geom_sf(data = tb_polygon_df, fill = "red", color = "black") +
  ggtitle("Rectangle on Baseball Diamond") +
  theme_minimal()


zone_entry_time_second_b <- function(game,play,position = 12){
  curr_play <- pos %>% filter(game_str == game,play_id == play,player_position == position)
  entry_time <- NA
  entry_x <- NA
  entry_y <- NA
  if(nrow(curr_play)== 0){
    return(list(tb_entry_time = NA, tb_entry_x = NA,tb_entry_y = NA))
  }else{
    for(i in 1:nrow(curr_play)){
      x <- curr_play$field_x[i]
      y <- curr_play$field_y[i]
      point <- st_point(c(x, y))
      point_sf <- st_sfc(point)
      point_sf <- st_sf(geometry = point_sf)
      contains <- st_contains(tb_polygon_sf, point_sf, sparse = FALSE)
      if(contains){
        entry_time <- curr_play$timestamp[i]
        entry_x <- x
        entry_y <- y
        break
      }
    }
    return(list(tb_entry_time = entry_time, tb_entry_x = entry_x,tb_entry_y = entry_y))


  }
}

tb_entry <- data.frame()

for(i in 1:nrow(steal_stats)){
  if(is.na(steal_stats$second_baserunner.x[i])){
    tb_entry <- rbind(tb_entry,list(tb_entry_time = NA, tb_entry_x = NA,tb_entry_y = NA))
    next
  }

  time <- zone_entry_time_second_b(steal_stats$game_str[i],steal_stats$play_id[i])
  tb_entry <- rbind(tb_entry,time)
  print(i)
}
tb_entry

steal_stats <- cbind(steal_stats,tb_entry)
View(steal_stats)


## calculating leadoff / running position
threshold <- 0.5

starting_time <- function(game,play,position = 12){
  curr_play <- pos %>% filter(game_str == game,play_id == play,player_position == position)
  curr_play <- curr_play %>% select(c(-Season,-HomeTeam,-AwayTeam,-Day))
  curr_play$movement <- sqrt((diff(c(NA,curr_play$field_x)))^2 +  (diff(c(NA,curr_play$field_y)))^2)
  curr_play$time_differences <- c(NA,diff(curr_play$timestamp)) / 1000
  curr_play$speed <- curr_play$movement / curr_play$time_differences
  curr_play$acceleration <- c(NA,diff(curr_play$speed)) / curr_play$time_differences
  start_time <- NA
  start_x <- NA
  start_y <- NA
  if(nrow(curr_play)== 0){
    return(list( NA, NA,NA))
  }else{
    for(i in 1:nrow(curr_play)){
      x <- curr_play$field_x[i]
      y <- curr_play$field_y[i]
      if(!is.na(curr_play$acceleration[i]) & curr_play$acceleration[i] > threshold){
        start_time <- curr_play$timestamp[i]
        start_x <- x
        start_y <- y
        break
      }
    }
    return(list( start_time,  start_x,start_y))


  }
}

## for loop that iterates through all first base runners and calculates their start time
fb_leaving_pos <- data.frame()

for(i in 1:nrow(steal_stats)){
  if(is.na(steal_stats$first_baserunner.x[i])){
    print("hi")
    fb_leaving_pos  <- rbind(fb_leaving_pos ,list( NA, NA, NA))
    next
  }

  time <- starting_time(steal_stats$game_str[i],steal_stats$play_id[i],11)
  fb_leaving_pos <- rbind(fb_leaving_pos,time)
  print(i)
}

colnames(fb_leaving_pos) <- c("fb_start_time","fb_start_x","fb_start_y")

steal_stats <- cbind(steal_stats,fb_leaving_pos)

## this loop iterates through second base runners

sb_leaving_pos <- data.frame()

for(i in 1:nrow(steal_stats)){
  if(is.na(steal_stats$second_baserunner.x[i])){
    print("hi")
    sb_leaving_pos  <- rbind(sb_leaving_pos ,list(NA,  NA, NA))
    next
  }

  time <- starting_time(steal_stats$game_str[i],steal_stats$play_id[i],12)
  sb_leaving_pos <- rbind(sb_leaving_pos,time)
  print(i)
}

colnames(sb_leaving_pos) <- c("sb_start_time","sb_start_x","sb_start_y")

steal_stats <- cbind(steal_stats,sb_leaving_pos)

write.csv(steal_stats,"Data_csvs/Timings_with_zone_entry.csv")


## lets calculate first base runner leadoff and first baserunner speed
View(steal_stats)
steal_stats$sb_entry_time <- steal_stats$sb_entry_time / 1000
steal_stats$fb_start_time <- steal_stats$fb_start_time/ 1000
steal_stats$time_to_second <- ifelse(is.na(steal_stats$sb_entry_time)|is.na(steal_stats$fb_start_time),
                                     NA,steal_stats$sb_entry_time - steal_stats$fb_start_time)

steal_stats$tb_entry_time <- steal_stats$tb_entry_time / 1000
steal_stats$sb_start_time <- steal_stats$sb_start_time / 1000
steal_stats$time_to_third <- ifelse(is.na(steal_stats$tb_entry_time)|is.na(steal_stats$sb_start_time),
                                    NA,steal_stats$tb_entry_time - steal_stats$sb_start_time)

steal_stats$time_to_third

first_base <- c(62.58,63.64)
second_base <- c(0,127.28)
third_base <- c(0,0.71)

compute_distance <- function(vec1, vec2) {
  # Ensure the vectors have the same length
  if(length(vec1) != length(vec2)) {
    stop("Vectors must be of the same length")
  }

  # Calculate the Euclidean distance
  distance <- sqrt(sum((vec1 - vec2)^2))

  return(distance)
}

first_base_leadoff <- c()

for(i in 1:nrow(steal_stats)){
  if(is.na(steal_stats$fb_start_x[i]) | is.na(steal_stats$fb_start_y[i])){
    first_base_leadoff <- c(first_base_leadoff,NA)
  }else{
    lead <- compute_distance(c(steal_stats$fb_start_x[i],steal_stats$fb_start_y[i]),first_base)
    first_base_leadoff <- c(first_base_leadoff,lead)
  }

}

steal_stats$fb_leadoff <- first_base_leadoff

first_base_leadoff
second_base_leadoff <- c()

for(i in 1:nrow(steal_stats)){
  if(is.na(steal_stats$sb_start_x[i]) | is.na(steal_stats$sb_start_y[i])){
    second_base_leadoff <- c(second_base_leadoff,NA)
  }else{
    lead <- compute_distance(c(steal_stats$sb_start_x[i],steal_stats$sb_start_y[i]),second_base)
    second_base_leadoff <- c(second_base_leadoff,lead)
  }

}

steal_stats$sb_leadoff <- second_base_leadoff

baseball_diamond2 <- ggplot() +
  geom_baseball(league = "MLB", display_range = "full") +
  coord_fixed() +
  theme_void()

steal_stats
heatmap_plot <- ggplot(steal_stats, aes(x = fb_start_x, y = fb_start_y)) +
  stat_density_2d(aes(fill = ..density..), geom = "tile", contour = FALSE) +
  scale_fill_viridis_c(option = "plasma") +  # Use a continuous color scale
  labs(title = "Heatmap of Player Positions",
       x = "X Position",
       y = "Y Position") +
  theme_minimal()

heatmap_plot
combined_plot

write.csv(steal_stats,"Data_csvs/Timings_with_zone_entry.csv")


View(steal_stats)
