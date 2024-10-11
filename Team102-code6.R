library(tidyverse)
library(dplyr)
library(arrow)
library(gt)
library(webshot2)

plays <- read.csv("Data_csvs/plays_cse.csv")
View(plays)

Pitcher_steals <- plays %>%
  group_by(pitcher) %>%
  dplyr::summarize(
    steal_allowed = sum(valid_steal),
    steal_attempts = n(),
    expected_steals_with_average_catcher = sum(steal_with_average_catcher),
    average_pitch_time = mean(pitch_time)
  )

Pitcher_steals$caught_stealing_above_average <- Pitcher_steals$expected_steals_with_average_catcher - Pitcher_steals$steal_allowed

top_ten_csaX <- Pitcher_steals %>% arrange(desc(caught_stealing_above_average)) %>%
  slice_head(n=10)


bottom_ten_csaX <- Pitcher_steals  %>% arrange((caught_stealing_above_average)) %>%
  slice_head(n=10)

bottom_ten_csaX

top_ten_pitchers <- top_ten_csaX %>%
  gt() %>%
  tab_header(
    title = "Top Ten Pitchers In Caught Stealing Above Average",
    subtitle = "Steals Attempted and Allowed"
  ) %>%
  cols_label(
    pitcher = "Pitcher ID",
    steal_allowed = "Total Steals Allowed",
    steal_attempts = "Total Attempts",
    expected_steals_with_average_catcher = "Steals Allowed With Average Catcher",
    average_pitch_time = "Average Pitch Time",
    caught_stealing_above_average = "Caught Stealing Above Average"
  ) %>%
  fmt_number(
    columns = c(average_pitch_time
    ),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c(steal_allowed, steal_attempts,
                expected_steals_with_average_catcher,
                caught_stealing_above_average),
    decimals = 0
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
    footnote = "",
    locations = cells_body(
      columns = steal_allowed,
      rows = steal_allowed > 10
    )
  ) %>%
  tab_source_note(
    source_note = "SMT Data"
  )


bottom_ten_pitchers <- bottom_ten_csaX %>%
  gt() %>%
  tab_header(
    title = "Bottom Ten Pitchers In Caught Stealing Above Average",
    subtitle = "Steals Attempted and Allowed"
  ) %>%
  cols_label(
    pitcher = "Pitcher ID",
    steal_allowed = "Total Steals Allowed",
    steal_attempts = "Total Attempts",
    expected_steals_with_average_catcher = "Steals Allowed With Average Catcher",
    average_pitch_time = "Average Pitch Time",
    caught_stealing_above_average = "Caught Stealing Above Average"
  ) %>%
  fmt_number(
    columns = c(average_pitch_time
    ),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c(steal_allowed, steal_attempts,
                expected_steals_with_average_catcher,
                caught_stealing_above_average),
    decimals = 0
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
    footnote = "",
    locations = cells_body(
      columns = steal_allowed,
      rows = steal_allowed > 10
    )
  ) %>%
  tab_source_note(
    source_note = "SMT Data"
  )

gtsave(top_ten_pitchers, "TopTenPitchers.png")
gtsave(bottom_ten_pitchers, "BottomTenPitchers.png")


#####

catcher_steals <- plays %>%
  group_by(catcher) %>%
  dplyr::summarize(
    steal_allowed = sum(valid_steal),
    steal_attempts = n(),
    expected_steals_with_average_pitcher = sum(steals_with_average_pitcher),
    average_pop_time = mean(pop_time)
  )

catcher_steals$caught_stealing_above_average <- catcher_steals$expected_steals_with_average_pitcher - catcher_steals$steal_allowed

write.csv(catcher_steals,"Data_csvs/catcher_stats.csv")
write.csv(Pitcher_steals,"Data_csvs/pitcher_stats.csv")

top_ten_catcher <- catcher_steals %>% arrange(desc(caught_stealing_above_average)) %>%
  slice_head(n=10)


bottom_ten_catcher <- catcher_steals  %>% arrange((caught_stealing_above_average)) %>%
  slice_head(n=10)

top_ten_catchers <- top_ten_catcher %>%
  gt() %>%
  tab_header(
    title = "Top Ten Catchers In Caught Stealing Above Average",
    subtitle = "Steals Attempted and Allowed"
  ) %>%
  cols_label(
    catcher = "Catcher ID",
    steal_allowed = "Total Steals Allowed",
    steal_attempts = "Total Attempts",
    expected_steals_with_average_pitcher = "Steals Allowed With Average Pitcher",
    average_pop_time = "Average Pop Time",
    caught_stealing_above_average = "Caught Stealing Above Average"
  ) %>%
  fmt_number(
    columns = c(average_pop_time
    ),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c(steal_allowed, steal_attempts,
                expected_steals_with_average_pitcher,
                caught_stealing_above_average),
    decimals = 0
  ) %>%
  tab_source_note(
    source_note = "SMT Data"
  )


bottom_ten_catchers <- bottom_ten_catcher %>%
  gt() %>%
  tab_header(
    title = "Bottom Ten Catchers In Caught Stealing Above Average",
    subtitle = "Steals Attempted and Allowed"
  ) %>%
  cols_label(
    catcher = "Catcher ID",
    steal_allowed = "Total Steals Allowed",
    steal_attempts = "Total Attempts",
    expected_steals_with_average_pitcher = "Steals Allowed With Average Pitcher",
    average_pop_time = "Average Pop Time",
    caught_stealing_above_average = "Caught Stealing Above Average"
  ) %>%
  fmt_number(
    columns = c(average_pop_time),
    decimals = 2
  ) %>%

  fmt_number(
    columns = c(steal_allowed, steal_attempts,
                expected_steals_with_average_pitcher,
                caught_stealing_above_average),
    decimals = 0
  ) %>%
  tab_source_note(
    source_note = "SMT Data"
  )

gtsave(top_ten_catchers, "TopTencatchers.png")
gtsave(bottom_ten_catchers, "BottomTencatchers.png")
