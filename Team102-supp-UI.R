library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)

# Define UI for application
ui <- fluidPage(

  tabsetPanel(
    # First tab with the plot
    tabPanel("Ball location in Steal Plays",
             sidebarLayout(
               # Sidebar panel for inputs
               sidebarPanel(
                 # Input: Select the option to change the CSV
                 selectInput("Levels", "Select a level:",
                             choices = c("All", "Rookie ball", "Single A", "Double A", "Triple A"))
               ),
               # Main panel for displaying outputs
               mainPanel(
                 # Output: Dynamic plot
                 plotOutput("zones"),
               )
             )
    ),
    # Second tab with additional content
    tabPanel("Pitch Time Vs Pop Time",
             sidebarLayout(
               sidebarPanel(
                 # Input: Select the option to change the CSV
                 selectInput("level", "Select a level:",
                             choices = c("All", "Rookie ball", "Single A", "Double A", "Triple A"))
               ),
               mainPanel(
                 # Add any additional outputs here
                 plotlyOutput("pitchpop")
               )
             )
    ),
    tabPanel("Pitcher Stats",
             sidebarLayout(
               sidebarPanel(
                 h4("About this page"),
                 p("This app displays all stats in steal plays for pitchers in our farm system."),
                 p("Use the table to explore how various pitchers are in steal scenarios")

               ),
               mainPanel(
                 titlePanel("Pitcher Stats"),
                 DT::dataTableOutput("PitcherStats")


               )
             )
    ),
    tabPanel("Catcher Stats",
             sidebarLayout(
               sidebarPanel(
                 h4("About this page"),
                 p("This app displays all stats in steal plays for catchers in our farm system."),
                 p("Use the table to explore various catchers stats in steal scenarios")

               ),
               mainPanel(
                 titlePanel("Catcher Stats"),
                 DT::dataTableOutput("CatcherStats")


               )
             )
    )

  )
)

server <- function(input, output) {
  # Load the CSV data
  data <- read.csv("Team102-supp-UI-CSV1.csv")
  data_2 <- read.csv("Team102-supp-UI-CSV2.csv")
  Pitcher_stats <- read.csv("Team102-supp-UI-CSV3.csv")
  Pitcher_stats <- Pitcher_stats %>% select(-X)
  colnames(Pitcher_stats)[2] <- "steals_allowed"
  Catcher_stats <- read.csv("Team102-supp-UI-CSV4.csv")
  Catcher_stats <- Catcher_stats %>% select(-X) %>% filter(!is.na(steal_allowed))
  colnames(Catcher_stats)[2] <- "steals_allowed"

  # Reactive expression to filter data based on the selected option
  filtered_data <- reactive({
    option <- input$Levels
    switch(option,
           "All" = data,
           "Rookie ball" = filter(data, substr(game_str, nchar(game_str) - 1, nchar(game_str)) == "1A"),
           "Single A" = filter(data, substr(game_str, nchar(game_str) - 1, nchar(game_str)) == "2A"),
           "Double A" = filter(data, substr(game_str, nchar(game_str) - 1, nchar(game_str)) == "3A"),
           "Triple A" = filter(data, substr(game_str, nchar(game_str) - 1, nchar(game_str)) == "4A")
    )
  })

  filtered_data_pitch_pop <- reactive({
    option <- input$level
    switch(option,
           "All" = data_2,
           "Rookie ball" = filter(data_2, substr(game_str, nchar(game_str) - 1, nchar(game_str)) == "1A"),
           "Single A" = filter(data_2, substr(game_str, nchar(game_str) - 1, nchar(game_str)) == "2A"),
           "Double A" = filter(data_2, substr(game_str, nchar(game_str) - 1, nchar(game_str)) == "3A"),
           "Triple A" = filter(data_2, substr(game_str, nchar(game_str) - 1, nchar(game_str)) == "4A")
    )
  })

  # Reactive expression to process the filtered data
  processed_data <- reactive({
    data <- filtered_data()
    x_bin_size <- 1
    y_bin_size <- 1
    x_breaks <- seq(-2.5, 2.5, by = x_bin_size)
    y_breaks <- seq(0, 5, by = y_bin_size)

    zones <- data %>%
      mutate(
        ball_x_pos = cut(ball_position_x, breaks = x_breaks, include.lowest = TRUE, right = FALSE, labels = x_breaks[-1]),
        ball_y_pos = cut(ball_position_z, breaks = y_breaks, include.lowest = TRUE, right = FALSE, labels = y_breaks[-1]),
        level = substr(game_str, nchar(game_str) - 1, nchar(game_str))
      ) %>%
      filter(!is.na(ball_x_pos) & !is.na(ball_y_pos)) %>%
      group_by(ball_x_pos, ball_y_pos) %>%
      summarise(
        zone_value = n() - sum(valid_steal),
        total_zone = n()
      ) %>%
      ungroup() %>%
      mutate(
        ball_x_pos = as.numeric(as.character(ball_x_pos)),
        ball_y_pos = as.numeric(as.character(ball_y_pos))
      )

    bin_combinations <- expand.grid(
      ball_x_pos = x_breaks[-1],
      ball_y_pos = y_breaks[-1]
    )

    zones <- bin_combinations %>%
      left_join(zones, by = c("ball_x_pos", "ball_y_pos")) %>%
      replace_na(list(zone_value = 0, total_zone = 0))

    zones <- zones %>%
      group_by(ball_x_pos, ball_y_pos) %>%
      mutate(percentage = ifelse(zone_value > 0, zone_value / total_zone * 100, 0)) %>%
      ungroup() %>%
      mutate(highlight = ifelse(ball_x_pos >= -0.5 & ball_x_pos <= 2 & ball_y_pos >= 2 & ball_y_pos <= 4, TRUE, FALSE))

    return(zones)
  })

  processed_pitchpop <- reactive({
    pitchpop <- filtered_data_pitch_pop()


    x_hm_bin_size <- 0.05
    y_hm_bin_size <- 0.01

    # Create breaks for x and y
    x_hm_breaks <- seq(1.5,2.2,by = x_hm_bin_size)
    y_hm_breaks <- seq(0.45,0.6, by = y_hm_bin_size)

    zones_hm <- pitchpop %>% mutate (
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

    return(zones_hm)
  })

  # Update the plot
  output$zones <- renderPlot({
    zones <- processed_data()
    plot_colour <- "#D55E00"
    border_colour <- "steelblue"

    heatmap_plot <- ggplot(zones, aes(x = ball_x_pos, y = ball_y_pos, fill = percentage)) +
      geom_tile(color = "black") +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), color = "black") +
      scale_fill_gradient(low = "white", high = plot_colour) +
      geom_rect(data = subset(zones, highlight),
                aes(xmin = ball_x_pos - 0.5, xmax = ball_x_pos + 0.5, ymin = ball_y_pos - 0.5, ymax = ball_y_pos + 0.5),
                fill = NA, color = border_colour, size = 1.5) +
      theme_minimal() +
      labs(title = "Caught stealing percentage in strike zones ", x = "X", y = "Y", fill = "Caught Stealing Percentage") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      )
    heatmap_plot
  })

  output$pitchpop <- renderPlotly({
    pitchpop <- processed_pitchpop()
    plot_colour <- "#D55E00"
    border_colour <- "steelblue"

    g <- ggplot(pitchpop, aes(x = pop_time, y = pitch_time, fill = percentage)) +
      geom_tile(color = "black") +
      scale_fill_gradient(low = border_colour, high = plot_colour,na.value = "white") +
      theme_minimal() +
      labs(title = "Pop time vs Pitch time Heat Map", x = "Pop Time", y = "Pitch Time", fill = "Caught Stealing Percentage") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      )
    ggplotly(g)
  })

  output$PitcherStats <- DT::renderDataTable({
    datatable(Pitcher_stats)
  })
  output$CatcherStats <- DT::renderDataTable({
    datatable(Catcher_stats)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
