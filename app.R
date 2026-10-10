# Running Backs

library(httr2)
library(rvest)
library(tidyverse)

# Making a function so all i have to do is put in the year to get the clean data
scrape_espn_season <- function(year) {
  message("Scraping season: ", year)
  
  url <- sprintf("https://www.espn.com/nfl/stats/player/_/stat/rushing/season/%d/seasontype/2", year)
  
  req <- request(url)
  page <- req_perform(req) |> resp_body_html()
  
  all_tables <- page |> html_table()
  
  names_table <- all_tables[[1]]  # RK, Name
  stats_table <- all_tables[[2]]  # POS, YDS, AVG, TD, INT, etc.
  
  
  stats_cleaned <- stats_table |> 
    # Removes the comma from the numbers so it can be converted to int
    mutate(across(everything(), ~ str_replace_all(as.character(.x), ",", ""))) |> 
    # This is keeps convert everything to numeric except for POS. Without -POS, POS became N/A
    mutate(across(-POS, ~ suppressWarnings(as.numeric(.x))))
  
  #Combine names table and new stat_table
  full_data <- cbind(names_table, stats_cleaned)
  
  # 3. Process player names, team abbreviations, and metadata
  clean_data <- full_data |> 
    mutate(
      Season = year,
      Rank   = suppressWarnings(as.integer(RK)),
      Team   = str_extract(Name, "[A-Z]+$"),
      Player = str_remove(Name, "\\s+[A-Z]+$"),
      Player = str_sub(Player, 1, -4)  # Removes the team name from players names
    ) |> 
    select(Season, Rank, Player, Team, POS, everything(), -RK, -Name)
  
  return(clean_data)
}

# Define years to scrape
years <- c(2023, 2024, 2025, 2026)

# Combine datasets seamlessly across all years
master_multi_year_data <- map_dfr(years, scrape_espn_season)

# View final combined multi-year table
print(master_multi_year_data)



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NFL DATA"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("years",
                        "Pick a Year:",
                        choices = years,
                        selected = 2025
        ),
        
        selectInput(
          "stat",
          "Pick a Stat:",
          choices = c("Rushing Yards" = "YDS", 
                      "Touchdowns"    = "TD", 
                      "Attempts"      = "ATT", 
                      "Yards/Carry"   = "AVG"),
          selected = "YDS"
        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Plot Title", plotOutput("rushingPlot")),
            tabPanel("Table Title", tableOutput("rushingTable")) # or DT::DTOutput
          )
        )
    )
)

server <- function(input, output) {
  
  # 1. Reactive dataset (Sits at the top level of server)
  filtered_data <- reactive({
    master_multi_year_data |> 
      filter(Season == as.numeric(input$years)) |> 
      slice_max(order_by = .data[[input$stat]], n = 10, with_ties = FALSE)
  })
  
  # 2. Render the Top 10 Bar Chart (Tab 1)
  output$rushingPlot <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = reorder(Player, .data[[input$stat]]), 
                   y = .data[[input$stat]], 
                   fill = Team)) +
      geom_col() +
      geom_text(aes(label = .data[[input$stat]]), hjust = -0.2, size = 4.5) +
      coord_flip() +
      theme_minimal(base_size = 14) +
      labs(
        title = sprintf("Top 10 Rushing Leaders (%s)", input$years),
        x = NULL,
        y = input$stat
      )
  })
  
  # 3. Render the Raw Data Table (Tab 2)
  output$rushingTable <- renderTable({
    filtered_data()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
