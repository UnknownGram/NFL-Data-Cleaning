# Running Backs

library(httr2)
library(rvest)
library(tidyverse)

# Testing Webscaping
url <- sprintf("https://www.espn.com/nfl/stats/player/_/stat/rushing/season/2025/seasontype/2")

req <- request(url)
page <- req_perform(req) |> resp_body_html()

all_tables <- page |> html_table()

names_table <- all_tables[[1]]  # RK, Name, POS
stats_table <- all_tables[[2]]  # YDS, AVG, TD, INT, etc.
names_table
stats_table

stats_cleaned <- stats_table |> 
  mutate(across(everything(), ~ str_replace_all(as.character(.x), ",", ""))) |> 
  mutate(across(-POS, ~ suppressWarnings(as.numeric(.x))))
stats_cleaned

# Making a function so all i have to do is put in the year to get the clean data
scrape_espn_season <- function(year) {
  message("Scraping season: ", year)
  
  url <- sprintf("https://www.espn.com/nfl/stats/player/_/stat/rushing/season/2025/seasontype/2", year)
  
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



# Debate on adding other POS