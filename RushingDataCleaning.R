# Running Backs

library(httr2)
library(rvest)
library(tidyverse)

# Added this so the graph can help team color. Also made it easier to remove the team name from player name
nfl_teams <- c("ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN","DET","GB",
               "HOU","IND","JAX","KC","LAC","LAR","LV","MIA","MIN","NE","NO","NYG",
               "NYJ","PHI","PIT","SEA","SF","TB","TEN","WSH")
team_regex <- paste0("(", paste(nfl_teams, collapse = "|"), ")$")

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
      Team   = str_extract(Name, team_regex),
      Player = str_remove(Name, team_regex)  # Removes the team name from players names
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


# EDA

# Looking at the top 10 player with the most yards
Data2025 <- map_dfr(2025, scrape_espn_season)
Data2025 |> slice_max(YDS, n = 5) |>  # Show top 5 players
  ggplot(aes(x = Player, y = YDS)) + geom_col()


# Looking at the top 10 player with the most fumbles
Data2025 <- map_dfr(2025, scrape_espn_season)
Data2025 |> slice_max(FUM, n = 5) |>  # Show top 5 players
  ggplot(aes(x = Player, y = FUM)) + geom_col()

# Looking at the top 10 player with the most attempted
Data2025 <- map_dfr(2025, scrape_espn_season)
Data2025 |> slice_max(ATT, n = 5) |>  # Show top 5 players
  ggplot(aes(x = Player, y = ATT)) + geom_col()


# Looking at the numbers of fumbles and ranking them is unfair to the ones that run the ball more
# So we are going to do the proporation 
Data2025 <- map_dfr(2025, scrape_espn_season)
Data2025 |> mutate(prop = FUM / ATT) |> slice_max(prop, n = 5) |>  # Show top 5 players
  ggplot(aes(x = reorder(Player, prop), y = prop)) + geom_col() # Looked up reorder to make the bars go in order

# You can see in 2025 josh Allen had the highest fumbles ratio based on the number of attempts
