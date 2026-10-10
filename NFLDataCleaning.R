#  https://www.espn.com/nfl/stats/_/season/2025/seasontype/2


#install.packages("httr2")
#install.packages("http")
#install.packages("rvest")
#install.packages("tidyverse")
library("httr2")
library("http")
library("rvest")
library("tidyverse")
# Fetch and parse the page
req <- request("https://www.espn.com/nfl/stats/_/season/2025/seasontype/2")
page <- req_perform(req) |> resp_body_html()

# 1. Get the stats table (the second table in the list)
all_tables <- page |> 
  html_elements("table") |> 
  html_table()

stats_table <- all_tables[[2]] 

# 2. Extract the player names directly from the HTML nodes
# We find the first table, look at each row, and grab the name from the image 'alt' attribute
all_html_tables <- page |> html_elements("table")


# I had to this because the text was a clickable text so the name wasn't getting picked up by rvest
player_names <- all_html_tables[[1]] |> 
  html_elements("tbody tr") |>  # Go through each row
  html_element("img") |>        # Target the player's headshot image
  html_attr("alt")              # Extract the name from the image's 'alt' attribute

# 3. Combine the extracted names with the stats table
clean_data <- cbind(Player = player_names, stats_table)

# 4. Remove the final "Complete Leaders" row at the bottom
clean_data <- head(clean_data, -1)

clean_data$Rushing |> str_extract("^\\d+")

clean_data <- clean_data |> mutate(Rank = str_extract(clean_data$Rushing, "^\\d+")) |> rename(Team = Rushing)
clean_data$Team <- clean_data$Team |> str_remove_all("^\\d+")

# Renaming th Rushing TO Team

# View the final result
head(clean_data)


library(httr2)
library(rvest)
library(dplyr)
library(stringr)

url <- "https://www.espn.com/nfl/stats/_/season/2025/seasontype/2"
req <- request(url)
page <- req_perform(req) |> resp_body_html()

# 1. Select all <a> tags that contain the text "Complete Leaders"
leader_links <- page |> 
  html_elements("a") |> 
  # Filter only links whose visible text says "Complete Leaders"
  keep(~ str_detect(html_text(.x), "(?i)complete leaders")) |> 
  # Extract the 'href' destination URL
  html_attr("href")

# View the extracted URLs
print(leader_links)


library(httr2)
library(rvest)
library(tidyverse)

# Fetch and parse the full stats page
req <- request("https://www.espn.com/nfl/stats/player/_/season/2026/seasontype/2")
page <- req_perform(req) |> resp_body_html()

# 1. Get all tables on the page
all_tables <- page |> html_table()

names_table <- all_tables[[1]]  # Contains RK, Name, POS
stats_table <- all_tables[[2]]  # Contains YDS, AVG, TD, INT, etc.

# 2. Combine the names/teams table and stats table side-by-side
full_data <- cbind(names_table, stats_table)

# 3. Clean up and split the 'Name' column into Player Name and Team
clean_data <- full_data |> 
  mutate(
    # Extract Team abbreviation (uppercase letters at the end of the text string)
    Team = str_extract(Name, "[A-Z]+$"),
    
    # Strip the Team abbreviation off the end to leave just the Player Name
    Player = str_remove(Name, "\\s+[A-Z]+$"),
    
    # Ensure Rank is formatted as an integer
    Rank = as.integer(RK)
  ) |> 
  # Reorder columns nicely
  select(Rank, Player, Team, POS, everything(), -RK, -Name)

# View the clean results
head(clean_data)
