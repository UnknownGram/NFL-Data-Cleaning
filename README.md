# NFL-Data-Cleaning and Shiny App



This project scraps and visualizes players rushing yards through the year 2023 to 2026. Using the offical NFL website, i webscaped the data and did some data cleaning to make sure the data can be used for data visualization. 

Data Cleaning Process: 
All of the data cleaning is done in the 'RushingDataCleaning.R' file.
The data cleaning process was mainly string manuplication since the data had either extra conponemts or data merged together during the webscaping process. Once the data cleaning process was done, i put it into a function just so i can collect and clean data for other years and not just one.
The reason i did this was because the data from other years is on different urls.

EDA:
When looking at just rushing data for just 2025. James Cook III was ranked 1st in Rushing yards in the 2025 NFL season with Derrick Henry being a close 2nd.
I also looked at the who had the highest fumble in the season which James Cook III and Josh Allen had the most with 6. But I realized that this isn't a fair comparsion since James Cook is a RB so he would have more attempts of running the ball which also means more likely to have more fumbles
That's when I grab the top 5 players with the most attempts and you can confirm that James Cook had a lot of attempts.
So i decided to get the proporational of fumbles and attempts to see who fumbles more based on their attempts and you can see in 2025 josh Allen had the highest fumbles ratio based on the number of attempts

Shiny:
The shiny app i created allows you to view the top 10 player for each stats and year in a simple bar chart. 
The app also had a way to compare players in that stat and year to see how they perform compare to other players.

