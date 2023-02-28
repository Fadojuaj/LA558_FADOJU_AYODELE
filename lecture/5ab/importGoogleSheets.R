# Importing Google Sheets - lecture 5a
# Tutorial: https://www.digitalocean.com/community/tutorials/google-sheets-in-r
# February 14, 2023
# Chris Seeger


#Install the required package
install.packages('googlesheets4')

#Load the required library 
library(googlesheets4)

#Read Google sheets data into R
gamesWon <- read_sheet('https://docs.google.com/spreadsheets/d/13gg0Gn1XwC_eb785fxoiSVsJUmrYv1lnUS67vjENEqA/edit?usp=sharing')
head(gamesWon)

## I want to use tidyverse so I need to load it. Since it is already in my list
## of packages, I only need to access the library

library(tidyverse)
glimpse(gamesWon) #a viewing feature of tidyverse

#How many games were played?
totalGamesPlayed <- gamesWon %>%
  summarize(GamesWon = sum(Games_Won))
totalGamesPlayed

## To view total wins by team we will group and summarize the data
teamWin <- gamesWon %>%
  group_by(Team) %>%
  summarize(GamesWon = sum(Games_Won), 
    team_players = n())

## To view total wins house and team we will group and summarize the data
HouseTeamWin <- gamesWon %>%
  group_by(House, Team) %>%
  summarize(GamesWon = sum(Games_Won), 
      team_players = n())


#which house won the most games as a percent of the total games?
housePerWin <- gamesWon %>%
  group_by(House) %>%
  summarize(GamesWon = sum(Games_Won)) %>%
  mutate(percentWins = round(GamesWon /  sum(GamesWon), 3)*100) %>%
  as.data.frame()

# now manually check your results!



# View the results as a graph, I referenced the following tutorial 
# while applying this to my data. It clearly shows several other examples
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

# Basic barplot
v1<-ggplot(data=housePerWin, aes(x=House, y=percentWins)) +
  geom_bar(stat="identity")
v1

# Note From the docs, "geom_col() uses stat_identity(): it leaves the data 
# as is". So, it expects you to already have the y values calculated and to use 
# them directly.

# Horizontal bar plot
v1 + coord_flip()

# Basic barplot with a slight formatting
v1<-ggplot(data=housePerWin, aes(x=House, y=percentWins)) +
  geom_bar(stat="identity", width=0.5, color="blue", fill="white")
v1

# Basic barplot with a slight formatting
v1<-ggplot(data=housePerWin, aes(x=House, y=percentWins)) +
  geom_bar(stat="identity", width=0.5, fill = "red") +
  geom_text(aes(label=percentWins), vjust=2.0, size=3.5,color="pink")+
  theme_minimal()
v1

#add a title
v1 + labs(title="House Win Percentage", x ="Team's House", y = "Percentage")
v1