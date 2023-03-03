
# Assignment 3
# Working With Tidy Census
#
# March 1, 2023
# Fadoju, Ayodele


# load necessary libraries

install.packages(c("tidycensus", "tidyverse"))
install.packages("tigris")
library(tigris)
install.packages("sf")
install.packages("idbr")
install.packages("WDI")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")






library(tidycensus)
library(tidyverse)
library(ggplot2)
library(tigris)
library(sf)
library("RColorBrewer")           
library(dplyr)
library(sf)
library(tidyverse)
library(tidycensus)
library(idbr)
library(readr)

# I tried to use the continent of Africa using get_Idb but it did not work.
# I probably will need to get on a call with you to clear up some stuffs.


# Plot 1-----------------------------------------------------

ia_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "IA",
  year = 2020,
  moe_level = 99
) %>%
  mutate(NAME = str_remove(NAME, ", Iowa"))

ia_income_Fivehighs <- ia_income %>%
  top_n(5, estimate) %>%
  arrange(desc(estimate))

ggplot(ia_income_Fivehighs, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_bar(color = "Blue", fill = "Red", alpha = 0.5, width = 0.5, stat = "identity", show.legend = FALSE) + 
  labs(title = "Median Household Income, 2016-2020 ACS", 
       subtitle = "Five Counties with high Income in Iowa", 
       x = "Median Income",
       y = "County",
       caption = "Source: ACS 2016-2020") +
  coord_flip() +
  scale_fill_manual(values = c("Blue", "Red")) +
  theme_minimal(base_size = 14)


# Plot 2---------------------------------------------------------------------------

vars <- load_variables(2021, "acs5")
View(vars)

Educational_attainment <- get_acs(
  geography = "county",
  state = "Georgia",
  variables = "B15003_022",
  summary_var = "B15003_001",
  year = 2021,
  geometry = TRUE
)
View(Educational_attainment)

plot(Educational_attainment["estimate"])

Educational_attainment <- Educational_attainment %>%
  mutate(percent = round(estimate/ summary_est, 3)*100)

plot(Educational_attainment["percent"]) 

plot2 <- ggplot(data = Educational_attainment) +
  geom_sf(aes(fill = percent)) + 
  scale_fill_gradientn(colors = c("white", "orange")) +
  coord_sf(crs = "+init=epsg:3081") +
  labs(title = "Percent of population with educational attainment of 
       bachelor's degree or higher", fill="percent") +
  theme(rect = element_blank(), axis.ticks = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank())

plot2


## Plot 3-----------------------------------------------

# read data
employment_data <- read.csv("Employment_By_Industry.csv", stringsAsFactors = FALSE)

# melt data
employment_data_long <- employment_data %>%
  pivot_longer(cols = -c(Employment_opportunities, Change_2010_2021), names_to = "year", values_to = "value") %>%
  mutate(year = str_replace(year, "Year_", ""))

# plot
ggplot(employment_data_long, aes(x = factor(1), y = value, fill = Employment_opportunities)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  facet_wrap(vars(year), ncol = 5) +
  scale_fill_hue() +
  labs(title = "Changes in Employment by Industry", fill = NULL)
