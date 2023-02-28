# Assignment 2
# Data Visualization Using R
#
# February 26, 2023
# Fadoju, Ayodele

#--------------------------------------------------------------------------------------------------
install.packages("tidyverse")


library(tidyverse)

library("readxl")

# Read the data from the Excel file
employment_data <- read_excel("Employment_By_Industry.xlsx")
names(employment_data)

                                    
head(Employment_By_Industry)
glimpse(Employment_By_Industry)

# Mutate the data to classify the sectors
employment_data <- employment_data %>%
  mutate(Sector = if_else(Employment_opportunities %in% c("Farm", "Construction", "Manufacturing"),
                          "Non-service sector",
                          if_else(Employment_opportunities == "Government",
                                  "Government",
                                  "Service sector"))) %>%
  group_by(Sector) %>%
  summarise(Year_2001 = sum(Year_2001),
            Year_2010 = sum(Year_2010),
            Year_2021 = sum(Year_2021),
            Change_2010_2021 = sum(Change_2010_2021)) %>%
  ungroup()


# group by sector and calculate the sum of employment opportunities for each sector in each year
employment_data_summary <- employment_data %>%
  group_by(Sector) %>%
  summarise(Year_2001 = sum(Year_2001),
            Year_2010 = sum(Year_2010),
            Year_2021 = sum(Year_2021)) %>%
  ungroup()

# reshape data into a tidy format
employment_data_summary_tidy <- employment_data_summary %>%
  pivot_longer(cols = starts_with("Year"),
               names_to = "Year",
               values_to = "Employment") %>%
  mutate(Year = factor(Year, levels = c("Year_2001", "Year_2010", "Year_2021")),
         Percentage = Employment / sum(Employment) * 100)

# plot the data using ggplot2
ggplot(employment_data_summary_tidy, aes(x = Year, y = Percentage, fill = Sector)) +
  geom_col(position = "dodge", color = "white") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")),
            position = position_dodge(width = 1),
            vjust = -0.5,
            size = 4) +
  labs(title = "Percentage of Employment Opportunities by Sector over Time",
       x = "Year",
       y = "Percentage",
       fill = "Sector") +
  theme_minimal()


#### Part Two

library(readr)
library(sf)


nigeria <- st_read("Nigera_Shapefiles.shp")
water_data <- read_csv("Source_Of_DrinkingWater_COVID19.csv")
nigeria_data <- left_join(nigeria, water_data, by = c("ID" = "ID"))

water_data <- water_data %>% 
  mutate(unimproved_Water_source = 100 - Improved_water_source)
names(water_data)


ggplot() +
  geom_sf(data = nigeria_data,
          aes(fill = unimproved_Water_source),
          color = "gray") +
  scale_fill_gradient(low = "white",
                      high = "darkblue",
                      name = "Unimproved water supply (%)") +
  theme_minimal() +
  ggtitle("A Map of Nigeria Showing Areas With Un-improved Water Source") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_void()




