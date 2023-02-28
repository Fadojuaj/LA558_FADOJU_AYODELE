# Intro to TidyCensus - lecture 5b
# 
# February 15, 2023
# Chris Seeger



install.packages(c("tidycensus", "tidyverse"))
library(tidycensus)
library(tidyverse)



# Load required API Keys ####
# If your Census key is stored properly it will be loaded 
# optionally instead of retrieving it, you could add the key below.

census_api_key('key', overwrite = FALSE, install = TRUE)


#working with American Community Survey (ACS) Data
# create a table of the ACS Variables
vars <- load_variables(2021, "acs5")
View(vars)


iowa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "IA",
  year = 2021,
  geometry = TRUE
)

plot(iowa_income["estimate"])


##### Get Census counts form 2010 and 2020 and find the change in population


#Working with Decennial data currently is limited to the avaialble data 
#which you can see as a list using this
dec20 <- load_variables(year = 2020, dataset = "pl")
View(dec20)

## Step 1) Get the total populations for each county in Iowa for 2010
county_pop_10 <- get_decennial(
  state = "IA",
  geography = "county",
  variables = "P001001",
  year = 2010
)
head(county_pop_10)

#From the 2019 data I only want the value and the Geoid so I can join it 
#to the 2020 data. However I need to name the value field value10 so it
#does not conflict with the 2020 value field.
county_pop_10_clean <- county_pop_10 %>%
  select(GEOID, value10 = value)
county_pop_10_clean


## Get the total populations for each county in Iowa for 2020
## Also get the geometry with this pull
county_pop_20 <- get_decennial(
  state = "IA",
  geography = "county",
  variables = "P1_001N",
  year = 2020,
  geometry = TRUE
) %>% rename(value20 = value)

# Merge the data using a join using hte GEOID field
county_pop_20 %>%
  select(GEOID, NAME, value20)
county_joined <- county_pop_20 %>%
  left_join(county_pop_10_clean, by = "GEOID")

#calcualte the differece and percent
county_change <- county_joined %>%
  mutate(
    total_change = value20 - value10,
    percent_change = 100 * (total_change / value10)
  )


#plot county change as the actual count
plot(county_change["total_change"])

#plot county change as a percent
plot(county_change["percent_change"])




