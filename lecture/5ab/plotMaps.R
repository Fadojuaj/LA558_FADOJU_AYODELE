
#source
#https://cran.r-project.org/web/packages/usmap/vignettes/introduction.html
#https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
#https://cran.r-project.org/web/packages/usmap/vignettes/advanced-mapping.html


library(usmap)
library(ggplot2)

#Blank US state map
usmap::plot_usmap()

# US county map
usmap::plot_usmap(regions = "counties")


#Raw map data
#The raw US map data for counties or states can be obtained for further 
#manipulation (and joining with data). The default regions is "states".
states_df <- usmap::us_map()
counties_df <- usmap::us_map(regions = "counties")


# FIPS codes
# FIPS codes are defined in the Federal Information Processing Standards by the 
# US government. One usage is uniquely identifying US states and counties 
# (among other things such as identifying countries for the CIA World Factbook).
# Downloading datasets from the US Census will often include FIPS codes as 
# identifiers so it can be helpful to know what a FIPS code represents. 
# The functions in usmap are built around the FIPS code identification 
# system and so convenience methods for accessing them and performing 
# reverse-lookups have been included.
# 
# State/County FIPS lookup
# Get FIPS code for a state
usmap::fips(state = "MA")
#> [1] "25"
usmap::fips(state = "Massachusetts")
#> [1] "25"

# Get FIPS code for a county
usmap::fips(state = "NJ", county = "Bergen")
#> [1] "34003"
usmap::fips(state = "CA", county = "Orange County")
#> [1] "06059"
# The parameters are NOT case sensitive!
usmap::fips(state = "ca", county = "oRanGe cOUNty")
#> [1] "06059"


# FIPS reverse lookup
# If the FIPS code is known and want to see what state/county it corresponds 
# to, use the reverse lookup function fips_info.

usmap::fips_info(c("30", "33", "34"))
#>   abbr fips          full
#> 1   MT   30       Montana
#> 2   NH   33 New Hampshire
#> 3   NJ   34    New Jersey

usmap::fips_info(c("01001", "01003", "01005", "01007"))
#>  full       abbr       county  fips
#> 1 Alabama   AL Autauga County 01001
#> 2 Alabama   AL Baldwin County 01003
#> 3 Alabama   AL Barbour County 01005
#> 4 Alabama   AL Bibb    County 01007




plot_usmap(regions = "counties") + 
labs(title = "US Counties",
     subtitle = "This is a blank map of the counties of the United States.") + 
theme(panel.background = element_rect(color = "black", fill = "lightblue"))



#Plot only certain states
plot_usmap(include = c("CA", "ID", "NV", "OR", "WA")) +
labs(title = "Western US States",
     subtitle = "These are the states in the Pacific Timezone.")


#Add some data to the map
plot_usmap(data = statepop, values = "pop_2015", color = "red") + 
scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
theme(legend.position = "right")


#Change fill color scale
plot_usmap(data = statepop, values = "pop_2015", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Population (2015)", label = scales::comma
  ) + theme(legend.position = "right")





########################################################
# Required Data Format
# 
# The data passed to the data parameter in plot_usmap() must be a data frame 
# with at least two columns. One of the columns must be named "fips" or "state" 
# and contain either the FIPS code, the state abbreviation, or the state name 
# (for county maps only the FIPS code is supported). The second column must be 
# the values to be plotted for each region. The default name of the values 
# column is "values". If a different name is used in the data frame, the name 
# can be specified in the values parameter of plot_usmap. Any extra columns 
# in the data frame will be ignored.



#FIPS column with default values column
df <- data.frame(
  fips = c("02", "01", "05", "04"),
  values = c(14, 18, 19, 8)
)
plot_usmap(data = df)


#FIPS column with custom values column
#Name of values column must be specified in values parameter if it is not "values".
df <- data.frame(
  fips = c("02", "01", "05", "04"),
  population = c(14, 18, 19, 8)
)
plot_usmap(data = df, values = "population")


#States
#Abbreviations and full names can be mixed if desired.
df <- data.frame(
  state = c("AL", "Alaska", "AR", "AZ"),
  values = c(14, 18, 19, 8)
)
plot_usmap(data = df)


# Counties
# County names are not supported in plot_usmap data frames. Use fips instead.
df <- data.frame(
  fips = c("10001", "10003", "10005"),
  values = c(93, 98, 41)
)
plot_usmap(data = df)






#Built-in Regions
#usmap provides some built-in regions based on the US Census Bureau Regions 
#and Divisions. These can be used in place of the include/exclude parameters 
#when using us_map or plot_usmap and start with a . (dot):
usmap::plot_usmap(include = .south_region)
usmap::plot_usmap(include = .east_south_central)
usmap::plot_usmap(include = .south_region, exclude = .east_south_central)

#This also works with county maps. The regions can also be combined with actual 
#state or FIPS values within the include/exclude parameters:
usmap::plot_usmap("counties", 
    include = c(.south_region, "IA"), 
    exclude = c(.east_south_central, "12"))  # 12 = FL

#You can even include or exclude individual counties (county-level 
# inclusions/exclusions can only be done via their FIPS codes due to duplicate 
# county names across states; for example eight different states have an 
# “Orange County”):
usmap::plot_usmap("counties", fill = "yellow", alpha = 0.25,
    # 06065 = Riverside County, CA
    include = c(.south_region, "IA", "06065"),
    # 12 = FL, 48141 = El Paso County, TX
    exclude = c(.east_south_central, "12", "48141"))

# These parameters therefore allow for the possibility of some complex 
# compositions of states and counties, to create the exact map that is desired.