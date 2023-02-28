# Importing an excel file - lecture 5a and 5b
# 
# February 14 and 15, 2023
# Chris Seeger


install.packages(c("tidyverse", "googlesheets4"))

#library(googlesheets4)
#library(tidyverse)
library("readxl")

# You need to know what your working directory is
# I like to save my file and then go to the Session --> Set Working Directory
# --> To Source File Location. As you become more familiar with R you can use
# other methods to do this.


cEnrollment <- read_excel("2021-2022 Iowa Public School District PreK-12 Enrollments by District, Grade, Race and Gender.xlsx", "for R") 

#while I am grouping and summarizing many fields, you could just have one or two.
cEnrollmentList <- cEnrollment %>%
  group_by(COUNTYNAME) %>%
  summarize(TotalK12= sum(TotalK12), 
            HispanicTotal = sum(HispanicTotal), 
            HispanicPercent = (sum(HispanicTotal)/sum(TotalK12))*100, 
            NativeAmericanTotal = sum(NativeAmericanTotal), 
            AsianTotal = sum(AsianTotal), 
            BlackTotal = sum(BlackTotal), 
            PacificIslanderTotal= sum(PacificIslanderTotal), 
            WhiteTotal = sum(WhiteTotal), 
            MultiRaceTotal = sum(MultiRaceTotal), 
            MaleTotal = sum(TotalMale), 
            MalePercent = sum(TotalMale)/sum(TotalK12), 
            FemaleTotal = sum(TotalFemale)/sum(TotalK12),
            dCount = n()) 
cEnrollmentList 

#load the simple features package so we can read a shapefile
install_packages("sf")
library("sf")

iowaCounties_sf <- st_read("Counties.shp")
#notice that a geometry column is present in this data set

#We can plot this shapefile with ggplot2 below but it is just a generic map
ggplot() + 
  geom_sf(data = iowaCounties_sf, size = 3, color = "black", fill = "red") + 
  ggtitle("LA 558") + 
  coord_sf()

#while it would be best to have FIPS or GEOID values, we will
#use the county name to join. But to make the work, the easiest is to have 
#the column name be the same. So we will modify the column name

iowaCounties_sf <- iowaCounties_sf %>% 
  rename("COUNTYNAME" = "COUNTY")

#to join the geometry to our cEnrollmentList data use dplyr
#note I put the data withthe geometry on the left of the join
myMap <- left_join(iowaCounties_sf, cEnrollmentList, by = "COUNTYNAME")

#Plot the variable HispanicPercent and add a title.
ggplot(myMap) + 
  geom_sf(aes(fill = HispanicPercent)) +
  ggtitle("LA 558 Assignment 2")

#remove the background and center the title
ggplot(myMap) + 
  geom_sf(aes(fill = HispanicPercent))+
  ggtitle("LA 558 Assignment 2") + 
  theme_void() +
  # move the title text to the middle
  theme(plot.title=element_text(hjust=0.5))
  



  
  