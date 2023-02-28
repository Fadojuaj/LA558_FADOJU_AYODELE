#Lecture week 5 extra!
#Professor Seeger put together some simpler examples to help you compete 
# Assignment 2, where you are asked to create a map graphic in R.

#Load in these three packages if you don't have them already loaded.
install.packages(c("usmap", "usmapdata", "ggplot2"))

#add libraries
library(usmap)
library(usmapdata) #I did not seem to need this library, but add just in case!
library(ggplot2)

#An example loading States
#Abbreviations and full names can be mixed if desired.
#the data frame df has a column of states and one of values
df <- data.frame(
  state = c("AL", "Alaska", "AR", "AZ"),
  values = c(14, 18, 19, 8)
)

#now just plot the data contained in df
plot_usmap(data = df)

#From the above demo, you could utilize your own data. For example A fictional
# list of the states I have visited or lived in. In this example I added the 
# states and for the values, I entered 10 if I lived there, 5 if I visited 
#frequently and 1 if I visited, but rarely.
df <- data.frame(
  state = c("ND", "IA", "MN", "AL", "Alaska", "AR", "NC"),
  values = c(10, 10, 5, 1, 1, 1, 1)
)

#once the data is defined, I am ready to create the plot
#Note that I stored this plot inside of a variable named myMap. I did this
#because when stored in a variable I can add changes to the plot later without 
#having to completely remake the plot.
myMap <- plot_usmap(data = df) +
  labs(title = "States Visited",
  subtitle = str_wrap("I actually have visited more states than this, but 
      this will work for this exercise. Notice that I am wrapping this subtitle
      text so it fits on multiple lines if it exceeds 80 characters. Do be 
      careful as it is eazy to hav typo erarors!", 80))  + 
  theme(panel.background = element_rect(color = "black", fill = "white"), legend.position = "right")

#display the plot
myMap 

#after some consideration, I have decided to modify the title and I can do this
#by adding a + and the change of the title parameter. Here I adjust the title
myMap + labs(title = "Some of the States I have Visited")

#display the plot with the new title
myMap 

#There are additional modifications to the theme, Legend that could be made, but
#for the purpose of assignment 2, why don't we try and load in an external 
#excel fie and make a quick plot of that data. First make sure and have the 
#readxl file loaded.
install.packages("readxls")
library("readxl")

# You need to know what your working directory is
# I like to save my file and then go to the Session --> Set Working Directory
# --> To Source File Location. As you become more familiar with R you can use
# other methods to do this.

#I set the working directory to be the same as my R file
#note that the population values here are old, so don't use for 
#anything other than practice.

#read the excel fiel and save it in state_df. Not this is actually a tibble, 
#but it will still work.
statePop_df <- read_excel("statePop.xlsx") 

#create the plot_usmap and save it as statePop
statePop <- plot_usmap(data = statePop_df )
                       
#create a plot
statePop

#Add the title
statePop + labs(title = "State Populations")

#continue to modify the map plot parameters, maybe change the color and 
#the legend title?

statePop + scale_fill_continuous(
  low = "white", high = "green", name = "Population (7/1/2013)", label = scales::comma
) + theme(legend.position = "right")

#finally, you need to export the map. You can do this manually from the 
#plots pane or with htis code

#See this reference for details on customization of the export. 
#My code will export a 480x480 graphic file.
# http://www.cookbook-r.com/Graphs/Output_to_a_file/

png("plot.png")
statePop
dev.off()