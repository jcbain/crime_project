setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_2/')

library(ggplot2)
library(plotly)

df13<-read.csv('output_data/new_2013.csv')

# histograms of super-types of crime
hist(df13$Crimes_Against_Persons/df13$Population)
hist(df13$Crimes_Against_Property/df13$Population)
hist(df13$Crimes_Against_Society/df13$Population)

# transform counts into proportions (each offense/total population per 100,000 people)
props<-df13[, 7:ncol(df13)]/(df13$Population) # first find the proportion for each crime per 100,000
prop13<-na.omit(cbind(df13[,1:6],props))

# play around with some graphs to explore the data
plot_ly(prop13, x = Population, y = Crimes_Against_Persons, text = paste("Agency: ", Agency_Name),
        mode = "markers", color = Agency_Type)

plot_ly(prop13, x = Crimes_Against_Persons, color = State, type = "box")




