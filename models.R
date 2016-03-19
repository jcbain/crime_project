
setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_2/')

library(klaR)
library(party)
library(rpart)
library(mlbench)
library(caret)
library(ggplot2)
library(plotly)

#######################
## READ IN THE FILES ##
#######################

## read in a year of the data ##
df12<-read.csv('output_data/prop12.csv')
df13<-read.csv('output_data/prop13.csv')
df14<-read.csv('output_data/prop14.csv')

## count the number of NAs in a data frame ##
as.data.frame(sapply(df14, function(y) (sum(length(which(is.na(y))))))) 

df<-na.omit(df14)


