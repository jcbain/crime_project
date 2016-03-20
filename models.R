setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_2/')

library(klaR)
library(party)
library(rpart)
library(mlbench)
library(caret)
library(ggplot2)
library(plotly)
library(caret)

######################
## CUSTOM FUNCTIONS ##
######################

## a function to discritize a feature into equal binning based ##
EqualFreq <- function(x,n){ # x is the feature vector, and n is the number of bins
  nx <- length(x)
  nrepl <- floor(nx/n)
  nplus <- sample(1:n,nx - nrepl*n)
  nrep <- rep(nrepl,n)
  nrep[nplus] <- nrepl+1
  x[order(x)] <- rep(seq.int(n),nrep)
  x
}

#######################
## READ IN THE FILES ##
#######################

## read in a year of the data ##
p12<-read.csv('output_data/prop12.csv')
p13<-read.csv('output_data/prop13.csv')
p14<-read.csv('output_data/prop14.csv')

## read in a year of the data ##
df12<-read.csv('output_data/new_2012.csv')
df13<-read.csv('output_data/new_2013.csv')
df14<-read.csv('output_data/new_2014.csv')


## create state codes ##
df12$codes<-as.factor(state.abb[match(df12$State,state.name)] )
df13$codes<-as.factor(state.abb[match(df13$State,state.name)] )
df14$codes<-as.factor(state.abb[match(df14$State,state.name)] )

## aggregate some data based on the sum of offenses per each state ##
states12<-aggregate(cbind(Population, Total_Offenses,Crimes_Against_Persons,Crimes_Against_Property,Crimes_Against_Society)~codes, data=df12, sum, na.rm=TRUE)
states13<-aggregate(cbind(Population, Total_Offenses,Crimes_Against_Persons,Crimes_Against_Property,Crimes_Against_Society)~codes, data=df13, sum, na.rm=TRUE)
states14<-aggregate(cbind(Population, Total_Offenses,Crimes_Against_Persons,Crimes_Against_Property,Crimes_Against_Society)~codes, data=df14, sum, na.rm=TRUE)

## find proportions based off of population for each state ##
proStates12<-states12[,3:ncol(states12)]/states12$Population
prost12<-cbind(states12[,1:2],proStates12)

proStates13<-states13[,3:ncol(states13)]/states13$Population
prost13<-cbind(states13[,1:2],proStates13)

proStates14<-states14[,3:ncol(states13)]/states14$Population
prost14<-cbind(states14[,1:2],proStates14)

prost<-rbind(prost12,prost13,prost14)

## count the number of NAs in a data frame ##
as.data.frame(sapply(df14, function(y) (sum(length(which(is.na(y))))))) 

df<-na.omit(df14)

pop_discr<-EqualFreq(df$Crimes_Against_Persons,10)

x = subset(df,select=c("Population","Total_Offenses","Crimes_Against_Property","Crimes_Against_Society" ))
x = subset(df,select=c("Population"))
y = as.factor(pop_discr)

EqualFreq <- function(x,n){
  nx <- length(x)
  nrepl <- floor(nx/n)
  nplus <- sample(1:n,nx - nrepl*n)
  nrep <- rep(nrepl,n)
  nrep[nplus] <- nrepl+1
  x[order(x)] <- rep(seq.int(n),nrep)
  x
}

model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
