setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_2/')

library(klaR)
library(party)
library(rpart)
library(mlbench)
library(caret)
library(ggplot2)
library(plotly)
library(caret)
library(Hmisc)
require(e1071)

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
as.data.frame(sapply(df, function(y) (sum(length(which(is.na(y))))))) 




########********########
## NAIVE BAYES !!!!!! ##
########********########

df<-subset(p14, select=c('Agency_Type','Agency_Name','codes','Region','Population','Crimes_Against_Persons',
                          'Crimes_Against_Property','Crimes_Against_Society','Total_Offenses',
                          'Homicide_Offenses', 'Murder_and_Nonnegligent_Manslaughter',
                          'Negligent_Manslaughter','Justifiable_Homicide'))

df<-na.omit(df)
df<-df[df$Murder_and_Nonnegligent_Manslaughter >0,]



## discritize crimes against persons ##
df$discr<-EqualFreq(df$Murder_and_Nonnegligent_Manslaughter,5)

df$discr<-as.factor(df$discr)

# relabel based off of lower than values
df$disc<- factor(df$discr,
                 levels = c(1,2,3,4,5),
                 labels = c("< 3.134367e-05","< 5.273844e-05","< 9.168564e-05","< 0.0001800504","< 0.008196721"))

## create a subset of data ##
sub = sample(nrow(df), floor(nrow(df) * 0.6))

## 1 part of the subset will be a training the other will be a test ##
train<-df[sub,]
test<-df[-sub,]

xTrain<-subset(train,select=c("Population","codes"))
yTrain<-as.factor(train$disc)

xTest<-subset(test,select=c("Population","codes"))
yTest<-as.factor(test$disc)

model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))
prop.table(table(predict(model$finalModel,xTest)$class,yTest))
table(predict(model$finalModel,xTest)$class,yTest)

## plot box plots based on discritization ##

## no lables ##
ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

## plot in plotly murder rate per capita  
plot_ly(df, x = Population, color = factor(disc), type = "box") %>%
  layout(title= "Murder Rate Per Capita ~ Population Size", yaxis = ax)


## What about those that didn't have a murder rate ? ##
df2<-df[df$Murder_and_Nonnegligent_Manslaughter ==0,] 

ggplot(data=df2, aes(x = Population)) + geom_histogram() +
  geom_vline(aes(xintercept=mean(Population, na.rm=T)),  color="red", linetype="dashed", size=1) +
  ggtitle("Population for those Place with a 0 Murder Rate")


#************************************************#
#************************************************#

#############################
### COMBINING DATAFRAMES ####
#############################

s12<-subset(p12, select=c('Agency_Type','codes','Region','Population','Crimes_Against_Persons',
                          'Crimes_Against_Property','Crimes_Against_Society','Total_Offenses',
                          'Homicide_Offenses', 'Murder_and_Nonnegligent_Manslaughter',
                          'Negligent_Manslaughter','Justifiable_Homicide'))
s13<-subset(p13, select=c('Agency_Type','codes','Region','Population','Crimes_Against_Persons',
                          'Crimes_Against_Property','Crimes_Against_Society','Total_Offenses',
                          'Homicide_Offenses', 'Murder_and_Nonnegligent_Manslaughter',
                          'Negligent_Manslaughter','Justifiable_Homicide'))
s14<-subset(p14, select=c('Agency_Type','codes','Region','Population','Crimes_Against_Persons',
                          'Crimes_Against_Property','Crimes_Against_Society','Total_Offenses',
                          'Homicide_Offenses', 'Murder_and_Nonnegligent_Manslaughter',
                          'Negligent_Manslaughter','Justifiable_Homicide'))

s12$year<-2012
s13$year<-2013
s14$year<-2014


df<-rbind(s12,s13,s14)

df<-na.omit(df)


## discritize crimes against persons ##
df$discr<-EqualFreq(df$Crimes_Against_Persons,2)

## create a subset of data ##
sub = sample(nrow(df), floor(nrow(df) * 0.6))

## 1 part of the subset will be a training the other will be a test ##
train<-df[sub,]
test<-df[-sub,]

xTrain<-subset(train,select=c("Population","codes"))
yTrain<-as.factor(train$discr)

xTest<-subset(test,select=c("Population","codes"))
yTest<-as.factor(test$discr)

model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))
prop.table(table(predict(model$finalModel,xTest)$class,yTest))
table(predict(model$finalModel,xTest)$class,yTest)

