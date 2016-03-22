setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_2/')

library(ggplot2)
library(plotly)
library(reshape2)

#######################
## READ IN THE FILES ##
#######################

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

proStates14<-states14[,3:ncol(states14)]/states14$Population
prost14<-cbind(states14[,1:2],proStates14)


## transform counts into proportions (each offense/total population per 100,000 people)
props12<-df12[6:64]/df12$Population 
prop12<-cbind(df12[,c(1:5,ncol(df12)-1,ncol(df12))],props12)

props13<-df13[, 6:68]/(df13$Population) 
prop13<-cbind(df13[,c(1:5,69:70)],props13)

props14<-df14[, 6:68]/(df14$Population) 
prop14<-cbind(df14[,c(1:5,69:70)],props14)


## write proportion data frames to csv ##
write.csv(prop12,file = 'output_data/prop12.csv',row.names=FALSE)
write.csv(prop13,file = 'output_data/prop13.csv',row.names=FALSE)
write.csv(prop14,file = 'output_data/prop14.csv',row.names=FALSE)

## some simple models ##
model<-lm(Population ~Prostitution,data = prop13)

# play around with some graphs to explore the data
plot_ly(prop12, x = Crimes_Against_Persons, y = Population, text = paste("Agency: ", Agency_Name),
        mode = "markers", color = codes)

plot_ly(prop13, x = Total_Offenses, color = Region, type = "box")
plot_ly(prop13, x = Region,y = Homicide_Offenses, color = State, type = "box") %>%
  layout(boxmode = "group")

##########################
####### MAPS!!!!! ########
##########################

# give state boundaries a black border
l <- list(color = toRGB("black"), width = .5)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('gray')
)

plot_ly(prost13, z = Total_Offenses,locations = codes, text=paste0('<br>Population: ', Population), type = 'choropleth',
        locationmode = 'USA-states', color = Total_Offenses,colors = 'Reds',
        marker = list(line = l),colorbar = list(title = "Total Offenses Rate")) %>%
  layout(title = '2013 Crime Rates per Capita<br>(Hover for breakdown)', geo = g)




