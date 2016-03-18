setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_2/')

library(ggplot2)
library(plotly)
library(reshape2)

# read in a year of the data
df13<-read.csv('output_data/new_2013.csv')

#create state codes
df13$codes<-as.factor(state.abb[match(df13$State,state.name)] )

# aggregate some data based on the sum of offenses per each state
states<-aggregate(cbind(Population, Total_Offenses,Crimes_Against_Persons,Crimes_Against_Property,Crimes_Against_Society)~codes, data=df13, sum, na.rm=TRUE)

#find proportions based off of population for each state
proSt<-states[,3:ncol(states)]/states$Population
prost<-cbind(states[,1:2],proSt)



# transform counts into proportions (each offense/total population per 100,000 people)
props<-df13[, 6:68]/(df13$Population) # first find the proportion for each crime
prop13<-na.omit(cbind(df13[,c(1:5,69:70)],props))



# play around with some graphs to explore the data
plot_ly(prop13, x = Population, y = Crimes_Against_Persons, text = paste("Agency: ", Agency_Name),
        mode = "markers", color = Agency_Type)

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

plot_ly(prost, z = Total_Offenses,locations = codes, text=paste0('<br>Population: ', Population), type = 'choropleth',
        locationmode = 'USA-states', color = Total_Offenses,colors = 'Reds',
        marker = list(line = l),colorbar = list(title = "Total Offenses Rate")) %>%
  layout(title = '2013 Crime Rates per Capita<br>(Hover for breakdown)', geo = g)


