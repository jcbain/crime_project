setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_2/')

library(ggplot2)
library(plotly)
library(reshape2)

#######################
## READ IN THE FILES ##
#######################

## read in a year of the data ##
df14<-read.csv('output_data/new_2014.csv')


## create state codes ##
df14$codes<-as.factor(state.abb[match(df14$State,state.name)] )

## aggregate some data based on the sum of offenses per each state ##
states14<-aggregate(cbind(Population, Murder_and_Nonnegligent_Manslaughter,Crimes_Against_Persons,Negligent_Manslaughter,Homicide_Offenses)~codes, data=df14, sum, na.rm=TRUE)

## find proportions based off of population for each state ##
proStates14<-(states14[,3:ncol(states14)]/states14$Population)*100000
prost14<-cbind(states14[,1:2],proStates14)




# give state boundaries a black border
l <- list(color = toRGB("black"), width = .5)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('gray')
)

plot_ly(prost14, z = Murder_and_Nonnegligent_Manslaughter,locations = codes, text=paste0('<br>Population: ', Population), type = 'choropleth',
        locationmode = 'USA-states', color = Murder_and_Nonnegligent_Manslaughter,colors = 'Reds',
        marker = list(line = l),colorbar = list(title = "Total Murder Rate")) %>%
  layout(title = '2014 Murder Rates per 100,000 Individuals', geo = g)

plot_ly(prost14, z = Negligent_Manslaughter,locations = codes, text=paste0('<br>Population: ', Population), type = 'choropleth',
        locationmode = 'USA-states', color = Negligent_Manslaughter,colors = 'Blues',
        marker = list(line = l),colorbar = list(title = "Total Manslaughter Rate")) %>%
  layout(title = '2014 Negligent Manslaughter Rates <br> per 100,000 Individuals', geo = g)

plot_ly(prost14, z = Homicide_Offenses,locations = codes, text=paste0('<br>Population: ', Population), type = 'choropleth',
        locationmode = 'USA-states', color = Homicide_Offenses,colors = 'Purples',
        marker = list(line = l),colorbar = list(title = "Total Homicide Rate")) %>%
  layout(title = '2014 Homicide Rates <br> per 100,000 Individuals', geo = g)
