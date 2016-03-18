setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_2/')

library(ggplot2)
library(plotly)


df13<-read.csv('output_data/new_2013.csv')

#create state codes
df13$codes<-as.factor(state.abb[match(df13$State,state.name)] )

# histograms of super-types of crime
hist(df13$Crimes_Against_Persons/df13$Population)
hist(df13$Crimes_Against_Property/df13$Population)
hist(df13$Crimes_Against_Society/df13$Population)

# transform counts into proportions (each offense/total population per 100,000 people)
props<-df13[, 7:ncol(df13)-2]/(df13$Population) # first find the proportion for each crime
prop13<-na.omit(cbind(df13[,c(1:6,69:70)],props))

# play around with some graphs to explore the data
plot_ly(prop13, x = Population, y = Crimes_Against_Persons, text = paste("Agency: ", Agency_Name),
        mode = "markers", color = Agency_Type)

plot_ly(prop13, x = Crimes_Against_Persons, color = Region, type = "box")
plot_ly(prop13, x = Region,y = Crimes_Against_Persons, color = State, type = "box") %>%
  layout(boxmode = "group")

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

plot_ly(df13, z = Population,locations = codes, type = 'choropleth',
        locationmode = 'USA-states', color = Population,colors = 'PuOr',
        marker = list(line = l),colorbar = list(title = "Millions USD")) %>%
  layout(title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)', geo = g)



