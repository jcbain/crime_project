setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_2/')

df13<-read.csv('output_data/new_2013.csv')

# histograms of super-types of crime
hist(df13$Crimes_Against_Persons/df13$Population1)
hist(df13$Crimes_Against_Property/df13$Population1)
hist(df13$Crimes_Against_Society/df13$Population1)

# transform counts into proportions (each offense/total population per 100,000 people)
props<-df13[, 7:ncol(df13)]/(df13$Population1 * 100000) # first find the proportion for each crime per 100,000
prop13<-cbind(df13[,1:6],props)

plot(prop13$Population1,prop13$Simple_Assault)

