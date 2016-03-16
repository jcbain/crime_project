setwd(dir='Desktop/Spring 2016/CS 7001/Project_1/part_2/')

df13<-read.csv('output_data/new_2013.csv')

# histograms of super-types of crime
hist(df13$Crimes_Against_Persons/df13$Total_Offenses)
hist(df13$Crimes_Against_Property/df13$Total_Offenses)
hist(df13$Crimes_Against_Society/df13$Total_Offenses)

# transform counts into proportions (each offense/total offense)
prop13<-df13[, 7:ncol(df13)]/df13$Total_Offenses
