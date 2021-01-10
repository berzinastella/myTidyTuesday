library(tidytuesdayR)
library(tidyverse)

#get tidytuesday data
tuesdata <- tidytuesdayR::tt_load('2021-01-05')

#make it nicer :)
data<-tuesdata[["transit_cost"]]

#total number of stations and summative line length by country
sumdata<-group_by(data,country)%>%
  summarise(sum_stations=sum(stations), sum_length=sum(length))%>%
  ungroup()

#convert country codes to continents 
library(countrycode)
Countries <- sumdata$country
Continents <- countrycode(Countries, origin='iso2c', destination='continent', custom_match = c('UK'='Europe'))

#add continents to the data set with total line length and station number
contdata<-sumdata
contdata$Continents <- Continents  

#take out NA 
nacontdata<-na.omit(contdata)

#make a plot
fig1<-ggplot(nacontdata, aes(x=sum_stations, y=sum_length, colour=Continents))+
  geom_point()+
  ggtitle("Transit Line Length vs Number of Stations")+
  ylim(0,450)+
  xlim(0,250)+
  labs(x="Total number of stations",
       y="Total lenght of the line (km)",
       subtitle="Summative values per country",
       caption = "Data:Transit Costs Project")+
  theme_classic()

fig1

