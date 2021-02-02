library(tidyverse)
library(tidytuesdayR)
library(extrafont)
#get data
#tuesdata <- tidytuesdayR::tt_load('2021-02-02')
fhs<-tuesdata$female_hs_students
fbs<-tuesdata$female_bach_students

#select data I need (decades from female highschool data)
femhs<-fhs[c(1:6, 12, 22),1:2]
fembs<-fbs[c(1:6,12, 22 ), 1:2]
femdata<-merge(femhs, fembs, by="Total")
names(femdata)[1]<-"year"
names(femdata)[2] <- "highschool"
names(femdata)[3] <- "collage"

#make column that will fill up to 100%
datai<-mutate(femdata, no=100-highschool )
#highschool-collage so areas dont duplicate
datai<-mutate(datai, hmc=highschool-collage)

#I rename them again because I want more elaborate legend
names(datai)[5] <- "only highschool"
names(datai)[3] <- "highschool and collage"
names(datai)[4] <- "no highschool"
datai<-datai[, c(1,2,4,5,3)]

#stack columns to make a stacked plot
datai<- data.frame(datai[1], stack(datai[3:ncol(datai)]))



#make a plotty plot plot
fig<-ggplot(datai, aes(y=values, x=year, fill=ind)) + 
  geom_bar(stat="identity")+
  labs(title = "Women's education",
       subtitle = "The percentage of US female population with highschool and bachelors degrees",
       y="Population (%)",
       caption = "Data: Data.World, Viz : Stella Berzina")+
  scale_x_continuous(breaks = seq(from=1940, to=2010,  by = 10))+
  scale_fill_manual(values=c("#ffb26b", "#ffd56b", "#939b62"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        
        axis.title.x=element_blank(),
        axis.text.x = element_text(vjust =0.5, size=10, color="black", family = "Corbel"),
        axis.text.y = element_text(color="black", family = "Corbel", size=11),
        axis.title.y = element_text(size=12, family = "Corbel"),
        
        
        panel.background = element_rect(fill = "#ff7b54",color = NA),
        plot.background = element_rect(fill = "#ff7b54",color = NA),
        plot.title = element_text(family="Corbel", face = "bold",color="white", size=25, hjust=0.5),
        plot.caption = element_text(color="white", vjust=-20, family="Corbel"),
        plot.subtitle = element_text(color = "white", hjust = 0.7, size=12, family="Corbel"),
        
        plot.margin=unit(c(1,1,2,1),"cm"),
        
        legend.position="bottom",
        legend.background=element_rect(fill = "#ff7b54",color = NA),
        legend.title = element_blank()
)

fig



ggsave("week6.jpg",
         width = 15,
         height = 17,
         units = c("cm")
  )
