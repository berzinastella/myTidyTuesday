library(tidyverse)
library(tidytuesdayR)
library(extrafont)


tuesdata <- tidytuesdayR::tt_load(2021, week = 9)
earn<-tuesdata$earn

#get the data i need- avg weekly earning in 2010 and 2020 in 3 races and 2 genders
e<-earn%>%
      filter(age=="16 years and over" & ethnic_origin== "All Origins")%>%
      select(-ethnic_origin, -age, -n_persons)%>%
      filter(sex!="Both Sexes" & year %in% c("2010", "2020")& race!="All Races")%>% 
      group_by(sex, race, year)%>%
      summarise(income=mean(median_weekly_earn))%>%
      ungroup()

#final datasets to plot
y2010<-filter(e, year=="2010")
y2020<-filter(e, year=="2020")


#adding location of the bubbles
y2010m<-y2010%>%
      mutate(a=c(1, 2, 3, 4, 5,6))%>% #this is so the colors would be different for all bubbles
      mutate(x=c(2, 4, 4, 2.5, 2.7, 0))%>%
      mutate(y=c(1, 1.5, 0.7, 2, 0.5, 1))

y2020m<-y2020%>%
   mutate(a=c(1, 2, 3, 4, 5,6))%>%
   mutate(x=c(12, 14, 14, 12.5, 12.7, 10))%>%
   mutate(y=c(1, 1.5, 0.7, 2, 0.5, 1))
   
total<-rbind(y2010m, y2020m)%>% #merged vertically
   mutate(round=floor(income))
#colors I will use 
my_colors<- c("#ff7143", "#e854a0", "#cddc39", "#ffcd83", "#e9afdf", "#c2ffdb")
 

#figure
figt<- ggplot(total,aes(x=x, y=y,size=income, label=round))+
   geom_point(aes(color=as.factor(a)), alpha=0.5)+
   geom_text(size=3, color="white", vjust=0.5)+
   #scale_color_manual(values = c("#7bbcd5", "#d0e2af", "#e89c81", "#6e7cb9", "#f5db99", "#d2848d"))+
   scale_color_manual(values = c("#ff7143", "#e854a0", "#cddc39", "#ffcd83", "#e9afdf", "#8bdfa6"))+
   scale_size(range = c(7, 40))+
   expand_limits(y = c(-0.5, 3.5), x=c(-1,16))+
   labs(title = "WEEKLY INCOME IN $$$",
      subtitle = "data: BLS | viz: Stella Berzina")+
   theme_void()+
   theme(
      legend.position = "none",
      
      plot.title = element_text(family="Corbel", face = "bold", size=25, vjust= -10, hjust=0.5, color = "grey"),
      plot.subtitle = element_text(vjust=-8, hjust = 0.5, size=9, family="Corbel", color = "grey"),
      plot.margin = unit(c(0,0,3,0),"cm"),
   )+
   annotate("text", x = 2.5, y = 0, label = "2010", colour = "grey", size=6)+
   annotate("text", x = 12.5, y = 0, label = "2020", colour = "grey", size=6)

#!!! the legend is added in canva bc I couldn't figure out how to do it here :) sorry to disappoint

figt
ggsave("t.jpg",
       width = 15,
       height = 16,
       units = c("cm"))
