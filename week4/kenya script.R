library(tidyverse)
library(rKenyaCensus)
data("DataCatalogue")
phonedata<-V4_T2.32

#het the values I need from the df
total<-phonedata$Total[1]
phoneusers<-phonedata$MPO_Total[1]

#create df for the plot
value<-c(total,phoneusers)
group<-c("Doesn't own a phone", "Phone users")
data<-data.frame(group, value)

#colours
library(inauguration)
inauguration("inauguration_2021")

#fonts
library(extrafont)
fonts()

#make plot
fig1<-ggplot(data, aes(x="", y=value, fill=group), explode=1) +
  geom_bar(stat="identity", size=5, color=inauguration("inauguration_2021")[2])+
  coord_polar("y", start=0)+
  scale_fill_manual(values =inauguration("inauguration_2021")[3:4])+
  geom_text(aes(label = group), position = position_stack(vjust = 0.5), color="white", size=6, family="Cooper Black")+
  labs(title="PHONE USERS IN KENYA",
       caption="Data:rKenyaCensus | Graphic:Stella Bērziņa")+
  theme_void()+
  theme(plot.background = element_rect(fill = inauguration("inauguration_2021")[2]),
        plot.title = element_text(family="Bookman Old Style", face = "bold",color="white", size=25, hjust = 0.5, vjust = -4),
        plot.caption = element_text(color="white"),
        legend.position = "none")

  
  
fig1
ggsave("kenya.png", width=20, height = 20, units="cm")

      
