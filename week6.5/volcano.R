#just for fun vizz
#Stella Bērziņa
#TidyTuesday May 11, 2020 data


library(tidytuesdayR)
library(tidyverse)
library(viridis)
library(maps)
library(wesanderson)
library(extrafont)
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')

#rename messy countries (code chunk from @Juanma_MN to save time)
volcano$country <- recode(volcano$country , "United States" = "USA",
                           "United Kingdom" = "UK",
                           "DR Congo" = "Democratic Republic of the Congo",
                           "El Salvador-Guatemala"="El Salvador",
                           "Eritrea-Djibouti"="Eritrea",
                           "Ethiopia-Djibouti"="Ethiopia",
                           "Ethiopia-Kenya"="Ethiopia",
                           "Guatemala-El Salvador"="Guatemala",
                           "Mexico-Guatemala"="Mexico",
                           "North Korea-South Korea"="North Korea",
                           "Saint Kitts and Nevis"="Saint Kitts",
                           "Saint Vincent and the Grenadines"="Saint Vincent",
                           "Syria-Jordan-Saudi Arabia"="Syria",
                           "Uganda-Rwanda"="Uganda",
                           "DR Congo-Rwanda"="Democratic Republic of the Congo",
                           "Burma (Myanmar)"= "Myanmar",
                           "Japan - administered by Russia" = "Japan",
                           "Armenia-Azerbaijan"= "Armenia",
                           "Chile-Argentina"="Chile",
                           "Chile-Bolivia"="Chile",
                           "Chile-Peru"="Chile",
                           "China-North Korea"="China",
                           "Colombia-Ecuador"="Colombia")




#get the data I need
novolcanos<-group_by(volcano,country)%>%
      summarise(number=n())%>%
      ungroup() %>% 
      rename(region=country)
 
#connect wold map data to volcano data
world_map <- map_data("world")
volcano_map <- left_join(novolcanos, world_map, by="region")


#plot base and then over it data I need
base<-ggplot() + 
   geom_polygon( data=world_map, aes(x=long, y=lat, group=group), #this plots the whole world map
                    color="#374045", fill="#e7e6e1" )+
   geom_polygon( data=volcano_map, #this plots the filled countries
                    aes(x=long, y=lat, group=group, fill = number), 
                    color="black", size = 0.1)+
   
  #choose color gradient for the volcano countries 
   scale_fill_gradientn(colors=c("#374045","#ffe227", "#ff6f3c"))+
  
   labs(title="Countries of Lava",
        caption="Data: The Smithsonian Institution, Viz: Stella Berzina",
        fill="Number \nof Volcanos")+
   
   theme_void()+
   theme(
      #text altering
      plot.background = element_rect(fill = "#e7e6e1"),
      plot.title = element_text(family="Arial Black", face = "bold",color="#374045", size=25, hjust=0.6),
      plot.caption = element_text(color="#374045", hjust = 0.6, vjust=173, family="Arial", size=8),
      plot.subtitle = element_text(color = "white", hjust = 0.7, size=12, family="Corbel"),
      legend.title = element_text(color="#374045",size=9),
      plot.margin=unit(c(0.5,0.5,0,0.5),"cm"),
     
   )+
   
   #add annotations
   annotate("text", x = 149, y = 27, label = "No 1 Japan \n(104)", colour = "#374045", size=2)+
   annotate("text", x = -139, y = 41, label = "No 2 USA \n(99)", colour = "#374045", size=2)+
   annotate("text", x = 95, y = -15, label = "No 3 Indonesia \n(95)", colour = "#374045", size=2) 






base

#save the plot the size you want
ggsave("week6.5.jpg",
       width = 20,
       height = 12,
       units = c("cm")
)

