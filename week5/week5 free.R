library(tidytuesdayR)
library(tidyverse)
library(extrafont)

#get data
tuesdata <- tidytuesdayR::tt_load('2019-01-29')
states <- tuesdata$state_milk_production
products <- tuesdata$milk_products_facts
cheese<-tuesdata$clean_cheese

datamj<-select(products, year,  fluid_milk, fluid_yogurt)



#make a graph
milk<-ggplot(datamj, aes(x=year, y=fluid_milk))+
  geom_ribbon(aes(ymin = 100, ymax = fluid_milk),
              fill = "white") +
  geom_line(color = "grey", lwd = .8) +
  scale_x_continuous(breaks = seq(from=1975, to=2015,  by = 5))+
  scale_y_continuous(breaks = seq(from=100, to=250,  by = 25))+
  labs(title = "THE FALL OF MILK",
       subtitle = "Decrease of milk consumption in the US (lbs per person)",
       caption = "Data: United States Department of Agriculture --- #TidyTuesday week 5 --- Stella Bērziņa")+
  theme_bw()+
  theme(panel.background = element_rect(fill = "#173045",color = NA),
        plot.background = element_rect(fill = "#173045",color = NA),
        plot.title = element_text(family="Segoe Print", face = "bold",color="white", size=25, hjust=0.5),
        plot.caption = element_text(color="white", vjust = -10),
        plot.subtitle = element_text(color = "white", hjust = 0.5, size=10, family="Segoe Print"),
        plot.margin=unit(c(1,1,1,1),"cm"),
        
        #axis
        axis.title.x=element_blank(),
        axis.title.y =element_blank(),
        axis.text.x = element_text(angle = 90, vjust =0.5, color="white"),
        axis.text.y = element_text(color="white"),
        
        #remove all grid bits
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank()
  )

milk

#save
ggsave("milk.jpg",
width = 14,
height = 20,
units = c("cm")
)

