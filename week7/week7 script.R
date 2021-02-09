#made by:Stella Bērziņa, 09.02.21


library(tidyverse)
library(extrafont)
library(cowplot)
library(treemapify)


#get data
lifetime_earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/lifetime_earn.csv')
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')

#lifetime earning data
le<-mutate(lifetime_earn,millions=lifetime_earn/1000000)%>%
   rename(Race=race)
le$Race <- recode(le$Race , "Hispanic any race" = "Hispanic") #rename variable

#lifetime earning figure
lefig<-ggplot(le, aes(fill=Race, y=millions, x=gender)) + 
      geom_bar(position="dodge", stat="identity")+
      labs(title="LIFETIME EARNINGS", y="Millions $\n")+
      scale_fill_manual(name="Race", values=c("#39311d", "#7e7474", "#d4d4dc"))+
      scale_y_continuous(breaks = seq(from=0, to=3,  by = 0.5))+
      theme_bw()+
      theme(panel.grid.major.y = element_line(color="#7e7474"),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.border = element_blank(),
            
            axis.title.x=element_blank(),
            axis.text.x = element_text(vjust =0.5, size=10, color="black", family = "Consolas"),
            axis.text.y = element_text(color="black", family = "Consolas", size=9),
            axis.title.y = element_text( size=10, color="black", family = "Consolas"),
            
            
            panel.background = element_rect(fill = "#feda6a",color = NA),
            plot.background = element_rect(fill = "#feda6a",color = NA),
            plot.title = element_text(family="Consolas", face = "bold",color="black", size=12, hjust=0.5),
            plot.subtitle = element_text(color = "white", hjust = 0.7, size=12, family="Consolas"),
            
            plot.margin = unit(c(1,0.5,0.5,0.5),"cm"),
            
            legend.position = "none"
            #legend.position = "bottom",
            #legend.background=element_rect(fill = "#feda6a",color = NA),
            #legend.text = element_text(family="Consolas"),
            #legend.title = element_blank(),
            #legend.key = element_rect(colour = "#feda6a", size=1),#makes the annoying white border of legend key blend in with the background
            #I SAVED THIS LEGEND AS A PHOTO AND PHYSICALLY ADDED IT OVER THE FINAL PLOT
      )

lefig

#home ownership data
home_owner<-rename(home_owner, Race=race)
home_owner<-mutate(home_owner,percentho=home_owner_pct*100)


#home ownership figure
homefig<-ggplot(home_owner, aes(x=year, y=percentho, group=Race)) +
   geom_line(aes(color=Race),size=2)+
   labs(y="% Home Owners\n",x="Year", title = "HOME OWNERSHIP\n",
        caption = "\nData: Urban Institute | Viz: Stella Berzina")+
   scale_color_manual(values=c("#39311d", "#7e7474", "#d4d4dc"))+
   theme_bw()+
   theme(panel.grid.major.y = element_line(color="#7e7474"),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.x = element_line(color="#7e7474"),
         panel.border = element_blank(),
         
         
         axis.title.x=element_text(vjust =0.5, size=10, color="black", family = "Consolas"),
         axis.text.x = element_text(vjust =0.5, size=10, color="black", family = "Consolas"),
         axis.text.y = element_text(color="black", family = "Consolas", size=11),
         axis.title.y = element_text(vjust =0.5, size=10, color="black", family = "Consolas"),
         
         panel.background = element_rect(fill = "#feda6a",color = NA),
         plot.background = element_rect(fill = "#feda6a",color = NA),
         plot.title = element_text(family="Consolas", face = "bold",color="black", size=12, hjust=0.5),
         plot.caption = element_text(color="white", family="Consolas", size=9, hjust=0.5),
         plot.subtitle = element_text(color = "white", hjust = 0.7, size=11, family="Consolas"),
         
         
         legend.position = "none",
         #legend.key=element_rect(fill = 'transparent', color= '#feda6a'),
         
         
         plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")
         )

   
homefig

#retirement data
ret16<-filter(retirement, year=="2016")%>%
   mutate(money=retirement/1000)

#retirement figure
retfig<-ggplot(ret16, aes(area=money, fill=race, label=money))+
   geom_treemap(size=3, color="#feda6a")+
   geom_treemap_text(family="Consolas",color="white", size=12)+
   labs(title = "RETIREMENT SAVINGS",
        subtitle= "Thousands of $ in 2016")+
   scale_fill_manual(name="Race", values=c("#39311d", "#7e7474", "#d4d4dc"))+
   theme(
      plot.title = element_text(family="Consolas", face = "bold",color="black", size=12, hjust=0.5),
      plot.subtitle = element_text(color = "black", hjust = 0.5, size=11, family="Consolas"),
      panel.background = element_rect(fill = "#feda6a",color = NA),
      plot.background = element_rect(fill = "#feda6a",color = NA),
      legend.position = "none",
      plot.margin=unit(c(0.2,1,1,1),"cm")
   )
retfig


#make empty figure to put text in
#fake dataset
a<-c(1,2,3,4,5,6,7,8)
b<-c(1,2,3,4,5,6,7,8)
df<-data.frame(a,b)
#title figure
emptyfig<-ggplot(df, aes(a,b))+
   labs(title="\n\nRACIAL WEALTH\nINEQUALITY\nIN THE US")+
   theme_void()+
   theme(panel.background = element_rect(fill = "#feda6a",color = NA),
         plot.background = element_rect(fill = "#feda6a",color = NA),
         plot.title = element_text(family="Consolas", face = "bold",color="black", size=24, hjust = 0.9),
         plot.subtitle = element_text(color = "black", hjust = 1, size=15, family="Consolas"),
         
      
      
   )

emptyfig


#add together all plots
plot_grid(emptyfig,lefig, retfig, homefig, ncol=2)

#save
ggsave("week7.jpg",
       width = 20,
       height = 16,
       units = c("cm")
)
