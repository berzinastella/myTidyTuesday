library(plyr)
library(dplyr)
library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(waffle)
library(cowplot)
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

#work with data
moviesdf<-movies%>%
      mutate(mygenre = str_extract(genre, "\\w+"))%>% #takes the first work of genre column
      filter(mygenre=="Animation")%>%
      select(1,3,5,6,18,35)%>% #I really don't need all columns
      filter(clean_test!="dubious")%>% #discard dubious
      filter(rated %in% c("G", "PG"))#get only the animation movies for kids

df1<-moviesdf%>%
      group_by(binary)%>%
      summarise(count=n())%>%
      ungroup()

df2<-moviesdf%>%
      group_by(clean_test)%>%
      summarise(count=n())%>%
      ungroup()

fig1<-ggplot(df2, aes(fill = clean_test, values = count)) +
      geom_waffle(color = "#faf8da", size =3, n_rows = 10, make_proportional = TRUE, flip = TRUE, show.legend = FALSE)+
      labs(
            title = "What do the kids see?",
            subtitle = "The representation of women in animated films (1988-2013)",
            caption = "data:FiveThirtyEight\nviz:Stella Berzina "
            
      )+
      scale_fill_manual(values = c( "#62d4d4","#fcc323","#245c74","#fca49c"))+
      theme_void()+
      theme(plot.margin = unit(c(1,0,0,0),"cm"),
            panel.background = element_rect(fill = "#faf8da",color = NA),
            plot.background = element_rect(fill = "#faf8da",color = NA),
            plot.title = element_text(family="Arial Rounded MT Bold",color="black", size=17, hjust=0.3, vjust=-0.5),
            plot.subtitle = element_text(family = "Arial", hjust = 0.3, size=12),
            plot.caption = element_text(colour = "black", size = 8, vjust = 1.5, hjust = 0.95)
      )

fig1

#I need to make another figure to add annotation outside the first ggplot
#fake dataset
a<-c(1,2,3,4,5,6,7,8)
b<-c(1,2,3,4,5,6,7,8)
df<-data.frame(a,b)
#text figure
emptyfig<-ggplot(df, aes(a,b))+
      xlim(1,10)+
      ylim(1,10)+
      theme_void()+
      theme(
            panel.background = element_rect(fill = "#faf8da",color = NA),
            plot.background = element_rect(fill = "#faf8da",color = NA))+
      annotate("text", x = 10, y = 8,  family="Arial Rounded MT Bold", label = "At least 2 women who\ntalk to each other about\nthings other than men", colour = "#fca49c", size=4, hjust=1)+
      annotate("text", x = 10, y = 6, family="Arial Rounded MT Bold", label = "Less than two women\nin the film", colour = "#245c74", size=4,hjust=1)+
      annotate("text", x = 10, y = 4, family="Arial Rounded MT Bold",  label = "Women don't talk\nto each other", colour = "#fcc323", size=4, hjust=1)+
      annotate("text", x = 10, y = 2, family="Arial Rounded MT Bold",  label = "Women talk only\nabout men", colour = "#62d4d4", size=4, hjust=1)+
      annotate("text", x = 10, y = 3.5, family="Arial",  label = "e.g. Rio, Up, Finding Nemo", colour = "#fcc323", size=3, hjust=1)+
      annotate("text", x = 10, y = 1.5, family="Arial",  label = "e.g. Tarzan, Cars 2", colour = "#62d4d4", size=3, hjust=1)+
      annotate("text", x = 10, y = 5.5, family="Arial", label = "e.g. Ratatouille, Madagascar, Ice Age", colour = "#245c74", size=3,hjust=1)+
      annotate("text", x = 10, y = 7,  family="Arial", label = "e.g. Tangled, The Smurfs,\nDespicable Me", colour = "#fca49c", size=3, hjust=1)
emptyfig


plot_grid(emptyfig, fig1, rel_widths = c(1, 3))#combines both figure in 1:3 ratio

ggsave("Bechdel.png",
      width = 22,
      height = 14,
      unit = "cm",
      dpi = 320)
