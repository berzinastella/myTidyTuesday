library(tidytuesdayR)
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load(2021, week = 3)
artwork <- tuesdata$artwork
artists<-tuesdata$artists%>%
  rename(artistId=id)

#merge 2 datasets
full<-merge(artwork, artists, by="artistId")

#only female artists
femartists<-filter(full, gender=="Female")

#how many artworks each female artist has
femgroups<-group_by(femartists, artist, artistId)%>%
  summarise(sum_artworks=n())%>%
  ungroup()

#make names look pretty
splits <- str_split_fixed(femgroups$artist, ", ", 2)
femgroups$artist <- paste(splits[,2], splits[,1], sep = ' ')


#treemap
install.packages("treemap")
install.packages("treemapify")
library(treemap)
library(treemapify)

#note to self- don't use (treemap) in the future- make it from ggplot

group <- femgroups$artist
value <- femgroups$sum_artworks
data <- data.frame(group,value)

png(filename="tree5001.png",width=800, height=800) #line to save high quality jps. together with the dev.off() 
treemap(data,
       index="group",
       vSize="value",
       type="index",
        
      #title things 
      title="Which woman has the most artworks in the Tate?",
      fontsize.title=27,
      fontfamily.title = c("mono"),
      
        
      #labels
      inflate.labels=F,
      fontsize.labels = c(12),
      fontcolor.labels=c("black"),
      fontfamily.labels = c("mono"),
      force.print.labels=T,
      
      #colours 
      palette = "Set3", 
      border.col=c("white"),
      border.lwds=c(3)
    
)
dev.off()
