
#Run through example from website
#https://medium.com/optima-blog/create-basic-sunburst-graphs-with-ggplot2-7d7484d92c61

library(ggplot2) # For awesome viz
library(dplyr) # For awesome data wrangling
library(scales) # Utilities for scales and formatting
library(viridis)

#define variables
shore1 <- 500000
shore2<- 250000

boat1 <- 0 
boat2<- 0
  
sumField <- (shore1 + shore2 + boat1 + boat2)

sumWQ <- 250000

ELISA <- 600000
qpcPrep <-36000
qpcAnalysis <-200000
micros<-40000

sumCA <- (ELISA + qpcPrep + qpcAnalysis + micros)

a <- tibble(shore1, shore2, boat1, boat2, sumWQ, ELISA, qpcPrep, qpcAnalysis, micros) %>%
  gather() %>% 
  rename("cost" = "value")
  
a$key <- fct_relevel(a$key, levels = c("shore1", "shore2", "boat1", "boat2", "sumWQ", "ELISA", "qpcPrep", "qpcAnalysis", "micros"))

b <- tibble(sumField , sumWQ , sumCA) %>% 
  gather()%>% 
  rename("cost" = "value")

b$key <-fct_relevel(b$key, level = c("sumField", "sumWQ", "sumCA"))

rm(shore1, shore2, boat1, boat2, sumField, sumWQ, ELISA, qpcPrep, qpcAnalysis, micros, sumCA)

#FIRST LEVEL INSIDE THE SUNBURST

myFirstLevel = a %>% summarize(total_cost=sum(cost))
sum_total_cost = sum(a$cost)

sunburst_1 = ggplot() +
  geom_bar(data = myFirstLevel, aes(x=1, y=total_cost), fill='darkgrey', stat='identity') +
  geom_text(data = myFirstLevel, aes(x=1, y=sum_total_cost/2, label=paste('Total Field Cost = $\n',
            sum_total_cost)), color='white')

sunburst_1

#SECOND LEVEL INSIDE THE SUNBURST

#Visualize as a stacked bar

sunburst_1 +
  geom_bar(data=a,
           aes(x=2, y=cost, fill=fct_rev(key)),
           color='white', position='stack', stat='identity', size=0.6) + 
  geom_text(data=a, aes(label=paste(key, cost), x=2, y=cost), position='stack')

#First, function to rotate text to a readable angle

compute_angle = function(perc){
  angle = -1
  
  if(perc < 0.5) # 1st half [90, -90]
    angle = (180 - (perc/0.5) * 180) - 90
  else # 2nd half [90, -90]
    angle = (90 - ((perc - 0.5)/0.5) * 180)
  
  return(angle)
}

#define colors
scales::viridis_pal()(9)

myColors <- c(shore1= "#440154FF", 
              shore2 = "#472D7BFF",   
              boat1 = "#3B528BFF",
              boat2 = "#2C728EFF",
              sumWQ = "#21908CFF", 
              ELISA = "#27AD81FF", 
              qpcPrep = "#5DC863FF",
              qpcAnalysis = "#AADC32FF",
              micros = "#FDE725FF",
              sumField = "#472D7BFF", 
              sumWQ = "#21908CFF", 
              sumCA = "#85D54AFF")

secondLevel = a %>% 
  mutate(running=cumsum(cost), pos = running - cost/2) %>%
  group_by(1:n()) %>%
  mutate(angle=compute_angle((running - cost/2) / sum_total_cost))

sunburst_2 = sunburst_1 +
  geom_bar(data=secondLevel, aes(x=2, y=cost, fill=fct_rev(key)),
           color='white', position='stack', stat = 'identity', size = 0.6) +
  geom_text(data=secondLevel,aes(label=paste(key),
                x=2, y=pos, angle=angle))+
  #scale_y_continuous(labels=comma) +
  #scale_fill_continuous(low='white', high='darkred') +
  #scale_fill_viridis(discrete = TRUE)+
  scale_fill_manual(values = myColors)+
  coord_polar('y') + theme_minimal()

sunburst_2

#Third LEVEL INSIDE THE SUNBURST

#Visualize as a stacked bar

sunburst_1 +
  geom_bar(data=a,
           aes(x=2, y=cost, fill=fct_rev(key)),
           color='white', position='stack', stat='identity', size=0.6) + 
  geom_text(data=a, aes(label=paste(key, cost), x=2, y=cost), position='stack')+
  
  geom_bar(data=b,
           aes(x=3, y=cost, fill=fct_rev(key)),
           color = 'white', position='stack', stat='identity', size=0.6) + 
  geom_text(data=b, aes(label=paste(key, cost), x=3, y=cost), position='stack')

#Function to rotate text to a readable angle should be run above

ThirdLevel = b %>% 
  mutate(running=cumsum(cost), pos = running - cost/2) %>%
  group_by(1:n()) %>%
  mutate(angle=compute_angle((running - cost/2) / sum_total_cost))

sunburst_3 =
  sunburst_2 +
  geom_bar(data=ThirdLevel, aes(x=3, y=cost, fill=fct_rev(key)),
           color='white', position='stack', stat = 'identity', size = 0.6) +
  geom_text(data=ThirdLevel,aes(label=paste(key),
                                 x=3, y=pos, angle=angle))

sunburst_3



