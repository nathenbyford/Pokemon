setwd("C:/Users/Connor/Desktop/CompStat4373/CompStat/Project 1 Pokemon")
library(tidyverse)



pokemon <- read.csv("pokemon.csv")

ggplot(pokemon, aes(weight, speed)) +
  geom_point()+
  xlim(c(0, max(pokemon$weight))) + 
  ylim(c(0, max(pokemon$speed))) 

