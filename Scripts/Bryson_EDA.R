#Connor Bryson
#Pokemon

library(tidyverse)
library(patchwork)
library(viridisLite)


pokemon <- read.csv(".\\data\\pokemon.csv")


theme_set(theme_bw() + 
            theme(
              panel.grid.minor.x = element_blank(), 
              axis.title.y = element_text(angle = 0, vjust = .5)
            )
)

#Weight vs Speed (organized by Generation)- BEST PLOT
ggplot(pokemon, aes(x =weight, y =speed, color= gen_introduced)) +
  geom_point()+
  labs(title="Weight vs Speed", x= "Weight", y="Speed")+
  xlim(c(0, max(pokemon$weight))) + 
  ylim(c(0, max(pokemon$speed)))+
  viridis::scale_color_viridis()+
  theme(plot.title = element_text(hjust = 0.5))
 

#Speed vs Height
ggplot(pokemon, aes(x =speed, y =height, color= gen_introduced)) +
  geom_point()+
  labs(title="Speed vs Height", x= "Speed", y="Height")+
  xlim(c(0, max(pokemon$speed))) + 
  ylim(c(0, max(pokemon$height)))+
  viridis::scale_color_viridis()+
  theme(plot.title = element_text(hjust = 0.5))
  




#Speed vs Capture Rate - It seems as the pokemon get faster, 
ggplot(pokemon, aes(x =speed, y =capture_rate,  color= gen_introduced)) +
  geom_point()+
  labs(title="Speed vs Capture Rate", x= "Speed", y="Capture Rate")+
  xlim(c(0, max(pokemon$speed))) + 
  ylim(c(0, max(pokemon$capture_rate)))+
  geom_smooth(se=FALSE)+
  viridis::scale_color_viridis()+
  theme(plot.title = element_text(hjust = 0.5))




#Speed vs Attack - Little bit of correlation
ggplot(pokemon, aes(x =speed, y =attack,  color= gen_introduced)) +
  geom_point()+
  labs(title="Speed vs Attack", x= "Speed", y="Attack")+
  xlim(c(0, max(pokemon$speed))) + 
  ylim(c(0, max(pokemon$attack)))+
  geom_smooth()+
  viridis::scale_color_viridis()+
  theme(plot.title = element_text(hjust = 0.5))

#Speed vs Defense - No relationship really
ggplot(pokemon, aes(x =speed, y =defense,  color= gen_introduced)) +
  geom_point()+
  labs(title="Speed vs Defense", x= "Speed", y="Defense")+
  xlim(c(0, max(pokemon$speed))) + 
  ylim(c(0, max(pokemon$defense)))+
  geom_smooth()+
  viridis::scale_color_viridis()+
  theme(plot.title = element_text(hjust = 0.5))




















#Speed vs Special Attack 
ggplot(pokemon, aes(x =speed, y =special_attack)) +
  geom_point()+
  xlim(c(0, max(pokemon$speed))) + 
  ylim(c(0, max(pokemon$special_attack)))+
  geom_smooth()

#Speed vs Special Defense
ggplot(pokemon, aes(x =speed, y =special_defense)) +
  geom_point()+
  xlim(c(0, max(pokemon$speed))) + 
  ylim(c(0, max(pokemon$special_defense)))+
  geom_smooth()









#Speed vs Pokedex_number - None
ggplot(pokemon, aes(x =speed, y =pokedex_number)) +
  geom_point()+
  xlim(c(0, max(pokemon$speed))) + 
  ylim(c(0, max(pokemon$pokedex_number)))+
  geom_smooth()

#Speed vs HP - Little positive
ggplot(pokemon, aes(x =speed, y =hp)) +
  geom_point()+
  xlim(c(0, max(pokemon$speed))) + 
  ylim(c(0, max(pokemon$hp)))+
  geom_smooth()

#Speed vs Base Happiness -  
ggplot(pokemon, aes(x =speed, y =base_happiness)) +
  geom_point()+
  xlim(c(0, max(pokemon$speed))) + 
  ylim(c(0, max(pokemon$base_happiness)))+
  geom_smooth()
 











#Speed vs Egg Cycles 
speedvseggcycles<- ggplot(pokemon, aes(x =speed, y =egg_cycles)) +
  geom_point()+
  xlim(c(0, max(pokemon$speed))) + 
  ylim(c(0, max(pokemon$egg_cycles)))+
  geom_smooth()
  
#Speed vs Female Rate
speedvsfemalerate<- ggplot(pokemon, aes(x =speed, y =female_rate)) +
  geom_point()+
  xlim(c(0, max(pokemon$speed))) + 
  ylim(c(0, max(pokemon$female_rate)))







#Separating Pokemon by Type and BMI
pokemon_new <-  pokemon %>%
  separate(typing, into = c("primary", "secondary")) %>%
  mutate(BMI = (weight/10)/((height/10)^2)) %>%
  filter(BMI <= 50000)


#Speed vs Shape
ggplot(pokemon_new, aes(x =shape, y =speed)) +
  geom_point() + 
  ylim(c(0, max(pokemon_new$speed)))
  













