## Nathen Byford
## Pokemon

library(tidyverse); theme_set(theme_bw())

pokemon <- read_csv(".\\data\\pokemon.csv")

dim(pokemon)
colnames(pokemon)

ggplot(pokemon, aes(primary_color, hp)) +
  geom_boxplot()

ggplot(pokemon, aes(shape, speed)) +
  geom_jitter(width = 0.25, color = "steelblue", alpha = .5)

ggplot(pokemon, aes(weight, speed)) +
  geom_point()

ggplot(pokemon, aes(typing, speed)) +
  geom_boxplot()

ggplot(pokemon, aes(typing, speed)) +
  geom_violin()

# slpit type into primary and secondary

pokemon_new <- pokemon %>% separate(typing, into = c("primary", "secondary"))

unique(pokemon_new$primary)
unique(pokemon_new$secondary)

ggplot(pokemon_new, aes(primary, speed)) +
  geom_boxplot()

ggplot(pokemon_new, aes(primary, weight)) +
  geom_boxplot()

pokemon_new |> 
  ggplot(aes(primary, weight)) +
    geom_boxplot() 
