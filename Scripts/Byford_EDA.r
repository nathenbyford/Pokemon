## Nathen Byford
## Pokemon

library("tidyverse")

theme_set(theme_bw() + 
  theme(
    panel.grid.minor.x = element_blank(), 
    axis.title.y = element_text(angle = 0, vjust = .5)
  )
)

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

pokemon_new |> 
  ggplot(aes(reorder(primary, speed), speed)) +
  geom_boxplot() +
  labs(x = "Primary Type", y = "Speed")

pokemon_new |> 
  ggplot(aes(reorder(primary, speed), speed)) +
  geom_jitter(width = 0.25) +
  labs(x = "Primary Type", y = "Speed")

pokemon_new |> 
  ggplot(aes(reorder(primary, speed), speed)) +
  geom_bin2d() +
  scale_fill_viridis_c() +
  labs(x = "Primary Type", y = "Speed") +
  theme(panel.grid.major.x = element_blank()) 


