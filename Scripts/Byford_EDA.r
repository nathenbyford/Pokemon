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

pokemon_new <-  pokemon %>%
  separate(typing, into = c("primary", "secondary")) %>%
  mutate(BMI = (weight/10)/((height/10)^2)) %>%
  filter(BMI <= 50000)

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


## Looking at Base happyness

  # Potential points of interest; Type, baby_pokemon, legendary, primary_color,
  #mythical, BMI, shape, female rate

pokemon_new |> ggplot(aes(primary, base_happiness)) +
  geom_jitter(alpha = 0.2) 

pokemon_new |> ggplot(aes(primary, base_happiness)) +
  geom_bin2d()

pokemon_new |> ggplot(aes(baby_pokemon, base_happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(baby_pokemon, base_happiness)) +
  geom_violin()

pokemon_new |> ggplot(aes(legendary, base_happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(legendary, base_happiness)) +
  geom_violin(aes(fill = legendary)) +
  geom_boxplot(width = .03)

pokemon_new |> ggplot(aes(primary_color, base_happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(primary_color, base_happiness)) +
  geom_boxplot()

pokemon_new |> ggplot(aes(mythical, base_happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(mythical, base_happiness)) +
  geom_boxplot()

pokemon_new |> ggplot(aes(BMI, base_happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(shape, base_happiness)) +
  geom_jitter(alpha = .1)

pokemon_new |> ggplot(aes(shape, base_happiness)) +
  geom_bin2d()

pokemon_new |> ggplot(aes(shape, base_happiness)) +
  geom_boxplot()

pokemon_new |> ggplot(aes(female_rate, base_happiness)) +
  geom_jitter(alpha = .2)

pokemon_new |> ggplot(aes(as.factor(female_rate), base_happiness)) +
  geom_boxplot()

pokemon_new |> ggplot(aes(as.factor(gen_introduced), base_happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(as.factor(gen_introduced), base_happiness)) +
  geom_boxplot()

pokemon_new |> ggplot(aes(genderless, base_happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(genderless, base_happiness)) +
  geom_boxplot()
