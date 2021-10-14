## Nathen Byford
## Pokemon

library(patchwork)
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

factorize <- function(x) {
  factor(case_when(
            x == (0) ~ "Very unhappy",
            x == (35) ~ "Moderatly unhappy",
            x == (50) ~ "Slightly happy",
            x == (70) ~ "Neutral",
            x == (90) ~ "Slightly unhappy",
            x == (100) ~ "Moderatly happy",
            x == (140) ~ "Very happy"
            ), 
         levels = c(
           "Very unhappy", 
           "Moderatly unhappy", 
           "Slightly happy",
           "Neutral",
           "Slightly unhappy",
           "Moderatly happy",
           "Very happy"
           ), 
         ordered = TRUE
         )
}

pokemon_new <-  pokemon %>%
  separate(typing, into = c("primary", "secondary")) %>%
  mutate(BMI = (weight/10)/((height/10)^2)) %>%
  filter(BMI <= 50000) %>%
  mutate(happiness = factorize(factor(base_happiness, ordered = TRUE)))

unique(pokemon_new$primary)
unique(pokemon_new$secondary)


# graphs

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

pokemon_new |> ggplot(aes(primary, happiness)) +
  geom_jitter(alpha = 0.2) 

pokemon_new |> ggplot(aes(primary, happiness)) +
  geom_bin2d()

pokemon_new |> ggplot(aes(baby_pokemon, happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(baby_pokemon, happiness)) +
  geom_violin()

pokemon_new |> ggplot(aes(legendary, happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(legendary, base_happiness)) +
  geom_violin(aes(fill = legendary)) +
  geom_boxplot(width = .03)

pokemon_new |> ggplot(aes(primary_color, happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(primary_color, happiness)) +
  geom_boxplot()

pokemon_new |> ggplot(aes(mythical, happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(mythical, base_happiness)) +
  geom_boxplot()

pokemon_new |> ggplot(aes(BMI, happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(shape, happiness)) +
  geom_jitter(alpha = .1)

pokemon_new |> ggplot(aes(shape, base_happiness)) +
  geom_jitter(width = .2)

pokemon_new |> ggplot(aes(shape, base_happiness)) +
  geom_bin2d()

pokemon_new |> ggplot(aes(shape, base_happiness)) +
  geom_boxplot()

pokemon_new |> ggplot(aes(female_rate, base_happiness)) +
  geom_jitter(alpha = .2)

pokemon_new |> ggplot(aes(as.factor(female_rate), happiness)) +
  geom_boxplot()

pokemon_new |> ggplot(aes(as.factor(gen_introduced), happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(as.factor(gen_introduced), base_happiness)) +
  geom_violin(aes(fill = as.factor(gen_introduced))) +
  scale_fill_viridis_d() +
  theme(legend.position = "None")

pokemon_new |> ggplot(aes(genderless, happiness)) +
  geom_jitter()

pokemon_new |> ggplot(aes(genderless, base_happiness)) +
  geom_boxplot()


# final plots

pokemon_new |> ggplot(aes(as.factor(female_rate), happiness)) +
  geom_bin2d(alpha = .9) +
  scale_fill_viridis_c() +
  labs(title = "Happiness of Pokemon by Female Rate", x = "Female Rate", y = "Happiness") +
  theme(panel.grid.major.x = element_blank())


pokemon_new |> ggplot(aes(as.factor(gen_introduced), base_happiness)) +
  geom_boxplot()


## Basic boxplots

b1 <- pokemon_new |> ggplot(aes(mythical, base_happiness)) +
  geom_boxplot() + 
  labs(title = "Happiness of Mythical Pokemon", x = "Mythical", y = "Happiness")

b2 <- pokemon_new |> ggplot(aes(legendary, base_happiness)) +
  geom_boxplot() + 
  labs(title = "Happiness of Legendary Pokemon", x = "Legendary", y = "Happiness")

b3 <- pokemon_new |> ggplot(aes(primary, base_happiness)) +
  geom_boxplot() + 
  labs(title = "Happiness of Pokemon my Primary Type", x = "Primary Type", 
       y = "Happiness")

b4 <- pokemon_new |> ggplot(aes(as.factor(gen_introduced), base_happiness)) +
  geom_boxplot() 

(b1 + b2) / (b3) / b4
