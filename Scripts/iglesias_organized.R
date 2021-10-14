library("tidyverse")
library("patchwork")

data = read_csv(".\\data\\pokemon.csv")

theme_set(theme_bw() + 
            theme(
              panel.grid.minor.x = element_blank()
            )
)

view(data)

#Tidy the data by separating typing in primary and secondary types
#Add a column called BMI derived from weight and height
#Take variable of interest
#Remove the one BMI anomaly that ruins the BMI graph
tb <- data %>%
  separate(typing, into = c("primary", "secondary")) %>%
  mutate(BMI = (weight/10)/((height/10)^2)) %>%
  select(name, primary, hp:defense, speed:weight, female_rate:capture_rate,
         shape, BMI, base_happiness, gen_introduced, can_evolve) %>%
  filter(BMI <= 50000)


#Create a plot that shows the relationship between capture_rate and BMI
#Limit to not include a lot of empty space
BMI <- ggplot(tb, aes(BMI, capture_rate)) +
  geom_jitter(alpha = 0.33, color = "Steel Blue") +
  geom_smooth() +
  coord_cartesian(ylim = c(0,275)) +
  labs(x = "Body Mass Index", y = "Capture Rate")


#Create a plot that shows the relationship between capture_rate and Height
#Limit included to allow for best view of relationship
Height <- ggplot(tb, aes(height/10, capture_rate)) +
  geom_jitter(alpha = 0.33, color = "Steel Blue") +
  geom_smooth() +
  coord_cartesian(xlim = c(0,5), ylim = c(0,275)) +
  labs(x = "Height (m)", y = "Capture Rate")
  
#Create a plot that shows the relationship between capture_rate and weight
#Limit included to allow for best view of relationship

Weight <- ggplot(tb, aes(weight/10, capture_rate)) + 
  geom_jitter(alpha = 0.33, color = "Steel blue") +
  geom_smooth(alpha = 0.5) +
  coord_cartesian(ylim = c(0,275)) +
  labs(x = "Weight (kg)", y = NULL)


BMI / (Weight | Height)


p1 <- ggplot(tb) +
  geom_boxplot(aes(reorder(shape, capture_rate, FUN = median), capture_rate)) +
  labs(x = "Shape", y = "Capture Rate")

p2 <- ggplot(tb, aes(mythical, capture_rate)) + 
  geom_boxplot() +
  labs(x = "Mythical")

p3 <- ggplot(tb, aes(legendary, capture_rate)) + 
  geom_boxplot() +
  labs(x = "Legendary", y = NULL)

p4 <- ggplot(tb, aes(genderless, capture_rate)) + 
  geom_boxplot() +
  labs(x = "Genderless", y = NULL)

p5 <- ggplot(tb, aes(baby_pokemon, capture_rate)) + 
  geom_boxplot() +
  labs(x = "Baby Pokemon", y = NULL)
 
p6 <- ggplot(tb, aes(as.factor(female_rate*100), capture_rate)) +
  geom_boxplot() +
  labs(x = "Percent Female", y = "Capture Rate")
             
(p2 | p3 | p4| p5) / p6
 
