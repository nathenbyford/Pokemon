#Getting the data/packages

library("tidyverse")

data = read_csv(".\\data\\pokemon.csv")


theme_set(theme_bw())

tb <-  data %>%
  separate(typing, into = c("primary", "secondary")) %>%
  mutate(BMI = (weight/10)/((height/10)^2)) %>%
  select(name, primary, hp:defense, speed:weight, female_rate:capture_rate,
         shape, BMI, base_happiness, gen_introduced, can_evolve) %>%
  filter(BMI <= 50000)


view(tb)

ggplot(tb, aes(BMI, capture_rate)) +
  geom_jitter(alpha = 0.5) +
  geom_smooth() +
  coord_cartesian(ylim = c(0,275))

ggplot(tb, aes(height, capture_rate)) +
  geom_jitter(alpha = 0.33) +
  geom_smooth(alpha = 0.5) +
  coord_cartesian(xlim = c(0,40), ylim = c(0,275))

ggplot(tb, aes(weight, capture_rate)) + 
  geom_jitter(alpha = 0.33) +
  geom_smooth(alpha = 0.5) +
  coord_cartesian(ylim = c(0,275))

ggplot(tb) +
  geom_boxplot(aes(reorder(shape, capture_rate, FUN = median), capture_rate))

ggplot(tb, aes(base_happiness, capture_rate)) + geom_jitter(alpha = 0.33, aes(color = genderless))


ggplot(tb, aes(as.factor(female_rate))) +
  geom_bar(aes(fill = can_evolve))
  
  
ggplot(tb) + 
  geom_jitter(aes(reorder(factor(female_rate), base_happiness, FUN = median), base_happiness), alpha = 0.33, color = "steel blue"
              )

ggplot(tb, aes(genderless, weight)) + geom_jitter(alpha = 0.33)

ggplot(tb) +
  geom_boxplot(aes(reorder(factor(shape), base_happiness, FUN = median), y = base_happiness))


ggplot(tb) + geom_bar(aes(x = female_rate))




evolve_gender <- tb %>%  group_by(as.factor(female_rate), can_evolve) %>%
  count() %>% 
  ungroup()

evolve_gender

ggplot(evolve_gender) +  
  geom_mosaic(aes(product(`as.factor(female_rate)`), fill = can_evolve, weight = n)) + 
  guides(fill = guide_legend(reverse = TRUE))


ggplot(tb, aes(attack, capture_rate)) + geom_jitter() + geom_smooth()

