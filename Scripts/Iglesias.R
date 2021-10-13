#Getting the data/packages

library("tidyverse")

data = read_csv(".\\data\\pokemon.csv")

theme_set(theme_bw() + 
            theme(
              panel.grid.minor.x = element_blank(), 
              axis.title.y = element_text(angle = 0, vjust = .5)
            )
)

tb <-  data %>%
  separate(typing, into = c("primary", "secondary")) %>%
  mutate(BMI = (weight/10)/((height/10)^2)) %>%
  select(name, primary, hp:defense, speed:weight, female_rate:capture_rate,
         shape, BMI) %>%
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
  geom_smooth(alpha = 0.5)

ggplot(tb, aes(female_rate, capture_rate)) + geom_jitter(alpha = 0.5, aes(color = legendary))

ggplot(tb, aes(genderless, capture_rate)) + geom_point(alpha = 0.05)



