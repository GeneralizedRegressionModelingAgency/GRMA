install.packages("palmerpenguins") 
library(palmerpenguins)
library(tidyverse)
data(penguins)



filter_penguin <- filter(penguins, species %in% c("Gentoo","Chinstrap")) %>% 
  mutate(species = droplevels(species))

n <- 276
train_id <- sample(1:n, floor(n/5), replace = F)

train <- filter_penguin[train_id,]
test <- filter_penguin[!train_id,]


penguin_mod <- glm(species ~flipper_length_mm , family = "binomial", train)
summary(penguin_mod)
AIC(penguin_mod)



## question 9.9
library(GLMsData)
data(belection)

belection <- belection %>% 
  mutate(prop = Females/(Females +Males))

ggplot(belection) + 
  geom_boxplot(aes(x = Party, y = prop))

ggplot(belection) + 
  geom_boxplot(aes(x = Region, y = prop))

election_glm <- glm(prop ~ Party,
                    weights = Males + Females, 
                    family = "binomial",
                    data = belection)

summary(election_glm)

predict(election_glm, newdata = data.frame(Party = "Cons"))
predict(election_glm, newdata = data.frame(Party = "Labour"))
