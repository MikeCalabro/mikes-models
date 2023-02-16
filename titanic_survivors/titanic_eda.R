# Exploratory Data Analysis

library(tidyverse)
library(stringr)
library(janitor)
library(lubridate)
library(tidymodels)
library(ggmosaic)

train <- read_csv('titanic_survivors/train.csv') %>%
  clean_names()

train %>% head()

summary(train)

train %>%
  ggplot(aes(x = age)) +
  geom_histogram() +
  ggtitle("Age of Passengers on Titanic")

train %>%
  ggplot(aes(x = sex)) +
  geom_bar()

train %>%
  ggplot(aes(x = factor(pclass))) +
  geom_bar()

train %>%
  ggplot(aes(x = factor(pclass), fill = factor(survived))) +
  geom_bar(position = 'fill')

train %>%
  ggplot(aes(x = sex, fill = factor(survived))) +
  geom_bar(position = 'fill')

train %>%
  ggplot(aes(x = factor(age), fill = factor(survived))) +
  geom_bar(position = 'fill')

train %>%
  ggplot(aes(x = embarked, fill = pclass)) +
  geom_bar(position = 'fill')

round_n <- function(num, inc) {
  
    if(is.na(num)) {
      return("No Age")
    }

    check <- inc
    while(num > check) {
      check <- check + inc
    }
    
    return(paste(str_pad(check - inc + 1, 2, pad = "0"), '-', str_pad(check, 2, pad = "0")))

}

round_n.p_V <- Vectorize(round_n)

train %>%
  mutate(age_bkt = round_n.p_V(age, 4.0)) %>%
  ggplot(aes(x = age_bkt, fill = factor(survived))) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5)) +
  facet_grid(rows = vars(pclass), cols = vars(sex))
