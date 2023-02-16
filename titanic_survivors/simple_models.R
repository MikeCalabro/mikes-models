# Making really simple models using age and sex

library(tidyverse)
library(janitor)
library(tidymodels)
library(rpart.plot)

set.seed(617)
 
train <- read_csv('titanic_survivors/train.csv') %>% 
  clean_names() %>%
  mutate(survived = factor(survived))

test <- read_csv('titanic_survivors/test.csv') %>% 
  clean_names() 

folds <- vfold_cv(train, v = 10)

tree_spec <-
  decision_tree(min_n = 20) %>%
  set_engine('rpart') %>%
  set_mode('classification')

tree_recipe <- 
  recipe(survived ~ pclass + sex + age, data = train)

tree_workflow <-
  workflow() %>%
  add_model(tree_spec) %>%
  add_recipe(tree_recipe)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

tree_res <- 
  tree_workflow %>% fit_resamples(resamples = folds, control = keep_pred)

collect_metrics(tree_res)
