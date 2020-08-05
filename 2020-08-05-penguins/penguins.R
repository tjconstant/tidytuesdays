# Wed Aug  5 18:18:29 2020 ------------------------------

library(palmerpenguins)
library(tidyverse)

# data view ---------------------------------------------------------------

penguins %>% 
  glimpse()

# summary stats -----------------------------------------------------------

penguins %>% 
  group_by(species) %>% 
  summarise(across(is.numeric, list(avg = mean, sd = sd), na.rm = T)) %>% 
  glimpse()

penguins %>% 
  group_by(species) %>% 
  count()

# exploratory plots -------------------------------------------------------

penguins %>% 
  ggplot(aes(bill_length_mm, flipper_length_mm, colour = species)) +
  geom_point()

# yearly trend ------------------------------------------------------------

penguins %>% 
  ggplot(aes(year, flipper_length_mm, colour = species)) +
  stat_summary() +
  geom_line(stat = "summary")


# sex ---------------------------------------------------------------------

penguins %>% 
  group_by(sex) %>% 
  count(sort = T)

penguins %>% 
  group_by(sex, year) %>% 
  count(sort = T)

# Are males or females bigger?
penguins %>% 
  group_by(sex) %>% 
  summarise(mean(body_mass_g))

penguins %>% 
  ggplot(aes(sex, body_mass_g, fill = sex)) + 
  geom_violin()

#split by species
penguins %>% 
  ggplot(aes(sex, body_mass_g, fill = species)) + 
  geom_violin()

# NAs ---------------------------------------------------------------------

penguins %>% 
  mutate(across(everything(), is.na)) %>% 
  summarise(across(everything(), sum))

# mostly sex that is undetermined.

penguins %>% 
  group_by(species) %>% 
  mutate(across(everything(), is.na)) %>% 
  summarise(across(everything(), sum))


# classification ----------------------------------------------------------

library(tidymodels)
library(themis)

p_sample <- penguins %>% initial_split(strata = species)
p_training <- training(p_sample)
p_testing <- testing(p_sample)

rcp <-
  recipe(species ~ ., data = p_training) %>% 
  step_upsample(species) %>% 
  step_bagimpute(all_nominal(), -all_outcomes()) %>% 
  step_medianimpute(all_numeric())

mdl <- rand_forest(mode = "classification")

wf <- 
  workflow() %>% 
  add_model(mdl) %>% 
  add_recipe(rcp)

fitted <- 
  wf %>% 
  fit(data = p_training)

pred_class <-
  fitted %>% 
  predict(new_data = p_testing) %>% 
  cbind(p_testing)

pred_class %>% 
  conf_mat(truth = species, estimate = .pred_class)

metric_set(accuracy, precision, recall)(
  pred_class,
  truth = species, 
  estimate = .pred_class
)

pred_prob <- 
  fitted %>%   
  predict(new_data = p_testing, type = "prob") %>% 
  cbind(p_testing)
  
pred_prob %>% 
  roc_curve(truth = species, estimate = .pred_Adelie:.pred_Gentoo) %>% 
  autoplot()

pred_prob %>% 
  pr_curve(truth = species, estimate = .pred_Adelie:.pred_Gentoo) %>% 
  autoplot()

# Wed Aug  5 18:56:35 2020 ------------------------------

