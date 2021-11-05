https://www.kdnuggets.com/2020/12/undersampling-change-base-rates-model-predictions.html

library(tidyverse)
library(modelr)
library(ggplot2)
library(gridExtra)
library(purrr)

theme_set(theme_bw())

# convert log odds to probability
convert_lodds <- function(log_odds) exp(log_odds) / (1 + exp(log_odds))

set.seed(123)

minority_data <- tibble(rand_lodds = rnorm(1000, log(.03 / (1 - .03)), sd = 1),
                        rand_probs = convert_lodds(rand_lodds)) %>% 
  mutate(target = map(.x = rand_probs, ~rbernoulli(100, p = .x))) %>% 
  unnest() %>% 
  mutate(id = row_number())

# Change the name of the same of the variables to make the dataset more
# intuitive to follow.
example <- minority_data %>% 
  select(id, target, feature = rand_lodds)

table(example$target)
prop.table(table(example$target))

example %>% 
  count(target) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>% 
  knitr::kable()

# Make 80-20 train - test split9.
set.seed(123)
train <- example %>% 
  sample_frac(0.80)

test <- example %>% 
  anti_join(train, by = "id")

# We have one important input to our model named feature
train %>% 
  ggplot(aes(feature, fill = target))+
  geom_histogram()+
  labs(title = "Distribution of values of 'feature'",
       subtitle = "Greater values of 'feature' associate with higher likelihood 'target' = TRUE")

# Make a new sample train_downsamp that keeps all positive cases in the 
# training set and an equal number of randomly sampled negative cases so 
# that the split is no longer 5-95 but becomes 50-50.

minority_class_size <- sum(train$target)

set.seed(1234)

train_downsamp <- train %>% 
  group_by(target) %>% 
  sample_n(minority_class_size) %>% 
  ungroup()

train_downsamp %>% 
  count(target) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>% 
  knitr::kable()

train_downsamp %>% 
  ggplot(aes(feature, fill = target))+
  geom_histogram()+
  labs(title = "Distribution of values of 'feature' (down-sampled)",
       subtitle = "Greater values of 'feature' associate with higher likelihood 'target' = TRUE")


mod_5_95 <- glm(target ~ feature, family = binomial("logit"), data = train)
mod_50_50 <- glm(target ~ feature, family = binomial("logit"), data = train_downsamp)


test_with_preds <- test %>% 
  gather_predictions(mod_5_95, mod_50_50) %>% 
  mutate(pred_prob = convert_lodds(pred))

test_with_preds %>% 
  ggplot(aes(x = pred_prob, fill = target))+
  geom_histogram()+
  facet_wrap(~model, ncol = 1)

test_with_preds %>% 
  filter(id == 2) %>%
  arrange(id) %>% 
  select(-pred) %>% 
  knitr::kable(digits = 2)

# Rescale Predictions to Predicted Probabilities
mod_50_50_rescaled_calibrated <- train %>% 
  add_predictions(mod_50_50) %>% 
  glm(target ~ pred, family = binomial("logit"), data = .)


test_with_preds_adjusted <- test %>% 
  spread_predictions(mod_5_95, mod_50_50) %>% 
  rename(pred = mod_50_50) %>% 
  spread_predictions(mod_50_50_rescaled_calibrated) %>% 
  select(-pred) %>% 
  gather(mod_5_95, mod_50_50_rescaled_calibrated, key = "model", value = "pred") %>% 
  mutate(pred_prob = convert_lodds(pred)) 

test_with_preds_adjusted %>% 
  ggplot(aes(x = pred_prob, fill = target))+
  geom_histogram()+
  facet_wrap(~model, ncol = 1)









