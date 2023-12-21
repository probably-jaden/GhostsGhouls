library(tidymodels)
library(tidyverse)
library(vroom)
library(themis)
library(recipes)
library(discrim)
library(klaR)
library(parsnip)
library(bonsai)
library(lightgbm)

test<-vroom("test.csv")
train<-vroom("train.csv")
sample<-vroom("sample_submission.csv")

glimpse(train)

# Create recipe, change nominal predictors to be dummy variables

recipe <- recipe(type ~ ., data = train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), threshold=tune())

# check to make sure it looks reasonable
recipe

# prep and bake
prepped_recipe <- prep(recipe, training = train)
baked_test <- bake(prepped_recipe, new_data = test)

#Naive Bayes model
boost_model <- boost_tree(tree_depth=tune(),
                          trees=tune(),
                          learn_rate=tune()) %>%
  
set_engine("lightgbm") %>% #or "xgboost" but lightgbm is faster6
  set_mode("classification")

boost_workflow <- workflow() %>%
  add_model(boost_model) %>%
  add_recipe(recipe)

grid <-grid_regular(threshold(), tree_depth(), trees(), learn_rate(), levels = 5)
cv_splits <- vfold_cv(train, v = 5)

CV_results <- boost_workflow %>%
  tune_grid(resamples=cv_splits,
            grid=grid)

best_parameters <- select_best(CV_results, "accuracy")

final_wf <- boost_workflow %>% 
  finalize_workflow(best_parameters) %>% 
  fit(data = train)
    
## Look at the fitted LM model this way
extract_fit_engine(final_wf) %>%
  summary()

## Get Predictions for test set AND format for Kaggle
test_preds <- predict(final_wf, new_data = test) %>% 
  bind_cols(., test) %>% 
  rename(type=.pred_class) %>%  #rename pred to count (for submission to Kaggle) as well as      undo the log
  dplyr::select(id, type)


glimpse(test_preds)

vroom_write(x=test_preds, file="TestPredsBoost.csv", delim=",")



