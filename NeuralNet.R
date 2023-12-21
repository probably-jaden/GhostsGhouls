library(tidymodels)
library(tidyverse)
library(vroom)
library(themis)
library(recipes)
library(discrim)
library(klaR)

test<-vroom("test.csv")
train<-vroom("train.csv")
sample<-vroom("sample_submission.csv")

glimpse(train)

# Create recipe, change nominal predictors to be dummy variables

recipe <- recipe(type ~ ., data = train) %>%
  update_role(id, new_role="id") %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_predictors())

# check to make sure it looks reasonable
recipe

# prep and bake
prepped_recipe <- prep(recipe, training = train)
baked_test <- bake(prepped_recipe, new_data = test)

#Neural Network model
nn_model <- mlp(hidden_units = tune(),
                penalty = tune(),
                epochs = 50) %>%
                set_engine("nnet", ) %>% #verbose = 0 prints off less
                set_mode("classification")

nn_workflow <- workflow() %>%
  add_model(nn_model) %>%
  add_recipe(recipe)

grid <-grid_regular(hidden_units(range=c(1, 20)), penalty(), levels = 10)

cv_splits <- vfold_cv(train, v = 5)

CV_results <- nn_workflow %>%
  tune_grid(resamples=cv_splits,
            grid=grid)

best_parameters <- select_best(CV_results, "accuracy")

CV_results %>% collect_metrics() %>%
  filter(.metric=="accuracy") %>%
  ggplot(aes(x=hidden_units, y=mean)) + geom_line()

final_wf <-nn_workflow %>% 
  finalize_workflow(best_parameters) %>% 
  fit(data = train)

## Look at the fitted LM model this way
extract_fit_engine(final_wf) %>%
  summary()

## Get Predictions for test set AND format for Kaggle
test_preds <- predict(final_wf, new_data = test) %>% 
  bind_cols(., test) %>% 
  rename(type=.pred_class) %>%  #rename pred to count (for submission to Kaggle) as well as undo the log
  dplyr::select(id, type)


glimpse(test_preds)

vroom_write(x=test_preds, file="TestPredsNN.csv", delim=",")



