library(tidymodels)
library(tidyverse)
library(vroom)
library(themis)
library(recipes)
library(discrim)

test<-vroom("test.csv")
train<-vroom("train.csv")
trainMV<-vroom("trainWithMissingValues.csv")
sample<-vroom("sample_submission.csv")

glimpse(trainMV)

#Naive approach take the median of all missing values in a recipe creation
naiveRecipe <- recipe(type ~ ., data = trainMV) %>% 
  step_impute_median(c(bone_length, rotting_flesh, hair_length, has_soul)) %>%
  prep(training = trainMV) %>%
  juice()

# Print results to verify NAs are replaced
naiveRecipe %>%
  summarize(missing_bone_length = sum(is.na(has_soul)))


# KNN recipe for just bone_length
boneKNNrecipe <- recipe(type ~ ., data = trainMV) %>%
  step_impute_knn(bone_length, neighbors = 5) %>%
  prep(training = trainMV)

# Check the imputation (for demonstration, using juice here)
imputed_data <- juice(boneKNNrecipe)

# Verify that NAs are replaced in bone_length
imputed_data %>%
  summarize(missing_bone_length = sum(is.na(bone_length)))


# Define the recipe with knn imputation for all applicable features
KNNrecipe<- recipe(type ~ ., data = trainMV) %>%
  step_impute_knn(all_predictors(), neighbors = 5) %>%
  prep(training = trainMV)

# Apply the prepared imputation
imputed_data <- juice(KNNrecipe)

# Check for missing values (optional verification step)
summarize(imputed_data, across(everything(), ~ sum(is.na(.))))

nb_model <- naive_Bayes(Laplace=tune(), smoothness=tune()) %>%
  set_mode("classification") %>%
  set_engine("naivebayes") # install discrim library for the naivebayes eng

nb_wf <- workflow() %>%
  add_recipe(KNNrecipe) %>%
  add_model(nb_model)

## Tune smoothness and Laplace here
neighbors_grid <- grid_regular(smoothness, laplace, levels = 5)

## Predict
predict(nb_wf, new_data=myNewData, type=)


# Create tibble of non-missing data to be the train set for our model
trainBone <- trainMV %>% 
  filter(!is.na(bone_length))

trainFlesh <- trainMV %>% 
  filter(!is.na(bone_length))

trainHair <- trainMV %>% 
  filter(!is.na(bone_length))

# Create recipe for hair missing values
hairRecipe <- recipe(bone_length, data = trainHair) %>% 
  step_

rmse_vec(trainSet[is.na(missSet)], imputedSet[is.na(missSet)])
