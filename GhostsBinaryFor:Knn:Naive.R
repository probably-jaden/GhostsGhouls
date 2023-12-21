# For data manipulation and tidying
library(dplyr)

# For data visualizations
library(ggplot2)
library(fpc)

# For modeling and predictions
library(caret)
library(glmnet)
library(ranger)
library(e1071)
library(clValid)

train <- read.csv('../input/train.csv', header = TRUE, stringsAsFactors = FALSE)
train$Dataset <- "train"

test <- read.csv('../input/test.csv', header = TRUE, stringsAsFactors = FALSE)
test$Dataset <- "test"

full <- bind_rows(train, test)

train_2 <- full[full$Dataset == 'train', ]

pairs(full[,2:5], 
      col = full$type, 
      labels = c("Bone Length", "Rotting Flesh", "Hair Length", "Soul"))

full <- full %>%
  mutate(hair_soul = hair_length * has_soul)

full <- full %>%
  mutate(bone_flesh = bone_length * rotting_flesh,
         bone_hair = bone_length * hair_length,
         bone_soul = bone_length * has_soul,
         flesh_hair = rotting_flesh * hair_length,
         flesh_soul = rotting_flesh * has_soul)

# Set the seed
set.seed(100)

# Extract creature labels and remove column from dataset
creature_labels <- full$type
full2 <- full
full2$type <- NULL

# Remove categorical variables (id, color, and dataset) from dataset
full2$id <- NULL
full2$color <- NULL
full2$Dataset <- NULL

# Perform k-means clustering with 3 clusters, repeat 30 times
creature_km_1 <- kmeans(full2, 3, nstart = 30)

dunn_ckm_1 <- dunn(clusters = creature_km_1$cluster, Data = full2)

# Print results
dunn_ckm_1

table(creature_km_1$cluster, creature_labels)

train_complete <- full[full$Dataset == 'train', ]
test_complete <- full[full$Dataset == 'test', ]

I will create a system that will perform 20 repeats of a 10-Fold cross-validation of the data.

myControl <- trainControl(
  method = "cv", 
  number = 10,
  repeats = 20, 
  verboseIter = TRUE
)

set.seed(10)

rf_model <- train(
  type ~ bone_length + rotting_flesh + hair_length + has_soul + color + hair_soul + bone_flesh + bone_hair + 
    bone_soul + flesh_hair + flesh_soul,
  tuneLength = 3,
  data = train_complete, 
  method = "ranger", 
  trControl = myControl,
  importance = 'impurity'
  
)

set.seed(10)

rf_model_2 <- train(
  type ~ bone_length + rotting_flesh + hair_length + has_soul + hair_soul + bone_flesh + bone_hair + 
    bone_soul + flesh_hair + flesh_soul,
  tuneLength = 3,
  data = train_complete, 
  method = "ranger", 
  trControl = myControl,
  importance = 'impurity'
)

set.seed(10)

glm_model <- train(
  type ~ bone_length + rotting_flesh + hair_length + has_soul + color + hair_soul + bone_flesh + bone_hair + 
    bone_soul + flesh_hair + flesh_soul, 
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0:1,
                         lambda = seq(0.0001, 1, length = 20)),
  data = train_complete,
  trControl = myControl

)

set.seed(10)

glm_model_2 <- train(
  type ~ bone_length + rotting_flesh + hair_length + has_soul + hair_soul + bone_flesh + bone_hair + 
    bone_soul + flesh_hair + flesh_soul, 
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0:1,
                         lambda = seq(0.0001, 1, length = 20)),
  data = train_complete,
  trControl = myControl
)

# Create a list of models
models <- list(rf = rf_model, rf2 = rf_model_2, glmnet = glm_model, glmnet2 = glm_model_2)

# Resample the models
resampled <- resamples(models)

# Generate a summary
summary(resampled)

# Plot the differences between model fits
dotplot(resampled, metric = "Accuracy")

# Reorder the data by creature ID number
test_complete <- test_complete %>%
  arrange(id)

# Make predicted survival values
my_prediction <- predict(glm_model_2, test_complete)

# Create a data frame with two columns
my_solution_GGG_03 <- data.frame(id = test_complete$id, Type = my_prediction)

# Write the solution to a csv file 
write.csv(my_solution_GGG_03, file = "my_solution_GGG_03.csv", row.names = FALSE)