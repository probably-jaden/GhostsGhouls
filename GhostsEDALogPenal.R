# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")
train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")
result<-read_csv("../input/sample_submission.csv")

# Any results you write to the current directory are saved as output.

# Knowing the rows in both the datasets
cat(sprintf("Training set has %d rows and %d columns\n", nrow(train), ncol(train)))
cat(sprintf("Test set has %d rows and %d columns\n", nrow(test), ncol(test)))

# Knowing the structure of both dataset
summary(train)
summary(test)

# changing the variable datatype
train$type<-as.factor(train$type)
train$color<-as.factor(train$color)
test$color<-as.factor(test$color)

# again checking the summary
summary(train)
summary(test)

# Custom function for calculating outliers
mystats <- function(x) {
  UC <- mean(x)+3*sd(x)
  LC <- mean(x)-3*sd(x)
  return(c(UC=UC, LC=LC ))
}
# Duplicating train dataset for further analysis
train1<-train
vars<-c("bone_length","rotting_flesh","has_soul","hair_length")
diag_stats<-t(data.frame(apply(train1[vars], 2, mystats)))

# write.csv(diag_stats,"diag_stats.csv")
# As per the Upper and lower cut of the data given through the above created file, it needs to be modified
train1$bone_length[train1$bone_length>0.832659128722003]<-0.832659128722003
train1$bone_length[train1$bone_length<0.0356608033744191]<-0.0356608033744191
train1$rotting_flesh[train1$rotting_flesh>0.945921728038551]<-0.945921728038551
train1$rotting_flesh[train1$rotting_flesh<0.0677752635132345]<-0.0677752635132345
train1$has_soul[train1$has_soul>0.999780051047224]<-0.999780051047224
train1$has_soul[train1$has_soul<(-0.0569959866620412)]<-(-0.0569959866620412)
train1$hair_length[train1$hair_length>1.03881976324586]<-1.03881976324586
train1$hair_length[train1$hair_length<0.0194088567658867]<-0.0194088567658867

# Creating a naive bayes model
library(ranger)
fit<-ranger(type~., data=train1)

# Predicting the test results
pred<-predict(fit,test,type="response")

# writing the results for output
result<-data.frame(id=test$id,type=pred$predictions)
write.csv(result,"result.csv",row.names=F)