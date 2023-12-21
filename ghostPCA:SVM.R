# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

train <- read.csv("../input/train.csv")

# Splitting up the data by 'color'...
black <- train %>% subset(color == "black");
blood <- train %>% subset(color == "blood");
blue <- train %>% subset(color == "blue");
clear <- train %>% subset(color == "clear");
green <- train %>% subset(color == "green");
white <- train %>% subset(color == "white")

# ... and by type
ghost <- train %>% subset(type == "Ghost");
ghoul <- train %>% subset(type == "Ghoul");
goblin <- train %>% subset(type == "Goblin")

# Summarized by type shows that two most common colors are clear and white...
summary(ghost[2:7]); summary(ghoul[2:7]); summary(goblin[2:7])