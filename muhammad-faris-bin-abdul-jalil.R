#install dependencies
install.packages(c(
  "tidyverse", 
  "caret", 
  "car",
  "MASS",
  "broom",
  "corrplot",
  "GGally",
  "lmtest"
  ))

#load dependencies
library(tidyverse)
library(caret)
library(car)
library(MASS)
library(broom)
library(corrplot)
library(GGally)
library(lmtest)

#know whether my RStudio is updated and dependencies installed
sessionInfo()

#replicable randomizer
set.seed(123)

#load data into RStudio
url <- "https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv"
df <- read.csv(url)

#general overview, quick statistics, find out how many data points missing
str(df); summary(df); sum(is.na(df))

#correlation matrix, put a nice table with colors
num_df <- df %>% select_if(is.numeric)
cor_mat <- cor(num_df)
corrplot::corrplot(cor_mat, method="color", tl.cex=0.8)

#train test split 70:30
library(caret)
train_idx <- createDataPartition(df$medv, p = 0.7, list = FALSE)
train <- df[train_idx, ]
test <- df[-train_idx, ]

#baseline full model
full_mdl <- lm(medv ~ ., data = train)
summary(full_mdl) #qn2

#qn3
train_pred <- predict(full_mdl, newdata = train)
test_pred <- predict(full_mdl, newdata = test)

train_r2 <- cor(train_pred, train$medv)^2
test_r2 <- cor(test_pred, test$medv)^2

train_r2
test_r2
train_r2 - test_r2


#qn5
library(caret)

var_imp <- varImp(full_mdl)
top2 <- var_imp$Overall %>% sort(decreasing = TRUE) %>% head(2)
top2

#can just use
summary(full_mdl)
#just take a look at the absolute t values
