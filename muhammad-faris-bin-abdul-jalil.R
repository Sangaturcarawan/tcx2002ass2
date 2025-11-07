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

#case 2

#prep
set.seed(123)
url2 <- "https://raw.githubusercontent.com/rashida048/Datasets/master/Heart.csv"
df2 <- read.csv(url2)
head(df2)
str(df2)
sum(is.na(df2)) # 6 missing values altogether
colSums(is.na(df2)) # Ca has 4 missing values, Thal has 2
unique(df2$Sex)

#factorise so that later on for making the model it would not explode
df2$Sex <- factor(df2$Sex)
df2$ChestPain <- factor(df2$ChestPain)
str(df2$ChestPain)
class(df2$ChestPain)
levels(df2$ChestPain)
df2$Fbs <- factor(df2$Fbs)
df2$RestECG <- factor(df2$RestECG)
df2$ExAng <- factor(df2$ExAng)
df2$Slope <- factor(df2$Slope)
df2$Ca <- factor(df2$Ca)
df2$Thal <- factor(df2$Thal)
df2$AHD <- factor(df2$AHD, levels = c("No","Yes"))

#qn 1
summary(df2)
table(df2$AHD)
prop.table(table(df2$AHD)) * 100

#qn 2
#clean the data first by handling missing values
unique(df2$Sex)
sum(is.na(df2))
colSums(is.na(df2))
#omit because missing values are small
6/303*100
# % missing values quite small
df2 <- na.omit(df2)
sum(is.na(df2))
colSums(is.na(df2))

library(caret)
set.seed(123)
train2_idx <- createDataPartition(df2$AHD, p = 0.8, list = FALSE)
train2_idx
train2 <- df2[train2_idx, ]
test2 <- df2[-train2_idx, ]

mdl2 <- glm(AHD ~ ., data = train2[, -1], family = binomial) #remove column 1 because not needed
summary(mdl2)

#odds ratio
exp(coef(mdl2))
exp(confint(mdl2))

#qn3

pred_probs <- predict(mdl2, newdata = test2[, -1], type = "response")

pred_classes <- ifelse(pred_probs > 0.5, "Yes", "No")