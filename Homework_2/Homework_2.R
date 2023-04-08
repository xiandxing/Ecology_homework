# Ecology Homework 2, Author: 陈凯星, Student ID: SA22234107

# This assignment bases on the dataset used in Homework_01. The goal is to learn how to training a model with the caret package. 
# It consists of two major sections: 
# 1. The first one is to preProcess data. 
# 2. The second involves training a tree-based model to answer the question raised by the dataset. 

# Submission
# 1. Export your R script as a Homework_2.R file 
# 2. Submit your homework through your own github
# 3. Due Date: April 11, 2023

rm(list = ls()) # Delete environment variables

library('tidyverse') # Load tool package
library('caret') # Load tool package, being used in section II
library('ade4') # Load data package
library('car') # Load graph package


### Section I: Read data and data pre-Process – preliminary. This section covers:

# - Reading env and fish data
# - Summarizing fish abundance data by sites
# - Combining env and total fish to a new data frame named as "env_fish". 

data('doubs')

total_fish = rowSums(doubs$fish) # Summarizing fish abundance data by sites

env_fish = cbind(doubs$env, total_fish) # Combining env and total fish to a new data frame named as "env_fish"

# - Visualizing the features of the new env_fish set using scatterplot(). 
# - Do you find any linear relationships between environmental variables and the total fish abundance at the sites?
# Based on the scatterplots showed below, 
# I think there is a positive linear correlation between dfs(distsour) / flo(flowrate) and total fish abundance,
# and there is a negative linear correlation between slo(slope) and total fish abundance.

options(repr.plot.width=12, repr.plot.height=8) 

env_fish %>% 
gather(-total_fish, key = "value", value = "env") %>%
ggplot(aes(x = env, y = total_fish)) +
geom_point()+
geom_smooth(se = FALSE) +
facet_wrap(~value, scales = "free") +
theme_bw()

### Plot method 2
# featurePlot(x = env_fish[, -12],
#             y = env_fish[, 12],
#             plot = 'scatter',
#             type = c('p', 'smooth'),
#             layout = c(3, 4),
#             warn = FALSE)

### Plot method 3
# for (i in 1:(ncol(env_fish)-1)) { # circulating for scatterplots between all 11 features and total fish abundance
#     scatterplot(eval(as.name(colnames(env_fish)[ncol(env_fish)])) ~ eval(as.name(colnames(env_fish)[i])), 
#                 data=env_fish,
#                 xlab=colnames(env_fish)[i], 
#                 ylab=colnames(env_fish)[ncol(env_fish)])
# }

# - Having the sites with no fishes? If yes, deleting such sites
 
env_fish.f_f <- env_fish %>% subset(total_fish > 0) # delete rows contain 0 fish

### Ref: https://blog.csdn.net/kMD8d5R/article/details/95937477

# - Having null values or outliers? If yes, removing all rows where any column contains an outlier. 

library("outliers") # Load packages to delete outliers

env_fish.f_f$sel = TRUE # To choose which row will be remained
for (i in 1:(ncol(env_fish.f_f)-2)) {
    tmp_col = colnames(env_fish.f_f)[i]
    env_fish.f_f[scores(env_fish.f_f[, tmp_col], type="z", prob=0.95),'sel'] = FALSE # type="z" means (x-mean)/sd
}
env_fish.filtered <- env_fish.f_f %>% subset(sel == TRUE)

# - Identifying near zero-variance, outlies of the env variables. If yes, excluding them for analysis.

var_names = colnames(env_fish.filtered)[1:11]

var_list = c()
for (i in 1:length(var_names)) {
    var_list = c(var_list, var(env_fish.filtered[,var_names[i]])) # var() to get variance of the single column in R
}

### 'slo' and 'pH' near zero-variance, excluding them for analysis

env_fish.filtered_v2 = env_fish.filtered %>% select(-var_names[c(3,5)])

# - Detecting the collinearity among env variables and removing highly correlated features (with an absolute correlation of 0.75 or higher) 

### Ref: https://www.projectpro.io/recipes/drop-out-highly-correlated-features-in-python
# In data science, it's essential to understand how to deal with highly correlated features. 
# Highly correlated features refer to variables that have a strong linear relationship with each other. 
# When two or more variables are highly correlated, they carry almost the same information, making it redundant to include all of them in a model. 
# This can lead to a problem known as multicollinearity where it becomes difficult to determine the independent effect of each variable on the target variable. 
# To address this issue, removing features with high correlation is advisable to improve the efficiency and accuracy of the prediction model. 
# Thus, removing highly correlated variables is a crucial step in data preprocessing and can help improve the performance of your machine-learning models. 

cor_matrix <- cor(env_fish.filtered_v2[,c(1:(ncol(env_fish.filtered_v2)-2))]) # use default method 'pearson' to calculate collinearity

# Judging highly +/- correlated features

for (i in 1:(nrow(cor_matrix)-1)) {
    for (j in (i+1):ncol(cor_matrix)) {
        if (abs(cor_matrix[i, j]) >= 0.75) {
            print(paste0(rownames(cor_matrix)[i], ' - ', colnames(cor_matrix)[j]))
        }
    }
}

# Delete 'alt', 'flo', 'nit', 'bdo' and other unuseful columns

env_fish.filtered_v3 <- env_fish.filtered_v2 %>% select(!c('alt', 'flo', 'nit', 'bdo', 'sel'))


### Section II: Building a regression model. This section covers:

# - Splitting data into training and test sets

set.seed(1234)

ind <- sample(2, nrow(env_fish.filtered_v3), replace=TRUE, prob=c(0.7,0.3))

xtrain <- env_fish.filtered_v3[ind==1,]
xtest <- env_fish.filtered_v3[ind==2,]

# - Visualizing the features and targets of the training set
# create a panel of simpler density plots by attribute

options(repr.plot.width=18, repr.plot.height=4) 

par(mfrow=c(1,ncol(xtrain)))
for(i in 1:ncol(xtrain)) {
    plot(density(xtrain[,i]), main=names(xtrain)[i])
}

# - Creating and evaluating a baseline model between the environmental variables and the total fish abundance with the tree-based algorithm
# Trying regression tree, with 'rpart' package
# RMSE(train) = 5.62231345996236, RMSE(test) = 3.59007623862875

library(randomForest)

fit <- randomForest(total_fish ~ ., xtrain, ntree = 500, mtry = 5) # Fitting model
pred_train <- predict(fit, xtrain) # Predicting output
pred_test <- predict(fit, xtest)

RMSE(pred_train, xtrain$total_fish)
RMSE(pred_test, xtest$total_fish)

# Trying regression tree, with 'rpart' package
# RMSE(train) = 2.94924383795004, RMSE(test) = 3.14006369362152

library('rpart')
library('rpart.plot')

ctrl <- rpart.control(minsplit = 2, cp = 0.01, maxdepth = 10)
m.rpart <- rpart(total_fish ~ ., xtrain, method = 'anova', control = ctrl)

pred_train <- predict(m.rpart, xtrain)
pred_test <- predict(m.rpart, xtest)

RMSE(pred_train, xtrain$total_fish)
RMSE(pred_test, xtest$total_fish)

options(repr.plot.width=8, repr.plot.height=4) 
rpart.plot(m.rpart)

# Trying linear regression model, perform bad
# RMSE(train) = 7.37934636134502, RMSE(test) = 9.39771365466787

lr_model <- lm(total_fish ~ ., xtrain)

pred_train <- predict(lr_model, xtrain) # Predicting output
pred_test <- predict(lr_model, xtest)

RMSE(pred_train, xtrain$total_fish)
RMSE(pred_test, xtest$total_fish)
