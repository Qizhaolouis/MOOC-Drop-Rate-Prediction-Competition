###submission made easy
submission <- function(ID,pred,name){
  result = tibble(enrollment_id = ID,	
                  dropout_prob = pred) %>% 
    write_csv(paste('submission_',name,'.csv',sep=''))
}

## Machine Learning
## XGBoost
library(xgboost)
library(tidyverse)
## binary classification:
train <- read_csv("MOOC_train.csv") %>% .[,-1] 
label <- train$dropout_prob
train <- train[,-ncol(train)] %>% data.matrix()
test <- read_csv("MOOC_test.csv")
enrollment_id <- test$enrollment_id
test <- test[,-1] %>% data.matrix()


bst <- xgboost(data = train, label = label, max_depth = 3,
               eta = 0.5, nthread = 3, nrounds = 200, 
               objective = "binary:logistic")
# use all trees by default
pred <- predict(bst, test)
submission(enrollment_id,pred,"XGBoost")
