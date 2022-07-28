
gc()

library(arrow)
library(magrittr)
library(tidyverse)
library(keras)
library(splitTools)
library(randomForest)
library(ROCR)
library(glue)

readFromParquet = function(filePath){
  ads = arrow::open_dataset(sources =  filePath)
  ## Create a scanner
  scan = Scanner$create(ads)
  ## Load it as n Arrow Table in memory
  at = scan$ToTable()
  ## Convert it to an R data frame
  as.data.frame(at)
}

#sensitivity specificity and accuracy
tabfunc <- function(pred, obs){
  tab <-table(obs, pred)
  acc <- sum(diag(tab))/sum(tab)
  sens <- tab[2,2]/sum(tab[2,c(1,2)])
  spec <- tab[1,1]/sum(tab[1,c(1,2)])
  return(c(sens, spec, acc))
  
}


parquets_dir <- "C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/parquets/data_lastPerCustomerID.parquet"

data <- readFromParquet(parquets_dir)
#dim(data)


#partition target
data_y = data$target

# Remove customer_ID and target
data = data  %>% dplyr::select(-c(customer_ID,target, S_2)) 


#One hot encode categoricals
# catCols = intersect(c('B_30', 'B_38', 'D_114', 'D_116', 'D_117', 'D_120',
# 'D_126', 'D_66', 'D_68','D_63','D_64'), colnames(data))
# for(i in 1:length(catCols)){
#   print(catCols[i])
#   data[ , which(names(data) %in% catCols[i])] = as.factor(data[ , which(names(data) %in% catCols[i])])
#   
# }


#Scale floats
for(i in 1:ncol(data)){
  if("double" == typeof(data[,i][[1]])){
    data[,i] = scale(data[,i][[1]])
  }
}

#Replace all remaining NA with zero
data = data %>% mutate_all(~replace(., is.na(.), 0))

data <- as.data.frame(data)


data$B_30 <- as.factor(data$B_30)
data$B_38 <- as.factor(data$B_38)
data$D_114 <- as.factor(data$D_114)
data$D_116 <- as.factor(data$D_116)
data$D_117 <- as.factor(data$D_117)
data$D_120 <- as.factor(data$D_120)
data$D_126 <- as.factor(data$D_126)
data$D_66 <- as.factor(data$D_66)
data$D_68 <- as.factor(data$D_68)
data$D_63 <- as.factor(data$D_63)
data$D_64 <- as.factor(data$D_64)




# train/val and test split 
load("C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/parquets/trainTestIndex")
#trainTestIndex$trainVal
#trainTestIndex$test

x_trainval <- data[trainTestIndex$trainVal,]
x_test <- data[trainTestIndex$test,]

y_trainval <- data_y[trainTestIndex$trainVal]
y_test <- data_y[trainTestIndex$test]

rm(data, data_y)



# RANDOM FOREST
###################################################################################


# three folds and 10 repeats
train_ctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 10, allowParallel = TRUE)

tune_grid <- expand.grid( mtry = c(10,20,30,40,50) )

rf <- train(x = x_trainval, y = as.factor(y_trainval), method = "rf",
                      trControl = train_ctrl, tuneGrid = tune_grid, ntree = 150)


pred <- predict(rf, newdata = x_test)

rf$bestTune # best number of features
tabfunc(y_test, pred) # test results




# LOG REG
###################################################################################

# three folds and 10 repeats
train_ctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 10, allowParallel = TRUE)

# fit model
lr <- train(x = x_trainval, y = as.factor(y_trainval), method = "glm", family = "binomial",trControl = train_ctrl)



# predict test data
# extract probability to revier/tune tau
pred <- predict(lr, newdata = x_test, type = "prob")[,2]
# load ROCR
library(ROCR)
pred_obj <- prediction(pred, y_test)
sens <- performance(pred_obj, "sens")
spec <- performance(pred_obj, "spec")
tau <- sens@x.values[[1]]
sens_spec <- sens@y.values[[1]] + spec@y.values[[1]]
best <- which.max(sens_spec)
#predictions
pred <- ifelse(pred > tau[best], 1, 0)

tabfunc(pred, y_test) # test results
tau[best] # optimal tau

gc()









