library(arrow)
library(magrittr)
library(tidyverse)
library(keras)
library(splitTools)
library(randomForest)


rf_complete <- function(pred_train , target_train ,
                        pred_val , target_val,
                        pred_test , target_test){
  #  default number of trees
  def_tree <- 150
  #tuning run - get number of trees to minimize validation error
  ntrees <- which.min(randomForest(x = pred_train , y=target_train, 
                                   xtest = pred_val, ytest = target_val,
                                   mtry = 30, ntree = def_tree)$test$err.rate[,1] )
  # test run - get confusion matrix
  conf <- randomForest(x = pred_train , y=target_train,
                       xtest = pred_test, ytest = target_test,
                       ntree = min(def_tree, ntrees))$test$confusion
  
  # calculate sensitivity, specificity and accuracy
  # columns are predicted values
  # rows are observed values
  sens <- conf[2,2]/sum(conf[2,c(1,2)])
  spec <- conf[1,1]/sum(conf[1,c(1,2)])
  acc <- sum(diag(conf))/ sum(conf[c(1,2),c(1,2)])
  # return results as list
  results <- list(ntrees, sens, spec, acc)
  names(results) = c("ntrees","sens", "spec", "acc")
  
  return( results)
}


readFromParquet = function(filePath){
  ads = arrow::open_dataset(sources =  filePath)
  ## Create a scanner
  scan = Scanner$create(ads)
  ## Load it as n Arrow Table in memory
  at = scan$ToTable()
  ## Convert it to an R data frame
  as.data.frame(at)
}



# readFromParquet("/Users/root1/Documents/amex-default-prediction/parquet/")
data <- readFromParquet("C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/parquets/cleansed_4900001")

data_y  = data$target


data = data  %>% dplyr::select(-c(customer_ID,S_2, target))

#Scale floats
for(i in 1:ncol(data)){
  if("double" == typeof(data[,i][[1]])){
    data[,i] = scale(data[,i][[1]])
  }
}


#Replace all remaining NA with zero
data = data %>% mutate_all(~replace(., is.na(.), 0))

N = nrow(data)

i_partitions = partition(1:N, p = c(train = 0.7, valid = 0.2, test = 0.1))

# Assign dataframes
x_train = data[i_partitions$train,] %>% as.matrix()  
x_val = data[i_partitions$valid,] %>% as.matrix() 
x_test = data[i_partitions$test,] %>% as.matrix() 
rm(data)

#Assign lables
y_train = data_y[i_partitions$train] %>% as.factor()
y_val = data_y[i_partitions$valid] %>% as.factor()
y_test = data_y[i_partitions$test] %>% as.factor()
rm(data_y)

start.time <- Sys.time()
rs <- rf_complete(pred_train= x_train , target_train = y_train,
                  pred_val = x_val, target_val =y_val,
                  pred_test = x_test, target_test =y_test)


rs
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken






