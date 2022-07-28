library(arrow)
library(magrittr)
library(tidyverse)
library(keras)
library(splitTools)
library(randomForest)
library(ROCR)

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
train <- readFromParquet("C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/train/train_1.parquet")
val <- readFromParquet("C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/validation/validation_1.parquet")
#test <- readFromParquet("C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/test/test_1.parquet")


y_train  = as.factor(train$target) 
y_val = as.factor(val$target)
y_test  = as.factor(test$target)

x_train = train  %>% dplyr::select(-c(customer_ID,S_2, target))
x_val = val  %>% dplyr::select(-c(customer_ID,S_2, target))
x_test = test  %>% dplyr::select(-c(customer_ID,S_2, target))

x_train = x_train %>% mutate_all(~replace(., is.na(.), 0))
x_val = x_val %>% mutate_all(~replace(., is.na(.), 0))
x_test = x_test %>% mutate_all(~replace(., is.na(.), 0))


start.time <- Sys.time()
samp <- randomForest(x = as.matrix(x_train) , y=y_train, 
             xtest = as.matrix(x_val), ytest = y_val,
             mtry = 30, ntree = 100)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


conf <- samp$test$confusion
conf
sens <- conf[2,2]/sum(conf[2,c(1,2)])
spec <- conf[1,1]/sum(conf[1,c(1,2)])
acc <- sum(diag(conf))/ sum(conf[c(1,2),c(1,2)])
vec
as.matrix(x_train)[1:50000,] %>% dim()


######################################################################################################

# directories
train_dir <- "C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/train/"
val_dir <- "C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/validation/"
test_dir <- "C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/test/"
res_dir <-"C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/results/"

# file lists
train_files <- list.files(path = train_dir)
val_files <- list.files(path = val_dir)
test_files <- list.files(path = test_dir)


for (i in 1:20){
  
  print(i)

  # load and prepare data
  
  train <-readFromParquet(paste(train_dir, train_files[i], sep=""))
  val <-readFromParquet(paste(val_dir, val_files[i], sep=""))
  test <-readFromParquet(paste(test_dir, test_files[i], sep=""))
  
  y_train  = as.factor(train$target) 
  y_val = as.factor(val$target)
  y_test  = as.factor(test$target)
  
  x_train = train  %>% dplyr::select(-c(customer_ID,S_2, target))
  x_val = val  %>% dplyr::select(-c(customer_ID,S_2, target))
  x_test = test  %>% dplyr::select(-c(customer_ID,S_2, target))
  
  rm(test,train,val)
  
  x_train = x_train %>% mutate_all(~replace(., is.na(.), 0))
  x_val = x_val %>% mutate_all(~replace(., is.na(.), 0))
  x_test = x_test %>% mutate_all(~replace(., is.na(.), 0))
  
  #tune model
  ntrees <- which.min(randomForest(x = x_train , y=y_train, 
                                   xtest = x_val, ytest = y_val,
                                   mtry = 30, ntree = 100)$test$err.rate[,1] )
  
  # save model
  model <- randomForest(x = x_train , y=y_train, mtry = 30, ntree = min(100, ntrees))
  saveRDS(model, file = paste(res_dir, train_files[i],"_results.rds", sep=""))
  
  rm(model)
}

model
rm(model)
model <-readRDS(paste(res_dir, train_files[1],"_results.rds", sep=""))
model

train <-readFromParquet(paste(train_dir, train_files[1], sep=""))
train <-readFromParquet(paste(val_dir, val_files[1], sep=""))
train <-readFromParquet(paste(test_dir, test_files[1], sep=""))








