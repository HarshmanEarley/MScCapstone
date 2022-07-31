
##########################################################################
######### Function for tuning Random Forest and Logistic Models #########
#########################################################################

model_rf_logreg <- function(model = ""){
  
  if(model == ""){
    print("paramater model must be specified as one of:")
    print("lr - logistic regression")
    print("rf - random forest")
    return(0)
  }
  
  print(glue("running models_rf_lr for model : ", model))
  # load data
  data <-readFromParquet(getFilePath('data_lastPerCustomerID', '.parquet'))

  #partition target
  data_y <- data$target
  
  # Remove customer_ID and target
  data <- data  %>% dplyr::select(-c(customer_ID,target, S_2)) 

  #Scale floats
  for(i in 1:ncol(data)){
    if("double" == typeof(data[,i][[1]])){
      data[,i] = scale(data[,i][[1]])
    }
  }
  
  #Replace all remaining NA with zero
  data <- data %>% mutate_all(~replace(., is.na(.), 0))
  
  # Define categorical values with factors
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
  
  # ensure trainTestIndex is loaded
  load(getFilePath('trainTestIndex', ''))
  
  # Split data into training and test sets
  x_trainval <- data[trainTestIndex$trainVal,]
  x_test <- data[trainTestIndex$test,]
  
  y_trainval <- data_y[trainTestIndex$trainVal]
  y_test <- data_y[trainTestIndex$test]
  
  # Remove data and garbage collect
  rm(data, data_y)
  gc()
  
  # Enable parallel processing
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  if(model = "rf"){
    print("starting training of random forest")
    # three folds and 10 repeats
    train_ctrl <- caret::trainControl(method = "repeatedcv", number = 3, repeats = 5, allowParallel = TRUE)
    tune_grid <- expand.grid( mtry = c(10,20,30,40))

    rf <- caret::train(x = x_trainval, y = as.factor(y_trainval), method = "rf",
                       trControl = train_ctrl, tuneGrid = tune_grid, ntree = 100)
    
    # save model
    savePath = glue(PATH_DB,"models/","rf_model.rds")
    print(glue("saving model to : ",savePath))
    saveRDS(rf, file = savePath)
  }
  
  if(model = "lr"){
    print("starting training of logistic regression")

    # fit model
    lr <- caret::train(x = x_trainval, y = as.factor(y_trainval), method = "glm", family = "binomial",trControl = train_ctrl)
    
    # save model
    savePath = glue(PATH_DB,"models/","lr_model.rds")
    print(glue("saving model to : ",savePath))
    saveRDS(lr, file = savePath)
  }
}

#rf_lr(model = "lr")
#rf_lr(model = "rf")


rf <- readRDS(paste(parquets_dir, "rf_model.rds", sep=""))
lr <- readRDS(paste(parquets_dir, "lr_model.rds", sep=""))