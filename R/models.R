
#############################################
######### Logistic Model for P_2  #########
#############################################

# Model of target vs P_2 as most highly corolated value to target variable
P2_logisticModel = function(){
  DF = readFromParquet(getFilePath("data_train",".parquet"))  %>% select(c(P_2,target))
  
  # Remove NA values from P_2
  DF = DF[is.na(DF$P_2) == 0,]
  trainTestIndex =  partition(1:nrow(y), p = c(train = 0.7, test= 0.2))
  
  trainval = dplyr::slice(DF, trainTestIndex$train)
  test  =  dplyr::slice(DF, trainTestIndex$test)
  
  # three folds and 10 repeats
  train_ctrl <- caret::trainControl(method = "repeatedcv", number = 3, repeats = 10, allowParallel = TRUE)
  
  # fit model
  lr <- caret::train(x = trainval[,'P_2'], y = as.factor(trainval$target), method = "glm", family = "binomial",trControl = train_ctrl)
  
  
  
  # predict test data
  # extract probability to revier/tune tau
  pred <- predict(lr, newdata = test[,'P_2'], type = "prob") %>% select('1') %>% unlist
  pred_obj <- prediction(pred, test$target)
  sens <- performance(pred_obj, "sens")
  spec <- performance(pred_obj, "spec")
  tau <- sens@x.values[[1]]
  sens_spec <- sens@y.values[[1]] + spec@y.values[[1]]
  best <- which.max(sens_spec)
  #predictions
  pred <- ifelse(pred > tau[best], 1, 0)
  
  tabfunc <- function(pred, obs){
    tab <-table(obs, pred)
    acc <- sum(diag(tab))/sum(tab)
    sens <- tab[2,2]/sum(tab[2,c(1,2)])
    spec <- tab[1,1]/sum(tab[1,c(1,2)])
    return(list(sensitivity = sens, specificity = spec, accuracy = acc))
    
  }
  
  # test results
  results = tabfunc(pred, test$target)
  
  #get amex metric
  results['amex'] = amex_metric(test$target, as.integer(pred))
  
  
  return(results)
}