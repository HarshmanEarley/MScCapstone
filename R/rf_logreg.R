
gc()

source('C:\\Users\\sidne\\Documents\\GitHub\\DAC_Project\\R\\config.R')

library(arrow)
library(magrittr)
library(tidyverse)
library(keras)
library(splitTools)
library(randomForest)
library(ROCR)
library(glue)
library(caret)

readFromParquet = function(filePath){
  ads = arrow::open_dataset(sources =  filePath)
  ## Create a scanner
  scan = Scanner$create(ads)
  ## Load it as n Arrow Table in memory
  at = scan$ToTable()
  ## Convert it to an R data frame
  as.data.frame(at)
}


# parquets_dir <- getFilePath('data_lastPerCustomerID','.parquet')

parquets_dir <- "C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/parquets/"


rf_lr <- function(path){
  
  #data <- readFromParquet()
  data <-readFromParquet(paste(path, "data_lastPerCustomerID.parquet", sep=""))
  
  #partition target
  data_y = data$target
  
  # Remove customer_ID and target
  data = data  %>% dplyr::select(-c(customer_ID,target, S_2)) 

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
  
  
  load(paste(path,'trainTestIndex', sep=""))

  x_trainval <- data[trainTestIndex$trainVal,]
  x_test <- data[trainTestIndex$test,]
  
  y_trainval <- data_y[trainTestIndex$trainVal]
  y_test <- data_y[trainTestIndex$test]
  
  rm(data, data_y)
  
  cl <- makeCluster(8)
  registerDoParallel(cores=4)
  
  # three folds and 10 repeats
  train_ctrl <- caret::trainControl(method = "repeatedcv", number = 3, repeats = 5, allowParallel = TRUE)
  
  tune_grid <- expand.grid( mtry = c(10,20,30,40))
  
  print("starting lr")
  
  rf <- caret::train(x = x_trainval, y = as.factor(y_trainval), method = "rf",
                     trControl = train_ctrl, tuneGrid = tune_grid, ntree = 100)
  
  
  saveRDS(rf, file = paste(path, "rf_model.rds", sep=""))
  print("starting lr")

  # fit model
  lr <- caret::train(x = x_trainval, y = as.factor(y_trainval), method = "glm", family = "binomial",trControl = train_ctrl)
  
  saveRDS(lr, file = paste(path, "lr_model.rds", sep=""))
  
}


rf_lr(parquets_dir)


rf <- readRDS(paste(parquets_dir, "rf_model.rds", sep=""))
lr <- readRDS(paste(parquets_dir, "lr_model.rds", sep=""))






############################################################################################################################################
############################################################################################################################################



parquets_dir
colnames(data)


data <-readFromParquet(paste(parquets_dir, "data_lastPerCustomerID.parquet", sep=""))
dim(data)


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
load(paste(parquets_dir,'trainTestIndex',sep=''))
#trainTestIndex$trainVal
#trainTestIndex$test

x_trainval <- data[trainTestIndex$trainVal,]
x_test <- data[trainTestIndex$test,]

y_trainval <- data_y[trainTestIndex$trainVal]
y_test <- data_y[trainTestIndex$test]

rm(data, data_y)


#sensitivity specificity and accuracy
tabfunc <- function(pred, obs){
  tab <-table(obs, pred)
  acc <- sum(diag(tab))/sum(tab)
  sens <- tab[2,2]/sum(tab[2,c(1,2)])
  spec <- tab[1,1]/sum(tab[1,c(1,2)])
  return(c(sens, spec, acc))
  
}

# RANDOM FOREST
###################################################################################





rf_model <- readRDS("C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/parquets/rf_model.rds")
lr_model <- readRDS("C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/results/lr_model.rds")

pred_nn <- readRDS("C:/Users/denis/Documents/GitHub/DAC_Project/db/models/model_NN")
pred_rf <- predict(rf_model, newdata = x_test, type = "prob")
pred_lr <- predict(lr_model, newdata = x_test, type = "prob")
pred_p2 <- predict(P_2_model, newdata = x_test, type = "prob")

pred_rf <- pred_rf$`1`
pred_lr <- pred_lr$`1`
pred_p2 <- pred_p2$`1`
pred_nn <- as.vector(pred_nn)


pred_obj <- prediction(pred_p2, y_test)

sens <- performance(pred_obj, "sens")
spec <- performance(pred_obj, "spec")
tau <- sens@x.values[[1]]
sens_spec <- sens@y.values[[1]] + spec@y.values[[1]]
best <- which.max(sens_spec)
plot(tau, sens_spec, type = "l")
points(tau[best], sens_spec[best], pch = 19, col = adjustcolor("darkorange2", 0.5))
tau[best] # optimal tau

load("C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/results/best_tau.rda")
tau_best

library(ROCR)
library(ggpubr)

results <- readRDS("C:/Users/denis/Documents/GitHub/DAC_Project/Paper/results.rds")

results

AUC <- data.frame(lab,val,meas)

lab <- c("Logistic Regression","Logistic Regression (P_2 only)","Neural Network","Random Forest")
meas <- c("AUC","AUC","AUC","AUC") 
val <- c(0.9527,0.9168,0.9547,0.9187)



results <- rbind(results,AUC)
saveRDS(results , file = "C:/Users/denis/Documents/GitHub/DAC_Project/Paper/results.rds")

rf_rocobj <- roc(y_test, pred_rf)
rf_auc <- round(auc(y_test, pred_rf),4)
lr_rocobj <- roc(y_test, pred_lr)
lr_auc <- round(auc(y_test, pred_lr),4)
p2_rocobj <- roc(y_test, pred_p2)
p2_auc <- round(auc(y_test, pred_p2),4)
nn_rocobj <- roc(y_test, as.numeric(pred_nn$prediction))
nn_auc <- round(auc(y_test, as.numeric(pred_nn$prediction)),4)



#create ROC plot
p1 <- ggroc(lr_rocobj, colour = '#440154', size = 2) +
    ggtitle(paste0('Logistic Regression ', '(AUC = ', lr_auc, ')')) +
  theme_bw() +
  xlab("Specificity") +
  ylab("Sensitivity")
p2 <- ggroc(p2_rocobj, colour = '#31688e', size = 2) +
  ggtitle(paste0('LR - P_2 only ', '(AUC = ', p2_auc, ')')) +
  theme_bw()+
  xlab("Specificity") +
  ylab("Sensitivity")
p3 <- ggroc(nn_rocobj, colour = '#35b779', size = 2) +
  ggtitle(paste0('Neural Network ', '(AUC = ', nn_auc, ')')) +
  theme_bw()+
  xlab("Specificity") +
  ylab("Sensitivity")
p4 <- ggroc(rf_rocobj, colour = '#fde725', size = 2) +
  ggtitle(paste0('Random Forest ', '(AUC = ', rf_auc, ')')) +
  theme_bw()+
  xlab("Specificity") +
  ylab("Sensitivity")


ggarrange(p1, p2,p3,p4,
          ncol = 2, nrow = 2)


# LOG REG
###################################################################################

# three folds and 10 repeats
train_ctrl <- caret::trainControl(method = "repeatedcv", number = 3, repeats = 10, allowParallel = TRUE)

# fit model
P_2 <- x_trainval[,1, drop=FALSE]
P_2_model <- caret::train(x = P_2, y = as.factor(y_trainval), method = "glm", family = "binomial",trControl = train_ctrl)

P_2_model 

saveRDS(P_2_model, file = "C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/parquets/P_2_model.rds")

colnames(x_trainval)


#sensitivity specificity and accuracy
tabfunc <- function(pred, obs){
  tab <-table(obs, pred)
  acc <- sum(diag(tab))/sum(tab)
  sens <- tab[2,2]/sum(tab[2,c(1,2)])
  spec <- tab[1,1]/sum(tab[1,c(1,2)])
  return(c(sens, spec, acc))
  
}

results <- readRDS("C:/Users/denis/Documents/GitHub/DAC_Project/Paper/results.rds")

res <- data.frame(results)
sum(res$y_test != pred_nn$target)


results$meas[1:4] <- "Specificity"
results$meas[5:8] <- "Sensitivity"

saveRDS(results, file = "C:/Users/denis/Documents/GitHub/DAC_Project/Paper/results.rds")
