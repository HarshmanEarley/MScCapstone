
library(keras)
library(magrittr)
library(tidyverse)
library(data.table)
library(dplyr)
library(keras)
library(randomForest)


setwd("~/ACM40960 - Projects in Maths Modelling/database")
# create data
labels <- read.csv("train_labels.csv", header =T,nrows=2400)
data <- read.csv("cleandata.csv", header =T, nrows=24000)

#merge
data <- merge(data, labels, by = "customer_ID")

# date clean (optional)
data <- date_clean(data)

# Set NA's to 0
is.na(data) %>% colSums() %>% sum()
data[is.na(data)] <- 0

colnames(data)

# create predict and target data
pred <- data[ , -which(names(data) %in% c( 'customer_ID', 'target'))] 
target <- as.factor(data$target)

# subset to 50 columns for efficiency
# REMOVE LATER
pred  <- pred[, 1:50]


###################### RF - tuning number of trees- 15 iterations - 30% validation data ######################

B = 15
n <- nrow(pred)
p <- nrow(pred)*(4/13)

tun_res <- matrix(0, nrow = B, ncol = 2)


for (i in 1:B){
  print(i)
  test <- sample(1:n, p, replace = FALSE)
  train <- setdiff(1:n, test)
  
  pred_test <- pred[test,]
  pred_train <- pred[train,]
  
  target_test <- target[test]
  target_train <- target[train]
  
  err_rate <-  randomForest(x = pred_train , y=target_train, 
                            xtest = pred_test, ytest = target_test)$test$err.rate[,1]
  tun_res[i,] <- c(which.min(err_rate), min(err_rate))
  
}

tun_res
summary(tun_res[,1])
# 250 - 350?

num_tree = 350

###################### simple RF - no test data ######################

RF_simple <- function(pred = pred, target = target){
  # run RF
  conf <- randomForest(x = pred , y=target )$confusion
  # calculate sensitivity, specificity and accuracy
  # columns are predicted values
  # rows are observed values
  sens <- conf[2,2]/sum(conf[2,c(1,2)])
  spec <- conf[1,1]/sum(conf[1,c(1,2)])
  acc <- sum(diag(conf))/ sum(conf[c(1,2),c(1,2)])
  # return results as list
  results <- list(sens, spec, acc)
  names(results) = c("sens", "spec", "acc")
  return(results)
}

res <- RF_simple(pred = pred, target = target)
res



###################### RF - split data into random 30% test data ######################

RF_test1 <- function(pred = pred, target = target){
  n <- nrow(pred)
  p <- nrow(pred)*(4/13) # test proportion

  test <- sample(1:n, p, replace = FALSE)
  train <- setdiff(1:n, test)
  
  pred_test <- pred[test,]
  pred_train <- pred[train,]
  
  target_test <- target[test]
  target_train <- target[train]
  
  # run RF
  conf <- randomForest(x = pred_train , y=target_train, 
                       xtest = pred_test, ytest = target_test)$test$confusion
  # calculate sensitivity, specificity and overall accuracy
  sens <- conf[2,2]/sum(conf[2,c(1,2)])
  spec <- conf[1,1]/sum(conf[1,c(1,2)])
  acc <- sum(diag(conf))/ sum(conf[c(1,2),c(1,2)])
  # return results as list
  results <- list(sens, spec, acc)
  names(results) = c("sens", "spec", "acc")
  return(results)
}

res <- RF_test1(pred = pred, target = target)
res

###################### RF - data already split into test and train ######################

# sample split
n <- nrow(pred)
p <- nrow(pred)*(4/13) 
test <- sample(1:n, p, replace = FALSE)
train <- setdiff(1:n, test)
pred_test <- pred[test,]
pred_train <- pred[train,]
target_test <- target[test]
target_train <- target[train]


RF_test2 <- function(pred_train, target_train, pred_test, target_test){
  
  # run RF
  conf <- randomForest(x = pred_train , y=target_train, 
                       xtest = pred_test, ytest = target_test)$test$confusion
  # calculate sensitivity, specificity and overall accuracy
  sens <- conf[2,2]/sum(conf[2,c(1,2)])
  spec <- conf[1,1]/sum(conf[1,c(1,2)])
  acc <- sum(diag(conf))/ sum(conf[c(1,2),c(1,2)])
  # return results as list
  results <- list(sens, spec, acc)
  names(results) = c("sens", "spec", "acc")
  return(results)
}

res <- RF_test2(pred_train = pred_train, target_train = target_train, pred_test = pred_test, target_test = target_test)
res








###################### time cleaning ###########################

setwd("~/ACM40960 - Projects in Maths Modelling/database")
headings <- colnames(read.csv("cleandata.csv", header =T, nrows=1))
headings_lab <- colnames(read.csv("train_labels.csv", header =T, nrows=1))
# create data
labels <- read.csv("train_labels.csv", header =T,nrows=5000, col.names = headings_lab)

data <- read.csv("cleandata.csv", header =T, nrows=12000, col.names = headings)



# merge data and labels
data <- merge(data, labels, by = "customer_ID")


# clean date (S_2) columns
date_clean <- function(DF){
  
  # convert to date object
  DF$S_2 <- as.Date(as.character(DF$S_2), format = "%Y-%m-%d")
  
  # remove customers who began after first month (incomplete set of 13 months)
  too_late <- unique(DF$customer_ID)[aggregate(x = DF$S_2, 
                                               by = list(unique.values = DF$customer_ID), 
                                               FUN = min)[,2] > "2017-03-31"]
  # remove customers who ended before last month (incomplete months)
  too_early <- unique(DF$customer_ID)[aggregate(x = DF$S_2, 
                                                by = list(unique.values = DF$customer_ID), 
                                                FUN = max)[,2] < "2018-03-01"]
  # drop customers
  DF <- DF[ -which(DF$customer_ID %in% c(too_late, too_early)), ]
  # convert date to month starting with March 2017 as 0
  DF$S_2 <- ((DF$S_2 %>% year() - 2017)*12 + DF$S_2 %>% month() - 3)
  
  # insert missing rows with previous months values
  for (i in 2:(nrow(DF)-1)){
    if ( (DF$S_2[i] != (DF$S_2[i-1] + 1)) & (DF$S_2[i-1] != 12)){ #test if month is missing
      DF[seq(i+1,nrow(DF)+1),] <- DF[seq(i,nrow(DF)),] # shifts remaining data down one row
      DF[i,] <- DF[i-1,] # copies row above into new place
      DF$S_2[i] <- (DF$S_2[i-1] + 1) # resets month
    }
  }
  # return cleaned data
  return(DF)
}

dim(data)
data <- date_clean(data)
dim(data)

dim(data)[1]/13
unique(data$customer_ID) %>% length
