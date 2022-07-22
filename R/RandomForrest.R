
library(keras)
library(magrittr)
library(tidyverse)
library(data.table)
library(dplyr)
library(keras)
library(randomForest)


###### sample set up of data ######
# creates train, val and test data for models


setwd("~/ACM40960 - Projects in Maths Modelling/database")
# create data
labels <- read.csv("train_labels.csv", header =T,nrows=10000)
data <- read.csv("cleandata.csv", header =T, nrows=50000)

#merge
data <- merge(data, labels, by = "customer_ID")

# date clean (optional)
data <- date_clean(data)

# Set NA's to 0
data[is.na(data)] <- 0

# shuffle
data <- data[sample(1:nrow(data)), ]

# NOTE
# subset to 50 columns for efficiency
data <- data[, -c(50:166)]

# create predict and target data
pred <- data[ , -which(names(data) %in% c( 'customer_ID', 'target'))] 
target <- as.factor(data$target)

# split the data linearly as it was shuffled/randomised above
n <- nrow(pred)
tr <- n*(.7) 
va <- n*(.2)
te <- n*.1

train <- 1:tr
val <- tr:(tr+va)
test <- (tr+va):n

pred_train <- pred[train,]
pred_val <- pred[val,]
pred_test <- pred[test,]

target_train <- target[train]
target_val <- target[val]
target_test <- target[test]


###### tune number of trees ######
# returns number of tree to minimize the error on validation data

rf_tuning <- function( pred_train , target_train ,
                       pred_val , target_val){
  
  return( which.min(randomForest(x = pred_train , y=target_train, 
                                 xtest = pred_val, ytest = target_val)$test$err.rate[,1] ) )
  
}

ntrees <- rf_tuning( pred_train = pred_train, target_train = target_train,
           pred_val = pred_val , target_val = target_val)

ntrees

###### evaluate on test data ######
# returns sensitivity, specificity and accuracy

rf_eval <- function( pred_train, target_train,
                     pred_test, target_test,
                     ntrees){
  
  # run RF
  # test data confusion matrix
  conf <- randomForest(x = pred_train , y=target_train,
                       xtest = pred_test, ytest = target_test,
                       ntree = as.integer(x = ntrees))$test$confusion
  # calculate sensitivity, specificity and accuracy
  # columns are predicted values
  # rows are observed values
  sens <- conf[2,2]/sum(conf[2,c(1,2)])
  spec <- conf[1,1]/sum(conf[1,c(1,2)])
  acc <- sum(diag(conf))/ sum(conf[c(1,2),c(1,2)])
  # return results as list
  results <- list(sens, spec, acc)
  names(results) = c("sens", "spec", "acc")
  
  return( results)
  
}

# evaluate using training data or training and validation?
result <- rf_eval( pred_train = rbind(pred_train,pred_val), target_train = c(target_train,target_val),
                   pred_test = pred_test , target_test = target_test,
                   ntrees = ntrees)

result


###### loop over 500000 obs ######

# column names for skipped rows etc.
colnames_data <- colnames(read.csv("cleandata.csv", header =T, nrows=1))
colnames_label <- colnames(read.csv("train_labels.csv", header =T, nrows=1))

# labels
labels <- read.csv("train_labels.csv", header =T,nrows=50000)

# dataframe to store results
df_result <- data.frame(matrix(0, nrow = 10, ncol = 4))
colnames(df_result) <- c("ntrees","sens", "spec", "acc")

# loop over 10 with 50000 obs
for (i in 1:10){
  
  # create data
  data <- read.csv("cleandata.csv", header =F, nrows=50000, skip = 1 + ((i-1)*50000))
  colnames(data) <- colnames_data
  
  #merge
  data <- merge(data, labels, by = "customer_ID")
  
  # Set NA's to 0
  data[is.na(data)] <- 0
  
  # shuffle
  data <- data[sample(1:nrow(data)), ]
  
  # subset to 50 columns for efficiency
  data <- data[, -c(50:166)]
  
  # create predict and target data
  pred <- data[ , -which(names(data) %in% c( 'customer_ID', 'target'))] 
  target <- as.factor(data$target)
  
  n <- nrow(pred)
  tr <- n*(.7) 
  va <- n*(.2)
  te <- n*.1
  
  # split the data linearly as it was shuffled/randomised above
  train <- 1:tr
  val <- tr:(tr+va)
  test <- (tr+va):n
  
  pred_train <- pred[train,]
  pred_val <- pred[val,]
  pred_test <- pred[test,]
  
  target_train <- target[train]
  target_val <- target[val]
  target_test <- target[test]
  
  # get number of trees by tuning
  ntrees <- rf_tuning( pred_train = pred_train, target_train = target_train,
                       pred_val = pred_val , target_val = target_val)
  
  # evaluate on test data
  result <- rf_eval( pred_train = rbind(pred_train,pred_val), target_train = c(target_train,target_val),
                     pred_test = pred_test , target_test = target_test,
                     ntrees = ntrees)
  
  # return number of trees and performance of model
  df_result[i,] <- c(ntrees, result$sens, result$spec, result$acc)
  
  
  cat(i, "\n")
}

df_result


# sample results
# ntrees      sens      spec       acc
# 1     312 0.9196911 0.9643821 0.9528094
# 2     305 0.8851133 0.9702523 0.9492102
# 3     326 0.8982786 0.9653505 0.9482104
# 4     430 0.8986750 0.9661108 0.9488102
# 5     454 0.8841270 0.9716653 0.9496101
# 6     350 0.8841310 0.9755906 0.9538092
# 7     495 0.9029203 0.9638457 0.9484103
# 8     108 0.8634868 0.9682959 0.9428114
# 9     469 0.8956805 0.9753577 0.9558088
# 10    445 0.8936170 0.9762154 0.9568086




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
