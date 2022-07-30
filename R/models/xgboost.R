
source('/Users/root1/Documents/DAC_Project/R/config.R')

dataPath = getFilePath("data_lastPerCustomerID",".parquet")
print(glue('Reading data from ',dataPath))
data = readFromParquet(dataPath) 
#dataPath = getFilePath("data_lastPerCustomerID",".parquet")

#####################################
#####     Data Pre-processing       #####
#####################################
print(glue('Data Pre-processing'))

#partition target
data_y = data$target

# Remove customer_ID ,date and target
data = data  %>% dplyr::select(-c(customer_ID,S_2,target))

#Scale floats
for(i in 1:ncol(data)){
  if("double" == typeof(data[,i][[1]])){
    data[,i] = scale(data[,i][[1]])
  }
}

#One hot encode categoricals
catCols = intersect(c('B_30', 'B_38', 'D_114', 'D_116', 'D_117', 'D_120', 'D_126', 'D_66', 'D_68','D_63','D_64'), colnames(data))
for(i in 1:length(catCols)){
  data = cbind(data, data %>% select(catCols[i]) %>% mutate(across(catCols[i],factor)) %>% as.data.table %>% one_hot()) %>% select(-catCols[i])
}

N = nrow(data)

#Load data partition index
load(getFilePath('trainTestIndex',''))
trainTestIndex =  c(trainTestIndex, partition(trainTestIndex$trainVal, p = c(train = 0.7, val= 0.3)))

# Assign dataframes
x_train = data[trainTestIndex$trainVal,] %>% as.matrix()  
x_test = data[trainTestIndex$test,]  %>% as.matrix() 
rm(data)

#Assign lables
y_train = data_y[trainTestIndex$trainVal]
y_test = data_y[trainTestIndex$test]
rm(data_y)

gc()


#set parameter search grid
params = makeParamSet(
  makeDiscreteParam("booster",values = c("gbtree","gblinear")), 
  makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
  makeNumericParam("subsample",lower = 0.5,upper = 1), 
  makeNumericParam("colsample_bytree",lower = 0.5,upper = 1)
)


xgb.train = xgb.DMatrix(data=x_train, label=y_train)
xgb.test = xgb.DMatrix(data=x_test, label=y_test)


amexMetric = function(preds, dtrain){
  target = as.numeric(xgboost::getinfo(dtrain, "label"))
  predicion <- as.numeric(preds)
  amex = amex_metric(target, predicion)
  return(list(name = 'amex', value = amex , higher_better = TRUE))
}

library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())

xgb.fit=xgb.train(
  data=xgb.train,
  nthread = 6,
  nrounds = 100, 
  nfold = 5,
  par.set = params,
  objective="binary:logistic",
  watchlist = list(train=xgb.train, test=xgb.test),
  eval_metric=amexMetric,
  maximize = TRUE,
  verbose=1
  
)


pred = as.numeric(predict(xgb.fit, x_test) > 0.5)

amex_metric(y_test,res)



amex_metric(y_test,res)
