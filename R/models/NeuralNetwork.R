model_neuralNetwork = function(dataPath, tuning = FALSE, bestFlags = NA){
  
  #dataPath = getFilePath("data_lastPerCustomerID",".parquet")
  data = readFromParquet(dataPath) 

  
  #####################################
  #####     Data Processing       #####
  #####################################
  
  #partition target
  data_y = data$target
  
  # Remove customer_ID ,date and target
  data = data  %>% dplyr::select(-c(customer_ID,S_2,target))
  
  #Scale floats
  for(i in 1:length(numericCols)){
    if("double" == typeof(data[,i][[1]])){
      data[,i] = scale(data[,i][[1]])
    }
  }
  
  #One hot encode categoricals
  catCols = intersect(c('B_30', 'B_38', 'D_114', 'D_116', 'D_117', 'D_120', 'D_126', 'D_66', 'D_68','D_63','D_64'), colnames(data))
  for(i in 1:length(catCols)){
    data = cbind(data, data %>% select(catCols[i]) %>% mutate(across(catCols[i],factor)) %>% as.data.table %>% one_hot()) %>% select(-catCols[i])
  }
  
  #Hard replace all remaining NA with zero
  data = data %>% mutate_all(~replace(., is.na(.), 0))
  
  N = nrow(data)
  
  #Load data partition index
  load(getFilePath('trainTestIndex',''))
  trainTestIndex =  c(trainTestIndex, partition(trainTestIndex$trainVal, p = c(train = 0.7, val= 0.3)))
  
  # Assign dataframes
  x_train = data[trainTestIndex$train,] %>% as.matrix()  
  x_val = data[trainTestIndex$val,]   %>% as.matrix() 
  x_test = data[trainTestIndex$test,]  %>% as.matrix() 
  rm(data)
  
  #Assign lables
  y_train = data_y[trainTestIndex$train]
  y_val = data_y[trainTestIndex$val]
  y_test = data_y[trainTestIndex$test]
  rm(data_y)
  
  gc()
  
  #####################################
  #####            PCA            #####
  #####################################
  #Fit PCA
  pca_fit = prcomp(x_train)
  
  #Get variance and select number of PCA components needed to capture for 95% of variance
  pca_var <- pca_fit$sdev^2
  pca_cumVar <- cumsum(pca_var / sum(pca_var))
  pca_comps = which(pca_cumVar >= 0.95) %>% min
  
  x_train_pca <- pca_fit$x[,1:pca_comps]
  x_val_pca <- x_val %*% pca_fit$rotation[,1:pca_comps]
  x_test_pca <- x_test %*% pca_fit$rotation[,1:pca_comps]

  
  #########################################
  #####     Neural Network Tuning     ####
  ########################################  
  if(tuning == TRUE){
    tuning_run(getFilePath("DNN",".R", checkDBOnly = FALSE),
             runs_dir = glue(PATH_DB,"NN_tuningRuns"),
             flags = list(
               N_input =  ncol(x_train_pca),
               dropout = c(0.001,0.01,0.1),
               lambda =  c(0,0.001,0.01), #l2 reg
               normalization = c(TRUE,FALSE),
               lr = c(0.001,0.01),
               bs = round(nrow(x_train_pca)*0.1),
               epochs = 200,
               verbose = 1,
               activationHidden = c('relu',"sigmoid"),
               activationOut = 'sigmoid',
               loss = "binary_crossentropy"
             ),
             confirm = FALSE
  )
 }
  
 if(!is.na(bestFlags)){
   modelRes = tuning_run(getFilePath("DNN",".R", checkDBOnly = FALSE),
                         runs_dir = glue(PATH_DB,"NN_bestModel"),
                         flags = list(
                           N_input =  ncol(x_train_pca),
                           dropout = topModel$flags.dropout,
                           lambda =  topModel$flags.lambda, #l2 reg
                           normalization = topModel$flags.normalization,
                           lr = topModel$flags.lr,
                           bs = topModel$flags.bs,
                           epochs = 200,
                           verbose = 1,
                           activationHidden = topModel$flags.activationHidden,
                           activationOut = topModel$flags.activationOut,
                           loss = topModel$flags.loss
                         ),
                         confirm = FALSE
   )
 }
  
}