
############################################
#####    Neural Network Model Wrapper #####
############################################

model_NeuralNetwork = function(dataPath, tuning = FALSE, bestModelFlags = NA){
  
  # laod data
  print(glue('Reading data from ',dataPath))
  data = readFromParquet(dataPath) 
  
  #dataPath = getFilePath("data_lastPerCustomerID",".parquet")
  
  #########################################
  #####     Data Pre-processing       #####
  #########################################
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
  
  #One hot encode categoricals / discrete variables
  catCols = intersect(unique(c('B_30', 'B_38', 'D_114', 'D_116', 'D_117', 'D_120', 'D_126', 'D_66', 'D_68','D_63','D_64',getNoiseIntervals()$column)), colnames(data))
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
  
  N_input = ncol(x_train)

  #########################################
  #####     Neural Network Tuning     ####
  ########################################  
  if(tuning == TRUE){
    print(glue('Tuning Neural Network'))
    tuning_run(getFilePath("DNN",".R", checkDBOnly = FALSE),
             runs_dir = glue(PATH_DB,"NN_tuningRuns_final"),
             flags = list(
               N_input =  ncol(x_train),
               dropout = c(0,0.25,0.5),
               equalWidths = c(TRUE,FALSE),
               lambda =  c(0,0.001,0.0001), #l2 reg
               normalization = c(TRUE,FALSE),
               lr = 0.0001,
               bs = 1024,
               epochs = 100,
               verbose = 0,
               activationHidden = 'relu',
               activationOut = 'sigmoid',
               loss = "binary_crossentropy"
             ),
             confirm = FALSE
  )
 }
  
 if(!is.na(bestModelFlags)){
   print(glue('Training Neural Network on best flags'))
   FLAGS <- flags(
     flag_numeric("N_input", ncol(x_train)),
     flag_numeric("dropout", bestModelFlags$flags.dropout),
     flag_boolean("equalWidths", bestModelFlags$flags.equalWidths),
     flag_numeric("lambda", bestModelFlags$flags.lambda),
     flag_boolean("normalization", bestModelFlags$flags.normalization),
     flag_numeric("lr", bestModelFlags$flags.lr),
     flag_numeric("bs", bestModelFlags$flags.bs),
     flag_numeric("epochs", bestModelFlags$flags.epochs),
     flag_numeric("verbose", bestModelFlags$flags.verbose),
     flag_string("activationHidden",bestModelFlags$flags.activationHidden),
     flag_string("activationOut",bestModelFlags$flags.activationOut),
     flag_string("loss",bestModelFlags$flags.loss)
   )
   
   model <- keras_model_sequential() %>%
     layer_dense(units = floor(N_input), input_shape = N_input, activation = FLAGS$activationHidden, name = "layer_1",
                 kernel_regularizer = regularizer_l2(FLAGS$lambda)) %>%
     layer_batch_normalization(center = FLAGS$normalization,scale = FLAGS$normalization) %>%
     layer_dropout(rate = FLAGS$dropout) %>%
     
     layer_dense(units = floor(N_input/ifelse(FLAGS$equalWidths,1,2)), activation = FLAGS$activationHidden, name = "layer_2",
                 kernel_regularizer = regularizer_l2(FLAGS$lambda)) %>%
     layer_batch_normalization(center = FLAGS$normalization,scale = FLAGS$normalization) %>%
     layer_dropout(rate = FLAGS$dropout) %>%
     
     layer_dense(units = floor(N_input/ifelse(FLAGS$equalWidths,1,4)), activation = FLAGS$activationHidden, name = "layer_3",
                 kernel_regularizer = regularizer_l2(FLAGS$lambda)) %>%
     layer_batch_normalization(center = FLAGS$normalization,scale = FLAGS$normalization) %>%
     
     layer_dense(units = 1, activation = FLAGS$activationOut, name = "layer_out") %>%
     compile(loss = FLAGS$loss, 
             metrics = "accuracy",
             optimizer = optimizer_adam(learning_rate = FLAGS$lr ),
     )
   # training and evaluation
   fit <- model %>% fit(
     x = x_train, y = y_train,
     validation_data = list(x_val, y_val),
     epochs = FLAGS$epochs,
     batch_size = FLAGS$bs,
     verbose = FLAGS$verbose,
     callbacks = callback_early_stopping(monitor = "val_loss", patience = 10, restore_best_weights = TRUE)
   )
   
   #Get predicted lables
   predictions = model %>% predict(x_test)
   NN = list(model = model, target = y_test, prediction = predictions)
   
   # save model
   savePath = glue(PATH_DB,"models/","NN_model.rds")
   print(glue("saving model to : ",savePath))
   saveRDS(NN, file = savePath)
   
   return(NN)
 }
  
}