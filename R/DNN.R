
#############################################
#########    DNN NEURAL NETWORK    #########
#############################################

# set default flags
FLAGS <- flags(
  flag_numeric("N_input", 0),
  flag_boolean("equalWidths", TRUE),
  flag_numeric("dropout", 0.4),
  flag_numeric("lambda", 0.01),
  flag_boolean("normalization", FALSE),
  flag_numeric("lr", 0.01),
  flag_numeric("bs", 512),
  flag_numeric("epochs", 5),
  flag_numeric("verbose", 1),
  flag_string("activationHidden","relu"),
  flag_string("activationOut","sigmoid"),
  flag_string("loss","binary_crossentropy")
)
# model configuration
model <- keras_model_sequential() %>%
  layer_dense(units = floor(FLAGS$N_input), input_shape = N_input, activation = FLAGS$activationHidden, name = "layer_1",
              kernel_regularizer = regularizer_l2(FLAGS$lambda)) %>%
              layer_batch_normalization(center = FLAGS$normalization,scale = FLAGS$normalization) %>%
              layer_dropout(rate = FLAGS$dropout) %>%
  
  layer_dense(units = floor(FLAGS$N_input/ifelse(FLAGS$equalWidths,1,2)), activation = FLAGS$activationHidden, name = "layer_2",
              kernel_regularizer = regularizer_l2(FLAGS$lambda)) %>%
              layer_batch_normalization(center = FLAGS$normalization,scale = FLAGS$normalization) %>%
              layer_dropout(rate = FLAGS$dropout) %>%
  
  layer_dense(units = floor(FLAGS$N_input/ifelse(FLAGS$equalWidths,1,4)), activation = FLAGS$activationHidden, name = "layer_3",
              kernel_regularizer = regularizer_l2(FLAGS$lambda)) %>%
              layer_batch_normalization(center = FLAGS$normalization,scale = FLAGS$normalization) %>%
  
  layer_dense(units = 1, activation = FLAGS$activationOut, name = "layer_out") %>%
  compile(loss = FLAGS$loss, 
          metrics = "accuracy",
          optimizer = optimizer_adam(learning_rate = FLAGS$lr),
  )
# training and evaluation
fit <- model %>% fit(
  x = x_train_pca, y = y_train,
  validation_data = list(x_val_pca, y_val),
  epochs = FLAGS$epochs,
  batch_size = FLAGS$bs,
  verbose = FLAGS$verbose,
  callbacks = callback_early_stopping(monitor = "val_loss", patience = 10, restore_best_weights = TRUE)
)
# store accuracy on test set for each run
score <- model %>% evaluate(
  x_test_pca, y_test,
  verbose = FLAGS$verbose
)