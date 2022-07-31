####################################################
####################################################

# Functions to asses results of neural network tuning

####################################################
####################################################


####################################################
# Function to read NN Turning metrics from DB
####################################################
readTuningMetrics = function(asDataFrame = TRUE){
  metrics = read_metrics()
  if(asDataFrame){
    return(as.data.frame(metrics))
  }
  return(metrics)
}

############################################################
# Return top 3 performing models by validation accuracy
############################################################
getTop3Models = function(){
  
  metrics = read_metrics()
  # extract validation accuracy
  acc = sapply(metrics, "[[", "val_accuracy")
  loss = sapply(metrics, "[[", "val_loss")
  evaluation = sapply(metrics, "[[", "evaluation")
  
  val_res = data.frame(
    i = 1:length(metrics),
    val_accuracy = apply(acc, 2, max, na.rm = TRUE),
    val_loss =  apply(loss, 2, min, na.rm = TRUE),
    evaluation = evaluation[2,]
  )
  
  val_res = val_res %>% arrange(-val_accuracy)
  top3 = val_res[1:3,]
  
  return(top3)
}

############################################################
# Plot Leaning curves, top 3 model curves colored
############################################################
plotLearningCurves = function(){
  metrics = read_metrics()
  # extract validation accuracy
  acc = sapply(metrics, "[[", "val_accuracy")
  loss = sapply(metrics, "[[", "val_loss")
  evaluation = sapply(metrics, "[[", "evaluation")
  
  val_res = data.frame(
    i = 1:length(metrics),
    val_accuracy = apply(acc, 2, max, na.rm = TRUE),
    val_loss =  apply(loss, 2, min, na.rm = TRUE),
    evaluation = evaluation[2,]
  )
  
  val_res = val_res %>% arrange(-val_accuracy)
  top3 = val_res[1:3,]
  
  par(mfrow = c(1,2))
  plot_learning_curve(acc, col = adjustcolor("black", 0.3), ylim = c(0.8, 0.9),ylab = "Val accuracy", top = top3$i, topCol = 'skyblue')
  plot_learning_curve(loss, col = adjustcolor("black", 0.3), ylim = c(0.2, 0.6),ylab = "Val loss", top = top3$i, topCol = 'orange')

}

############################################################
# Plot accuracy / loss boxplots
############################################################
plotAccLossBoxplots = function(){
  metrics = read_metrics()
  # extract validation accuracy
  acc = sapply(metrics, "[[", "val_accuracy")
  loss = sapply(metrics, "[[", "val_loss")
  evaluation = sapply(metrics, "[[", "evaluation")
  
  val_res = data.frame(
    i = 1:length(metrics),
    val_accuracy = apply(acc, 2, max, na.rm = TRUE),
    val_loss =  apply(loss, 2, min, na.rm = TRUE),
    evaluation = evaluation[2,]
  )

  par(mfrow = c(1,2))
  boxplot(val_res$val_accuracy, outline=TRUE, xlab = 'validation accuracy')
  boxplot(val_res$val_loss, outline=TRUE, xlab = 'validation loss')
}


singleInstanceNNTraining = function(){
  bestModel = as.data.frame(metrics[[top3[1,]$i]])[1,5:16]
  model_neuralNetwork(getFilePath("data_lastPerCustomerID",".parquet"), tuning = FALSE, bestModelFlags = bestModel)
}



####################################################
# Auxiliary functions, not individually meaningful
####################################################
read_metrics = function(){
  path = glue(PATH_DB,"/NN_tuningRuns/")
  files = list.files(path)
  n = length(files)
  out = vector("list", n)
  for ( i in 1:n ) {
    dir = paste0(path,'/', files[i], "/tfruns.d/")
    out[[i]] = jsonlite::fromJSON(paste0(dir, "metrics.json"))
    out[[i]]$flags = jsonlite::fromJSON(paste0(dir, "flags.json"))
    out[[i]]$evaluation = jsonlite::fromJSON(paste0(dir, "evaluation.json"))
  }
  return(out)
}

plot_learning_curve = function(x, ylab = NULL, cols = NULL, top = NULL, topCol = "deepskyblue2", span = 0.4, ...){
  # to add a smooth line to points
  smooth_line = function(y) {
    x = 1:length(y)
    out = predict( loess(y ~ x, span = span) )
    return(out)
  }
  matplot(x, ylab = ylab, xlab = "Epochs", type = "n",...)
  grid()
  matplot(x, pch = 19, col = adjustcolor(cols, 0.3), add = TRUE)
  tmp = apply(x, 2, smooth_line)
  tmp = sapply( tmp, "length=", max(lengths(tmp)) )
  cl = rep(cols, ncol(tmp))
  cl[top] = topCol
  matlines(tmp, lty = 1, col = cl, lwd = 2)
}

