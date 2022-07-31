####################################
# Plots for Exploritory Data Analysis
##############################


#Plot variable catagory frequencies
plot_variableCatagoryFrequencies = function(){
  df = readFromParquet(getFilePath("train_data",".parquet")) 
  freqs =  as.data.frame(sort(table(sapply(str_split(colnames(df)[2:190], "_"), "[[", 1))/length(colnames(df))))
  ggplot(data=freqs, aes(x=Var1, y=Freq, fill = c('B','D','P','R','S'))) +
    geom_bar(stat="identity") + geom_col(position = "identity") +
    xlab("") +
    ylab("Frequency") +
    ggtitle("Frequency of Variable Catagories")
}

#Plot corolation of each feature to the target variable
plot_corolationToTarget = function(){
  cors = ""
  cors = tryCatch(readRDS(getFilePath("corolationToTarget","")))
  
  if(length(cors)==1){
    #Calculate corolations to target variable
    df = readFromParquet(getFilePath("train_data",".parquet")) 
    cors = data.frame(x = colnames(df %>% removeNonNumerics %>% select(-c(customer_ID,target))), y = 'target')
    cors$variableCategories = substr(cors$x, start = 1, stop = 1)
    for(i in 1:nrow(cors)){
      cors[i,'cor'] = cor(df[,cors[i,1]], df[,cors[i,2]], method = "pearson", use = 'pairwise.complete.obs')
    }
    
    #Save to cache
    saveRDS(cors, glue(PATH_DB,"cache/corolationToTarget"))
  }
  
  # Plot
  ggplot(cors, aes(x = reorder(x, -cor), y = cor, fill = variableCategories)) + geom_col(position = "identity") +
    theme(axis.text.x=element_blank()) +
    xlab("Variable") +
    ylab("Correlation To Target") +
    ggtitle("Correlation to Target")
}

#Distributions of P_2 variables by the target variables
plot_p2DistributionByTarget = function(){
  df = readFromParquet(getFilePath("train_data",".parquet")) 
  ggplot(df,aes(x=P_2))+geom_histogram(col="skyblue")+facet_grid(~target) +     
    ggtitle("Distribution of P_2 by Target")
}


#Plot histogram of NA frequencies
plot_NAFrequencies = function(){
  naCounts = as.data.frame(getNACounts("train_data")/train_data_N)
  naCounts$variableCategories = substr(rownames(naCounts), start = 1, stop = 1)
  colnames(naCounts) = c('coln','variableCategories')
  
  ggplot(naCounts, aes(x = reorder(rownames(naCounts), -coln), y = coln, fill = variableCategories)) + geom_col(position = "identity") +
    theme(axis.text.x=element_blank()) +
    xlab("Variable") +
    ylab("% NA") +
    geom_hline(yintercept =0.1) +
    ggtitle("Variable Missing Observations")
}

#Plot varience of each predictor variable
plot_VariableVariences = function(){
  vars = getVariance("train_data")
  vars = data.frame(variable = colnames(vars), vars = unlist(vars))
  vars$variableCategories = substr(rownames(vars), start = 1, stop = 1)
  
  # Remove high outliers 
  vars = vars[vars$vars < 1,]
  
  ggplot(vars, aes(x = reorder(variable, -vars), y = vars, fill = variableCategories)) + geom_col(position = "identity") +
    theme(axis.text.x=element_blank()) +
    xlab("Variable") +
    ylab("% NA") +
    ggtitle("Variable Variance")
}

#Plot histogram a sample of columns know to have had noise injected
plot_noiseAddedColumns = function(){
  df = readFromParquet(getFilePath("train_data",".parquet")) 
  noiseCols = c('S_6', 'D_111', 'B_16')
  par(mfrow = c(3,1))
  for(i in 1:length(noiseCols)){
    hist(df[,noiseCols[i]][[1]],  main =  noiseCols[i], xlab = "", breaks = 100000, col = 'skyblue')
  }
}