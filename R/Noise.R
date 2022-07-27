estimateInterval_loadData = function(columns,cachePath){
  print("Running estimateInterval_loadData")
  #read in full data from parquet
  DF = readFromParquet(glue(PATH_DB,"parquet"))
  cols_N = length(columns)
  
  #For each column call estimateInterval and save result
  for(i in 1:cols_N){
    print(glue("getting interval for ",columns[i]))
    
    # call estimateInterval for column
    vec = DF[,columns[i]][[1]] %>% na.omit
    res = estimateInterval(vec)
    
    ## Map NA results to NA list
    if(all(is.na(res))){
      res = list(column = columns[i], interval = NA, confidence = NA)
    }
    
    # save to cache
    INTERVALS <<- INTERVALS %>% add_row(column = columns[i], interval = res$interval, confidence = res$confidence)
    save(INTERVALS, file = cachePath)
  }
  
  INTERVALS
}


estimateInterval = function(vec){
  nums = c()
  ks_p = c()
  i = 1
  nums[1] = min(vec)
  max_vec = max(vec)
  
  #While below max value, find min value in chunks of 0.01
  while(max_vec > (last(nums) + 0.01)){
    boundMin = ifelse(i == 1, min(vec), min(vec[!vec <= (nums[i-1]+ 0.01)]))
    bound = vec[between(vec, boundMin, boundMin + 0.01)]
    nums[i] = min(bound)
    
    #Use Kolmogorov-Smirnov test to check remainders are uniform(0, 0.01)
    #H0: bound comes from unif dist
    ks_p[i] = ks.test(bound %% 0.01,"punif",0, 0.01)$p
    i = i+1
  }
  
  #Get GCD of two middle sequential values, take that to be our interval to nearest rational 
  intervals = c()
  for(k in 1:(length(nums)-1)){
    inter =  nums[k+1] - nums[k] 
    
    numDenom =  as.numeric(str_split(as.character(fractions(inter,  max.denominator = 100)), "/", simplify = TRUE))
    
    if(length(numDenom) == 1){
      intervals[k] = numDenom
      next
    }
    
    intervals[k] = (numDenom[1]/numDenom[2])
  }
  
  tableInter = table(intervals)
  candidate = as.numeric(names(tableInter[which(tableInter == max(tableInter))]))
  
  if(!length(candidate) == 1){ #Return NA if we cannot reach a consensus
    return(NA)
  }
  
  rem = vec %% candidate
 
  return(list(interval = candidate, confidence = sum(ks_p > 0.05)/length(ks_p)))
}


intervals_main = function(){  
  cachePath = glue(PATH_DB,"cache/intervals")
  print(glue("Running intervals_main, saving to ",cachePath))
  
  #read colnames from first row of training data
  columns = read_csv(getFilePath("train_data"), n_max = 1, show_col_types = FALSE)  %>% removeNonNumerics() %>% dplyr::select(-customer_ID) %>% colnames()
  INTERVALS <<- data.frame(column = character(), interval = numeric(), confidence = numeric())
  
  #Try load from cache
  try({load(cachePath, envir = .GlobalEnv)
    columns = columns[!columns %in%  INTERVALS$column]
  })
  
  cols_N = length(columns)
  
  #Return if we got em all.
  if(cols_N == 0){
    return(1)
  }
  
  #Else calc missing intervals 
  estimateInterval_loadData(columns,cachePath)
}

getNoiseIntervals = function(){
  
  cachePath = glue(PATH_DB,"cache/","interval")
  
  if(!exists("INTERVALS")){
    #Try load from cache
    try({load(cachePath, envir = .GlobalEnv)
    })
  }
  
  #If not in cache
  if(!exists("INTERVALS")){
    intervals_main()
  }
  
  #Return only intervals where we have 95% confidence in its validity
  inter = na.omit(INTERVALS)  %>% arrange(column)
  return(inter[inter$confidence > 0.95,])
}

convertNoiseToInt = function(DF){
  intervals = getNoiseIntervals()
  for(i in 1:nrow(intervals)){
    colName = intervals[i,]$column
    if(colName %in% colnames(DF)){
      DF[,colName] = as.integer(floor(DF[,colName] / (intervals[i,]$interval + 1e8))[[1]])
    }
  }
  
  DF
}

##########################################################
######## Plot columns with results to pdf ###############
#########################################################
plotNoiseHistByColumn = function(){
  print("Running plotNoiseHistByColumn, saving histograms to PDF")
  #load DF
  DF = readFromParquet(glue(PATH_DB,"parquet"))
  intervals = getNoiseIntervals()
  
  #select pdf
  pdf(file= glue(PATH_DB,"plots/noisePlots.pdf"),onefile=TRUE)
  print(glue("file = ",PATH_DB,"plots/noisePlots.pdf"))
  
  par( mfrow= c(3,1))
  for(i in 1:nrow(intervals)){
    interval = intervals[i,]
    print(glue("Saving plot to PDF : ", interval$column))
    vec = DF[, interval$column][[1]]
    
    hist(vec,  main =  glue(interval$column," : ",interval$interval, "\n Confidence :", interval$confidence), breaks = 100000)
    hist(vec, xlim = c(0,1), main =  " " , breaks = 100000)
    hist(vec, xlim = c(0,0.2), main = " ", breaks = 100000)
  }
  
  dev.off()
}


