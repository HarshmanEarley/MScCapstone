######################################################
########            Noise Elimination           #######
#######################################################

##########################################################################################
# Wrapper to estimateInterval
# Mandate is to load a dataframe and pass individual vectors to estimateInterval
##########################################################################################
estimateInterval_loadData = function(columns){
  print("Running estimateInterval_loadData")
  #read in full data from parquet
  DF = readFromParquet(getFilePath("train_data",".parquet"))
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
    save(INTERVALS, file = glue(PATH_DB,"cache/intervals"))
  }
  
  INTERVALS
}

#####################################################################################################
# Function to determine if a given continuous vector is actually discrete with noise injected.
# While loop:
# 1. Store minimum value from vector that is greater than the prior min + 0.01
# 2. Select bound of [minamum value, minamum value+0.01]
# 3. Get the remainders of this bound mod 0.01
# 4. Store p value Kolmogorov-Smirnov test for remainders against theoretical distribution Unif[0,0.01]
# For loop:
# 1. Round each minimum bound value to the nearest rational number (lower bound 0.01 as to not have intersecting intervals)
# 
# Get most frequently occurring rational and store as candidate interval (all intervals should be equal)
# Return candidate interval and confidence value (% of bounds that do not reject the null hypothesis of the Kolmogorov-Smirnov test)
#####################################################################################################

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

##########################################################################################
# Head function in stack to determine if a given variable has had noise injected
# Used as a buffer function, allowing R to be exited without loss of information
# Will only ask for variables to be calculated that are not already cahced
##########################################################################################
intervals_main = function(){  
  cachePath = glue(PATH_DB,"cache/intervals")
  print(glue("Running intervals_main, saving to ",cachePath))
  
  #read colnames from paraquet schema
  columns = (open_dataset(sources = getFilePath("train_data",".parquet"))$schema$names) 
  nonNumerics = c("customer_ID",'S_2', 'B_30', 'B_38', 'D_114', 'D_116', 'D_117', 'D_120', 'D_126', 'D_63', 'D_64', 'D_66', 'D_68')
  columns = columns[!columns %in% nonNumerics]
  
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
  estimateInterval_loadData(columns)
}


#################################################################################
# Load intervals from cache
# If not available, run intervals_main to calculate
# Return only intervals that have a conservative confidence above 0.95
#################################################################################
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

##########################################################################################
# Remove noise in variables deemed to have had noise injected
# For each interval:
# 1. Add 1e8 to avoid machine precision errors
# 2. Divide each variable by this augmented interval
# 3. Round to floor and convert to int
# 4. Results in ordinal array of ints representing the underlying rational number
##########################################################################################
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


##########################################################################################
# Plotting function for visual diagnostic of intervals      
##########################################################################################
plotNoiseHistByColumn = function(){
  print("Running plotNoiseHistByColumn, saving histograms to PDF")
  #load DF
  DF = readFromParquet(getFilePath("train_data",".parquet"))
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


