getInterval_loadData = function(file, column){
  print("getInterval_loadData")
  #read in column and omit NA
  print(glue("reading ", column))
  vec = read_csv(getFilePath(file), col_select = column, show_col_types = FALSE) %>% pull()
  vec = na.omit(vec)
  if(any(vec < 0)){ #gcd algo cant handle neg
    return(NA)
  }
  print(glue("getInterval ", column))
  getInterval(vec)
}


getInterval = function(vec){
  print("getInterval")
  nums = c()
  i = 1
  nums[1] = min(vec)
  max_vec = max(vec)
  
  #While below max value, find min value in chunks of 0.01
  while(max_vec > (last(nums) + 0.01)){
    #print(last(nums))
    boundMin = ifelse(i == 1, min(vec), min(vec[!vec <= (nums[i-1]+ 0.01)]))
    bound = vec[between(vec, boundMin, boundMin + 0.01)]
    #nums[i] = median(bound) - 0.005
    #if(nums[i] < 0){nums[i] = 0}
    nums[i] = min(bound)
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
    #invs = ccpwrap_gdcFloat( nums[k], nums[k+1])
    #Test to see if all values are multiples of the interval, to 0.000000000000001 precision (one place less than test case)
    #validation = all((nums %%  medInterval) < (0.0000001))
    #intervals[k] = ifelse(TRUE, invs, NA)
  }
  
  #median(intervals)
  tableInter = table(intervals)
  candidate = as.numeric(names(tableInter[which(tableInter == max(tableInter))]))
  
  if(!length(candidate) == 1){ #Return NA if we cannot reach a consensus
    return(NA)
  }
  
  #modulo remainder iid uniform (0,0.1) by assumptions, thus mu_hat = 0.005
  if(!0.005 == round(mean(vec %% candidate),5)){
    return(NA)
  }
  
  #Use Kolmogorov-Smirnov test to check remainders are uniform(0, 0.01) with 70
  #If we reject H0: bound comes from unif dist, return NA
  # As sample size is large, normalize p value 
  # https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.518.5341&rep=rep1&type=pdf
  rem = vec %% candidate
  ks_p = ks.test(rem,"punif",0, 0.01)$p
  #ks_p = min(0.5, (ks.test(rem,"punif",0, 0.01)$p) * sqrt(length(rem)/100))
  
  if(ks_p < 0.05){
    return(NA)
  }
  
  return(candidate)
}



intervals_main = function(){  
  print("Running intervals_main")
  #read colnames from first row of training data
  cols = read_csv(getFilePath("train_data"), n_max = 1, show_col_types = FALSE)  %>% removeNonNumerics() %>% dplyr::select(-customer_ID) %>% colnames()
  intervals = list()
  
  #Try load from cache
  cachePath = glue(PATH_DB,"cache/","intervals")
  try({load(cachePath)
      cols = cols[!cols %in% names(intervals)]
    })
  
  cols_N = length(cols)
  
  #Return if we got em all.
  if(cols_N == 0){
    return(1)
  }
  
  #Else calc missing intervals 
  for(i in 1:cols_N){
    print(glue("getting interval for ",cols[i]))
    intervals[[cols[i]]] = getInterval_loadData("train_data", cols[i])
    save(intervals, file = cachePath)
  }
}