source("~/Documents/DAC_Project/R/cleansing_v2.R")


inc_gdcFloat <- '
  // Recursive function to return gcd of a and b 
  double gcd(double a, double b){
    //Invert if a < b
    if (a < b){
        return gcd(b, a); 
    }
    // base case 
    if (fabs(b) < 0.0000001){
        return a;
    }
        
    return(gcd(b, a - floor(a / b) * b));
  }
'

body_gdcFloat <- '
  // CPP code for finding the GCD of two floating 
  // numbers. 
  //#include <bits/stdc++.h> 
  using namespace std; 
  double a = as<double>(arg_a);
  double b = as<double>(arg_b);
  
  return(wrap(gcd(a,b)));
'

ccp_gdcFloat <- cxxfunction(signature(arg_a= "double", arg_b= "double"),
                            body = body_gdcFloat,
                            includes = inc_gdcFloat,
                            plugin = "Rcpp")

# for 1:lseed iterations, call RandomWalk with specified seed for reproducability
ccpwrap_gdcFloat = function(a,b){
  ccp_gdcFloat(a,b)
}


getInterval_loadData = function(file, column){
  #read in column and omit NA
  print(glue("reading ", column))
  vec = read_csv(getFilePath(file), col_select = column, show_col_types = FALSE) %>% pull()
  vec = na.omit(vec)
  
  if(any(vec < 0)){ #gcd algo cant handle neg
    return(0)
  }
  
  print(glue("getInterval ", column))
  getInterval(vec)
}

getInterval = function(vec){
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
      intervals[k] = 1
      next
    }
    
    intervals[k] = (numDenom[1]/numDenom[2])
    print(numDenom[1]/numDenom[2])
    #invs = ccpwrap_gdcFloat( nums[k], nums[k+1])
    #Test to see if all values are multiples of the interval, to 0.000000000000001 precision (one place less than test case)
    #validation = all((nums %%  medInterval) < (0.0000001))
    #intervals[k] = ifelse(TRUE, invs, NA)
  }
  
  #median(intervals)
  tableInter = table(intervals)
  as.numeric(names(tableInter[which(tableInter == max(tableInter))]))
}