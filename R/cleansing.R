#######################################################################
#######################################################################
############# Library of Function used to process csv  ################
#######################################################################
#######################################################################


##########################################
#######      Get table counts        #####
##########################################
# Lambda to read the row length of train_data.csv file using bash
# Value is stored in cache for quick retrieval

{function ()
  if(!exists('train_data_N')){ #Check if train_data_N exists
    cachePath = getFilePath("CSVRowCounts","")
    if(cachePath == ""){ # Check if value cached
      #Get counts and cache
      train_data_N  <<-system(paste0("wc -l ", getFilePath("train_data",".csv")), intern=T) %>% str_trim %>% strsplit(" ") %>% unlist %>% first  %>% as.integer
      cachePath = glue(PATH_DB,"cache/CSVRowCounts")
      save(list = c("train_data_N"), file = cachePath)
      
    } else {
      # else load from cache
      load(cachePath, envir = .GlobalEnv)
    }
  }
}()

#####################################################################
#######      cleansing functions                               #####
#####################################################################
# Functions to remove specified columns from data
# removeNonNumerics - removes catagorical columns plus date
# removeCleansedCols - removes columns returned by getColsToRemove 
removeNonNumerics <- function(data){
  return(
    data[ , -which(names(data) %in% c(
      'S_2', 'B_30', 'B_38', 'D_114', 'D_116', 'D_117', 'D_120', 'D_126', 'D_63', 'D_64', 'D_66', 'D_68'))] 
  )
}

removeCleansedCols <- function(data){
  return(
    data[ , -which(names(data) %in% getColsToRemove())] 
  )
}

##########################################################################
########               low variance variables                     ########
##########################################################################
# Calculate and cache variances for all columns
# getVariance("train_data")

getVariance = function(file,override = FALSE){
  f <- function(x){
    x %>% removeNonNumerics %>% summarise_if(is.numeric, var, na.rm = TRUE)
  }
  
  getCache(file, f, prefix = "VARIANCE", override = override)
}

# Use stored variances to specify columns that are below target variance threshold
# cleansCols_VARIANCE("train_data")

cleansCols_VARIANCE = function(file, threshold = 0.001){
  x = getVariance(file)
  colnames(x)[x < threshold]
}


########################################
#######      NA/Missing values     #####
########################################
# Get total counts of NA columns and cache
# getNACounts("train_data")

getNACounts = function(file,override = FALSE){
  f = function(x){x %>% is.na %>% colSums}
  getCache(file, f, prefix = "NA", override = override)
}

# Use stored NA counts to specify columns that are above target threshold
# cleansCols_NA("train_data")

cleansCols_NA = function(file, threshold = 0.1){
  #remove cols with % NA above threshold
  nNA = getNACounts(file)
  names(nNA)[which((nNA/train_data_N) >= threshold)]
}

##########################################################################################
########                      correlation between variables                       ########
##########################################################################################
# Get pairwise correlations by batch from csv
# Process done iteratively as to be memory safe
# getCorolations("train_data")

getCorolations = function(file,override = FALSE){
  f <- function(x,pos){
   x  %>% removeNonNumerics %>% sapply(as.numeric) %>% as.matrix  %>% cor(use = "pairwise.complete.obs") %>% flattenCorMatrix() %>% t()
  }
  
  #Have to use chunking method as is memory safe
  getCacheCSV(file, f, prefix = "COR", override = override)
}

# Use stored correlations to specify columns that are above target threshold
# As correlations taken in bathes of 100000, mean value is considered per column pair across all batches  
# cleansCols_COROLATION("train_data")

cleansCols_COROLATION = function(file, threshold = 0.9){
  x = getCorolations(file)
  colnames(x)[which(abs(colMeans(x)) > threshold)] %>% strsplit("-")  %>% lapply('[[', 2) %>% unlist()
}

# Auxiliary function to flatten upper triangle of correlation matrix for storage

flattenCorMatrix <- function(corMatrix) {
  ut <- upper.tri(corMatrix)
  data.frame(
    cor  =(corMatrix)[upper.tri(corMatrix)],
    row.names = paste( rownames(corMatrix)[row(corMatrix)[ut]], rownames(corMatrix)[col(corMatrix)[ut]], sep="-")
  )
}

##########################################################################################
########                     Cache Unique Customer ID                           ########
##########################################################################################
# Wrapper function to call each of cleansCols_NA, cleansCols_COROLATION and cleansCols_VARIANCE.
# Return Union of columns 
# getColsToRemove("train_data")

getColsToRemove = function(file = "train_data"){
  Reduce(union,
         list(
           cleansCols_NA(file,threshold = 0.1),
           cleansCols_COROLATION(file,threshold = 0.9),
           cleansCols_VARIANCE(file,threshold = 0.001)
         )
  )
}

##########################################################################################
########                     Cache Unique Customer ID                           ########
##########################################################################################
# Function to get exaustive list of unique customer IDs from specified file and cache
# getUniqueCustomerID("train_data")

getUniqueCustomerID = function(file,override = FALSE){
  f <- function(x, pos){
    x$customer_ID  %>% unique
  }
  
  unique(c(getCache(file, f, prefix = "CUSTOMERID", override = override)))
}


##########################################################################################
########                     Convert Categorical Columns to INT                  ########
##########################################################################################
# Uses mappings to convert categorical variables to integer for storage improvements.
# More robust than using factors as when batching not all levels may be available.
# Solution deprecated by use of parquet files, but still maintained

catagoricalToInts = function(DF){
  #Convert double categorical variables to ints 
  cols_int = c('B_30', 'B_38', 'D_114', 'D_116', 'D_117', 'D_120', 'D_126', 'D_66', 'D_68')
  cols_int = cols_int[cols_int %in% colnames(DF)]
  
  for(i in 1:length(cols_int)){
    DF[,cols_int[i]] = as.integer(DF[,cols_int[i]][[1]])
  }
  
  # Replace char cols with ints
  cols_char = c('D_63','D_64')
  cols_char = cols_char[cols_char %in% colnames(DF)]
  #Define values to map
  map = list(D_63 = c('CL', 'CO', 'CR','XL', 'XM', 'XZ'), D_64 =c('-1','O', 'R', 'U'))
  for(i in 1:length(cols_char)){
    DF[,cols_char[i]] = as.integer(match(DF[,cols_char[i]][[1]], map[cols_char[i]][[1]]))
  }
  
  # Replace customer_ID with int
  if('customer_ID' %in% colnames(DF)){
    DF[,'customer_ID'] = as.integer(match(DF[,'customer_ID'][[1]], getUniqueCustomerID('train_data')))
  }
  
  DF
}

##########################################################################################
########                     CSV -> PARQUET                     ########
##########################################################################################
# Convert CSV to Parquet file in chunks
# Join labels by customer_ID 

writeCsvToParquet = function(file, chunkSize = 100000){ 
  file = getFilePath(file,".csv")
  
  f = function(x,pos){
    x = x %>%  left_join(train_labels, by="customer_ID") 
    write_parquet(x,  glue(PATH_DB,"parquet/parquet_",pos))
  }
  
  suppressWarnings(
    read_csv_chunked(
      file,
      SideEffectChunkCallback$new(f), 
      chunk_size = chunkSize,
      progress = TRUE
    )
  )
  
  gc()
  
  #Get temp files list so we can delete them later 
  toDelete = dir(path=glue(PATH_DB,"parquet"))
  
  #Read in paraquet data
  DF = readFromParquet(glue(PATH_DB,"parquet"))
  
  #Delete partitioned files
  for(i in 1:length(toDelete)){
    file.remove(glue(PATH_DB,"parquet/",toDelete[i]))
  }
  
  #Write single file to disk
  write_parquet(DF,  glue(PATH_DB,"parquet/",file,".parquet"))
  
  gc()
  
}

##########################################################################################
########                   Transform Parquet Files                                 ########
##########################################################################################
# Read in Parquet file from disk, transform and write down in place
# writeCleanedParaquet - use cleansing functions to transform the data
# writeCleanedParaquet("train_data")

writeCleansedParaquet = function(parquetFile){
  write_parquet(
    x = readFromParquet(getFilePath(parquetFile,".parquet")) %>% removeCleansedCols %>% catagoricalToInts %>% convertNoiseToInt,
    sink =getFilePath(parquetFile,".parquet")
  )
}

# writeLastRowPerCustomerDF - Save last row by date only
# writeLastRowPerCustomerDF("train_data")
writeLastRowPerCustomerDF = function(parquetFile){
  write_parquet(
    x = readFromParquet(getFilePath(parquetFile,".parquet"))  %>% group_by(customer_ID) %>% slice_max(S_2) %>% ungroup(),
    sink = getFilePath(parquetFile,".parquet")
  )
}

#writeLastRowPerCustomerDF("train_data")
