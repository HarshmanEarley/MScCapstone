##########################################
#######      Get table counts        #####
##########################################

{function ()
  if(!exists('train_data_N')){ #Check if train_data_N exists
    cachePath = getFilePath("CSVRowCounts","")
    if(cachePath == ""){ # Check if value cached
      #Get counts and cache
      train_data_N  <<-system(paste0("wc -l ", getFilePath("train_data",".csv")), intern=T) %>% str_trim %>% strsplit(" ") %>% unlist %>% first  %>% as.integer
      test_data_N <<-system(paste0("wc -l ", getFilePath("test_data",".csv")), intern=T) %>% str_trim %>% strsplit(" ") %>% unlist %>% first  %>% as.integer
      cachePath = glue(PATH_DB,"cache/CSVRowCounts")
      save(list = c("test_data_N", "train_data_N"), file = cachePath)
      
    } else {
      # else load from cache
      load(cachePath, envir = .GlobalEnv)
    }
  }
}()

#####################################################################
#######      cleansing functions (look to generalise)        #####
#####################################################################
  
  
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


getVariance = function(file,override = FALSE){
  f <- function(x, pos){
    x %>% removeNonNumerics %>% summarise_if(is.numeric, var, na.rm = TRUE)
  }
  
  getCache(file, f, prefix = "VARIANCE", override = override)
}

cleansCols_VARIANCE = function(file, threshold = 0.001){
  x = getVariance(file)
  colnames(x)[colMeans(x) < threshold]
}


########################################
#######      NA/Missing values     #####
########################################


getNACounts = function(file,override = FALSE){
  f <- function(x, pos){
    x %>% removeNonNumerics %>% is.na %>% colSums 
  }
  
  getCache(file, f, prefix = "NA", override = override)
}

#getNACounts("train_data")

cleansCols_NA = function(file, threshold = 0.1){
  #remove cols with % NA above threshold
  nNA = colSums(getNACounts(file))
  names(nNA)[which((nNA/train_data_N) >= threshold)]
}

##########################################################################################
########                      correlation between variables                       ########
##########################################################################################

flattenCorMatrix <- function(corMatrix) {
  ut <- upper.tri(corMatrix)
  data.frame(
    cor  =(corMatrix)[upper.tri(corMatrix)],
    row.names = paste( rownames(corMatrix)[row(corMatrix)[ut]], rownames(corMatrix)[col(corMatrix)[ut]], sep="-")
  )
}

getCorolations = function(file,override = FALSE){
  f <- function(x, pos){
   x  %>% removeNonNumerics %>% sapply(as.numeric) %>% as.matrix  %>% cor(use = "pairwise.complete.obs") %>% flattenCorMatrix() %>% t()
  }
  
  getCache(file, f, prefix = "COR", override = override)
}

#getCorolations("train_data")


cleansCols_COROLATION = function(file, threshold = 0.95){
  x = getCorolations(file)
  colnames(x)[which(colMeans(x) > threshold)] %>% strsplit("-")  %>% lapply('[[', 2) %>% unlist()
}

##########################################################################################
########                     Cache Unique Customer ID                           ########
##########################################################################################

getUniqueCustomerID = function(file,override = FALSE){
  f <- function(x, pos){
    x$customer_ID  %>% unique
  }
  
  unique(c(getCache(file, f, prefix = "CUSTOMERID", override = override)))
}


##########################################################################################
########                     Convert Catagorical Columns to INT                  ########
##########################################################################################

catagoricalToInts = function(DF){
  catagoricalToInts_debug <<- DF
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
########                     Cleansing Csv                       ########
##########################################################################################

getColsToRemove = function(file = "train_data"){
  Reduce(union,
         list(
           cleansCols_NA(file,threshold = 0.1),
           cleansCols_COROLATION(file,threshold = 0.9),
           cleansCols_VARIANCE(file,threshold = 0.001)
         )
  )
}

### 
# read csv, cleanse, and write result back to new file
###
writeCleansedToParquet = function(file, chunkSize = 100000){ 
  file = getFilePath(file,".csv")

  f = function(x,pos){
    x = x %>%  left_join(train_labels, by="customer_ID") %>% removeCleansedCols %>% catagoricalToInts
    write_parquet(x,  glue(PATH_DB,"parquet/cleansed_",pos))
  }
  
  suppressWarnings(
    read_csv_chunked(
      file,
      SideEffectChunkCallback$new(f), 
      chunk_size = chunkSize,
      progress = TRUE
    )
  )
  
  # Now we have a parquet file, run intervals_main to process column noise
  intervals_main()
  
  #Get temp files list so we can delete them later 
  toDelete = dir(path=glue(PATH_DB,"parquet"))
  
  #Read in paraquet data
  DF = readFromParquet(glue(PATH_DB,"parquet")) %>% convertNoiseToInt
  
  #Delete partitioned files
  for(i in 1:length(toDelete)){
    file.remove(glue(PATH_DB,"parquet/",toDelete[i]))
  }
  
  #Write single cleansed file to disk
  write_parquet(DF,  glue(PATH_DB,"parquet/data_train.parquet"))

}

#writeCleansedCSV(file = "train_data", newFile = "cleandata")
#getColsToRemove()


readFromParquet = function(filePath){
  ads = arrow::open_dataset(sources =  filePath)
  ## Create a scanner
  scan = Scanner$create(ads)
  ## Load it as n Arrow Table in memory
  at = scan$ToTable()
  ## Convert it to an R data frame
  as.data.frame(at)  
}

#  readFromParquet("/Users/root1/Documents/amex-default-prediction/parquet2/")


##########################################################################################
########                     Write Dataframe to paraquet in chunks                   ########
##########################################################################################

writeParaquetInChunks = function(data,dirName,chunks){
  chunks = 20
  n = 1:nrow(data)
  splits = split(n, cut(seq_along(n), chunks, labels = FALSE))
  
  for(j in 1:chunks){
    write_parquet(
      dplyr::slice(data, splits[[j]]), 
      glue(PATH_DB, "parquetTVT/",dirName,"/",dirName,"_",j,".parquet")
    )
  }
}

#writeParaquetTVT(x_train, "train", 20)
#writeParaquetTVT(x_val, "validation", 20)
#writeParaquetTVT(x_test, "test", 20)
