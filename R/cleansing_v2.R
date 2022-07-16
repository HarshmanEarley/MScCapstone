source('config.R')

##########################################
#######      Database management     #####
##########################################


getDB = function(db){
  res = if(db == 'csv'){
    glue(PATH_DB,'csv')
  } else if (db == 'parquet'){
    glue(PATH_DB,'parquet')
  } else {
    PATH_DB
  }
  
  return(res)
}

checkSplitsList = function(strings,split,seachTerm){
  strings[unlist(lapply(strings, function(vec) seachTerm %in% strsplit(vec,glue("\\",split))[[1]]))]
}

getFiles = function(path){
  list.files(get(path))
}

getFilePath = function(fileN, ext = ".csv", checkDBOnly = TRUE){
  
  res = list()
  pathsToCheck = if (checkDBOnly) 'PATH_DB' else PATH #restrict search to DB by default
  
  
  for(path in pathsToCheck){
    found = list.files(get(path), 
                       recursive = TRUE, 
                       full.names = TRUE, 
                       pattern = glue(fileN,ext)
    )
    
    res = append(res, found)
  }
  
  if(length(res) == 0){ #Return null string if no file found
    return("")
  }
  stopifnot("Multiple files found" = length(res) == 1)
  
  return(res[[1]])
}

getCache = function(file, callbackFunc, prefix, chunkSize = 100000, override = FALSE){
  
  filePath = getFilePath(file,".csv")
  cacheName = glue(prefix,"-",file)
  cachePath = getFilePath(cacheName,"")
  
  if(!override){ #If were not override updating the current cache
    if(!cachePath == ""){ #And we can find a cached file
      if(!exists(cacheName)){ 
        load(cachePath, envir = .GlobalEnv) #Load if cache not in memory
      }
      res = get(cacheName) 
      return(res)
    }
  }
  
  #If we cant find cache on disk, move to chunking
  # Load chunks and process using callbackFunc, assign to cacheName in the global env
  assign(cacheName,
         suppressWarnings(
           read_csv_chunked(
             filePath,
             DataFrameCallback$new(callbackFunc), 
             chunk_size = chunkSize,
             progress = TRUE
           )
         )
         ,envir = .GlobalEnv)
  
  # Save cache to disk and return
  cachePath = glue(PATH_DB,"cache/",cacheName)
  save(list = cacheName, file = cachePath)
  return(get(cacheName))
}

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
    x %>% numericVariables %>% summarise_if(is.numeric, var, na.rm = TRUE)
  }
  
  getCache(file, f, prefix = "VARIANCE", override = override)
}

cleansCols_VARIANCE = function(file, threshold = 0.001){
  x = getVariance(file)
  colnames(x)[colMeans(x) < 0.001]
}


########################################
#######      NA/Missing values     #####
########################################


getNACounts = function(file,override = FALSE){
  f <- function(x, pos){
    x %>% numericVariables %>% is.na %>% colSums 
  }
  
  getCache(file, f, prefix = "NA", override = override)
}

#getNACounts("train_data")

cleansCols_NA = function(file, threshold = 0){
  #threshold = pct of NA in col
  x = getNACounts(file)
  colnames(x)[-which(colSums(x) <= round(train_data_N*threshold))] 
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
   x  %>% numericVariables %>% sapply(as.numeric) %>% as.matrix  %>% cor(use = "pairwise.complete.obs") %>% flattenCorMatrix() %>% t()
  }
  
  getCache(file, f, prefix = "COR", override = override)
}

#getCorolations("train_data")


cleansCols_COROLATION = function(file, threshold = 0.95){
  x = getCorolations(file)
  colnames(x)[which(colMeans(x) > threshold)] %>% strsplit("-")  %>% lapply('[[', 2) %>% unlist()
}


##########################################################################################
########                     Cleansing Csv                       ########
##########################################################################################

getColsToRemove = function(file = "train_data"){
  Reduce(union,
         list(
           cleansCols_NA(file,threshold = 0),
           cleansCols_COROLATION(file,threshold = 0.9),
           cleansCols_VARIANCE(file,threshold = 0.001)
         )
  )
}

### 
# read csv, cleanse, and write result back to new file
###
writeCleansedCSV = function(file, newFile, chunkSize = 100000){ 
  file = getFilePath(file,".csv")
  newFile = paste(c(newFile,"csv"), collapse = ".")
  newFile = paste(c(strsplit(file, "/")[[1]] %>% head(-1),newFile), collapse = "/") #put new file in same dir as old
  
  f = function(x,pos){
    x = x %>%  left_join(train_labels, by="customer_ID") %>% removeCleansedCols %>% removeNonNumerics
    write_csv(x, newFile, append = ifelse(pos == 1, FALSE, TRUE))
  }
  
  suppressWarnings(
    read_csv_chunked(
      file,
      SideEffectChunkCallback$new(f), 
      chunk_size = chunkSize,
      progress = TRUE
    )
  )
}
