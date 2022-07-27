####################
# package manager #
###################

packages = c(
  "MASS",
  "glue",
  "chunked",
  "arrow",
  "sparklyr",
  "LaF",
  "microbenchmark",
  "tidyverse",
  "data.table",
  "rbenchmark",
  "tensorflow",
  "keras",
  "tfdatasets",
  "inline",
  "Rcpp",
  "e1071",
  "onlinePCA",
  "missMDA",
  "mltools",
  "splitTools"
)

install.packages(
  setdiff(
    packages, rownames(installed.packages())
  )
)

for(pkg in packages){
  library(pkg, character.only = TRUE)
}

#install_tensorflow()
#install_keras()


#######################
# PATHS ########
#######################

#Working directory
PATH_WD = '/Users/root1/Documents/DAC_Project/R/'
setwd(PATH_WD)

#Database files
PATH_DB = '/Users/root1/Documents/amex-default-prediction/'
FILES_DB = c('test_data','train_data','train_labels')

#Create cache
DB_CACHE = glue(PATH_DB,"cache")
if(!dir.exists(DB_CACHE)){
  dir.create(DB_CACHE)
}


#master list of all defined paths
PATH = ls()[unlist(lapply(ls(), function(vec) 'PATH' %in% strsplit(vec,"_")[[1]]))]

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
  
  print(glue(cacheName," - ","creating cache from csv: ",filePath))
  
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


##############
# Load Data #
###############

train_labels = read_csv(getFilePath("train_labels"))

################
# Packages ####
################

source(getFilePath("Noise", ".R", checkDBOnly = FALSE))
source(getFilePath("cleansing_v2", ".R", checkDBOnly = FALSE))