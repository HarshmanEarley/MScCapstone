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
  'tfruns',
  "tfdatasets",
  "inline",
  "Rcpp",
  "e1071",
  "onlinePCA",
  "missMDA",
  "mltools",
  "splitTools",
  "magrittr",
  "ROCR",
  "randomForest",
  "caret",
  "ggpubr",
  "parallel",
  "doParallel"
  
)

install.packages(
  setdiff(
    packages, rownames(installed.packages())
  )
)

for(pkg in packages){
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

#install_tensorflow()
#install_keras()


#######################
# PATHS ########
#######################
user = 'Sidney'

#Working directory
if(user == 'Sidney'){
  if(.Platform$OS.type == 'unix'){
    PATH_WD = '/Users/root1/Documents/DAC_Project/'
    PATH_DB = '/Users/root1/Documents/amex-default-prediction/'
  }
  if(.Platform$OS.type == 'windows'){
    PATH_WD = 'C:/Users/sidne/Documents/GitHub/DAC_Project/R/'
    PATH_DB = 'C:/Users/sidne/Documents/amexDatabase/'
  }
}
if(user == 'Denis'){
  if(.Platform$OS.type == 'unix'){
    PATH_WD = '/Users/root1/Documents/DAC_Project/R/'
    PATH_DB = '/Users/root1/Documents/amex-default-prediction/'
  }
  if(.Platform$OS.type == 'windows'){
    PATH_WD = 'C:/Users/denis/Documents/GitHub/DAC_Project/R/'
    PATH_DB = 'C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/database/'
  }
}

#Database files
FILES_DB = c('train_data','train_labels')

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

readFromParquet = function(filePath){
  ads = arrow::open_dataset(sources =  filePath)
  ## Create a scanner
  scan = Scanner$create(ads)
  ## Load it as n Arrow Table in memory
  at = scan$ToTable()
  ## Convert it to an R data frame
  as.data.frame(at)
}


getFilePath = function(fileN, ext = ".csv", checkDBOnly = FALSE){

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
  
  #Return null string if no file found
  stopifnot("No file found" = length(res) != 0)
  
  
  #If we get files of similar names, find exact name
  if(!length(res) == 1){
    f = (res %>% str_split("/",simplify = TRUE))
    res = res[glue(fileN,ext) == f[,ncol(f)]]
  }
  
  #If wmultiple, throw error 
  stopifnot("Multiple files found" = length(res) == 1)
  
  
  return(res[[1]])
}

getCacheCSV = function(file, callbackFunc, prefix, chunkSize = 100000, override = FALSE){
  
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


getCache = function(file, func, prefix, override = FALSE){
  
  filePath = getFilePath(file,".parquet")
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
  
  print(glue(cacheName," - ","creating cache from parquet: ",filePath))
  
  assign(cacheName,
         func(readFromParquet(getFilePath(file,".parquet"))),
         envir = .GlobalEnv
  )
  
  print(glue(cacheName," - ","saving to cache"))
  # Save cache to disk and return
  cachePath = glue(PATH_DB,"cache/",cacheName)
  save(list = cacheName, file = cachePath)
  return(get(cacheName))
}

train_labels = read_csv(getFilePath("train_labels"),show_col_types = FALSE);

##########################################################################################
########                 Scripts management                                  ########
##########################################################################################


source(getFilePath("Noise", ".R", checkDBOnly = FALSE))
source(getFilePath("cleansing", ".R", checkDBOnly = FALSE))
source(getFilePath("amex_metric", ".R", checkDBOnly = FALSE))