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
  colnames(x)[colMeans(x) < 0.001]
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

cleansCols_NA = function(file, threshold = 0.9){
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
########                     Cleansing Csv                       ########
##########################################################################################

getColsToRemove = function(file = "train_data"){
  Reduce(union,
         list(
           cleansCols_NA(file,threshold = 0.9),
           cleansCols_COROLATION(file,threshold = 0.9),
           cleansCols_VARIANCE(file,threshold = 0.001)
         )
  )
}

### 
# read csv, cleanse, and write result back to new file
###
writeCleansedCSV = function(file, newFile, chunkSize = 1000000){ 
  file = getFilePath(file,".csv")
  newFile = paste(c(newFile,"csv"), collapse = ".")
  newFile = paste(c(strsplit(file, "/")[[1]] %>% head(-1),newFile), collapse = "/") #put new file in same dir as old
  
  f = function(x,pos){
    x = x %>%  left_join(train_labels, by="customer_ID") %>% removeCleansedCols %>% removeNonNumerics
   # write_csv(x, newFile, append = ifelse(pos == 1, FALSE, TRUE))
    print("write_parquet")
    write_parquet(x, glue("/Users/root1/Documents/amex-default-prediction/parquet2/cleansed_",pos))
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

