####################
# package manager #
###################

packages = c(
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
  "MASS",
  "inline",
  "Rcpp"
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

#Dependencies
#Java


#######################
# PATHS ########
#######################
#Working directory
PATH_WD = '/Users/root1/Documents/GitHub/Summer/Project/Code/'
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



