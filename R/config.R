
#Working directory
#PATH_WD = '/Users/root1/Documents/GitHub/Summer/Project/Code/'
# DENIS PATH
PATH_WD = 'C:/Users/denis/Documents/GitHub/DAC_Project/R/'
setwd(PATH_WD)

#Database files
#PATH_DB = '/Users/root1/Documents/amex-default-prediction/'
# DENIS PATH
PATH_DB = "C:/Users/denis/Documents/ACM40960 - Projects in Maths Modelling/database/"
FILES_DB = c('train_data','train_labels')

#Create cache
DB_CACHE = glue(PATH_DB,"cache")
if(!dir.exists(DB_CACHE)){
  dir.create(DB_CACHE)
}


#master list of all defined paths
PATH = ls()[unlist(lapply(ls(), function(vec) 'PATH' %in% strsplit(vec,"_")[[1]]))]


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
  "tfdatasets"
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

