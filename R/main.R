## Welcome
## Please specify a user
user = 'Guest'

## If guest, please specify directories below
## NB - paths must end in a "/"
##    - Windows directories must have either "\\" or "/" separators
##    - Ensure directory is root, and not a parent of the root diectory
## Root working directory of Github clone
PATH_WD = '/Users/root1/Documents/DAC_Project/'

## Root database directory downloaded from Google Drive
## https://drive.google.com/drive/u/0/folders/1C2TYJRsVH681dylc8ZX5A4gP-LsoaW7o
PATH_DB = '/Users/root1/Desktop/AMEXDB/UCD2021-22 - Capstone/'

## Source database file to load all scripts
source(paste(PATH_WD,"R/database.R", sep = ""))