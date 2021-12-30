### Data exploration and preprocessing
### Omar El Nahhas


rm(list=ls())
set.seed(1337)

packages_used = c("rstudioapi")

for(package in packages_used){
  if(package %in% rownames(installed.packages()) == FALSE) {
    install.packages(package)
  }
}

setwd_current_path = function(){
  library(rstudioapi)
  current_path = getActiveDocumentContext()$path
  setwd(dirname(current_path))
  setwd('..')
  print(getwd())
}
setwd_current_path()


######### BEGIN LOAD DATA
athletes_data = read.csv("./Data/athletes.csv", header=T, as.is=T)
events_data = read.csv("./Data/events.csv", header=T, as.is=T)
######### END LOAD DATA


### To-do:
# Determine the goals - why is this data analysed? 
# Remove unused variables (id, name, nationality?)
# Discretize the ages
# Remove/impute samples without height/weight
# Make a function which easily filters the dataset by wished for search queries