### Data exploration and preprocessing
### Omar El Nahhas


rm(list=ls())
set.seed(1337)

packages_used = c("rstudioapi", 
                  "arules",
                  "dplyr")

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

library(arules)
library(dplyr)

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


### Goals:
# find relations between an athlete's background, physical properties and their success in the sport
# find relationships between sports: are some sports f.e. More skill based or more physical based?

### Remove unused variables
athletes_data <- subset(athletes_data, select = -c(id, name, info))

### Transform variables to factors
athletes_data$nationality = factor(athletes_data$nationality)
athletes_data$sex = factor(athletes_data$sex)
athletes_data$sport = factor(athletes_data$sport)

### Discretization of variables
## Age
current_year = 2022

athletes_data$date_of_birth = current_year - strtoi(substr(athletes_data$date_of_birth, 1, 4))
min(athletes_data$date_of_birth)
max(athletes_data$date_of_birth)
mean((athletes_data$date_of_birth))
hist(athletes_data$date_of_birth)

athletes_data$date_of_birth = arules::discretize(athletes_data$date_of_birth, method = "cluster", labels = c('young', 'middle-aged', 'mature'))

#rename to age_category instead of date of birth
athletes_data = dplyr::rename(athletes_data, age_category = date_of_birth)

## Height
# tall for male and female have different meanings, and are therefore labelled
# relative to the same gender. So 1.80 for male is considered average, but for a
# female 1.80 is considered tall.
hist(athletes_data$height[athletes_data$sex == 'female'])
hist(athletes_data$height[athletes_data$sex == 'male'])

athletes_data$height[athletes_data$sex == 'female'] = arules::discretize(athletes_data$height[athletes_data$sex == 'female'], method = "cluster",labels = c('short_f', 'average_f', 'tall_f'))
athletes_data$height[athletes_data$sex == 'male'] = arules::discretize(athletes_data$height[athletes_data$sex == 'male'], method = "cluster",labels = c('short_m', 'average_m', 'tall_m'))

athletes_data$height = factor(athletes_data$height, levels = c(1, 2, 3), labels = c('short', 'average', 'tall'))

## Weight
# heavy for male and female have different meanings, and are therefore labelled
# relative to the same gender. So 80kg for male is considered average, but for a
# female 80kg is considered heavy

hist(athletes_data$weight[athletes_data$sex == 'female'])
hist(athletes_data$weight[athletes_data$sex == 'male'])

athletes_data$weight[athletes_data$sex == 'female'] = arules::discretize(athletes_data$weight[athletes_data$sex == 'female'], method = "cluster",labels = c('light', 'average_f', 'heavy_f'))
athletes_data$weight[athletes_data$sex == 'male'] = arules::discretize(athletes_data$weight[athletes_data$sex == 'male'], method = "cluster",labels = c('light', 'average_m', 'heavy_m'))

athletes_data$weight = factor(athletes_data$weight, levels = c(1, 2, 3), labels = c('light', 'average', 'heavy'))

## Podium position (sum gold,silver,bronze > 0)