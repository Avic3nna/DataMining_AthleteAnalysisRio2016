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

athletes_data$date_of_birth = cut(athletes_data$date_of_birth, c(15,20, 25, 30, 35, 40, 45, 50, 55, 60, 65,70))

#rename to age instead of date of birth
athletes_data = dplyr::rename(athletes_data, age = date_of_birth)

## Height
# tall for male and female have different meanings, and are therefore labelled
# relative to the same gender. So 1.80 for male is considered average, but for a
# female 1.80 is considered tall.
hist(athletes_data$height)
hist(athletes_data$height[athletes_data$sex == 'female'])
hist(athletes_data$height[athletes_data$sex == 'male'])

summary(athletes_data$height)
athletes_data$height = cut(athletes_data$height, c(1.20,1.30,1.40, 1.50, 1.60, 1.70, 1.80, 1.90, 2.00, 2.10, 2.20))

# athletes_data$height[athletes_data$sex == 'female'] = arules::discretize(athletes_data$height[athletes_data$sex == 'female'], method = "cluster",labels = c('short_f', 'average_f', 'tall_f'))
# athletes_data$height[athletes_data$sex == 'male'] = arules::discretize(athletes_data$height[athletes_data$sex == 'male'], method = "cluster",labels = c('short_m', 'average_m', 'tall_m'))
# 
# athletes_data$height = factor(athletes_data$height, levels = c(1, 2, 3), labels = c('short', 'average', 'tall'))

## Weight
# heavy for male and female have different meanings, and are therefore labelled
# relative to the same gender. So 80kg for male is considered average, but for a
# female 80kg is considered heavy
hist(athletes_data$weight)
hist(athletes_data$weight[athletes_data$sex == 'female'])
hist(athletes_data$weight[athletes_data$sex == 'male'])

summary(athletes_data$weight)

athletes_data$weight = cut(athletes_data$weight, c(30,40,50,60, 70, 80, 90, 100, 110, 120, 130, 140, 170))
table(athletes_data$weight)

# athletes_data$weight[athletes_data$sex == 'female'] = arules::discretize(athletes_data$weight[athletes_data$sex == 'female'], method = "cluster",labels = c('light', 'average_f', 'heavy_f'))
# athletes_data$weight[athletes_data$sex == 'male'] = arules::discretize(athletes_data$weight[athletes_data$sex == 'male'], method = "cluster",labels = c('light', 'average_m', 'heavy_m'))
# 
# athletes_data$weight = factor(athletes_data$weight, levels = c(1, 2, 3), labels = c('light', 'average', 'heavy'))


## Podium position (has a medal)

athletes_data$podium = as.integer((athletes_data$gold + athletes_data$silver + athletes_data$bronze) > 0)



### Remove incomplete data rows
athletes_data = na.omit(athletes_data)
removed_rows = length(unique(attributes(athletes_data)$na.action))


######### DATA EXPLORATION
### Choose X amount of sports you find most interesting and analyse/compare those
table(athletes_data$sport)


#basketball, handball, volleyball, football, hockey, rugby
#why: similar amount of athletes, similar amount of medals, all team ball sports

winners = athletes_data[which(podium == 1 & sport %in% c('basketball', 'handball', 'volleyball',
                                                'football', 'hockey', 'rugby sevens')),]

losers = athletes_data[podium == 0 & sport %in% c('basketball', 'handball', 'volleyball',
                                                'football', 'hockey', 'rugby sevens'),]
table(winners$sport)

par(mfrow=c(2,1))
xx1 = barplot(prop.table(table(winners$height)) * 100, main = 'Winners height [%]', ylim = c(0,40))
text(x = xx1, y = prop.table(table(winners$height)) * 100, labels = round(prop.table(table(winners$height)) * 100, 1),pos = 3, cex = 0.8, col = "red")

xx2 = barplot(prop.table(table(losers$height)) * 100, main = 'Losers height [%]', ylim = c(0,40))
text(x = xx2, y = prop.table(table(losers$height)) * 100, labels = round(prop.table(table(losers$height)) * 100, 1),pos = 3, cex = 0.8, col = "red")


# 
# par(mfrow=c(2,1))
# xx1 = barplot(prop.table(table(winners$height[winners$sport == 'basketball'])) * 100, main = 'Basketball winners height [%]', ylim = c(0,40))
# text(x = xx1, y = prop.table(table(winners$height[winners$sport == 'basketball'])) * 100, labels = round(prop.table(table(winners$height[winners$sport == 'basketball'])) * 100, 1),pos = 3, cex = 0.8, col = "red")
# 
# xx2 = barplot(prop.table(table(losers$height[losers$sport == 'basketball'])) * 100, main = 'Basketball losers height [%]', ylim = c(0,40))
# text(x = xx2, y = prop.table(table(losers$height[losers$sport == 'basketball'])) * 100, labels = round(prop.table(table(losers$height[losers$sport == 'basketball'])) * 100, 1),pos = 3, cex = 0.8, col = "red")
# 
# 
# par(mfrow=c(2,1))
# barplot(prop.table(table(winners$height[winners$sport == 'handball'])) * 100, main = 'Handball winners height [%]', ylim = c(0,40))
# barplot(prop.table(table(losers$height[losers$sport == 'handball'])) * 100, main = 'Handball losers height [%]', ylim = c(0,40))
# 
# par(mfrow=c(2,1))
# barplot(prop.table(table(winners$height[winners$sport == 'volleyball'])) * 100, main = 'volleyball winners height [%]', ylim = c(0,40))
# barplot(prop.table(table(losers$height[losers$sport == 'volleyball'])) * 100, main = 'volleyball losers height [%]', ylim = c(0,40))
# 
# barplot(table(winners$height[winners$sport == 'football']))
# barplot(table(winners$height[winners$sport == 'hockey']))
# barplot(table(winners$height[winners$sport == 'rugby sevens']))






#########



