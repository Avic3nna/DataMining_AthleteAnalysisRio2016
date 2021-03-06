### Data exploration and preprocessing
### Omar El Nahhas


rm(list=ls())
set.seed(1337)

packages_used = c("rstudioapi", 
                  "arules",
                  "dplyr",
                  "RColorBrewer",
                  "stats",
                  "rpart",
                  "rpart.plot",
                  "randomForest",
                  "cluster",
                  "rgl",
                  "caret",
                  "mixtools",
                  "DMwR2",
                  "shiny",
                  "randomcoloR")

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
library(RColorBrewer)
library(stats)
library(rpart)
library(rpart.plot)
library(randomForest)
library(cluster)
library(rgl)
library(caret)
library(mixtools)
library(DMwR2)
library(shiny)
library(randomcoloR)

######### BEGIN LOAD DATA
athletes_data = read.csv("./Data/athletes.csv", header=T, as.is=T)
events_data = read.csv("./Data/events.csv", header=T, as.is=T)

summary(athletes_data)
######### END LOAD DATA


### Goals:
# find relations between an athlete's background, physical properties and their success in the sport
# find relationships between sports: are some sports f.e. More skill based or more physical based?


######### BEGIN DATA CLEANING
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

#save non-discretized dataset

athletes_data_nd = athletes_data

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

#splitting men/women
# athletes_data$weight[athletes_data$sex == 'female'] = arules::discretize(athletes_data$weight[athletes_data$sex == 'female'], method = "cluster",labels = c('light', 'average_f', 'heavy_f'))
# athletes_data$weight[athletes_data$sex == 'male'] = arules::discretize(athletes_data$weight[athletes_data$sex == 'male'], method = "cluster",labels = c('light', 'average_m', 'heavy_m'))
# 
# athletes_data$weight = factor(athletes_data$weight, levels = c(1, 2, 3), labels = c('light', 'average', 'heavy'))


## Podium position (has a medal)

athletes_data$podium = as.integer((athletes_data$gold + athletes_data$silver + athletes_data$bronze) > 0)
athletes_data_nd$podium = as.integer((athletes_data$gold + athletes_data$silver + athletes_data$bronze) > 0)

### Remove incomplete data rows
athletes_data = na.omit(athletes_data)
athletes_data_nd = na.omit(athletes_data_nd)
removed_rows = length(unique(attributes(athletes_data)$na.action))
######### END DATA PRE-PROCESSING



######### BEGIN DATA EXPLORATION
### Choose X amount of sports you find most interesting and analyse/compare those
table(athletes_data$sport)


#basketball, handball, volleyball, football, hockey, rugby
#why: similar amount of athletes, similar amount of medals, all team ball sports

winners = athletes_data[which(athletes_data$podium == 1 & athletes_data$sport %in% c('gymnastics', 'fencing', 'weightlifting', 'shooting', 'judo', 'cycling')),]

#tennis, aquatics, archery

#winners = athletes_data[which(athletes_data$podium == 1 & athletes_data$sport %in% c('basketball', 'handball', 'volleyball','football', 'hockey', 'rugby sevens')),]
#different sports, team sports may infer too big of correlation?


#c('basketball', 'handball', 'volleyball','football', 'hockey', 'rugby sevens')
#c('boxing', 'tennis', 'weightlifting', 'judo', 'gymnastics', 'wrestling')
#c('golf', 'fencing', 'weightlifting', 'shooting', 'archery', 'cycling')

#c('gymnastics', 'fencing', 'weightlifting', 'shooting', 'judo', 'cycling') -> similar amount of medals

### Big conclusion: Choosing team sports is showing major bias,
#   which explains why nationality is seen as the #1 variable
#   if your fellow country people have won a medal in the same sport,
#   and you are the same gender (i.e., you are on the same team)
#   OF COURSE you will get the same outcome as your team

losers = athletes_data[which(athletes_data$podium == 0 & athletes_data$sport %in% c('gymnastics', 'fencing', 'weightlifting', 'shooting', 'judo', 'cycling')),]
table(droplevels(winners$sport))

# height comparison
par(mfrow=c(2,1))
xx1 = barplot(prop.table(table(droplevels(winners$height))) * 100, main = 'Winners height [%]', ylim = c(0,40), col = 'green')
text(x = xx1, y = prop.table(table(droplevels(winners$height))) * 100, labels = round(prop.table(table(droplevels(winners$height))) * 100, 1),pos = 3, cex = 0.8, col = "green")

xx2 = barplot(prop.table(table(droplevels(losers$height))) * 100, main = 'Losers height [%]', ylim = c(0,40), col = 'red')
text(x = xx2, y = prop.table(table(droplevels(losers$height))) * 100, labels = round(prop.table(table(droplevels(losers$height))) * 100, 1),pos = 3, cex = 0.8, col = "red")


# weight comparison
par(mfrow=c(2,1))
xx1 = barplot(prop.table(table(droplevels(winners$weight))) * 100, main = 'Winners weight [%]', ylim = c(0,40), col = 'green')
text(x = xx1, y = prop.table(table(droplevels(winners$weight))) * 100, labels = round(prop.table(table(droplevels(winners$weight))) * 100, 1),pos = 3, cex = 0.8, col = "green")

xx2 = barplot(prop.table(table(droplevels(losers$weight))) * 100, main = 'Losers weight [%]', ylim = c(0,40), col = 'red')
text(x = xx2, y = prop.table(table(droplevels(losers$weight))) * 100, labels = round(prop.table(table(droplevels(losers$weight))) * 100, 1),pos = 3, cex = 0.8, col = "red")


## check the winning countries
table(droplevels(winners$nationality))

# some countries send only a few athletes, while others send hundreds (US f.e.)
# therefore, check the winners relative to the people that were sent
country_names_win = names(table(droplevels(winners$nationality)))
country_freq_win = unname(table(droplevels(winners$nationality)))

country_freq_total = unname(table(droplevels(athletes_data$nationality))[country_names_win])

relative_win_rate = round((country_freq_win/country_freq_total)*100,1)


names(relative_win_rate) = country_names_win

# plot relative win rate
par(mfrow=c(2,1))

col_pal = colorRampPalette(RColorBrewer::brewer.pal(n=8, 'Set2'))(length(table(droplevels(winners$nationality))))
xx1 = barplot(relative_win_rate, col = col_pal, ylab = '%', main = 'Winners/participants per country [%]', ylim = c(0,40)) 
text(x = xx1, y = relative_win_rate, labels = relative_win_rate, pos = 3, cex = 0.8, col = "black")


wins = table(droplevels(winners$nationality))
xx2 = barplot(wins, col = col_pal, ylab = 'Frequency', main = 'Winners per country', ylim = c(0,100)) 
text(x = xx2, y = wins, labels = wins, pos = 3, cex = 0.8, col = "black")


# add relative winrate per country as variable to learn from?

######### END DATA EXPLORATION



######### START DATA MODELLING
# join the winners / losers as 1 dataset
# we want equal probability, thus equal split winners/losers

losers_equal_idx = sample(seq_len(nrow(losers)), size = nrow(winners))
full_data = rbind(winners, losers[losers_equal_idx,])


#reduce categorical vars < 53 
other_categ = names(which(table(droplevels(full_data$nationality)) < 4))
other_idx = full_data$nationality %in% other_categ

full_data$nationality = as.character(full_data$nationality)

full_data$nationality[other_idx] = "OTHER"
full_data$nationality = factor(full_data$nationality)

# create a train/test set -> what is the dependent variable? Podium?
# 80/20 split
smp_size <- floor(0.8 * nrow(full_data))

train_idx <- sample(seq_len(nrow(full_data)), size = smp_size)

train <- full_data[train_idx, ]
test <- full_data[-train_idx, ]




### TEST SAMPLE IS A STATISTICAL REPRESENTATIVE OF TRAIN DATA :
# podium places have similar mean + similar m/f ratio
summary(train)
summary(test)

# visually similar distribution of age, height, weight
par(mfrow=c(2,1))
barplot(table(train$age), main = 'train age')
barplot(table(test$age), main = 'test age')

par(mfrow=c(2,1))
barplot(table(train$height), main = 'train height')
barplot(table(test$height), main = 'test height')

par(mfrow=c(2,1))
barplot(table(train$weight), main = 'train weight')
barplot(table(test$weight), main = 'test weight')

#nationalities differ, but this is an unavoidable phenomenon
par(mfrow=c(2,1))
barplot(table(droplevels(train$nationality)), main = 'train nationality')
barplot(table(droplevels(test$nationality)), main = 'test nationality')

# start with LR, basic model, try multiple independent vars
simple_lr = stats::lm(podium ~ (height)**2 + weight, data = athletes_data_nd)
summary(simple_lr)

par(mfrow = c(2,2))
plot(simple_lr)

simple_lr_2 = stats::lm(podium ~ (nationality), data = full_data)
summary(simple_lr_2)

par(mfrow = c(2,2))
plot(simple_lr_2)

#conclusion: R^2 max at 0.36, many vars pval > 0.05
#this problem cannot be solved by a linear model

### proceed with clustering to find some underlying patterns


### create a decision tree and assess

dt_fit <- rpart(podium ~ sex + age + height + weight + sport + nationality, data = train, method = 'class')
dt_fit = rpart::prune(dt_fit, cp = 0.01)
rpart.plot(dt_fit, tweak = 2)

pred_dt <-predict(dt_fit, test, type = 'class')
table_mat <- table(test$podium, pred_dt)
table_mat

acc_dt = sum(diag(table_mat))/sum(table_mat)
#summary(dt_fit)

caret::confusionMatrix(test$podium, pred_dt, mode = 'everything')
acc_dt

dt_fit$variable.importance

dt_fit_size = object.size(dt_fit)
# = 84.2% acc on test with group sports

### random forest
#equalize so that train/test have same type of factors (capped at 53 for rf)

train$podium = as.factor(train$podium)
test$podium = as.factor(test$podium)

train$nationality = droplevels(train$nationality)
test$nationality = droplevels(test$nationality)

xtrain = train
xtest = test
xtest <- rbind(xtrain[1, ] , xtest)
xtest <- xtest[-1,]


rf_fit <- randomForest::randomForest(podium ~ . - gold - silver - bronze, data = xtrain)
rf_fit


pred_rf <-predict(rf_fit, xtest, type = 'class')
table_mat <- table(xtest$podium, pred_rf)
table_mat

acc_rf = sum(diag(table_mat))/sum(table_mat)

acc_rf

rf_fit$importance

# = 87.8% accuracy on test with group sports

### Clustering
full_data_unsupervised = athletes_data_nd[athletes_data_nd$sport %in% c('gymnastics', 'fencing', 'weightlifting', 'shooting', 'judo', 'cycling'), c(3,4,5,6)]
full_data_unsupervised$sport = droplevels(full_data_unsupervised$sport)
# showing true classes 3D

age = full_data_unsupervised$date_of_birth
weight= full_data_unsupervised$weight
height = full_data_unsupervised$height
plotty = rgl::renderRglwidget({
  rgl::plot3d(age, weight, height, col = palette(rainbow(6))[as.numeric(full_data_unsupervised$sport)])
  rgl::par3d(windowRect = c(0, 0, 512, 512))
  rgl::legend3d("topright", legend = unique(as.character(full_data_unsupervised$sport)), pch = 16, col = palette(rainbow(6)), cex=0.8, inset=c(0.02))
  rgl::rglwidget()
})
plotty

#rgl.snapshot( "gt_3dplot.png")


kmeans_six = stats::kmeans(full_data_unsupervised[,1:3], centers = 6)

plot(age, height, col = )


# add cluster to original data 
clustered_data <-cbind(full_data_unsupervised,as.factor(kmeans_six$cluster))

clustered_data$cluster = levels(clustered_data$sport)[kmeans_six$cluster]

clustered_data$cluster <- clustered_data$sport
clustered_data$cluster[1:nrow(clustered_data)] <- levels(clustered_data$sport)[kmeans_six$cluster]


sum(clustered_data$sport == clustered_data$cluster) #amount of correct clusters

#rows = true, columns = predicted
table(clustered_data$sport,clustered_data$cluster)

#3rd party confmat
caret::confusionMatrix(clustered_data$cluster, clustered_data$sport)
#kmeans 16% acc


rgl::plot3d(age, weight, height, col = palette(rainbow(6))[as.factor(kmeans_six$cluster)])
rgl::par3d(windowRect = c(0, 0, 512, 512))
rgl::legend3d("topright", legend = unique(full_data_unsupervised$sport), pch = 16, col = palette(rainbow(6)), cex=0.8, inset=c(0.02))
rgl::rglwidget()

#rgl.snapshot( "kmeans_3dplot.png")


hclusterini = stats::hclust(dist(full_data_unsupervised[,1:3]))
clusterCut <- cutree(hclusterini, 6)


clustered_data$cluster[1:nrow(clustered_data)] <- levels(clustered_data$sport)[clusterCut]

table(clustered_data$sport, clustered_data$cluster)
caret::confusionMatrix(clustered_data$cluster, clustered_data$sport)
#hierarch 21% acc

rgl::plot3d(age, weight, height, col = palette(rainbow(6))[as.factor(kmeans_six$cluster)])
rgl::par3d(windowRect = c(0, 0, 512, 512))
rgl::legend3d("topright", legend = unique(full_data_unsupervised$sport), pch = 16, col = palette(rainbow(6)), cex=0.8, inset=c(0.02))
rgl::rglwidget()

rgl.snapshot( "hiera_3dplot.png")

### TO - DO: Draw conclusions regarding the clustering
# which sports have similar physical features?
# maybe better results when focusing only on winners? (more equal sample size)


####### OUTLIER DETECTION
#--> outlier detection = physical outlier, maybe skill based sport ?

outlier_data = athletes_data_nd[athletes_data_nd$sport %in% c('gymnastics', 'canoe', 'weightlifting', 'shooting', 'basketball', 'table tennis'), c(3,4,5,6,7,8,9,10)]
outlier_data$sport = droplevels(outlier_data$sport)



outlier.scores = DMwR2::lofactor(outlier_data[,2:3], k=3)
plot(density(na.omit(outlier.scores)), main = 'Outlier density plot (outliers beyond red line)')

threshold_max = 3


abline(v = threshold_max, col = 'red')




outliers = outlier.scores[outlier.scores > threshold_max]
outliers.ordered = order(outliers, decreasing = TRUE)

relative_skill_vs_physical = table(outlier_data$sport[outliers])/table(outlier_data$sport)

#Relatively speaking, these sports require more skill than physicality
#because the outliers were sought on purely height/weight (physical parameters)
#and the most outliers were found in the sports where skills > physicality
#So 9.5% of all table tennis occurences were seen as physical outliers, meaning that
#it is relative to the other sports way more skill based than physical based
xx1 = barplot(sort(round(relative_skill_vs_physical*100,1), decreasing = TRUE), 
        main = 'Outliers of sport physicality importance (relative frequency [%])', ylim = c(0,10), col = col_pal)

text(x = xx1, y =sort(round(relative_skill_vs_physical*100,1), decreasing = TRUE), labels = unname(sort(round(relative_skill_vs_physical*100,1), decreasing = TRUE)),pos = 3, cex = 0.8, col = "black")

full_outlier_profile = athletes_data_nd[outliers.ordered,]
summary(full_outlier_profile)
summary(outlier_data)

# outlier profile show almost half of podium success as the entire pool

## plotting the outliers
n <- nrow(outlier_data)
labels <- 1:n
labels[-outliers.ordered] <- "."
biplot(prcomp(outlier_data[,2:3]), cex=.8, xlabs=labels)


pch <- rep(".", n)
pch[outliers.ordered] <- "*"
col <- rep("black", n)
col[outliers.ordered] <- "red"

rgl::plot3d(outlier_data$date_of_birth, 
            outlier_data$height, outlier_data$weight,
              pch=pch, col=col)
pairs(outlier_data[,2:3], pch=pch, col=col)

####### END OUTLIER DETECTION

####### how would I do in the olympics?
nationality = "NED"
sex = "male"
age = "(20,25]"
height = "(1.9,2]"
weight = "(100,110]"
sport = "basketball"

omar_df = data.frame('nationality' = nationality, 'sex' = sex, 'age' = age, 
                         'height' = height, 'weight' = weight, 'sport' = sport, 'gold' = 0,
                     'silver' = 0, 'bronze' = 0, 'podium' = 0)

xtrain = train
xtest = test
omar_df <- rbind(xtrain[1,] , omar_df)
omar_df <- omar_df[-1,]

omar_df[1,] = data.frame('nationality' = nationality, 'sex' = sex, 'age' = age, 
                     'height' = height, 'weight' = weight, 'sport' = sport,'gold' = 0,
                    'silver' = 0, 'bronze' = 0, 'podium' = 0)

rownames(omar_df) = "Omar" 


# Predictions with own data
pred_dt <-predict(dt_fit, omar_df, type = 'class')


pred_dt

# Not a podium place :(


######### END DATA MODELLING




