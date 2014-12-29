library(glmnet)
library(corrplot)
library(ggplot2)
library(dplyr)
library(foreach)
require(doParallel)
require(parallel) 
library(iterators)
library(randomForest)
setwd(file.path("/Users/Lishali/Desktop/215A/final_project"))
################################################################
################################################################


Xtest <- read.table("test_standarized.csv")
Xtest[is.na(as.matrix(Xtest))] = 0
Xtest <- as.matrix(Xtest)
Ytest <- as.matrix(combined.s[,1:20])
Ytest.1 <- as.matrix(Ytest[,1])
Ytest.2 <- as.matrix(Ytest[,2])
Ytest.3 <- as.matrix(Ytest[,3])
Ytest.4 <- as.matrix(Ytest[,4])
Ytest.5 <- as.matrix(Ytest[,5])
Ytest.6 <- as.matrix(Ytest[,6])
Ytest.7 <- as.matrix(Ytest[,7])
Ytest.8 <- as.matrix(Ytest[,8])
Ytest.9 <- as.matrix(Ytest[,9])
Ytest.10 <- as.matrix(Ytest[,10])
Ytest.11 <- as.matrix(Ytest[,11])
Ytest.12 <- as.matrix(Ytest[,12])
Ytest.13 <- as.matrix(Ytest[,13])
Ytest.14 <- as.matrix(Ytest[,14])
Ytest.15 <- as.matrix(Ytest[,15])
Ytest.16 <- as.matrix(Ytest[,16])
Ytest.17 <- as.matrix(Ytest[,17])
Ytest.18 <- as.matrix(Ytest[,18])
Ytest.19 <- as.matrix(Ytest[,19])
Ytest.20 <- as.matrix(Ytest[,20])


train <- read.table("train.csv")
X <- as.matrix(train[,21:length(train)])
Y.1 <- train[,1]

#same CV:

#with CV:
#not gonna do CV for random forest, will use OOB and then test it with the same test set as others

rf2 <- randomForest(x= X , y = Y.2, xtest = Xtest, ytest = Ytest.2, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf3 <- randomForest(x= X , y = Y.3, xtest = Xtest, ytest = Ytest.3, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf4 <- randomForest(x= X , y = Y.4, xtest = Xtest, ytest = Ytest.4, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf5 <- randomForest(x= X , y = Y.5, xtest = Xtest, ytest = Ytest.5, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf6 <- randomForest(x= X , y = Y.6, xtest = Xtest, ytest = Ytest.6, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf7 <- randomForest(x= X , y = Y.7, xtest = Xtest, ytest = Ytest.7, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf8 <- randomForest(x= X , y = Y.8, xtest = Xtest, ytest = Ytest.8, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf9 <- randomForest(x= X , y = Y.9, xtest = Xtest, ytest = Ytest.9, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf10 <- randomForest(x= X , y = Y.10, xtest = Xtest, ytest = Ytest.10, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf11 <- randomForest(x= X , y = Y.11, xtest = Xtest, ytest = Ytest.11, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf12 <- randomForest(x= X , y = Y.12, xtest = Xtest, ytest = Ytest.12, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf13 <- randomForest(x= X , y = Y.13, xtest = Xtest, ytest = Ytest.13, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf14 <- randomForest(x= X , y = Y.14, xtest = Xtest, ytest = Ytest.14, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf15 <- randomForest(x= X , y = Y.15, xtest = Xtest, ytest = Ytest.15, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf16 <- randomForest(x= X , y = Y.16, xtest = Xtest, ytest = Ytest.16, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf17 <- randomForest(x= X , y = Y.17, xtest = Xtest, ytest = Ytest.17, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf18 <- randomForest(x= X , y = Y.18, xtest = Xtest, ytest = Ytest.18, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf19 <- randomForest(x= X , y = Y.19, xtest = Xtest, ytest = Ytest.19, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf20 <- randomForest(x= X , y = Y.20, xtest = Xtest, ytest = Ytest.20, ntree = 100, importance = TRUE, keep.forest = TRUE)


rf1

rf1$mse 

rf1$mse #one for each tree used! ok, great!!!!
#[1] 1.442550 1.334536 1.360831 1.231452 1.175640 1.116557 1.067728 1.053844 1.011200
#[10] 0.968793

rf1$test

predicted <- predict(rf1, Xtest)
predicted

error <- predicted-Ytest.1
error <- error^2
result <- sum(error)/350
result #this is given in rf1 call

length(predicted)

#[1] 1.681292 1.496491 1.434099 1.382662 1.373390 1.349692 1.339843 1.320787 1.319694
#[10] 1.313539

#though the validation was comparable to cv.lasso, on the test set, we are doing pretty bad
#this is with 10 trees, let's see how things improve with 1000
################################################################
################################################################


X <- as.matrix(train[,21:length(train)])
Y.1 <- train[,1]
#same CV:
newX[is.na(as.matrix(newX))]=0
#with CV:
#not gonna do CV for random forest, will use OOB and then test it with the same test set as others

rf2 <- randomForest(x= X , y = Y.1, xtest = newX, ytest = newY.1, ntree = 100, importance = TRUE, keep.forest = TRUE)
rf2
save(rf2, file = "rf_5050_100trees.RData")


rf2$mse #one for each tree used! ok, great!!!!
#[1] 1.442550 1.334536 1.360831 1.231452 1.175640 1.116557 1.067728 1.053844 1.011200
#[10] 0.968793

rf2

dim(newX)
dim(X)
predicted <- predict(rf2, newX)
predicted

error <- predicted-newY.1
error <- error^2
result <- sum(error)/875
result #this is given in rf1 call

1.398789

length(predicted)

load("test_Y_8020.RData")
load("test_X_standarized.RData")
load("test_8020.RData")
load("train_8020.RData")

X <- as.matrix(combined.s[,21:length(combined.s)])
Y <- as.matrix(combined.s[,1:20])
Y.1 <- as.matrix(Y[,1])
a[is.na(a)]=0
newX <- as.matrix(a)
newY.1 <- as.matrix(newY[,1])

rf.8020 <- randomForest(x= X , y = Y.1, xtest = newX, ytest = newY.1, ntree = 300, importance = TRUE, keep.forest = TRUE)
save(rf.8020, file = "rf_8020_300trees.RData")
rf.8020
#300 versus 100, MSE does not improve.  



#coefficients for lasso.1
co <- coef(cv.lasso,s="lambda.min")
#only 97 non zero coefficients:
co <- summary(co)
class(co)
#perhaps we can use this to reinterpret most of the pictures: what do they map to?
co <- as.data.frame(co)
co <- arrange(co, -(x)) 
top <- co[,"i"][1:10]
save(co, file = "coefficients_voxel1.RData")
#already standardized so let's look at the top ones for other successful models

file.names=as.list(dir(pattern="cv_lasso_voxel*"))
file.names
lapply(file.names,load,.GlobalEnv)


co10 <- coef(cv.lasso.10,s="lambda.min")
#only 97 non zero coefficients:
co10 <- summary(co10)

#perhaps we can use this to reinterpret most of the pictures: what do they map to?
co10 <- as.data.frame(co10)
co10 <- arrange(co10, -(x)) 
top <- co10[,"i"][1:10]
save(co10, file = "coefficients_voxel10.RData")

co14 <- coef(cv.lasso.14,s="lambda.min")
co14 <- summary(co14)

#perhaps we can use this to reinterpret most of the pictures: what do they map to?
co14 <- as.data.frame(co14)
co14 <- arrange(co14, -(x)) 
top <- co14[,"i"][1:10]
save(co14, file = "coefficients_voxel14.RData")

