library(glmnet)
library(corrplot)
library(ggplot2)
library(reshape2)
library(rgl)
library(dplyr)
library(foreach)
require(doParallel)
require(parallel) 
library(iterators)

RotateImageVector <- function(image.vec) {
  # Rotate a raw image vector into the
  # correct orientation for viewing.
  
  # There are this many rows and columns,
  # and transpose and re-ordering gives it the correct
  # orientation.
  kRows <- 128
  return(t(matrix(image.vec, nrow = kRows)[kRows:1,]))
}


ReadImage <- function(number) {
  # Read a single line from fit_stim.csv into
  # a matrix that can be plotted as an image.
  #
  # Args:
  #  - number: Which image number to load, starting
  #            with 1.
  #
  # Returns:
  #  - A matrix containing the raw image pixels.
  
  # The first line contains column names, so we always
  # skip at least one line.
  img <- scan("fit_stim.csv",
              skip = number, nlines = 1, sep=",")
  
  return(RotateImageVector(img))
  
}


ReadRealBasisFunction <- function(number) {
  # Read a single column from real_wav.csv into
  # a matrix that can be plotted as an image.  This
  # uses a bash command and may not work for windows users. 
  #
  # Args:
  #  - number: Which basis function to load, starting
  #            with 1.
  #
  # Returns:
  #  - A matrix containing the basis function.  NB:
  #    I am not 100% sure that the rotation is the same
  #    for the basis function as for the images.
  
  # Use bash to read a single column into a text file.
  # Note that this may not work for windows users.
  temp.file <- tempfile()
  system(paste("cat real_wav.csv | cut -d, -f ",
               number, " > ",
               temp.file))
  basis.data <- read.csv(temp.file, header=T)[[1]]
  return(RotateImageVector(basis.data))
}



###############################################################
###############################################################
setwd(file.path("/Users/Lishali/Desktop/215A/final_project"))
load("fMRIdata.RData")

#combin first, than scale to unit variance, little difference, but will mimic 
#how we handle new data


PrepRespData <- function(data){
  #input, the resp_dat dataframe, which contains the intensity
  #responses for each of the 20 Voxels
  #1750 X 1 dataframe.  
  
  resp.dat <- data.frame(data)
  resp.dat <- tbl_df(resp.dat)
  names(resp.dat)  <- paste("voxel", 1:ncol(resp.dat), sep="")
  rm(resp_dat)
  return(resp.dat)
}

PrepRespData(resp_dat)

#X = [1750 X 10921]
FitFeatPrep <- function(data){
  #input the fit_feat Rdata file
  #outputs the fit.feat dataframe and delete fit_feat
  fit.feat <- data.frame(data)
  rm(fit_feat)
  fit.feat <- tbl_df(fit.feat)
}

FitFeatPrep(fit_feat)



###############################################################
###############################################################
#I want to pick a training and test set, and within my training, also have separate 
#CV sets.  
#n = 1750, so we take 80% of that, sampled randomly.  

TestData <- function(data, response, fraction){
  # Takes the data and response vector
  # And spits out a test set of the set fraction
  
  combined <- cbind(data, response)     
  set.seed(808) 
  train <- sample_frac(combined, fraction) #dim train: 1400 10941
  
  #Errors
  if all(data!=combined[,21:length(combined)]) stop("Sanity check failed")
  if all(response!=combined[,1:20]) stop("Sanity check failed")
  if (fraction > 1) stop("please give fraction of sample")
  
  return(train)
}

TrainData <- function(data, response, fraction){
  # Takes the data and response vector
  # And spits out a test set of the set fraction

  combined <- cbind(data, response)     
  set.seed(808) 
  train <- sample_frac(combined, fraction) #dim train: 1400 10941
  test <- anti_join(combined, train) 
  
  #Errors
  if all(data!=combined[,21:length(combined)]) stop("Sanity check failed")
  if all(response!=combined[,1:20]) stop("Sanity check failed")
  if (fraction > 1) stop("please give fraction of sample")
  
  return(test)
}


standardize <- function(train){
  
  #standardize X matrix and make NaN's 0
  
  train.standardized <- train[,21:length(train)] %>% mutate_each(funs(scale))
  train <- cbind(train[,1:20], train.standardized)
  train[is.na(as.matrix(train))] = 0
  return(train)
}


###########
TestData(resp.data, fit.feat, 0.5)

TrainData(resp.data, fit.feat, 0.5)

TestData(resp.data, fit.feat, 0.8)

TrainData(resp.data, fit.feat, 0.8)

###############################################################

#data vectors
#voxel 1, 20 and 10 are distance maximizing. 

X <- as.matrix(combined.s[,21:length(combined.s)])

NameY <- function(training.set, i){
  paste0("Y.", i) <- as.matrix(training.set[,i])
  return(paste0("Y.", i))
}

for (i in 1:20){
NameY(train, i)  
}

Y.1 <- as.matrix(combined.s[,1]) #first voxel

Y.2 <- as.matrix(combined.s[,2])
Y.3 <- as.matrix(combined.s[,3])
Y.4 <- as.matrix(combined.s[,4])

Y.5 <- as.matrix(combined.s[,5])

Y.6 <- as.matrix(combined.s[,6])
Y.7 <- as.matrix(combined.s[,7])
Y.8 <- as.matrix(combined.s[,8])
Y.9 <- as.matrix(combined.s[,9])

Y.10 <- as.matrix(combined.s[,10])

Y.11 <- as.matrix(combined.s[,11])
Y.12 <- as.matrix(combined.s[,12])
Y.13 <- as.matrix(combined.s[,13])
Y.14 <- as.matrix(combined.s[,14])

Y.15 <- as.matrix(combined.s[,15])

Y.16 <- as.matrix(combined.s[,16])
Y.17 <- as.matrix(combined.s[,17])
Y.18 <- as.matrix(combined.s[,18])
Y.19 <- as.matrix(combined.s[,19])

Y.20 <- as.matrix(combined.s[,20])

###############################################################
###############################################################
#LASSO
#alpha = 1 (default)
#weights keep default, no prior to suggest otherwise
#nlambda, default = 100, probably fine enough
#lambda.max (automatically computed to be the smallest lambda such that all coefficients are 0)
#standarize = FALSE (need to set, since I already did it)



fit.lasso.1 <- glmnet(X, Y.1, alpha = 1, nlambda = 100, standardize = FALSE)


fit.lasso.2 <- glmnet(X, Y.2, alpha = 1, nlambda = 100, standardize = FALSE)
fit.lasso.3 <- glmnet(X, Y.3, alpha = 1, nlambda = 100, standardize = FALSE)
fit.lasso.4 <- glmnet(X, Y.4, alpha = 1, nlambda = 100, standardize = FALSE)

fit.lasso.5 <- glmnet(X, Y.5, alpha = 1, nlambda = 100, standardize = FALSE)

fit.lasso.6 <- glmnet(X, Y.6, alpha = 1, nlambda = 100, standardize = FALSE)
fit.lasso.7 <- glmnet(X, Y.7, alpha = 1, nlambda = 100, standardize = FALSE)
fit.lasso.8 <- glmnet(X, Y.8, alpha = 1, nlambda = 100, standardize = FALSE)
fit.lasso.9 <- glmnet(X, Y.9, alpha = 1, nlambda = 100, standardize = FALSE)

fit.lasso.10 <- glmnet(X, Y.10, alpha = 1, nlambda = 100, standardize = FALSE)

fit.lasso.11 <- glmnet(X, Y.11, alpha = 1, nlambda = 100, standardize = FALSE)
fit.lasso.12 <- glmnet(X, Y.12, alpha = 1, nlambda = 100, standardize = FALSE)
fit.lasso.13 <- glmnet(X, Y.13, alpha = 1, nlambda = 100, standardize = FALSE)
fit.lasso.14 <- glmnet(X, Y.14, alpha = 1, nlambda = 100, standardize = FALSE)

fit.lasso.15 <- glmnet(X, Y.15, alpha = 1, nlambda = 100, standardize = FALSE)

fit.lasso.16 <- glmnet(X, Y.16, alpha = 1, nlambda = 100, standardize = FALSE)
fit.lasso.17 <- glmnet(X, Y.17, alpha = 1, nlambda = 100, standardize = FALSE)
fit.lasso.18 <- glmnet(X, Y.18, alpha = 1, nlambda = 100, standardize = FALSE)
fit.lasso.19 <- glmnet(X, Y.19, alpha = 1, nlambda = 100, standardize = FALSE)

fit.lasso.20 <- glmnet(X, Y.20, alpha = 1, nlambda = 100, standardize = FALSE)

print(fit.lasso.1)
save(fit.lasso.1, file="lasso_voxel1.RData")
plot(fit.lasso.1, xvar = "lambda", label = TRUE)
plot(fit.lasso.1, xvar = "dev", label = TRUE)
plot(fit.lasso.1, xvar = "norm", label = TRUE)

print(fit.lasso.20)
save(fit.lasso.20, file="lasso_voxel20.RData")
plot(fit.lasso.20, xvar = "lambda", label = TRUE)
plot(fit.lasso.20, xvar = "dev", label = TRUE)
plot(fit.lasso.20, xvar = "norm", label = TRUE)

print(fit.lasso.10)
save(fit.lasso.10, file="lasso_voxel10.RData")
plot(fit.lasso.10, xvar = "lambda", label = TRUE)
plot(fit.lasso.10, xvar = "dev", label = TRUE)
plot(fit.lasso.10, xvar = "norm", label = TRUE)


plot(fit.lasso.11, xvar = "lambda", label = TRUE)
plot(fit.lasso.12, xvar = "lambda", label = TRUE)
plot(fit.lasso.13, xvar = "lambda", label = TRUE)
plot(fit.lasso.14, xvar = "lambda", label = TRUE)
plot(fit.lasso.16, xvar = "lambda", label = TRUE)
plot(fit.lasso.17, xvar = "lambda", label = TRUE)

save(fit.lasso.15, file="lasso_voxel15.RData")
save(fit.lasso.5, file="lasso_voxel5.RData")

save(fit.lasso.2, file="lasso_voxel2.RData")
save(fit.lasso.3, file="lasso_voxel3.RData")
save(fit.lasso.4, file="lasso_voxel4.RData")
save(fit.lasso.6, file="lasso_voxel6.RData")
save(fit.lasso.7, file="lasso_voxel7.RData")
save(fit.lasso.8, file="lasso_voxel8.RData")
save(fit.lasso.9, file="lasso_voxel9.RData")
save(fit.lasso.11, file="lasso_voxe11.RData")
save(fit.lasso.12, file="lasso_voxel2.RData")
save(fit.lasso.13, file="lasso_voxel3.RData")
save(fit.lasso.14, file="lasso_voxel4.RData")
save(fit.lasso.16, file="lasso_voxel6.RData")
save(fit.lasso.17, file="lasso_voxel7.RData")
save(fit.lasso.18, file="lasso_voxel8.RData")
save(fit.lasso.19, file="lasso_voxel9.RData")

###############################################################
###############################################################
#ridge


fit.ridge.1 <- glmnet(X, Y.1, alpha = 0, nlambda = 100, standardize = FALSE)
print(fit.ridge.1)
plot(fit.ridge.1, xvar = "lambda", label = TRUE)
plot(fit.ridge.1, xvar = "dev", label = TRUE)
plot(fit.ridge.1, xvar = "norm", label = TRUE)
###############################################################
###############################################################
#alpha = 0.5

fit.half.1 <- glmnet(X, Y.1, alpha = 0.5, nlambda = 100, standardize = FALSE)
print(fit.half.1)
plot(fit.half.1, xvar = "lambda", label = TRUE)
plot(fit.half.1, xvar = "dev", label = TRUE)
plot(fit.half.1, xvar = "norm", label = TRUE)


###############################################################
###############################################################
registerDoParallel(4)
#Let's try with CV to anchor down the right lambda penalty, 
#we will also need to loock at AIC, AICc, BIC and ES-CV

#Ridge
#alpha = 0 (need to set)

#try to choose optimal alpha

#with CV:
set.seed(1988)
foldid=sample(1:10, size = length(Y.9), replace=TRUE)
cv.lasso <- cv.glmnet(X, Y.1, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.ridge <- cv.glmnet(X, Y.1, foldid = foldid, alpha = 0, nlambda = 100, standardize = FALSE)
cv.half <- cv.glmnet(X, Y.1, foldid = foldid, alpha = 0.5, nlambda = 100, standardize = FALSE)


cv.lasso.20 <- cv.glmnet(X, Y.20, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.10 <- cv.glmnet(X, Y.10, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.5 <- cv.glmnet(X, Y.5, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.15 <- cv.glmnet(X, Y.15, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)

cv.lasso.2 <- cv.glmnet(X, Y.2, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.3 <- cv.glmnet(X, Y.3, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.4 <- cv.glmnet(X, Y.4, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.6 <- cv.glmnet(X, Y.6, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.7 <- cv.glmnet(X, Y.7, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.8 <- cv.glmnet(X, Y.8, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.9 <- cv.glmnet(X, Y.9, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.11 <- cv.glmnet(X, Y.11, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.12 <- cv.glmnet(X, Y.12, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.13 <- cv.glmnet(X, Y.13, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.14 <- cv.glmnet(X, Y.14, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.16 <- cv.glmnet(X, Y.16, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.17 <- cv.glmnet(X, Y.17, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.18 <- cv.glmnet(X, Y.18, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.lasso.19 <- cv.glmnet(X, Y.19, foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)

save(cv.lasso, file = "cv_lasso_voxel1.RData")
save(cv.lasso.20, file = "cv_lasso_voxel20.RData")
save(cv.lasso.10, file = "cv_lasso_voxel10.RData")
save(cv.lasso.5, file = "cv_lasso_voxel5.RData")
save(cv.lasso.15, file = "cv_lasso_voxel15.RData")
save(cv.lasso.2, file = "cv_lasso_voxel2.RData")
save(cv.lasso.3, file = "cv_lasso_voxel3.RData")
save(cv.lasso.4, file = "cv_lasso_voxel4.RData")
save(cv.lasso.6, file = "cv_lasso_voxel6.RData")
save(cv.lasso.7, file = "cv_lasso_voxel7.RData")
save(cv.lasso.8, file = "cv_lasso_voxel8.RData")
save(cv.lasso.9, file = "cv_lasso_voxel9.RData")
save(cv.lasso.11, file = "cv_lasso_voxel11.RData")
save(cv.lasso.12, file = "cv_lasso_voxel12.RData")
save(cv.lasso.13, file = "cv_lasso_voxel13.RData")
save(cv.lasso.14, file = "cv_lasso_voxel14.RData")
save(cv.lasso.16, file = "cv_lasso_voxel16.RData")
save(cv.lasso.17, file = "cv_lasso_voxel17.RData")
save(cv.lasso.18, file = "cv_lasso_voxel18.RData")
save(cv.lasso.19, file = "cv_lasso_voxel19.RData")

pdf("cv_lasso1.pdf")
plot(cv.lasso) #110  
dev.off()

pdf("cv_lasso1_half.pdf")
plot(cv.half) #110
dev.off()

pdf("cv_ridge1.pdf")
plot(cv.ridge) 
dev.off()


pdf("cv_lasso20.pdf")
plot(cv.lasso.20)
dev.off()

pdf("cv_lasso10.pdf")
plot(cv.lasso.10)
dev.off()

pdf("cv_lasso5.pdf")
plot(cv.lasso.5)
dev.off()

pdf("cv_lasso15.pdf")
plot(cv.lasso.15)
dev.off()

pdf("cv_lasso16.pdf")
plot(cv.lasso.16)
dev.off()

print(cv.lasso.11)

plot(cv.lasso.11)
plot(cv.lasso.12)
plot(cv.lasso.13)
plot(cv.lasso.14)
plot(cv.lasso.16)
plot(cv.lasso.17)
plot(cv.lasso.18)
plot(cv.lasso.19)

################################################################
################################################################

#AIC and BIC and AICc
#we need to rescale appropriately to get a good fit
variance <- read.table("variance_test_var.csv")
mean<- read.table("mean_test_var.csv")

save(test, file="test_8020.RData")
load("test_8020.RData")
a<- test
a <- scale(a[,21:length(a)], center = t(mean), scale = t(sqrt(variance)))
#looking at this scaling, I notice that the mean still lies the same, due to our random sampling

save(a, file="test_X_standarized.RData")


newX <- as.matrix(a)
newY <- test[,1:20]
newY.1 <- newY[,1]
save(newY, file="test_Y_8020.RData")


lasso.predict <- function(index){
  return(predict(get(paste0("cv.lasso.",index)), 
                 newx=newX, se.fit = TRUE, s = "lambda.min"))
}

lasso.predict.1se <- function(index){
  return(predict(get(paste0("cv.lasso.",index)), 
                 newx=newX, se.fit = TRUE, s = "lambda.1se"))
}


lasso.pred <- lapply(1:20, FUN=lasso.predict)
lasso.pred.1se <- lapply(1:20, FUN = lasso.predict.1se)
lasso.pred <- as.data.frame(lasso.pred)
lasso.pred <- tbl_df(lasso.pred)
lasso.pred.1se <- as.data.frame(lasso.pred.1se)
lasso.pred.1se <- tbl_df(lasso.pred.1se)
colnames(lasso.pred) <- sprintf("voxel%d", 1:20)
lasso.pred.1se
#this is only done for the best lambda
cv.lasso.pred <- lasso.pred

save(cv.lasso.pred, file = "lasso_cv_predicted_allvoxels.RData")

Y <- test[,1:20]

ES.CV.RSS <- (Y-cv.lasso.pred)^2
ES.CV.RSS <- ES.CV.RSS/350
MSE.cv.all <- apply(ES.CV.RSS, 2, sum)
ES.CV.RSS <- rbind(ES.CV.RSS, MSE.cv.all)
ES.CV.RSS

ES.CV.RSS <- (Y-lasso.pred.1se)^2
ES.CV.RSS <- ES.CV.RSS/350
MSE.cv.all <- apply(ES.CV.RSS, 2, sum)

MSE.cv.all

summary(MSE.cv.all)
####################################################################


#for AIC, we need:
lasso.results <- read.table("lasso_1_results.txt") #had to copy and paste the glmnet creature manually, how silly
colnames(RSS) <- lasso.results$Lambda
RSS["Df",] <- lasso.results$Df
AIC.table <- rbind(RSS[352,],RSS["Df",])
AIC.table <- t(AIC.table)
colnames(AIC.table)[1] <- "RSS"
AIC.table <- as.data.frame(AIC.table)
AIC.table <- tbl_df(AIC.table)
AIC.table<- AIC.table %>% mutate(AIC = 350*log(RSS)+(Df+1)*2)
AIC.table<- AIC.table %>% mutate(BIC = 350*log(RSS)+(Df)*log(350))
summary(AIC.table$Df)#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00   65.25  378.00  528.50  980.00 1406.00 
AIC.table<- AIC.table %>% mutate(AICc = AIC + (2*Df^2+2*Df)/1406)
#AICc doesn't really make sense in this regime, I used max(df) in the numerator

AIC.table$Lambda <- lasso.results$Lambda
AIC.table$Lambda <- as.numeric(AIC.table$Lambda)
AIC.table["AIC"]
AIC.table["BIC"]
AIC.table["AICc"]
plot(cv.lasso)
ggplot(AIC.table)+geom_point(aes(x=log(Lambda), y=AIC, color = "red"))+geom_point(aes(x=log(Lambda), y=BIC)) +geom_point(aes(x=log(Lambda), y=AICc, colour = "green"))

#prediction error:
ggplot(AIC.table)+geom_point(aes(x=log(Lambda), y=RSS))
summary(AIC.table$RSS)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.9035  0.9302  1.0080  1.0680  1.1680  1.4570 

#so far doing way better than random forest
#####################################################################

file.names=as.list(dir(pattern="cv_lasso_voxel*"))
file.names
lapply(file.names,load,.GlobalEnv)
#calculate the covariance between Y^hat and Y
cv.lasso.pred <-
#first get the dot product
Corr <- cor((as.matrix(newY)),(as.matrix(lasso.pred.1se)))
summary(Corr)
Corr
Correlations <- diag(Corr)
Correlations
summary(Correlations)
save(Correlations, file="Voxel_8020_correlations.RData")
save(Corr, file="corrplot_1se.RData")
load("corrplot.RData")

Corr[is.na(Corr)]=0
pdf("corrplot_1s.pdf")
corrplot(Corr, method = "ellipse")
dev.off()
Corr
Corr.rounded <- signif(as.matrix(Corr), digits = 2)
Corr.rounded <- apply(Corr, signif(.,digits=3))
library(gridExtra);
maxcol = 10;
npages = 2;
pdf("Corr_table1.pdf", height=11.5, width=8)
grid.table(Corr.rounded[1:10])
dev.off()

pdf("Corr_table2.pdf", height=11.5, width=8)
grid.table(Corr.rounded[11:20])
dev.off()



#####################################################################
#let's try with the other data, with mgaussian:
load("cv_lasso_mg.RData")
cv.lasso.all

predict.mg <- predict(cv.lasso.all, as.matrix(a))

dim(predict.mg)
predict.mg <- predict.mg[,,1]
head(predict.mg)


Corr.mg <- cor(newY, predict.mg)
save(Corr.mg, file= "multigaussian_cv_correlation.RData")

dim(Corr.mg)
pdf("corrplot_mg.pdf")
corrplot(Corr.mg, method = "ellipse")
dev.off()

library(gridExtra)
pdf("trade.pdf", height=11, width=8.5)
grid.table(df)
dev.off()

#now try it with variance lambda:

summary(Y[["voxel20"]]^2)
sum(Y[["voxel20"]]^2)

lasso.pred <- as.data.frame(lasso.pred)
lasso.pred <- tbl_df(lasso.pred)
lasso.pred$Y <- test[,1]
lasso.pred # now consists of 100 differenct Y^hat with the true Y in the last column

RSS <- lasso.pred$Y-lasso.pred[,1:100]
#sanity check

sum.squared <- function(entries){
  return(sum(entries^2)/350)
}
squared <- function(entries){
  return(entries^2)
}
nrow(RSS)
RSS[351,] <- apply(RSS[1:350,], 2, sum.squared)
RSS[352,] <- apply(RSS[1:350,]^2/350, 2, sum)
all(RSS[351,]==RSS[352,])
summary(RSS[351,]-RSS[352,]) #ok, just a rounding error


#so the last row is the RSS

#for AIC, we need:
lasso.results <- read.table("lasso_1_results.txt") #had to copy and paste the glmnet creature manually, how silly
colnames(RSS) <- lasso.results$Lambda
RSS["Df",] <- lasso.results$Df
AIC.table <- rbind(RSS[352,],RSS["Df",])
AIC.table <- t(AIC.table)
colnames(AIC.table)[1] <- "RSS"
AIC.table <- as.data.frame(AIC.table)
AIC.table <- tbl_df(AIC.table)
AIC.table<- AIC.table %>% mutate(AIC = 350*log(RSS)+(Df+1)*2)
AIC.table<- AIC.table %>% mutate(BIC = 350*log(RSS)+(Df)*log(350))
summary(AIC.table$Df)#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00   65.25  378.00  528.50  980.00 1406.00 
AIC.table<- AIC.table %>% mutate(AICc = AIC + (2*Df^2+2*Df)/1406)
#AICc doesn't really make sense in this regime, I used max(df) in the numerator

AIC.table$Lambda <- lasso.results$Lambda
AIC.table$Lambda <- as.numeric(AIC.table$Lambda)
AIC.table["AIC"]
AIC.table["BIC"]
AIC.table["AICc"]
plot(cv.lasso)
ggplot(AIC.table)+geom_point(aes(x=log(Lambda), y=AIC, color = "red"))+geom_point(aes(x=log(Lambda), y=BIC)) +geom_point(aes(x=log(Lambda), y=AICc, colour = "green"))

#prediction error:
ggplot(AIC.table)+geom_point(aes(x=log(Lambda), y=RSS))
summary(AIC.table$RSS)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.9035  0.9302  1.0080  1.0680  1.1680  1.4570 

#so far doing way better than random forest
#regularization seems to be really helping the MSE of this model, it seems not to be very linear, hence we get large mse with the OLS fit at the onset. 
#dispite AIC-BIC not applying in the right regime, it seems that it has selected the same lambda as CV

# we have used the assumption that hte error is iid, normal



write.table(test[,1:20], file="test_response.csv")



################################################################
################################################################

#ES-CV

# pretty robust variable selection
################################################################


################################################################
################################################################
#how robust is everything if we try a 60 40 split? Is lasso still awesome?  Or should we use rf?



set.seed(686) 
train2 <- sample_frac(combined, 0.5) #dim train: 875 10941
test2 <- anti_join(combined, train2) #dim test: 875 10941

###############################################################
#standardizing 

#when we rescale the Xs do we do so for the Ys??? no
#let's scale
train2 <- train2 %>% mutate_each(funs(scale), X1:X10921)
combined.s2 <- train2

#vector of training variances for the 10941 features:
variance2 <- apply(train2[,21:length(train2)], 2, var)
mean2 <- apply(train2[,21:length(train2)], 2, mean)

write.table(variance2, file= "variance2_test_var.csv")
write.table(mean2, file= "mean2_test_var.csv")
#sanity check
var(combined.s2$X1)
summary(mutate_each(combined.s2[,1:100], funs(var)))

#make sure all NaN's are 0

#the test data needs to be standarized in the same way the training was
#especially if finite sample we expect the training and test to be very different


combined.s2[is.na(as.matrix(combined.s2))] = 0

#sanity check
all(train2[,1:20]==combined.s2[,1:20]) #true


write.table(combined.s2, file="train2.csv")

#to choose the right model, we need to compare CV, BIC, AIC and AICc

#then we can use correlation between the two Y vectors, and other 
#similarity metrics to measure the closeness (MSE?)
#finally, this gives us hope that we can measure the remaining test set

###############################################################
###############################################################

#data vectors

X <- as.matrix(combined.s2[,21:length(combined.s2)])
Y.1 <- as.matrix(combined.s2[,1]) #first voxel

###############################################################
###############################################################
#LASSO
#alpha = 1 (default)
#weights keep default, no prior to suggest otherwise
#nlambda, default = 100, probably fine enough
#lambda.max (automatically computed to be the smallest lambda such that all coefficients are 0)
#standarize = FALSE (need to set, since I already did it)


#keep lambda small for the first run

fit.lasso.1 <- glmnet(X, Y.1, alpha = 1, nlambda = 100, standardize = FALSE)

print(fit.lasso.1)
plot(fit.lasso.1, xvar = "lambda", label = TRUE)
plot(fit.lasso.1, xvar = "dev", label = TRUE)
plot(fit.lasso.1, xvar = "norm", label = TRUE)



###############################################################
###############################################################
#ridge


fit.ridge.1 <- glmnet(X, Y.1, alpha = 0, nlambda = 100, standardize = FALSE)
print(fit.ridge.1)
plot(fit.ridge.1, xvar = "lambda", label = TRUE)
plot(fit.ridge.1, xvar = "dev", label = TRUE)
plot(fit.ridge.1, xvar = "norm", label = TRUE)
###############################################################
###############################################################
#alpha = 0.5

fit.half.1 <- glmnet(X, Y.1, alpha = 0.5, nlambda = 100, standardize = FALSE)
print(fit.half.1)
plot(fit.half.1, xvar = "lambda", label = TRUE)
plot(fit.half.1, xvar = "dev", label = TRUE)
plot(fit.half.1, xvar = "norm", label = TRUE)


###############################################################
###############################################################
registerDoParallel(4)
#Let's try with CV to anchor down the right lambda penalty, 
#we will also need to loock at AIC, AICc, BIC and ES-CV

#Ridge
#alpha = 0 (need to set)

#try to choose optimal alpha

#with CV:
set.seed(1888)
foldid=sample(1:10, size = length(Y.1), replace=TRUE)
set.seed(2)
foldid=sample(1:5, size = nrow(Y), replace=TRUE)

cv.lasso <- cv.glmnet(as.matrix(X), as.matrix(Y.1), foldid = foldid, alpha = 1, nlambda = 100, standardize = FALSE)
cv.ridge <- cv.glmnet(X, Y.1, foldid = foldid, alpha = 0, nlambda = 100, standardize = FALSE)
cv.half <- cv.glmnet(X, Y.1, foldid = foldid, alpha = 0.5, nlambda = 100, standardize = FALSE)


plot(cv.lasso) #110  
plot(cv.half) #110
plot(cv.ridge) 

################################################################
################################################################

#AIC and BIC and AICc
#we need to rescale appropriately to get a good fit
variance2
mean2

a <- scale(test2[,21:length(test2)], center = mean2, scale = sqrt(variance2))
#looking at this scaling, I notice that the mean still lies the same, due to our random sampling

write.table(a, file = "test_standarized2.csv")
newX <- as.matrix(a)
newY.1 <- test2[,1]

lasso.pred=predict(fit.lasso.1,newx=newX)
lasso.pred.cv <- predict(cv.lasso, newx=newX)
class(lasso.pred)
dim(lasso.pred)#350 X 100, since 100 lambdas.  For each lambda, we will calculate
#AIC

lasso.pred <- as.data.frame(lasso.pred)
lasso.pred <- tbl_df(lasso.pred)
lasso.pred$Y <- test2[,1]
lasso.pred # now consists of 100 differenct Y^hat with the true Y in the last column

lasso.pred.cv <- as.data.frame(lasso.pred.cv)
lasso.pred.cv <- tbl_df(lasso.pred.cv)


RSS <- lasso.pred$Y-lasso.pred[,1:100]
#sanity check

sum.squared <- function(entries){
  return(sum(entries^2)/875)
}
squared <- function(entries){
  return(entries^2)
}
nrow(RSS)
RSS[875+1,] <- apply(RSS[1:875,], 2, sum.squared)
RSS[875+2,] <- apply(RSS[1:875,]^2/875, 2, sum)
all(RSS[875+1,]==RSS[875+2,])
summary(RSS[876,]-RSS[877,]) #ok, just a rounding error

#do it for the cv.pred (should be not so diff):

RSS2 <- lasso.pred$Y-lasso.pred.cv[,1:100]
#sanity check

sum.squared <- function(entries){
  return(sum(entries^2)/875)
}
squared <- function(entries){
  return(entries^2)
}
nrow(RSS)
RSS[875+1,] <- apply(RSS[1:875,], 2, sum.squared)
RSS[875+2,] <- apply(RSS[1:875,]^2/875, 2, sum)
all(RSS[875+1,]==RSS[875+2,])
summary(RSS[876,]-RSS[877,]) #ok, just a rounding error

#so the last row is the RSS

#for AIC, we need:
lasso.results <- read.table("2lasso_cv_1.txt") #had to copy and paste the glmnet creature manually, how silly
colnames(RSS) <- lasso.results$Lambda
RSS["Df",] <- lasso.results$Df
AIC.table <- rbind(RSS[876,],RSS["Df",])
AIC.table <- t(AIC.table)
colnames(AIC.table)[1] <- "RSS"
AIC.table <- as.data.frame(AIC.table)
AIC.table <- tbl_df(AIC.table)
AIC.table<- AIC.table %>% mutate(AIC = 875*log(RSS)+(Df+1)*2)
AIC.table<- AIC.table %>% mutate(BIC = 875*log(RSS)+(Df)*log(875))
summary(AIC.table$Df)#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0.0    51.5   330.0   375.6   677.8   919.0
AIC.table<- AIC.table %>% mutate(AICc = AIC + (2*Df^2+2*Df)/919.0)
#AICc doesn't really make sense in this regime, I used max(df) in the numerator

AIC.table$Lambda <- lasso.results$Lambda
AIC.table$Lambda <- as.numeric(AIC.table$Lambda)
AIC.table["AIC"]
AIC.table["BIC"]
AIC.table["AICc"]
plot(cv.lasso)
ggplot(AIC.table)+geom_point(aes(x=log(Lambda), y=AIC, color = "red"))+geom_point(aes(x=log(Lambda), y=BIC)) +geom_point(aes(x=log(Lambda), y=AICc, colour = "green"))

#prediction error:
ggplot(AIC.table)+geom_point(aes(x=log(Lambda), y=RSS))
summary(AIC.table$RSS)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.9035  0.9302  1.0080  1.0680  1.1680  1.4570 

#so far doing way better than random forest
#regularization seems to be really helping the MSE of this model, it seems not to be very linear, hence we get large mse with the OLS fit at the onset. 
#dispite AIC-BIC not applying in the right regime, it seems that it has selected the same lambda as CV

# we have used the assumption that hte error is iid, normal

write.table(AIC.table, file="AIC_table2.csv")

write.table(test[,1:20], file="test_response.csv")



################################################################
################################################################

#ES-CV

# pretty robust variable selection
################################################################




###############################################################
###############################################################

#20 voxels.  For the multivariate case, we can choose a lambda independently and fit glmnet for each, or we can try the multi response one to choose the same lambda accross voxels
#the latter perhaps involves the assumption that a lambda 
#that works for one, should not be too different for the other.
#These voxels are responding to the same stimulus, and are all
#supposed to be in the same V1 processing group.  We test this
#assumption by fitting indivudally for a sample of 3, and also 
#doing multigaussian glment.  Though euclidean distance is not 
#the best metric for the topographic difference between voxels,
#in the absence of info for that map, we choose voxels that are
#very far apart in order to test this hypothesis.
#this extra constraint may actually be a buffer against overfitting  and is perhaps
#more interpretable.


X <- as.matrix(combined.s[,21:length(combined.s)])
Y <- as.matrix(combined.s[,1:20]) #all 20 voxels

cv.lasso.all <- cv.glmnet(x=X, y=Y, foldid=foldid, parallel=TRUE,alpha = 1, family = "mgaussian", nlambda = 100, standardize = FALSE)

#ES-CV for voxel1:

X <- combined.s[,21:length(combined.s)]
class(X)
table <- read.table("lambda_cv_lasso1.txt")
s.list <- table$Lambda
class(s.list)
s.list


cv.lasso.1 <- cv.lasso
cv.lasso.1$cvsd #this is a vector of standard deviations
#I just need to compute the l^2 norm of Y^hat
pred.voxel.1 <- predict(cv.lasso.1, as.matrix(X), s=s.list)
dim(pred.voxel.1)

L2.norm <- t(pred.voxel.1) %*% pred.voxel.1
dim(L2.norm)
L2.norm <- diag(L2.norm)
L2.norm

ESCV.table <- cbind(L2.norm, cv.lasso.1$cvsd)
colnames(ESCV.table) <- c("L2.norm", "variance")
class(ESCV.table)
ESCV.table <- as.data.frame(ESCV.table)
ESCV.table <- mutate(ESCV.table, variance = variance^2)
ESCV.table <- mutate(ESCV.table, ESCV = variance/L2.norm)
ESCV.table$Lambda <- table$Lambda

save(ESCV.table, file = "ESCV_table_lasso.RData")

ggplot(ESCV.table)+geom_point(aes(x=log(Lambda), y=ESCV))

ggplot(ESCV.table) + geom_point(aes(x=log(Lambda), y =variance ))

#with 5 CV folds we get:

cv.lasso$
plot(cv.lasso)

pred.voxel1.5 <- predict(cv.lasso, as.matrix(X), s=s.list)
dim(pred.voxel1.5)
L2.norm <- t(pred.voxel1.5) %*% pred.voxel1.5
dim(L2.norm)
L2.norm <- diag(L2.norm)
L2.norm
ESCV.5 <- cbind(L2.norm, cv.lasso$cvsd)
colnames(ESCV.5) <- c("L2.norm", "variance")
ESCV.5 <- as.data.frame(ESCV.5)
ESCV.5 <- mutate(ESCV.5, variance = variance^2)
ESCV.5 <- mutate(ESCV.5, ESCV = variance/L2.norm)
table <- read.table("cv_5_folds.txt")
ESCV.5$Lambda <- table$Lambda

save(ESCV.5, file = "ESCV_5folds.RData")
ggplot(ESCV.5) + geom_point(aes(x=log(Lambda), y =ESCV ))
ggplot(ESCV.5) + geom_point(aes(x=log(Lambda), y =variance ))
