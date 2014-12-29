# This file loads and looks at some of the data sets for the
# STAT215A Fall 2014 final project.

library(corrplot)
library(ggplot2)
library(reshape2)
library(rgl)
library(dplyr)

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

# Load the data.
setwd(file.path("/Users/Lishali/Desktop/215A/final_project"))
load("fMRIdata.RData")
ls()

# Load in a raw image.
img1 <- ReadImage(1)
image(img1, col=gray((1:500) / 501))

# Load in a raw basis function. Here, we look at the gabor wavelet transforms

wav1 <- ReadRealBasisFunction(150)
wav2 <- ReadRealBasisFunction(152)
wav3 <- ReadRealBasisFunction(1009)
image(wav1)
image(wav2)
image(wav3)


###############################################################
###############################################################


# Plot the physical locations of the voxels.
voxel.locs <- data.frame(loc_dat)
rm(loc_dat)
voxel.locs$random.cluster <- sample(1:4, nrow(voxel.locs), replace=TRUE)
rgl.spheres(voxel.locs$X1, voxel.locs$X2, voxel.locs$X3,
            color=voxel.locs$random.cluster, radius=0.3)
#the above is simply some random colouring.  



###############################################################
###############################################################

# Take a look at the distribution of responses.
resp.dat <- data.frame(resp_dat)
resp.dat <- tbl_df(resp.dat)
names(resp.dat)  <- paste("voxel", 1:ncol(resp.dat), sep="")
rm(resp_dat)
resp.melt <- melt(resp.dat)
resp.melt <- tbl_df(resp.melt)
ggplot(resp.melt) +
  geom_density(aes(x=value)) +
  facet_grid(variable ~ .)
dim(resp.melt) # 35000     2
dim(resp.dat) #1750   20, response vectors for each voxel
#the response intensities are decorrelated 
#these are my Y^hat values. 



ggplot(resp.melt) + geom_density(aes(x=value, group = variable, colour=variable))
#intensity density based on voxel is not easy to differentiate.  
#the intensity is centered at 0 and looks normally distributed
#they also look quite similiar.  
ggplot(resp.dat) + geom_density(aes(x=voxel1))
ggplot(resp.dat) + geom_density(aes(x=voxel2))
ggplot(resp.dat) + geom_density(aes(x=voxel3))

corrplot(cor(resp.dat))
#very interesting, this is the correlation in the intensity responses between 
#any two voxels accros the 1750 pictures. 
#the corrplot is much more informative.  Clearly there are clusters
#how significant is less clear.  We can cluster them for sure 

#each voxel's response to a single image has be reduced to a single number
# I guess I should want to know hte details for this.  

# Look at the first image's feature distribution.
#the feature set is less than the 128X128 pixel dataset.  
#we hope to relate the features, gotten from the Gabor transform
#to the voxel intensity

fit.feat <- data.frame(fit_feat)
rm(fit_feat)
fit.feat <- tbl_df(fit.feat)

qplot(x=as.numeric(fit.feat[1, ]), geom="density") #plot of the density of the features
#for one picture
qplot(x=as.numeric(fit.feat[,1]), geom="density") #plot of the density of the feature
#accross pictures

dim(fit.feat) #1750 X 10921 (row are the pictures, columns are the features)


#fit.feat are my features.  They are the same for each voxel, there are 
#1750 responses for each, and each response has 10921 features
#determined by the wavelet, we expect this to be the linear space 
#the preprosing that is correct, in that we can hope that linear regression is successful on this space





melt.feat.1 <- melt(fit.feat[1:9]) #let's plot the first 9 features and look how they are different
colnames(melt.feat.1)
melt.feat.1 <- mutate(melt.feat.1, intensity = as.numeric(value))
melt.feat.1$value <- NULL
is.numeric(melt.feat.1[1,2])

ggplot(melt.feat.1)+geom_density(aes(x=intensity, group=variable, colour = variable))

ggplot(melt.feat.1)+geom_point(aes(x=intensity, group=variable, colour = variable))



melt.feat.a <- melt(fit.feat[,1:30])
melt.feat.a
melt.feat.a <- mutate(melt.feat.a, intensity = as.numeric(value))
melt.feat.a$value <- NULL
is.numeric(melt.feat.1[1,2])



# Look at the validation set.
dim(val_feat)

#let's clean the feature set and standarize






