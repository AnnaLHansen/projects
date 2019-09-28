####################
# exercise 2.2.1
####################
# clear workspace before starting this exercise
rm(list = ls(all = TRUE))

# load the library R.matlab to enable the function readMat, which allows R to read the matlab .mat format. If you do not have the package R.matlab installed, get it by executing the command install.packages("R.matlab"). The R.matlab package depends on the package R.utils, so install this package first, using install.packages("R.utils")
library(R.matlab)
# the row of training data that we will look at
i <- 1 
# read in the data
dat <- readMat(file.path('Data', 'zipdata.mat'))
# check that the structure dat contains two matrices, testdata and traindata
names(dat)

# extract the matrices testdata and traindata from dat
testdata <- dat$testdata
traindata <- dat$traindata

# the features, i.e. images of digits, are stored in the rows of traindata, except for the first column. The first column contains the class of the row, i.e. the digit identity
X = traindata[,2:dim(traindata)[2]]
y = traindata[,1]

# view the i'th row of X
image(as.matrix(X[i,]), axes=FALSE, xlab="Pixel number", main="Digit in vector format")
# make ticks corresponding to pixels, i.e. values in row i
axis(1, at=(1:length(X[i,]))/length(X[i,]), labels=1:length(X[i,]))

# extract the i'th row of X and store it as the matrix I
I = X[i,]
# make the matrix I have dimensions 16 by 16
dim(I)=c(16,16)

# view the digit in the i'th row of X as an image. The function image rotates the matrix, so we need to rearrange the columns in the matrix I in order to get the correct view of the digit. 
image(I[,ncol(I):1], col=gray((32:0)/32), main="Digit as an image")
