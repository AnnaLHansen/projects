# exercise 4.1.6

rm(list = ls(all = TRUE))
library(MASS)
# Digits to include in analysis (to include all, n = 1:10);
n = 1 #c(1,5,9) # can be either a number denoting the digit to analyse or a vector of digits to analyse
n = sort(n)

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

ncols=ncol(traindata)
# Extract digits
X = traindata[,2:ncols];
y = traindata[,1];
classNames = c('0','1','2','3','4','5','6','7','8','9','10');
classLabels = classNames[y+1];

# Remove digits that are not to be inspected
j = match(y, n);
X = X[!is.na(j),];
classLabels = classLabels[!is.na(j)];
classNames = classNames[n+1];
y = y[!is.na(j)]
for(k in 0:(length(n)-1))
{
  classlab = n[k+1]
y[y==classlab]=k
}

# Compute mean, standard deviations, and covariance matrix 
mu = colMeans(X);
s = apply(X, 2, sd)
S = cov(X);

# Plot result
par(mfrow=c(1,2))
I =mu
dim(I)=c(16,16)
image(I[,16:1], main = 'Digits: Mean', col = gray(32:0/32));
I =s
dim(I)=c(16,16)
image(I[,16:1], main = 'Digits: SD', col = gray(32:0/32));
