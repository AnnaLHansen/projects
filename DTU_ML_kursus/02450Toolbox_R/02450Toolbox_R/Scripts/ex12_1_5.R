# ex12_1_5 
# Load wine dataset:
library(R.matlab)
library(arules)
dat <- readMat(file.path('Data', 'wine.mat'))
# extract variables
X <- dat$X
y <- dat$y
classNames <- dat$classNames
attributeNames <- dat$attributeNames
source("Tools/binarize2.R")
v <- binarize2(X,attributeNames)
# notice v has fields v$X and v$attributeNames