# exercise 4.3.2
# clear the workspace
rm(list=ls())
# Load the data
library(R.matlab)
dat <- readMat(file.path('Data', 'wine.mat'))
# view content of the Matlab data structure wine.mat
names(dat)
# extract variables
X <- dat$X
y <- dat$y
N <- dat$N
M <- dat$M
C <- dat$C
classNames <- dat$classNames
attributeNames <- dat$attributeNames
# assign attribute names as column names of the data matrix X
colnames(X) <- unlist(attributeNames)
# We start with a box plot of each attribute


# The histograms show that there are a few very extreme values in these
# three attributes. To identify these values as outliers, we must use our
# knowledge about the data set and the attributes. Say we expect volatide
# acidity to be around 0-2 g/dm^3, density to be close to 1 g/cm^3, and
# alcohol percentage to be somewhere between 5-20 % vol. Then we can safely
# identify the following outliers, which are a factor of 10 greater than
# the largest we expect.

idxOutlier = X[,2]>20 | X[,8]>10 | X[,11]>200

# Finally we will remove these from the data set
X = X[-which(idxOutlier),]
y = y[-which(idxOutlier)]
N = N-sum(idxOutlier);

# Now, we can repeat the process to see if there are any more outliers
# present in the data. We take a look at a histogram of all attributes:


##
Attributes = c(1,4,5,6);
NumAtr = length(Attributes)
par(mfrow=c(NumAtr,NumAtr))
par(mar=c(1,1,1,1)+1.5)
for (m1 in 1:NumAtr ) {
  for (m2 in 1:NumAtr ) {
    plot(unlist(X[,Attributes[m2]]), unlist(X[,Attributes[m1]]), col=ifelse(y==1, "red", "black"));
    
   }
}
##