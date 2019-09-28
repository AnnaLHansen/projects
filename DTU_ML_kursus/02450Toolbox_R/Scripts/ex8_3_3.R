# exercise 8.3.3
rm(list=ls())
source("setup.R")
graphics.off()
library(nnet) #install.packages("mlogit")

# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'synth1.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))

X_train <- dat$X.train
N_train <- dat$N.train
y_train<- dat$y.train

X_test <- dat$X.test
N_test <- dat$N.test
y_test <- dat$y.test

# Standardize based on training set
mu <- colMeans(X_train)
sigma <- apply(X_train, 2, sd)

X_train <- scale(X_train, mu, sigma)
X_test <- scale(X_test, mu, sigma)

# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)
X_traindf <- data.frame(X_train)
colnames(X_traindf) <- attributeNames
X_testdf <- data.frame(X_test)
colnames(X_testdf) <- attributeNames

## Fit a regularized multinomial regression model
regularization_strength = 1e3 
# Try a a high strength, e.g. 1e3 and analyze the distributions of labels
# Especially for synth2, synth3, and synth 4.
model <- glmnet(X_train, y_train, family="multinomial", alpha=0, lambda=regularization_strength) 

# Predict labels for both sets for current regularization strength
y_train_est <- as.integer(predict(model, X_train, type="class", s=regularization_strength))

# Get the predicted output for the test data
y_test_est <- as.integer(predict(model, X_test, type="class", s=regularization_strength))

# Subtract one to have y_test_est between 0 and C-1
y_test_est = y_test_est;

# Compute error rate
ErrorRate = sum(y_test!=y_test_est)/N_test;
print(paste('Error rate: ', ErrorRate*100, '%', sep=''));

breaks=c(seq(-.5, 3.5, 1))
par(mfrow=c(3,1))
par(cex.main=3) # Define size of title
par(cex.lab=2) # Define size of axis labels
par(cex.axis=2) # Define size of axis labels
par(mar=c(5,6,4,1)+.1) # Increase margin size to allow for larger axis labels
hist(y_train, breaks, freq=FALSE, ylim=c(0,1))
hist(y_test, breaks, freq=FALSE, ylim=c(0,1))
hist(y_test_est, breaks, freq=FALSE, ylim=c(0,1))
