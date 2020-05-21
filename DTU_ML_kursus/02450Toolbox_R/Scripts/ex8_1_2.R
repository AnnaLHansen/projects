# exercise 8.1.2
rm(list=ls())
source("~/projects/DTU_ML_kursus/02450Toolbox_R/setup.R")
library(cvTools)

# Load data
library(R.matlab)
dat <- readMat(file.path('DTU_ML_kursus/02450Toolbox_R/Data', 'wine2.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))

# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)

## Crossvalidation
# set.seed(1234) # for reproducibility, if needed
CV <- cvFolds(N, K=20) #K=2 -> 50 %, K=20 -> 5 %, K=100 -> 1 %
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Extract the training and test set
X_train <- X[CV$subsets[CV$which==1], ]; # Train on 1/20 of the data, or 5 percent
y_train <- y[CV$subsets[CV$which==1]];
X_test <- X[CV$subsets[CV$which!=1], ]; # Test on the rest
y_test <- y[CV$subsets[CV$which!=1]];
CV$TrainSize[1] <- length(y_train)
CV$TestSize[2] <- length(y_test)

# Standardize based on training set
mu <- colMeans(X_train)
sigma <- apply(X_train, 2, sd)

X_train <- scale(X_train, mu, sigma)
X_test <- scale(X_test, mu, sigma)

X_traindf <- data.frame(X_train)
colnames(X_traindf) <- attributeNames
X_testdf <- data.frame(X_test)
colnames(X_testdf) <- attributeNames

## Fit model
# Fit logistic regression model to training data to predict the type of wine

N_lambdas = 20
lambda_tmp <- 10^(seq(from=-8, to=0, length=N_lambdas))
# alpha=0 gives ridge regression 
# We'll use glmnet to fit, which you can install using: install.packages('glmnet')
mdl <- glmnet(X_train, y_train, family="binomial", alpha=0, lambda=lambda_tmp) 

train_error <- rep(NA,N_lambdas)
test_error <- rep(NA,N_lambdas)
coefficient_norm <- rep(NA,N_lambdas)
for(k in 1:N_lambdas){
  # Predict labels for both sets for current regularization strength
  y_train_est <- predict(mdl, X_train, type="class", s=lambda_tmp[k])
  y_test_est <- predict(mdl, X_test, type="class", s=lambda_tmp[k])
  
  # Determine training and test set error
  train_error[k] = sum(y_train_est != y_train)/length(y_train)
  test_error[k] = sum(y_test_est != y_test)/length(y_test)
  
  # Determine betas and calculate norm of parameter vector
  w_est = predict(mdl, type="coef", s=lambda_tmp[k])[-1] 
  coefficient_norm[k] = sqrt(sum(w_est^2))
}

min_error = min(test_error)
lambda_opt = lambda_tmp[which.min(test_error)]

par(mfrow=c(3,1))

par(cex.main=3) # Define size of title
par(cex.lab=2) # Define size of axis labels
par(cex.axis=2) # Define size of axis labels
par(mar=c(5,6,4,1)+.1) # Increase margin size to allow for larger axis labels

plot(range(log10(lambda_tmp)), range(100*c(test_error, train_error)), type='n',
                                   xlab='Log10(lambda)', ylab='Error (%)',
                                   main='Classification error')
lines(log10(lambda_tmp), train_error*100, col='red')
lines(log10(lambda_tmp), test_error*100, col='blue')
points(log10(lambda_opt), min_error*100, col='green', cex=5)
legend("topleft", c(paste("Training, n=", round(length(y_train), 2)), 
                    paste("Test, n=", round(length(y_test), 2))), 
                  col=c('red', 'blue'), lty=1, cex=2)
grid()

plot(range(-6,-1), range(0, 4), type='n',
     xlab='Log10(lambda)', ylab='Error (%)', main='Classification error (zoomed)')
lines(log10(lambda_tmp), train_error*100, col='red')
lines(log10(lambda_tmp), test_error*100, col='blue')
points(log10(lambda_opt), min_error*100, col='green', cex=5)
text(-4, 1.5, 
     labels= paste('Min error test: ', round(min_error*100, 2), ' % at 1e', round(log10(lambda_opt), 1)),
     cex = 2)
grid()

plot(range(log10(lambda_tmp)), range(coefficient_norm), type='n',
     xlab='Log10(lambda)', ylab='Norm', 
     main='Parameter vector L2-norm')
lines(log10(lambda_tmp), coefficient_norm)
grid()

