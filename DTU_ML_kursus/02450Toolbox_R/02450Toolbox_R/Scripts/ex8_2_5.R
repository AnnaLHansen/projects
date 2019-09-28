# exercise 8.2.5
rm(list=ls())
graphics.off() # close all open graphics windows
source("setup.R")
library(neuralnet) #install.packages("neuralnet")
library(cvTools)

# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'wine2.mat'))
X <- dat$X
N <- as.numeric(dat$N)
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- as.numeric(dat$M)
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))
# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)

# predict red cs. white wine type. 
# K-fold crossvalidation
K = 10;
set.seed(12345) # for reproducibility
CV <- cvFolds(N, K=K)
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Parameters for neural network classifier
NHiddenUnits = 2;  # Number of hidden units
NTrain = 5; # Number of re-trains of neural network

# Variable for classification error
Error = rep(NA, times=K)
(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse= "+"))))
attributeNames[1+length(attributeNames)] <- 'y_train'

for(k in 1:K){ # For each crossvalidation fold
    print(paste('Crossvalidation fold ', k, '/', K, sep=''))

    # Extract training and test set
    X_train <- X[CV$subsets[CV$which!=k], ];
    y_train <- y[CV$subsets[CV$which!=k]];
    X_test <- X[CV$subsets[CV$which==k], ];
    y_test <- y[CV$subsets[CV$which==k]];
    CV$TrainSize[k] <- length(y_train)
    CV$TestSize[k] <- length(y_test)
        
    X_traindf <- data.frame(X_train)
    X_traindf[, "y_train"] <- y_train
    
    
    colnames(X_traindf) <- attributeNames
    X_testdf <- data.frame(X_test)
    #colnames(X_testdf) <- attributeNames
        
    # Fit neural network to training set
    netwrk = neuralnet(fmla, X_traindf, 
                       rep=NTrain, # number of random restarts of the algorithm
                       hidden=NHiddenUnits, 
                       act.fct='logistic', # extra smoothing on output (used for classification)
                       linear.output=FALSE, 
                       err.fct='sse', # error function
                       lifesign='full', # display stuff to the console
                       threshold = 0.2, # relative threshold for convergence
                       stepmax = 20000, # maximum number of steps
                       algorithm = 'rprop+'); # Using resilient back-prop algorithm
    # Predict model on test data
    computeres <- compute(netwrk, X_testdf,rep = 1)
    y_test_est = unlist(computeres$net.result)
    
    # Compute error rate
    Error[k] = sum((y_test-y_test_est)^2); # Count the number of errors

}
    

# Print the error rate
print(paste('Mean Sum of Squares Error (MSSE): ', sum(Error)/sum(CV$TestSize), sep=''));

# Display the trained network (given for last cross-validation fold)
plot(netwrk,rep=1)

