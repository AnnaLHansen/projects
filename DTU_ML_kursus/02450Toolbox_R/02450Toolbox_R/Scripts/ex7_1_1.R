# exercise 7.1.1

rm(list=ls())
source('setup.R')
library(FNN)
library(cvTools)

# Load data
source("Scripts/ex4_2_1.R")

# Leave-one-out crossvalidation
CV <- cvFolds(N, K=N);
K = N

# K-nearest neighbors parameters
L = c(1, 20, 40)
yhat = array(rep(NA, times=N*length(L) ), dim=c(N,length(L)))
y_true = array(rep(NA, times=N*length(L) ), dim=c(N,1))

for(k in 1:K){ # For each crossvalidation fold
  print(paste('Crossvalidation fold ', k, '/', CV$NumTestSets, sep=''))
  
  # Extract training and test set
  X_train <- X[CV$subsets[CV$which!=k], ];
  y_train <- y[CV$subsets[CV$which!=k]];
  X_test <- X[CV$subsets[CV$which==k], ];
  y_test <- y[CV$subsets[CV$which==k]];
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  X_testdf <- data.frame(X_test)
  colnames(X_testdf) <- attributeNames
  X_traindf <- data.frame(X_train)
  colnames(X_traindf) <- attributeNames
      
  y_true[k,1] = y_test
  for(dl in 1:length(L)){ # For each number of neighbors
      l = L[dl]
      # Use knnclassify to find the l nearest neighbors
      out <- knn(X_traindf, X_testdf, cl=y_train, k = l, prob = FALSE, algorithm="kd_tree")
      y_test_est <- as.numeric(as.character(out[1]))  # one hour of my life wasted on this bullshit      
      yhat[k,dl] = y_test_est
    }
}

## Compute accuracies here using yhat and y_true

