# exercise 7.1.1

rm(list=ls())
source('DTU_ML_kursus/02450Toolbox_R/setup.R')
library(FNN)
library(cvTools)

# Load data
source("DTU_ML_kursus/02450Toolbox_R/Scripts/ex4_2_1.R")

# Leave-one-out crossvalidation
CV <- cvFolds(N, K=N);
K = N

# K-nearest neighbors parameters
L = c(1, 20, 80)
yhat = array(rep(NA, times=N*length(L) ), dim=c(N,length(L)))
y_true = array(rep(NA, times=N*length(L) ), dim=c(N,1))

for(k in 1:K){ # For each crossvalidation fold
  print(paste('Crossvalidation fold ', k, '/', CV$NumTestSets, sep=''))
  
  # Extract training and test set
  # traekker testsaet ud hvor data for den k' fold ikke er med
  # for baade x og y
  X_train <- X[CV$subsets[CV$which!=k], ];
  y_train <- y[CV$subsets[CV$which!=k]];
  # Traekker herefter den k' fold ud som der testes paa
  X_test <- X[CV$subsets[CV$which==k], ];
  y_test <- y[CV$subsets[CV$which==k]];
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  # Skriver testdata ud i seperate filer:
  X_testdf <- data.frame(X_test)
  colnames(X_testdf) <- attributeNames
  X_traindf <- data.frame(X_train)
  colnames(X_traindf) <- attributeNames
      
  y_true[k,1] = y_test
  for(dl in 1:length(L)){ # For each number of neighbors
      l = L[dl]
      # Use knnclassify to find the l nearest neighbors
      # finder nærmeste nabo og skriver den herefter ind i yhat
      out <- knn(X_traindf, X_testdf, cl=y_train, k = l, prob = FALSE, algorithm="kd_tree")
      y_test_est <- as.numeric(as.character(out[1]))  # one hour of my life wasted on this bullshit      
      yhat[k,dl] = y_test_est
    }
}

## Compute accuracies here using yhat and y_true

