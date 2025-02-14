library(caret)
library(magrittr)
library(cvTools)
source("DTU_ML_kursus/02450Toolbox_R/setup.R")
train <- readRDS("~/projects/train_standard.rds")
X <- as.matrix(train[, 2:39])
y <- as.matrix(train[, 1])
N <- nrow(X)
M <- ncol(X)
attributeNames <- colnames(X)




# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(PimaIndiansDiabetes[,1:8],
               PimaIndiansDiabetes[,9], 
               sizes=c(1:8), 
               rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))



# Linear regression criterion function

# This function takes as input a training and a test set.
#  1. It fits a linear model on the training set using lm.
#  2. It estimates the output of the test set using predict.
#  3. It computes the sum of squared errors.
funLinreg <- function(X_train, y_train, X_test, y_test){
  Xr <- data.frame(X_train)
  Xtest <- data.frame(X_test)
  if(dim(as.matrix(X_train))[2]!=0){
    xnam <- paste("X", 1:dim(as.matrix(X_train))[2], sep="")
    colnames(Xr) <- xnam
    colnames(Xtest) <- xnam
    (fmla <- as.formula(paste("y_train ~ ", paste(xnam, collapse= "+"))))
  }else{
    xnam <- 1
    (fmla <- as.formula(paste("y_train ~ ", paste(xnam, collapse= "+"))))
  }
  mod = lm(fmla, data=Xr)
  preds <- predict(mod, newdata = Xtest)
  sum((y_test-preds)^2)  
}

## Crossvalidation
# Create crossvalidation partition for evaluation
K = 5;
set.seed(1234) # for reproducibility
CV <- cvFolds(N, K=K)
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Initialize variables
Features <- matrix(rep(NA, times=K*M), nrow=K)
Error_train <- matrix(rep(NA, times=K), nrow=K)
Error_test <- matrix(rep(NA, times=K), nrow=K)
Error_train_fs <- matrix(rep(NA, times=K), nrow=K)
Error_test_fs <- matrix(rep(NA, times=K), nrow=K)

# For each crossvalidation fold
for(k in 1:K){
  paste('Crossvalidation fold ', k, '/', K, sep='')
  
  # Extract the training and test set
  X_train <- X[CV$subsets[CV$which!=k], ];
  y_train <- y[CV$subsets[CV$which!=k]];
  X_test <- X[CV$subsets[CV$which==k], ];
  y_test <- y[CV$subsets[CV$which==k]];
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  # Use 10-fold crossvalidation for sequential feature selection
  fsres <- forwardSelection(funLinreg, X_train, y_train, stoppingCrit='minCostImprovement');
  
  # Extract selected features from the forward selection routing
  selected.features <- fsres$featsIncluded
  
  # Save the selected features
  Features[k,] = fsres$binaryFeatsIncluded    
  # Compute squared error without feature subset selection
  Error_train[k] = funLinreg(X_train, y_train, X_train, y_train);
  Error_test[k] = funLinreg(X_train, y_train, X_test, y_test);
  # Compute squared error with feature subset selection
  Error_train_fs[k] = funLinreg(X_train[,selected.features], y_train, X_train[,selected.features], y_train);
  Error_test_fs[k] = funLinreg(X_train[,selected.features], y_train, X_test[,selected.features], y_test);
  
  # Show variable selection history
  # mfig(sprintf('(%d) Feature selection',k));
  I = length(fsres$costs) # Number of iterations
  par(mfrow=c(1,2))
  # Plot error criterion
  plot(fsres$costs, xlab='Iteration', ylab='Squared error (crossvalidation)', main='Value of error criterion');
  # Plot feature selection sequence
  bmplot(attributeNames, 1:I, fsres$binaryFeatsIncludedMatrix);
}


# Display results
print('Linear regression without feature selection:')
print(paste('- Training error:', sum(Error_train)/sum(CV$TrainSize)));
print(paste('- Test error:', sum(Error_test)/sum(CV$TestSize)));

print('Linear regression with sequential feature selection:');
print(paste('- Training error:', sum(Error_train_fs)/sum(CV$TrainSize)));
print(paste('- Test error:', sum(Error_test_fs)/sum(CV$TestSize)));

# Show the selected features
dev.off()
bmplot(attributeNames, 1:K, Features, xlab='Crossvalidation fold', ylab='', main='Attributes selected');

