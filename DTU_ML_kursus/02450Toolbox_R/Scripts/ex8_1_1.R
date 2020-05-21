# exercise 8.1.1
rm(list=ls())

source("DTU_ML_kursus/02450Toolbox_R/setup.R")
library(cvTools)
library(R.matlab)
library(glmnet)

# Load Data
dat <- readMat(file.path('DTU_ML_kursus/02450Toolbox_R/Data', 'body.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y
T <- 14 # antallet af afpøvede lambdaer

# Regularized Linear regression 
# include an additional attribute corresponding to the offset
X <- cbind( rep(1,N), X) # tilfoejer 1-taller til matrixen
M <- M[1,1]+1
attributeNames <- c("Offset",attributeNames)

# Crossvalidation
# Create crossvalidation partition for evaluation of performance of optimal model
K <- 5
#set.seed(1234) # for reproducibility
CV <- cvFolds(N, K=K)
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# values of lambda
lambda_tmp <- 10^(-5:8)

# Initialise variables
KK <- 10 # inner loop
temp <- rep(NA, M*T*KK); 
w <- array(temp, c(M, T, KK));  

Error_train2 <- matrix(rep(NA, times=T*KK), nrow = T)
Error_test2 <- matrix(rep(NA, times=T*KK), nrow = T)
lambda_opt <- rep(NA, K)
w_rlr <- matrix(rep(NA, times=M*K), nrow=M)
Error_train_rlr <- rep(NA,K)
Error_test_rlr <- rep(NA,K)
w_noreg <- matrix(rep(NA, times=M*K), nrow=M)
mu <- matrix(rep(NA, times=(M-1)*K), nrow=K)
sigma <- matrix(rep(NA, times=(M-1)*K), nrow=K)
Error_train <- rep(NA,K)
Error_test <- rep(NA,K)
Error_train_nofeatures <- rep(NA,K)
Error_test_nofeatures <- rep(NA,K)


for(k in 1:K){

  paste('Crossvalidation fold ', k, '/', K, sep='')
  
  # Extract the training and test set
  X_train <- X[CV$subsets[CV$which!=k], ];
  y_train <- y[CV$subsets[CV$which!=k]];
  X_test <- X[CV$subsets[CV$which==k], ];
  y_test <- y[CV$subsets[CV$which==k]];
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  # Use 10-fold crossvalidation to estimate optimal value of lambda    
  KK <- 10
  
  CV2 <- cvFolds( dim(X_train)[1], K=KK)
  CV2$TrainSize <- c()
  CV2$TestSize <- c()
  
  
  for(kk in 1:KK){
  
    # traekker herefter også traenings og test saettet ud 
    # til den 'indre' krydsvalidering som bruges til at bestemme 
    # den optimale lambda
    X_train2 <- X_train[CV2$subsets[CV2$which!=kk], ]
    y_train2 <- y_train[CV2$subsets[CV2$which!=kk]]
    X_test2 <- X_train[CV2$subsets[CV2$which==kk], ]
    y_test2 <- y_train[CV2$subsets[CV2$which==kk]]
    
    mu2 <- colMeans(X_train2[, 2:24])
    sigma2 <- apply(X_train2[, 2:24], 2, sd)
    
    # hvad laves her? scales er en funktion centrerer kolonnnerne i en matrice
    X_train2[, 2:24] <- scale(X_train2[, 2:24], mu2, sigma2)
    X_test2[, 2:24] <- scale(X_test2[, 2:24], mu2, sigma2)
    
    CV2$TrainSize[kk] <- length(y_train)
    CV2$TestSize[kk] <- length(y_test2)
    
    Xty2 <- t(X_train2) %*% y_train2
    XtX2 <- t(X_train2) %*% X_train2
    
    for(t in 1:length(lambda_tmp)){
      
      # Learn parameter for current value of lambda for the given inner CV_fold
      lambdaI = lambda_tmp[t]*diag(M);
      lambdaI[1,1] = 0; # don't regularize bias
      w[,t,kk] <- solve(XtX2+lambdaI) %*% Xty2
      
      # Evaluate training and test performance 
      Error_train2[t,kk] = sum((y_train2 - X_train2 %*% w[,t,kk])^2)
      Error_test2[t,kk] = sum((y_test2 - X_test2 %*% w[,t,kk])^2)
      
    }
  }
  
  # Display result for cross-validation fold
  w_mean <- apply(w, c(1,2), mean)
  
  # Plot weights as a function of the regularization strength (not offset)
  par(mfrow=c(1,2))
  plot(log(lambda_tmp), w_mean[2,], xlab="log(lambda)",
       ylab="Coefficient Values",main=paste("Weights, fold ",k,"/",K),
       ylim = c(min(w_mean[-1,]), max(w_mean[-1,])))
  lines(log(lambda_tmp), w_mean[2,])
  
  colors_vector = colors()[c(1,50,26,59,101,126,151,551,71,257,506,634,639,383)]

  for(i in 3:M){
    points(log(lambda_tmp), w_mean[i,], col=colors_vector[i])
    lines(log(lambda_tmp), w_mean[i,], col=colors_vector[i])
  }
  
  #matplot(log(lambda_tmp), t(w_mean), type = c("b"),pch=1,col = 1:4) #plot
  
  # Select optimal value of lambda
  ind_opt <- which.min(apply(Error_test2, 1, sum) / sum(CV2$TestSize))
  lambda_opt[k] <- lambda_tmp[ind_opt]
  
  
  par(cex.main=3) # Define size of title
  par(cex.lab=2) # Define size of axis labels
  par(cex.axis=2) # Define size of axis labels
  par(mar=c(5,6,4,1)+.1) # Increase margin size to allow for larger axis labels
  plot(log(lambda_tmp), log(apply(Error_train2,1,sum)/sum(CV2$TrainSize)), 
       xlab="log(lambda)", ylab="log(Error)" , 
       main = paste("Otimal lambda: 1e",log10(lambda_opt[k])))
  
  lines(log(lambda_tmp), log(apply(Error_train2,1,sum)/sum(CV2$TrainSize)))
  
  points(log(lambda_tmp), log(apply(Error_test2,1,sum)/sum(CV2$TestSize)) ,col="red")   
  lines(log(lambda_tmp), log(apply(Error_test2,1,sum)/sum(CV2$TestSize)) , col="red")
  
  legend("bottomright", legend=c("Training","Test"), col=c("black","red"), lty=1)
  
  # Standardize outer fold based on training set, and save the mean and standard
  # deviations since they're part of the model (they would be needed for
  # making new predictions) - for brevity we won't always store these in the scripts
  mu[k,] <- colMeans(X_train[, 2:24])
  sigma[k,] <- apply(X_train[, 2:24], 2, sd)
  
  X_train[, 2:24] <- scale(X_train[, 2:24], mu[k,], sigma[k,])
  X_test[, 2:24] <- scale(X_test[, 2:24], mu[k,], sigma[k,])
  
  # Estimate w for the optimal value of lambda
  Xty = t(X_train) %*% y_train
  XtX = t(X_train) %*% X_train
  
  lambdaI = lambda_opt[k] * diag(M)
  lambdaI[1,1] = 0; # don't regularize bia
  
  w_rlr[,k] = solve(XtX+lambdaI) %*% Xty
       
  # evaluate training and test error performance for optimal selected value oflambda
  Error_train_rlr[k] = sum( (y_train - X_train %*% w_rlr[,k])^2 )
  Error_test_rlr[k] = sum( (y_test - X_test %*% w_rlr[,k])^2 )
  
  # Compute squared error without regularization
  w_noreg[,k] = solve(XtX) %*% Xty
  Error_train[k] = sum( (y_train - X_train %*% w_noreg[,k])^2);
  Error_test[k] = sum( (y_test - X_test %*% w_noreg[,k])^2);
  
  # Compute squared error without using the input data at all
  Error_train_nofeatures[k] = sum((y_train - mean(y_train))^2);
  Error_test_nofeatures[k] = sum((y_test - mean(y_train))^2);
  
}

# Display Results
print('Linear regression without feature selection:');
print(paste('- Training error: ', sum(Error_train)/sum(CV$TrainSize)));
print(paste('- Test error', sum(Error_test)/sum(CV$TestSize)));
print(paste('- R^2 train:     %8.2f\n', (sum(Error_train_nofeatures)-sum(Error_train))/sum(Error_train_nofeatures)))
print(paste('- R^2 test:     %8.2f\n', (sum(Error_test_nofeatures)-sum(Error_test))/sum(Error_test_nofeatures)))

print('Regularized Linear regression:')
print(paste('- Training error:', sum(Error_train_rlr)/sum(CV$TrainSize)))
print(paste('- Test error:', sum(Error_test_rlr)/sum(CV$TestSize)))
print(paste('- R^2 train: ', (sum(Error_train_nofeatures)-sum(Error_train_rlr))/sum(Error_train_nofeatures)))
print(paste('- R^2 test:', (sum(Error_test_nofeatures)-sum(Error_test_rlr))/sum(Error_test_nofeatures)))


print('Weights in last fold :')
for(m in 1:M){
  print(paste(attributeNames[m], w_rlr[m, k]))
}

