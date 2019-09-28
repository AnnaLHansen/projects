rm(list=ls())
source("setup.R")
library(rpart)
library(cvTools)
# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'wine2.mat'))
X <- dat$X
N = nrow(X)
M = ncol(X)

attributeNames <- as.vector(unlist(dat$attributeNames))
# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)
fmla <- as.formula(paste(attributeNames[11], " ~ ", paste(attributeNames[1:10], collapse= "+")))

m = 1 # number of cross-validation repeats
K = 10 # folds in (repeated) cross validation
loss = 1 # loss type

y_true = matrix(,0,1)
yhat = matrix(,0,2)
r = matrix(, 0,1)
## set the seed to make your partition reproducible
for(dm in 1:m){  
  CV <- cvFolds(N, K=K)    
  for(k in 1:K){ # For each crossvalidation fold
      print(paste('Crossvalidation fold ', k, '/', CV$NumTestSets, sep=''))
      # Extract training and test set
      X_train <- X[CV$subsets[CV$which!=k], ];
      X_test <- X[CV$subsets[CV$which==k], ];
      
      Xdatframe_train <- data.frame(X_train)
      colnames(Xdatframe_train) <- attributeNames
      Xdatframe_test = data.frame(X_test)
      colnames(Xdatframe_test) <- attributeNames
    
      mytree <- rpart(fmla, data=Xdatframe_train, method="anova")
      
      yhatA <- predict(mytree, newdat=Xdatframe_train, type="vector")
      yhatA = as.matrix(yhatA)
      
      linearMod <- lm(fmla, data=Xdatframe_train)
      mytree <- rpart(fmla, data=Xdatframe_train, method="anova")
      
      yhatA <- predict(linearMod, Xdatframe_test)
      yhatB <- predict(mytree, Xdatframe_test)
      
      dyhat = cbind(yhatA,yhatB) 
      
      yhat <- rbind( yhat, dyhat)
      y_test = Xdatframe_test[attributeNames[11] ]
      y_true = rbind( y_true, y_test)  
      
      dr = mean( abs( yhatA-y_test ) ** loss - abs( yhatB-y_test) ** loss )
      r = rbind(r, dr)    
  }
}

# Perform tests using methods in setup II (correlated ttest)
alpha = 0.05
rho = 1/K
res = correlated_ttest(r, rho, alpha=alpha)

p_setupII = res$p
print(paste("In setup II the p-value was", p_setupII) )
print("and the CI was:")
CI_setupII = res$CI
print(CI_setupII)

if(m == 1){
  # Only perform setup I ttest in case m = 1 (otherwise it is not defined)  
  # perform statistical comparison of the models
  # compute z with squared error.
  zA = abs(y_true - yhat[,1] ) ** loss
  zB = abs(y_true - yhat[,2] ) ** loss
  z = zA - zB
  print("Corresponding p-values and confidence interval using methods in setup I:")
  t.test(z, alternative = "two.sided", alpha=0.05)
}