library(glmnet)

train <- readRDS("C:/Users/alhan/projects/train_standard.rds")
y <- as.matrix(train$fremskreven_pris_M2)
x <- as.matrix(train[, 2:39])

library(glmnet)
fit = glmnet(x, y)
# Hver kurve repræsentere en variabel. 
plot(fit)
# Each curve corresponds to a variable. It shows the path of its coefficient against 
# the ℓ1-norm of the whole coefficient vector at as λ varies. The axis above 
# indicates the number of nonzero coefficients at the current λ,
# which is the effective degrees of freedom (df ) for the lasso. Users may also
# wish to annotate the curves; this can be done by setting label = TRUE in the plot command.
cvfit = cv.glmnet(x, y)
plot(cvfit)
cvfit$lambda.min

fit = glmnet(x, y, alpha = 0.2, weights = c(rep(1,107542),rep(2,107541)), nlambda = 20)
plot(fit, xvar = "lambda", label = TRUE)

###########################################
#### ANN (Artificial Neural Network) ######
###########################################

library(neuralnet)

y_train <- train[, colnames(train) == "fremskreven_pris_M2"]
attributeNames <- colnames(train)[colnames(train) != "fremskreven_pris_M2" ]
(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse= "+"))))
# Fit neural network to training set
MSEBest = Inf
for(t in 1:1){ # Number of re-trains of neural network
  netwrk = neuralnet(fmla, 
                     train,
                     hidden=2, # Number of hidden units
                     act.fct='tanh',
                     linear.output=TRUE, 
                     err.fct='sse');
  
  mse <- sum((unlist(netwrk$net.result)-y_train)^2)
  
  if(mse<MSEBest){
    bestnet <- netwrk
    MSEBest <- mse
  }
}



