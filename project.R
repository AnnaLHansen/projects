setwd("C:/Users/alhan/projects")
#setwd("~/projects")
source("help_func_datainspec.R")
source("help_func_standardization.R")

train <- readRDS("anonym_data_kursus.rds")
#train <- readRDS("~/1_A_Machine_learning_kursus_data/anonym_data_kursus.rds")
train <- klargoer_salg(train) # tilbage imputerer og fjerner irrelevante attributter
# Laver et subset af salgene så det kun er de almindelige parcelhuse som optræder i data
# Herefter bliver der lavet en oversigt over de variable som findes i data 
# samt hvor mange missing values der er.
attribut <- undersoeger_attributter(train) # Laver oversigt over de attributter som er tilgængelige
# Missing values i data skyldes flere forskellige ting
# Der er mange missing værdier som fordi felterne er valgfrie felter når de udfyldes i bbr. 
# For afstandsvariablene er der missing værdier afstandene ud over en bestemt trunkeringsafstand ikke
# er blevet målt.
train <- samler_variable(train)
train <- missing_values_pct95(attribut, train)
attribut <- undersoeger_attributter(train) 

train <- trimmer_og_uniformiserer_data(train)

train <- subset(train, !is.na(train$fremskreven_pris_M2))
train <- subset(train, train$fremskreven_pris_M2 < 30000 & train$fremskreven_pris_M2 > 0)
train <- subset(train, !is.na(train$EV_NN_M2))
# One-out-of-k encoding:
# Find variabel og lave 1-out-of-k encoding. 
# hvor en faktor variabel laves om til mange variable med 1/0.
# Normalization:rescales your data into a range of [0;1]
# One-out-of-k er at normalisere data:
train <- one_out_of_k(train)
train <- binarisere_afstande(train)
# Plot af den fremskrevne handelspris mod de fremskrevne salgspriser for 
# de naermeste 15 naboer.
# plot(train$fremskreven_pris_M2, train$EV_NN_M2)

####################
# exercise 1 & 2
####################
# Ud over at PCA'en kan hjælpe med at udvælge de variable som 
# forklarer det meste variationen i data, vil det
# også være rigtig godt hvis man ud fra sin forretningsviden kan reducere
# antallet af attributter. 
train_backup <- train
# Standardization: rescales your data to have a mean of 0 and a standard deviation of 1
# Standardiseringen bliver lavet inden 
train <- standardize(train) # Bruger deres funktion til at standardisere
attributeNames <- colnames(train[1:ncol(train)])
X <- train[, 1:ncol(train)]
# Dimensions: 185018 23
N = dim(X)[1]
M = dim(X)[2]
## Laver simpelt plot af udvalgte variable:
plot(X[ ,"EV_NN_M2"], X[ , "fremskreven_pris_M2"])
# PCA analysen - selve PCA analysen bliver lavet med funktionen "svd" der kommer fra
# kursusfunktionerne - Hvordan skal man forstå PCA analysen?
# 1 er per kolonne, 2 er per row..
Y <- t(apply(X,1,'-', colMeans(X))) # subtract the column means form columns of X
# You can check the column means using: colMeans(Y)
# PCA by computing SVD of Y:
s <- svd(Y)
diagS <- s$d # d contains a vector of the singular values of x (of the length min(n, p))

rho <- (diagS^2)/(sum(diagS^2))
#sum(pcvariance[1:3])
threshold = 0.9

xlimits <- c(1, M); 
plot(rho,
     type='o',
     main="Variance explained by principal componenets",
     xlab="Principal components", 
     ylab="Variance explained",
     xlim=xlimits,
     ylim=c(0,1),
     col='blue')
lines(cumsum(rho), type='o', col='orange')
lines(xlimits, c(threshold, threshold), lty='dashed')

legend("right", # Define position
       legend=c("Individual", "Cumulative", "Threshold"), # Set strings for legend
       col=c("orange", "blue", "black"), lty=c(1,1,2), # Match appereance of lines
       cex=1.5, bg='lightblue') # Setup how the box looks (cex controls size)

# De foeste tre principal components kan man forklare 90% af variationen i data.
sum(rho[1:16])
# For at kommme over 95% skal man have de 18 foerste komponenter. 
sum(rho[1:18])
# Der er i alt 23 mulige komponenter.
# Det som det umiddelbart synes at betyde er at der ikke kan reduceres mange attibutter vaek
# Uden at det betyder at man mister variation.
# PCA analysen kan bruges til at visualiserer data, feature extraction og compression. 
# Principal component analyse kan bruges til at reducerer dementionerne i data. 
# PCA bliver udregnet ved bl.a. at udregne 'singular value decomposition'. Ud fra PCA resultaterne kan man 
# finde ud af hvor meget af variationen i data der kan forklares ud fra hver enkelt PCA komponent. 
# Prøv at se om det er muligt at lave en enkelt variabel ud fra de to koordinater - Eller - kan de indgå 
# som to selvstændige variable?
Z <- s$u%*%diag(s$d)
i <- 1
j <- 2
plot(Z[, i ], Z[, j])

# Laver endnu et plot af PCA analysen:
# Udtraekker v fra scd resultatet.
V <- s$v
library(ggplot2)
# We saw in 2.1.3 that the first 3 components explaiend more than 90
# percent of the variance. Let's look at their coefficients:
pcs <- 1:16
# Make some legend strings:
legendStrings = c()
for (pc in pcs) { legendStrings = c(legendStrings, paste('PC',toString(pc))) }

# Make a bar plot for coefficients:
mids <- barplot(t(V[, pcs]), beside=T,
                col=c('blue','orange','green'),
                legend=legendStrings,
                xlab='Attributes',
                ylab='Component coefficients',
                border="white")
axis(1, at=mids[2,], labels=attributeNames)
grid(lty='solid')

####################
# exercise 3
####################
# Measures of similarity, summary statistics and
# probabilities with R
mean_values <- sapply(colnames(train), function(x){
  mean(train[[x]])
})

sd_values <- sapply(colnames(train), function(x){
  sd(train[[x]])
}) # Er alle sammen 1 - fordi der er blevet standardiseret! 
# Hvis det skal give mening er det måske en god ide at gøre inden
# standardisering.

meadian_values <- sapply(colnames(train), function(x){
  median(train[[x]])
})

range_values <- sapply(colnames(train), function(x){
 diff(range(train[[x]]))
}) # range returns the minimum and maximum of the vector x. 
#To get the range, we must take the maximum minus the minimum.
#We do this using the function diff, which finds differences between 
#consecutive elements in a vector.

library(corrplot)
correlation <- cor(train)
corrplot(correlation) # Der er et eller andet galt med dimentionerne
# når den plotter. 

## Similarity measures mangler: og Et ordenligt correlation plot mangler
# også !

##############
X <- train$EV_NN_M2
y <- train$fremskreven_pris_M2
plot(X, y, main="Linear regression", xlab="X", ylab="y")

dev.off() # close the figure from exercise 5.2.1
# Estimate model parameters
w_est = lm(y ~ X);

# Plot the predictions of the model
plot(X, y, main='Linear regression', xlab="X", ylab="y");
y_est = w_est$coef[1] +w_est$coef[2]*X;
lines(X, y_est, col='red');
legend("topleft", legend=c("Data", "Fitted model"), fill=c("black", "red"))

###########
# x1 <- train[1:80000, ] 
# x2 <- train[80000:185018, ]
# 
# dat <- data.frame(x1[, c("EV_NN_M2","bolig_areal")])
# xnam <- paste("X", 1:Km, sep="")
# colnames(dat) <- xnam
# y <- train$fremskreven_pris_M2[1:80000]
# (fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))
# w_est = lm(fmla, data=dat);
# 
# # Plot the predictions of the model
# plot(train$EV_NN_M2, train$fremskreven_pris_M2, main="Linear regression", xlab="X", ylab="y")
# 
# newdat <- data.frame(x2[, c("EV_NN_M2","bolig_areal")])
# colnames(newdat) <- xnam
# y_est = predict(w_est, newdata=newdat)
# lines(newdat[,1], y_est, col='red');
# legend("topleft", legend=c('Data', 'Fitted model'), fill=c("black", "red"));

# Fit en lineær regressionsmodel til at forudsige salgspriser ud fra
# variablene 'ev_nn_m2' og 'bolig_areal'.
y =  train[, c("fremskreven_pris_M2")]
Xr =  train[, c("EV_NN_M2", "bolig_areal")]
xnam <- names(Xr)
(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))
w_est = lm(fmla, data=Xr)

# Make a scatter plot of predicted versus true values of Alcohol
y_est = w_est$fitted.values
plot(y, y_est, xlab='fremskreven_pris (true)', ylab='fremskreven_pris (estimated)',
     main='salgspriser', pch=20)

# Make a histogram of the residual error
hist(y-y_est, breaks=41, main="Residual error")

###########################
##### Regularization ######
###########################

source("DTU_ML_kursus/02450Toolbox_R/setup.R")
library(cvTools)
library(R.matlab)
library(glmnet)
dat <- readRDS("train_behandlet_og_standardz.rds")
X <- dat[, colnames(dat) != "fremskreven_pris_M2"]
N <- nrow(X)
attributeNames <- colnames(X)
M <- ncol(X)
y <- dat[, colnames(dat) == "fremskreven_pris_M2"]
T <- 14 # antallet af afpøvede lambdaer

# Regularized Linear regression 
# include an additional attribute corresponding to the offset
X <- cbind( rep(1,N), X) # tilfoejer 1-taller til matrixen
M <- M[1]+1
attributeNames <- c("Offset",attributeNames)

# Crossvalidation
# Create crossvalidation partition for evaluation of performance of optimal model
K <- 5
set.seed(1234) # for reproducibility
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
    
    # standardisere inde i det indre loop - Derfor bruge funktionen scale
    mu2 <- colMeans(X_train2[, 2:ncol(X_train2)])
    sigma2 <- apply(X_train2[, 2:ncol(X_train2)], 2, sd)
    
    # hvad laves her? scales er en funktion centrerer kolonnnerne i en matrice
    X_train2[, 2:ncol(X_train2)] <- scale(X_train2[, 2:ncol(X_train2)], mu2, sigma2)
    X_test2[, 2:ncol(X_train2)] <- scale(X_test2[, 2:ncol(X_train2)], mu2, sigma2)
    
    CV2$TrainSize[kk] <- length(y_train)
    CV2$TestSize[kk] <- length(y_test2)
    
    Xty2 <- t(X_train2) %*% y_train2
    X_train2 <- as.matrix(X_train2)
    XtX2 <- t(X_train2) %*% X_train2
    
    for(t in 1:length(lambda_tmp)){
      
      # Learn parameter for current value of lambda for the given inner CV_fold
      lambdaI = lambda_tmp[t]*diag(M);
      lambdaI[1,1] = 0; # don't regularize bias
      w[,t,kk] <- solve(XtX2+lambdaI) %*% Xty2
      
      # Evaluate training and test performance 
      Error_train2[t,kk] = sum((y_train2 - X_train2 %*% w[,t,kk])^2)
      X_test2 <- as.matrix(X_test2)
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
  mu[k,] <- colMeans(X_train[, 2:ncol(X_train)])
  sigma[k,] <- apply(X_train[, 2:ncol(X_train)], 2, sd)
  
  X_train[, 2:ncol(X_train)] <- scale(X_train[, 2:ncol(X_train)], mu[k,], sigma[k,])
  X_test[, 2:ncol(X_test)] <- scale(X_test[, 2:ncol(X_test)], mu[k,], sigma[k,])
  
  # Estimate w for the optimal value of lambda
  Xty = t(X_train) %*% y_train
  X_train <- as.matrix(X_train)
  XtX = t(X_train) %*% X_train
  
  lambdaI = lambda_opt[k] * diag(M)
  lambdaI[1,1] = 0; # don't regularize bia
  
  w_rlr[,k] = solve(XtX+lambdaI) %*% Xty
  
  X_test <- as.matrix(X_test)
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

