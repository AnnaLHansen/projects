

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

smp_size <- floor(0.2 * N)

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(N), size = smp_size)
X_train = X[train_ind,]
X_test = X[-train_ind,]

attributeNames <- as.vector(unlist(dat$attributeNames))
# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)

Xdatframe_train <- data.frame(X_train)
colnames(Xdatframe_train) <- attributeNames

(fmla <- as.formula(paste(attributeNames[11], " ~ ", paste(attributeNames[1:10], collapse= "+"))))

mytree <- rpart(fmla, data=Xdatframe_train, method="anova")
Xdatframe_test = data.frame(X_test)
colnames(Xdatframe_test) <- attributeNames

yhatA <- predict(mytree, newdat=Xdatframe_train, type="vector")
yhatA = as.matrix(yhatA)

linearMod <- lm(fmla, data=Xdatframe_train)
mytree <- rpart(fmla, data=Xdatframe_train, method="anova")

yhatA <- predict(linearMod, Xdatframe_test)
yhatB <- predict(mytree, Xdatframe_test)

y_test = Xdatframe_test[attributeNames[11] ]
               
# perform statistical comparison of the models
# compute z with squared error.
zA = abs(y_test - yhatA ) ** 2

# confidence interval for model A
res <- t.test(zA, alternative = "two.sided", alpha=0.05)
CIA = c( res$conf.int[1], res$conf.int[2] ) # Example on how to extract confidence interval

# Compute confidence interval of z = zA-zB and p-value of Null hypothesis
zB = abs(y_test - yhatB ) ** 2
z = zA - zB
t.test(z, alternative = "two.sided", alpha=0.05)
