# exercise 7.1.2
 
rm(list=ls())
source('setup.R')

# Load results from previous exercise. 
source("Scripts/ex7_1_1.R")

alpha = 0.05
rt <- jeffrey_interval(y_true, yhat[,1], alpha=alpha)
thetahatA <- rt$thetahat
CIA <- rt$CI

print(paste("Theta point estimate", thetahatA, " CI: "))
print(CIA)
