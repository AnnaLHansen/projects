# exercise 4.1.4
# If the function call library(MASS) gives an error, it is most likely because the package MASS is not installed on your machine. To install it, make the call install.packages("MASS"). After installation has finished, library(MASS) will work and load the MASS package.
library(MASS)
# Number of samples
N = 1000; 

# Mean
mu = c(13, 17);

# Covariance matrix
S = matrix(c(4, 3,3, 9), nrow=2, byrow=TRUE);  

# Generate samples from the Normal distribution
X = mvrnorm(N, mu=mu, Sigma=S);
dim(X) # inspect the dimensions of the matrix containing the generated multivariate normal vectors.

