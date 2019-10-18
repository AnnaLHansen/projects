####################
# exercise 2.1.3
####################
source('Scripts/ex2_1_1.R')

# 1 er per kolonne, 2 er per row..
Y<- t(apply(X,1,'-',colMeans(X))) # subtract the column means form columns of X
# You can check the column means using: colMeans(Y)

# PCA by computing SVD of Y:
s <- svd(Y)
diagS <- s$d # d contains a vector of the singular values of x (of the length min(n, p))

rho <- diagS^2/sum(diagS^2)
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


sum(rho[1:3]) # med de tre første principal components kan man forklare mere end 90% af variationen i data.
# For at kommme over 95% skal man have de 4 foerste komponenter. 
sum(rho[1:4])

