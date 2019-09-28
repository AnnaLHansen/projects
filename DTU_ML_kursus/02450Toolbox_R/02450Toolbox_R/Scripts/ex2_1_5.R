####################
# exercise 2.1.5
####################
source('Scripts/ex2_1_1.R')

Y <- t(apply(X,1,'-',colMeans(X))) # subtract the column means form columns of X

# PCA by computing SVD of Y:
s <- svd(Y)
V <- s$v

library(ggplot2) # install.packages('ggplot2)

# We saw in 2.1.3 that the first 3 components explaiend more than 90
# percent of the variance. Let's look at their coefficients:
pcs <- 1:3

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

# Inspecting the plot, we see that the 2nd principal component has large
# (in magnitude) coefficients for attributes A, E and H. We can confirm
# this by looking at it's numerical values directly, too:
print('PC2:')
print(V[,1])

# How does this translate to the actual data and its projections?
# Looking at the data for water:

# Projection of water class onto the 2nd principal component.
water <- Y[y==4,]

print('First water observation')
print(water[1,])

# Based on the coefficients and the attribute values for the observation
# displayed, would you expect the projection onto PC2 to be positive or
# negative - why? Consider *both* the magnitude and sign of *both* the
# coefficient and the attribute!

# You can determine the projection by (remove comments):
print('...and its projection onto PC2')
print(t(water[1,])%*%V[,2])
# Try to explain why.

