####################
# exercise 1.5.4
####################
# Repeat loading from exercise 1.5.3:
library('R.matlab')
data <- readMat('./Data/iris.mat')
X = data$X
y = data$y
C = data$C[1]
M = data$M[1]
N = data$N[1]
attributeNames = as.character(unlist(data$attributeNames))
classLabels = as.character(unlist(data$classLabels))
classNames = as.character(unlist(data$classNames))

## Classification problem
# The current variables X and y represent a classification problem, in
# which a machine learning model will use the sepal and petal dimesions
# (stored in the matrix X) to predict the class (species of Iris, stored in
# the variable y). A relevant figure for this classification problem could
# for instance be one that shows how the classes are distributed based on
# two attributes in matrix X:

X_c = X
y_c = y
attributeNames_c = attributeNames
colnames(X_c) <- attributeNames_c
colnames(y_c) <- 'Type'

# choose which attributes to plot
i = 1
j = 2
x1 = X_c[,i]
x2 = X_c[,j]

# Assign titles and labels to the plot, and determine its size by giving 
# the minimum and maximum values of the x1 abd x2. 
# Do not plot anything (the option type="n")
plot(c(min(x1), max(x1)), c(min(x2), max(x2)), 
     xlab=attributeNames[i], ylab=attributeNames[j], 
     main="Iris classification problem", type="n")
# plot points for each sensor in separate colors
cols <- c('red','green','blue')
for(i in sort(unique(y))){
  points(x1[y==i], x2[y==i], col=cols[i+1])
}
# get the order that classes were gone through and plotted in for loop
sorted <- sort(classNames, index.return=TRUE)
# add legend
legend("topright", legend=classNames[sorted$ix], fill = cols)

# Consider, for instance, if it would be possible to make a single line in
# the plot to delineate any two groups? Can you draw a line between
# the Setosas and the Versicolors? The Versicolors and the Virginicas?

## Regression problem
# Since the variable we wish to predict is petal length,
# petal length cannot any longer be in the data matrix X.
# The first thing we do is store all the information we have in the
# other format in one data frame:
data = as.matrix(cbind(X_c, y_c))

# We know that the petal length corresponds to the third column in the data
# matrix (see attributeNames), and therefore our new y variable is:
y_r = data[,3]
# Similarly, our new X matrix is all the other information but without the 
# petal length (since it's now the y variable):
X_r = data[,-(3)]

# To remove entries i to j from a vector x, use x[-(i:j)].
# To remove row i from a matrix X, you can use X=X[-i,].
# Likewise, to remove rows i to j,  use 'X=X[-(i:j),]'. 
# To remove columns i to j use 'X=X[,-(i:j)]'.
# Both columns and rows may be removed simultaneously.

# Since the iris class information (which is now the last column in X_r) is a
# categorical variable, we will do a one-out-of-K encoding of the variable.
# A simple way of doing this is using library mltools (install.packages('mltools'))
# as well as data.table:
library(mltools)
library(data.table)
species = as.data.table(as.data.frame(classLabels))
species_encoding = one_hot(species)
# The encoded information is now a 150x3 matrix. This corresponds to 150
# observations, and 3 possible species. For each observation, the matrix
# has a row, and each row has two 0s and a single 1. The placement of the 1
# specifies which of the three Iris species the observations was.

# We need to replace the last column in X (which was the not encoded
# version of the species data) with the encoded version:
X_r = X_r[,-(4)]
X_r = as.matrix(cbind(X_r, species_encoding))
attributeNames_r <- colnames(X_r)
targetName_r <- attributeNames[3]

# Now, X is of size 150x6 corresponding to the three measurements of the
# Iris that are not the petal length as well as the three variables that
# specifies whether or not a given observations is or isn't a certain type.

# Lastly, we update M, since we now have more attributes:
N = dim(X_r)[1]
M = dim(X_r)[2]

# A relevant figure for this regression problem could
# for instance be one that shows how the target, that is the petal length,
# changes with one of the predictors in X:
i = 3
xi = X_r[,i]
plot(xi, y_r,
     main='Iris regression problem',
     xlab=attributeNames_r[i],
     ylab=targetName_r)
# Consider if you see a relationship between the predictor variable on the
# x-axis (the variable from X) and the target variable on the y-axis (the
# variable y). Could you draw a straight line through the data points for
# any of the attributes (choose different i)? 
# Note that, when i is 3, 4, or 5, the x-axis is based on a binary 
# variable, in which case a scatter plot is not as such the best option for 
# visulizing the information. 


