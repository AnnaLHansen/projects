####################
# exercise 2.1.2
####################
# Run ex2.1.1:
source('Scripts/ex2_1_1.R')

# choose which sensors to plot
i = 1
j = 2

## Make simple plot
plot(X[ , i], X[ , j])

## Make more fancy plot
# First assign titles and labels to the plot, and determine its size by giving the minimum and maximum values of the sensors.
# Do not plot anything (the option type="n")
plot(c(min(X[ , i]), max(X[ , i])), c(min(X[ , j]), max(X[ , j])), 
     xlab=attributeNames[i], ylab=attributeNames[j], 
     main="NanoNose data", type="n")

# plot points for each sensor in separate colors
library(colorRamps); cols <- colorRamps::matlab.like2(C); library(scales)

for(c in 0:C-1){
  points(X[y==c, i], X[y==c, j], pch=19,cex=2, col=alpha(cols[c+1],.33))
}

# add legend
legend("topright", legend=classNames, fill = cols)
