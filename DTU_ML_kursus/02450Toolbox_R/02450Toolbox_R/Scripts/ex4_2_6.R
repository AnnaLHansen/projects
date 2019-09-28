# exercise 4.2.6


# make a non-interactive 3d plot of the data. Install the package scatterplot3d if you do not already have it installed
#install.packages("scatterplot3d")
library(scatterplot3d)
# note that if more than three classes are inspected, then this vector of colors is not long enough. Thus more colors need to be added.
cols <- c("blue", "green3", "red")
cols <- cols[1:length(classNames)]
s3d <- scatterplot3d(X[,1:3], type="n")
for(c in 1:C){
  s3d$points3d(X[(c-1)==y,1:3],  col=cols[c])
}
legend("top", legend=classNames, fill=unique(cols[match(y,as.numeric(classNames))]))


# make an interactive 3d plot of the data using the package rgl.

# for ubuntu, make sure to have the package libglu1-mesa-dev installed before installing the R package rgl
# i.e.: 'sudo apt-get install libglu1-mesa-dev' from the terminal
#install.packages("rgl")
library(rgl)
# if more than three classes are inspected this code must also be modified accordingly
cols = rep("black", times=length(y))
cols[y==0] <- "blue"
cols[y==1] <- "green3"
cols[y==2] <- "red"
plot3d(X[,1:3], col=cols, size=3) 
