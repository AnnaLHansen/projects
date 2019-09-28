####################
# exercise 2.1.4
####################
source('Scripts/ex2_1_3.R')
Z <- s$u%*%diag(s$d)
i <- 1
j <- 3

plot(c(min(Z[ , i]), max(Z[ , i])), c(min(Z[ , j]), max(Z[ , j])), 
     xlab=paste('PC',toString(i)), ylab=paste('PC',toString(j)), 
     main="NanoNose data: PCA", type="n")
# use paste to combine to strings and toString to convert number to string

# plot points for each sensor in separate colors
cols <- colorRamps::matlab.like2(C) ;

for(c in 0:C-1){
  points(Z[y==c, i], Z[y==c, j], pch=19,cex=2, col=alpha(cols[c+1],.33))
}

# add legend
legend("bottomright", legend=classNames, fill = cols)
