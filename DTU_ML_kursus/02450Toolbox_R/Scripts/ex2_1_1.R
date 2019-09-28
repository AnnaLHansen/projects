####################
# exercise 2.1.1
####################
# read data into R
dat <- read.csv("./Data/nanonose.csv", sep=",", check.names=FALSE)
# extract class labels of observations
classLabels <- colnames(dat)
classLabels <- classLabels[-(1:2)]
# extract attributes, i.e. sensor names
attributeNames <- dat[3:10,1]

# remove first two rows and columns and
# transpose data matrix
X <- t(dat[-(1:2),-(1:2)])

# check that dimensions are as they should be (90 rows and 8 columns)
N <- dim(X)[1]
M <- dim(X)[2]

# assign the class labels as row names and the attributes as column names
rownames(X) <- classLabels
colnames(X) <- attributeNames

# extract the class names present in data
classNames <- sort(unique(classLabels))
C <- length(classNames)

# Extract numeric class assignments
y <- as.numeric(as.factor(classLabels))
y <- y-1

# Clean up a bit in the variables, since we'll call this script from later scripts:
rm(dat)
