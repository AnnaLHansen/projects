####################
# exercise 1.5.2
####################
# For directly reading excel files, we need to install readxl:
# install.packages("readxl")
library('readxl')

data <- read_excel("./Data/iris.xls")

# Herefter sker der helt det samme som i exercise ex_1_5_1.R
# Data inspiseres:
# extract class labels of observations
attributeNames <- colnames(data[1:4])

# extract attributes
X <- as.data.frame(data[,1:4])
classLabels <- t(as.data.frame(data[ ,5]))
# Use the function 't()' to transpose a matrix.

# check that dimensions are as they should be (150 rows and 4 columns)
N = dim(X)[1]
M = dim(X)[2]

# assign the class labels as row names and the attributes as column names
colnames(X) <- attributeNames
# extract the class names present in data
classNames <- unique(classLabels)

# Extract numeric class assignments
y <- as.numeric(as.factor(classLabels))
y <- y-1

C = length(classNames)

