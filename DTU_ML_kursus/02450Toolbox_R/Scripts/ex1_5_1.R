####################
# exercise 1.5.1
####################
# Remember to set your working directory, change <PATH_TO_TOOLBOX>:
#setwd('<PATH_TO_TOOLBOX>/02450Toolbox_R/')

# read data into R
data <- read.csv("./Data/iris.csv", sep=",", check.names=FALSE)
# read more about read.csv by typing '?read.csv'
# Notice the optional argument of read.csv, 'check.names'. 
# Setting this to FALSE will keep R from automatically assigning 
# unique names to columns by adding a number at the end of the 
# class names in the columns of the Excel data sheet.
 
# extract class labels of observations
attributeNames <- colnames(data[1:4])
# Column and row names can be extracted using the functions 
# 'colnames' and 'rownames', respectively.
# Column and row names can also be assigned with the functions

# extract attributes
X <- data[,1:4]
classLabels <- data[, 5]

# check that dimensions are as they should be (150 rows and 4 columns)
N = dim(X)[1]
M = dim(X)[2]

# assign the class labels as row names and the attributes as column names
colnames(X) <- attributeNames # Hvorfor? De er der allerede

# extract the class names present in data
classNames <- unique(classLabels)

# Extract numeric class assignments
y <- as.numeric(as.factor(classLabels))# Konverterer faktor variablen til en numerisk værdi i stedet for en string
y <- y-1 # konverterer den til at starte fra 0

C = length(classNames)


