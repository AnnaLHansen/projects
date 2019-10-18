####################
# exercise 1.5.3
####################
# You will need the package "R.matlab", which contains the 
# function "readMat". The package "R.matlab" relies on the 
# package "R.utils". So first install these packages using 
# "install.packages('R.utils')" and "install.packages('R.matlab')". 
# Then load "R.matlab" using "library(R.matlab)".
library('R.matlab')
data <- readMat('./Data/iris.mat')

# You can see thhe variables loaded in data by checking:
#names(data). 

# Når data indlaeses fra en matlab fil ligger det som en liste 
# Med lister i. 

# We can then extract the information simply by:
X = data$X
y = data$y
C = data$C[1]
M = data$M[1]
N = data$N[1]
attributeNames = as.character(unlist(data$attributeNames))
classLabels = as.character(unlist(data$classLabels))
classNames = as.character(unlist(data$classNames))

# Loading the Iris data from the .mat-file was quite easy, because all the work
# of putting it into the correct format was already done. This is of course 
# likely not the case for your own data, where you'll need to do something 
# similar to the two previous exercises. We will, however, sometimes in the 
# course use .mat-files in the exercises.