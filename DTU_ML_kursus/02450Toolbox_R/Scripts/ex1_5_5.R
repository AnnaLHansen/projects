####################
# exercise 1.5.5
####################
library(mltools)
library(data.table)
# We start by defining the path to the file that we're we need to load.
# Upon inspection, we saw that the messy_data.data was infact a file in the
# format of a CSV-file with a ".data" extention instead.  
file_path = './Data/messy_data/messy_data.data'
# First of we simply read the file in using read.csv, however, we need to
# tell the function that the file is tab-seperated. We also need to specify
# that the header is in the second row, and so to skip the first line.
# Lastly, we tell it that if there are any empty elements, replace them with
# NA (not available) 
messy_data <- read.csv(file_path,
                       skip=1,
                       sep="\t", 
                       header = TRUE,
                       na.strings=c("", NA),
                       check.names=FALSE)
# We also need to remove the added header line in the .data file which seems
# to have included a shortend form the variables (check messy_data.head()):
messy_data = messy_data[-(1), ]


# We extract the attribute names:
attributeNames = colnames(messy_data)
# As we progress through this script, we might change which attributes are
# stored where. For simplicity in presenting the processing steps, we wont
# keep track of those changes in attributeNames in this example script.

# The last column is a unique string for each observation defining the
# car make and model. We decide to extract this in a variable for itself
# for now, and then remove it from messy_data:
car_names = messy_data['carname']
messy_data = messy_data[,-(9)]

# Try to look at messy_data and see if you can identify the formatting issues 
# that we need to handle.

# First off, we remove the question marks in displacement and replace
# them NA ('Not Available')
messy_data$displacement <- gsub("?", NA, messy_data$displacement, fixed = TRUE)
messy_data$displacement <- gsub("^$|^ $", NA, messy_data$displacement, fixed = TRUE)
# Similarly, we remove the formatting for a thousand seperator that is
# present for the weight attribute:
messy_data$weight <- gsub("'", "", messy_data$weight)
# And lastly, we replace all the commas that were used as decimal seperatos
# in the accceleration attribute with dots:
messy_data$acceleration <- gsub(",", ".", messy_data$acceleration)

# the data has some zero values that the README.txt tolds us were missing
# values - this was specifically for the attributes mpg and displacement,
# so we're careful only to replace the zeros in these attributes, since a
# zero might be correct for some other variables:
messy_data$mpg[messy_data$mpg == 0] = NA
messy_data$displacement[messy_data$displacement == 0] = NA

# We later on find out that a value of 99 for the mpg is not value that is
# within reason for the MPG of the cars in this dataset. The observations
# that has this value of MPG is therefore incorrect, and we should treat
# the value as missing. How would you add a line of code to this data
# cleanup script to account for this information?

## X,y-format
# If the modelling problem of interest was a classification problem where
# we wanted to classify the origin attribute, we could now identify obtain
# the data in the X,y-format as so:
X_c = messy_data[, -(8)]
y_c = messy_data[ , 8]

# However, if the problem of interest was to model the MPG based on the
# other attributes (a regression problem), then the X,y-format is
# obtained as:
X_r = messy_data[, -(1)]
y_r = messy_data[ , 1]

# Since origin is categorical variable, we can do as in previos exercises
# and do a one-out-of-K encoding.
# The README.txt doesn't supply a lot of information about what the
# levels in the origin variable mean, you'd have to either make an educated
# guess based on the values in the context, or preferably obtain the
# information from any papers that might be references in the README.
# In this case, you can inspect origin and car_names, to see that (north)
# american makes are all value 0 (try looking at car_names[messy_data$origin==1,],
# whereas origin value 1 is European, and value 2 is Asian.

X_r$origin <- gsub("1", 'American', messy_data$origin, fixed = TRUE)
X_r$origin <- gsub("2", 'European', messy_data$origin, fixed = TRUE)
X_r$origin <- gsub("3", 'Asian', messy_data$origin, fixed = TRUE)
origin = as.data.table(as.data.frame(X_r$origin))
origin_encoding = as.matrix(one_hot(origin))
X_r = X_r[,-(7)]
X_r = cbind(X_r, origin_encoding)
X_r = apply(X_r, 2, as.numeric)
attributeNames_r <- colnames(X_r)
targetName_r <- attributeNames[1]

## Missing values
# In the above X,y-matrices, we still have the missing values. In the
# following we will go through how you could go about handling the missing
# values before making your X,y-matrices as above.

# Once we have identified all the missing data, we have to handle it
# some way. Various apporaches can be used, but it is important
# to keep it mind to never do any of them blindly. Keep a record of what
# you do, and consider/discuss how it might affect your modelling.

# The simplest way of handling missing values is to drop any records 
# that display them, we do this by first determining where there are
# missing values:
data_drop_missing_obs = messy_data[complete.cases(messy_data), ]
# This reduces us to 15 observations of the original 29.



# Another approach is to first investigate where the missing values are.
# A quick way to do this is to visually look at the missing_idx:
missing_idx = apply(messy_data, 2, is.na)
C = t(missing_idx)
par(xaxt='n', yaxt='n')
image(C,xlab='Attributes', ylab='Observations',
      main='Visual inspection of missing values')

# From such a plot, we can see that the issue is the third column, the
# displacement attribute. This can be confirmed by e.g. doing:
#colSums(missing_idx)
# which shows that 12 observations are missing a value in the displacement column. 
# Therefore, another way to move forward is to disregard displacement 
# (for now) and remove the attribute. We then remove the few
# remaining observations with missing values:

data_wo_displacement = messy_data[, -(3)] 
data_drop_disp_then_missing = data_wo_displacement[complete.cases(data_wo_displacement),]
# Now we have kept all but two of the observations. This however, doesn't
# necesarrily mean that this approach is superior to the previous one,
# since we have now also lost any and all information that we could have
# gotten from the displacement attribute. 

# One could impute the missing values - "guess them", in some
# sense - while trying to minimize the impact of the guess.
# A simply way of imputing them is to replace the missing values
# with the median of the attribute. We would have to do this for the
# missing values for attributes 1 and 3:
data_imputed = apply(messy_data, 2, as.numeric)
data_imputed[is.na(data_imputed[,1]), 1] <- median(data_imputed[,1], na.rm=TRUE)
data_imputed[is.na(data_imputed[,3]), 3] <- median(data_imputed[,3], na.rm=TRUE)