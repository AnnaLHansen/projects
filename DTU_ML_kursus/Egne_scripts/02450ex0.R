# Exercise 0
# Opstartsoevelse som installerer noedvendige pakker
# og andre intro oevelser
pakker <- c("R.matlab", "mltools", "data.table", "readxl", "FNN", 
            "gplots", "cvTools", "neuralnet", "randomForest",
"mclust", "mixtools", "sm", "SnowballC", "scatterplot3d",
"rgl", "tm", "sos", "lsa", "glmnet")

lapply(pakker, function(x) install.packages(x, dependencies = TRUE))

## Gennengå de indledende oevelser
