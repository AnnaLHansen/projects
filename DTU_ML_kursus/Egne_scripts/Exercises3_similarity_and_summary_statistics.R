# Foerste skridt i ovelse3 er helt overordnet at 
# angive en masse summary-statistics som
# kan hjaelpe med at give et overblik over data
x <- c(-0.68, -2.11,  2.39,  0.26,  1.46,
       1.33,  1.03, -0.41, -0.33, 0.47)

mean(x)
sd(x)
median(x)
diff(range(x))


# Der findes flere forskellige maader man kan betragte data paa
# i forhold til at sige hvor ens de er 
# Det kan eks vaere 'SMC', 'Jaccard', 'ExtendedJaccard', 
# 'Cosine', 'Correlation' 
library(R.matlab)
source("DTU_ML_kursus/02450Toolbox_R/setup.R") 
dat <- readMat(file.path("DTU_ML_kursus/02450Toolbox_R/Data/digits.mat"))

i <- 1
SimilarityMeasure = 'Correlation'
names(dat)
X = dat$X;
dimX = dim(X); 
N <- dimX[1]; M <- dimX[2]
Q <- matrix(X[i,], ncol = M) # The query image 
Y <- X[-i,] # All images except the i'th

sim <- similarity(Q,Y, SimilarityMeasure)

# Lav herefter et et eller andet form for plot af hvor meget 
# data ligner hinanden. 
# Der er forskellige metoder til at maale similarity paa
# og de giver hver isaer et forskelligt resultat.
M = 5
x = as.matrix(runif(M))
y = as.matrix(runif(M))

# Two constants
a = 1.5
b = 1.5

# Check the statements in the exercise
similarity(x,y,'cos') - similarity(a*x,y,'cos')
similarity(x,y,'ext') - similarity(a*x,y,'ext')
similarity(x,y,'cor') - similarity(a*x,y,'cor')
similarity(x,y,'cos') - similarity(b+x,y,'cos')
similarity(x,y,'ext') - similarity(b+x,y,'ext')
similarity(x,y,'cor') - similarity(b+x,y,'cor')

