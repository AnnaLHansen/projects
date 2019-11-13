setwd("~/projects")
source("help_func_datainspec.R")
source("help_func_standardization.R")

train <- readRDS("~/1_A_Machine_learning_kursus_data/anonym_data_kursus.rds")
train <- klargoer_salg(train) # tilbage imputerer og fjerner irrelevante attributter
# Laver et subset af salgene så det kun er de almindelige parcelhuse som optræder i data
# Herefter bliver der lavet en oversigt over de variable som findes i data 
# samt hvor mange missing values der er.
attribut <- undersoeger_attributter(train) # Laver oversigt over de attributter som er tilgængelige
# Missing values i data skyldes flere forskellige ting
# Der er mange missing værdier som fordi felterne er valgfrie felter når de udfyldes i bbr. 
# For afstandsvariablene er der missing værdier afstandene ud over en bestemt trunkeringsafstand ikke
# er blevet målt.
train <- samler_variable(train)
train <- missing_values_pct95(attribut, train)
attribut <- undersoeger_attributter(train) 

train <- trimmer_og_uniformiserer_data(train)

train <- subset(train, !is.na(train$fremskreven_pris_M2))
train <- subset(train, train$fremskreven_pris_M2 < 30000 & train$fremskreven_pris_M2 > 0)
train <- subset(train, !is.na(train$EV_NN_M2))
# One-out-of-k encoding:
# Find variabel og lave 1-out-of-k encoding. 
# hvor en faktor variabel laves om til mange variable med 1/0.
# Normalization:rescales your data into a range of [0;1]
# One-out-of-k er at normalisere data:
train <- one_out_of_k(train)
train <- binarisere_afstande(train)
# Plot af den fremskrevne handelspris mod de fremskrevne salgspriser for 
# de naermeste 15 naboer.
# plot(train$fremskreven_pris_M2, train$EV_NN_M2)

####################
# exercise 1 & 2
####################
# Ud over at PCA'en kan hjælpe med at udvælge de variable som 
# forklarer det meste variationen i data, vil det
# også være rigtig godt hvis man ud fra sin forretningsviden kan reducere
# antallet af attributter. 
train_backup <- train
# Standardization: rescales your data to have a mean of 0 and a standard deviation of 1
# Standardiseringen bliver lavet inden 
train <- standardize(train) # Bruger deres funktion til at standardisere
attributeNames <- colnames(train[1:ncol(train)])
X <- train[, 1:ncol(train)]
# Dimensions: 185018 23
N = dim(X)[1]
M = dim(X)[2]
## Laver simpelt plot af udvalgte variable:
plot(X[ ,"EV_NN_M2"], X[ , "fremskreven_pris_M2"])
# PCA analysen - selve PCA analysen bliver lavet med funktionen "svd" der kommer fra
# kursusfunktionerne - Hvordan skal man forstå PCA analysen?
# 1 er per kolonne, 2 er per row..
Y <- t(apply(X,1,'-', colMeans(X))) # subtract the column means form columns of X
# You can check the column means using: colMeans(Y)
# PCA by computing SVD of Y:
s <- svd(Y)
diagS <- s$d # d contains a vector of the singular values of x (of the length min(n, p))

rho <- (diagS^2)/(sum(diagS^2))
#sum(pcvariance[1:3])
threshold = 0.9

xlimits <- c(1, M); 
plot(rho,
     type='o',
     main="Variance explained by principal componenets",
     xlab="Principal components", 
     ylab="Variance explained",
     xlim=xlimits,
     ylim=c(0,1),
     col='blue')
lines(cumsum(rho), type='o', col='orange')
lines(xlimits, c(threshold, threshold), lty='dashed')

legend("right", # Define position
       legend=c("Individual", "Cumulative", "Threshold"), # Set strings for legend
       col=c("orange", "blue", "black"), lty=c(1,1,2), # Match appereance of lines
       cex=1.5, bg='lightblue') # Setup how the box looks (cex controls size)

# De foeste tre principal components kan man forklare 90% af variationen i data.
sum(rho[1:16])
# For at kommme over 95% skal man have de 18 foerste komponenter. 
sum(rho[1:18])
# Der er i alt 23 mulige komponenter.
# Det som det umiddelbart synes at betyde er at der ikke kan reduceres mange attibutter vaek
# Uden at det betyder at man mister variation.
# PCA analysen kan bruges til at visualiserer data, feature extraction og compression. 
# Principal component analyse kan bruges til at reducerer dementionerne i data. 
# PCA bliver udregnet ved bl.a. at udregne 'singular value decomposition'. Ud fra PCA resultaterne kan man 
# finde ud af hvor meget af variationen i data der kan forklares ud fra hver enkelt PCA komponent. 
# Prøv at se om det er muligt at lave en enkelt variabel ud fra de to koordinater - Eller - kan de indgå 
# som to selvstændige variable?
Z <- s$u%*%diag(s$d)
i <- 1
j <- 2
plot(Z[, i ], Z[, j])

# Laver endnu et plot af PCA analysen:
# Udtraekker v fra scd resultatet.
V <- s$v
library(ggplot2)
# We saw in 2.1.3 that the first 3 components explaiend more than 90
# percent of the variance. Let's look at their coefficients:
pcs <- 1:16
# Make some legend strings:
legendStrings = c()
for (pc in pcs) { legendStrings = c(legendStrings, paste('PC',toString(pc))) }

# Make a bar plot for coefficients:
mids <- barplot(t(V[, pcs]), beside=T,
                col=c('blue','orange','green'),
                legend=legendStrings,
                xlab='Attributes',
                ylab='Component coefficients',
                border="white")
axis(1, at=mids[2,], labels=attributeNames)
grid(lty='solid')

####################
# exercise 3
####################
# Measures of similarity, summary statistics and
# probabilities with R
mean_values <- sapply(colnames(train), function(x){
  mean(train[[x]])
})

sd_values <- sapply(colnames(train), function(x){
  sd(train[[x]])
}) # Er alle sammen 1 - fordi der er blevet standardiseret! 
# Hvis det skal give mening er det måske en god ide at gøre inden
# standardisering.

meadian_values <- sapply(colnames(train), function(x){
  median(train[[x]])
})

range_values <- sapply(colnames(train), function(x){
 diff(range(train[[x]]))
}) # range returns the minimum and maximum of the vector x. 
#To get the range, we must take the maximum minus the minimum.
#We do this using the function diff, which finds differences between 
#consecutive elements in a vector.

library(corrplot)
correlation <- cor(train)
corrplot(correlation) # Der er et eller andet galt med dimentionerne
# når den plotter. 

## Similarity measures mangler: og Et ordenligt correlation plot mangler
# også !



