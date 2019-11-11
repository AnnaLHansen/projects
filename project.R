setwd("~/projects")
source("help_func_datainspec.R")
source("help_func_standardization.R")
# source("DTU_ML_kursus/02450Toolbox_R/Tools/source_tools.R")
# path = "DTU_ML_kursus/02450Toolbox_R/Tools/"
# sourceDir(path, exceptions=c("source_tools.R"))

train <- readRDS("~/anonym_data_kursus.rds")
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

####################
# exercise 1 & 2
####################
# Ud over at PCA'en kan hjælpe med at udvælge de variable som 
# forklarer det meste variationen i data, vil det
# også være rigtig godt hvis man ud fra sin forretningsviden kan reducere
# antallet af attributter. 

train_backup <- train
# Håndterer classes i data:
classLabels <- train[, "bygning.tagdaekningsmateriale"]
classNames <- unique(classLabels)
# Extract numeric class assignments
y <- as.numeric(as.factor(classLabels)) # Konverterer faktor variablen til en numerisk værdi i stedet for en string
y <- y-1 # konverterer den til at starte fra 0
train[["bygning.tagdaekningsmateriale"]] <- y

## Find variabel og lave 1-out-of-k encoding. 
# hvor en faktor variabel laves om til mange variable med 1/0.
train <- subset(train, !is.na(train$fremskreven_pris_M2))
train <- subset(train, train$fremskreven_pris_M2 < 30000)
train_std <- standardize(train) # Bruger deres funktion til at standardisere

attributeNames <- colnames(train_std[1:12])
X <- train_std[, 1:12]
# Dimensions: 204538 10
N = dim(X)[1]
M = dim(X)[2]

## Laver simpelt plot af udvalgte variable:
plot(X[ ,"bolig_areal"], X[ , "fremskreven_pris_M2"])
hist(X$bolig_areal)
hist(X$fremskreven_pris_M2)

# PCA analysen - selve PCA analysen bliver lavet med funktionen "svd" der kommer fra
# kursusfunktionerne - Hvordan skal man forstå PCA analysen?
# 1 er per kolonne, 2 er per row..
Y<- t(apply(X,1,'-',colMeans(X))) # subtract the column means form columns of X
# You can check the column means using: colMeans(Y)
# PCA by computing SVD of Y:
s <- svd(Y)
diagS <- s$d # d contains a vector of the singular values of x (of the length min(n, p))

rho <- diagS^2/sum(diagS^2)
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

sum(rho[1:3]) # med de tre første principal components kan man forklare mere end 90% af variationen i data.
# For at kommme over 95% skal man have de 4 foerste komponenter. 
sum(rho[1:4])
# Det ser ud til at den største del af variationen i data bliver forklaret med de første par komponenter. 
# 1 til 2 - 99 % bliver forklaret med de første fire komponenter.... Hvad betyder det? 

# Herefter plottes resltaterne fra PCA analysen:
# Forklar også hvad det betyder... Hvad betyder det ??? Og hvad bruger man det til?
# Principal component analyse bruges til at reducerer dementionerne i data. 
# PCA bliver udregnet ved bl.a. at udregne 'singular value decomposition'. Ud fra PCA resultaterne kan man 
# finde ud af hvor meget af variationen i data der kan forklares ud fra hver enkelt PCA komponent. 
# Prøv at se om det er muligt at lave en enkelt variabel ud fra de to koordinater - Eller - kan de indgå 
# som to selvstændige variable?
Z <- s$u%*%diag(s$d)
i <- 5
j <- 7
plot(Z[, i ], Z[, j])

# Laver endnu et plot af PCA analysen:
# Udtraekker v fra scd resultatet.
V <- s$v
library(ggplot2)
# We saw in 2.1.3 that the first 3 components explaiend more than 90
# percent of the variance. Let's look at their coefficients:
pcs <- 1:3
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

# Standardisering:- Standardiser og kør pca analysen igen - læg mærke til hvad det gør
# ved resultaterne:
stds <- apply(X, 2, sd)
barplot(stds, ylab='attribute standard deviation')

####################
# exercise 3
####################



