## Excersise 1:
library(mltools)
library(data.table)
library(magrittr)
library(dplyr)
setwd("C:/Users/alhan/projects")
data <- read.csv("DTU_ML_kursus/02450Toolbox_R/Data/iris.csv", sep=",", check.names=FALSE)
tibble::glimpse(data) # X er variablene 1:4 hvor y er variabel 5

# Identificer den afhængige og de uafhængige variable
X <- data[,1:4]
classlabels <- data[, 5] # Fordi at y er en faktorvariabel konverteres den om til en numerisk variabel
y <- as.numeric(as.factor(classlabels))
y <- y-1

# Dimentionerne på data
N = dim(X)[1]
M = dim(X)[2]


# Klassifikations problem:
# Trækker de mulige klasser ud
classnames <- unique(classlabels)
classnames_length <- length(classnames)

# Plot de forskellige attributer op mod hinanden:
i = 1
j = 2
x1 = X[,i]
x2 = X[,j]

# Laver tomt plot med dimentionerne som passer med de to attributter:
plot(c(min(x1), max(x1)), c(min(x2), max(x2)), 
     xlab=colnames(X)[i], ylab=colnames(X)[j], 
     main="Iris classification problem", type="n")

# Plotter herefter de to attributter men med farver som svarer til de 
# classlabels som ønskes forklaret med de to attributter.
cols <- c('red','green','blue') # Der er tre klasser
for(i in sort(unique(y))){
  points(x1[y==i], x2[y==i], col=cols[i+1])
}

sorted <- sort(classnames, index.return=TRUE)
legend("topright", legend=classnames[sorted], fill = cols)

# Der vil kunne tegnes en linje mellem 'Isris-setosa' og De to oevrige 'Iris' grupper.
# Iris-versicolor og Iris-virginica er to grupper som overlapper mens
# sepal bredten for Iris Setosa er bedre i forhold til deres laengde.

# Regressions problem:
# Det ønskes her at forudsige petal-length. 
# Petal-length fjernes derfor fra de afhængige variable i X og 
# bliver tilføjet som ny afhængig variabel y. 

y <- data["Petal Length"]
X <- data %>% select(-"Petal Length")

# Attributen "Type" er en faktor variabel - Den behandles med one-out-of-K 
# encoding. 
species = as.data.table(as.data.frame(classlabels))
species_encoding = one_hot(species)

X <- X %>% 
  select(-"Type") %>%
  bind_cols(.data, species_encoding)

N = dim(X)[1]
M = dim(X)[2]

i = 4
xi = X[,i]
plot(xi, y[,1],
     main='Iris regression problem',
     xlab=names(X)[i],
     ylab=names(y))

# Når en af de afhængige variable er binære variable er et scatter plot ikke at foretrække. 
# Her er et evt. boxplot muligvis bedre som visualisere information.

# Messy-data skal ryddes op i.
# 1. Sørg for at selve strukturen er på plads og at de forskellige attributer har en struktur
# som giver mening. Ensretning er vigtigt. 
# Hvis der er 0 værdier hvor det i virkeligheden er NA.. Hvis der er tegn hvor der burde være NA,
# Hvis der er kommaer eller andre special tegn som skal fjernes eller ændres.
# Optræder der værdier som ikke er falder indenfor det godkendte interval - disse skal behandles
# som missing. 

# Der kan foretages one-out-of-k enconding på de categoriske variable.
# Herefter kan man lave en behandling af missing værdier - Dette kan gøres på forskellige måder. 
# Men behandlingen af dem kan påvirke modelleringen, og behandlingen skal derfor ikke være foretages 
# hovedløst. 
# Man kan fjerne alle de obervationer hvor missing værdier optræder, men dette reducerer 
# naturligvis antallet af observationer. 
# Alternativ kan man undersøge i hvilke attributter at de manglende værdier forekommer eller
# Se bort fra de attributer med mange missig værdier. 
# En anden måde at håndtere missing værdier på er ved at imputere de manglende 
# Værdier uden at imputationen får for stor betydning. 
# En måde at gøre dette på kunne være ved at imputere med medianen af attributen. 
data2 <- data 
data2[1:10,]$`Sepal Length` <- NA
missing_idx = apply(data2, 2, is.na)
C = t(missing_idx)
par(xaxt='n', yaxt='n')
image(C,xlab='Attributes', ylab='Observations',
      main='Visual inspection of missing values')



