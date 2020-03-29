# Exercise 2 - Principal Component Analysis
library(mltools)
library(data.table)
library(magrittr)
library(dplyr)
library(colorRamps)
library(ggplot2)

dat <- read.csv("DTU_ML_kursus/02450Toolbox_R/Data/nanonose.csv", sep=",", check.names=FALSE)
# Data bestaar af maalinger af koncentrationer af forskellige
# stoffer inde i et gaskammer. 
# Der er i alt 8 forskellige sensorer som maaler.
# De er markeret med bogstaverne A-H. 
# Der bliver i alt maalt paa 5 forskellige stoffer. 
# Udtraekker variabelnavne
classLabels <- colnames(dat)
classLabels <- classLabels[-(1:2)]
# Udtraekker sensor-navne som variabel
attributeNames <- dat[3:10,1]

# Rydder op i data ved at fjerne de to foerste raekker 
# og de to foerste kolonner:
# Herefter laves en transpose til matrix
dat <- dat[-(1:2),-(1:2)]
X <- t(dat)
# Herefter er dimensionerne 8 attributter og 90 observationer
N <- dim(X)[1]
M <- dim(X)[2]

# Herefter tilfoejes labels og attributnavne igen til data
rownames(X) <- classLabels
colnames(X) <- attributeNames

# De unikke labels udtraekkes:
classNames <- sort(unique(classLabels))
C <- length(classNames)

# Herefter udtraekkes den afhaengige variabel.
# I dette tilfaelde er den afhaengige variabel de 
# stoffer som maales i kammeret. 
y <- as.numeric(as.factor(classLabels))
y <- y-1
# Da der er 8 forskellige attributter i data, har data 8 dimensioner. 
# Det kan vaere svaert at visualisere med en 2-3 dimensioner i et plot.
# For at lave et simpelt 2-dimensionelt plot vaelges 2 senore:
# Eks:
plot(X[ , 1], X[ , 2])

# For at lave et lidt mere fancy plot 
# Laves foerst et tomt plot med de korrekte dimensioner. 
j <- 1
i <- 2
plot(c(min(X[ , i]), max(X[ , i])), c(min(X[ , j]), max(X[ , j])), 
     xlab=attributeNames[i], ylab=attributeNames[j], 
     main="NanoNose data", type="n")

# Herefter plottes to udvalgte sensorer mod hinanden, 
# farvekodet efter deres y-label. 
cols <- colorRamps::matlab.like2(C)
for(c in unique(y)){
  points(X[y==c, i], X[y==c, j], pch=19,cex=2, col=alpha(cols[c+1],.33))
}
legend("topright", legend=classNames, fill = cols)


# For hver kolonneudregnes et gennemsnit og dette fratraekkes alle 
# enkelte observationer i den kolonne. Dette goeres for at standardisere 
# data. 
Y <- t(apply(X,1,'-',colMeans(X)))
# Herefter foretages en PCA analyse 
# ved at udregne SVD for data. 
s <- svd(Y)
diagS <- s$d # d contains a vector of the singular values of x (of the length min(n, p))
rho <- diagS^2/sum(diagS^2)
threshold <- 0.9

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

# Plotter X fra PCA Analysen - stadig hvor hver klasse af Y har
# sin egen farve
Z <- s$u%*%diag(s$d)
i <- 1
j <- 2
plot(c(min(Z[ , i]), max(Z[ , i])), c(min(Z[ , j]), max(Z[ , j])), 
     xlab=paste('PC',toString(i)), ylab=paste('PC',toString(j)), 
     main="NanoNose data: PCA", type="n")
cols <- colorRamps::matlab.like2(C) ;

for(c in 0:C-1){
  points(Z[y==c, i], Z[y==c, j], pch=19,cex=2, col=alpha(cols[c+1],.33))
}
legend("bottomright", legend=classNames, fill = cols)



# Herefter kigges der på v fra "SVD" 
# analysen. 
V <- s$v
# Da man for dette datasaet kunne forklare mere end 90 procent
# af variansen i data med de tre foerste komponenter, kigges
# der paa disse 3. 
pcs <- 1:3
legendStrings = c()
for (pc in pcs) { legendStrings = c(legendStrings, paste('PC',toString(pc))) }
mids <- barplot(t(V[, pcs]), beside=T,
                col=c('blue','orange','green'),
                legend=legendStrings,
                xlab='Attributes',
                ylab='Component coefficients',
                border="white")
axis(1, at=mids[2,], labels=attributeNames)
grid(lty='solid')

# Ved at undersoege plottet kan de ses at komponent 2 har store coefficienter 
# for sensor A, E og H. Dette kan bekraeftes ved at kigge paa
# Anden kolonne i V-matricen. 
V[,2]

# Herefter kigges der specifikt paa klassen med vandmaalinger. 
# Projektionen af vand-klassen ned på den 2 Principal component.
water <- Y[y==4,] # attribut vaerdierne for vand observationerne
water[1,]
t(water[1,]) %*% V[,2] ###??? Projektionen af 'water' nde på PC2 er negativ...
# Hvorfor??? 

# Herefter standardiseres data ved at dividere 
# data med standardafvigelsen
par(mfcol=c(1,1), pty='s')  
stds <- apply(X, 2, sd)
barplot(stds, ylab='NanoNose: attribute standard deviation')

Y2 <- t(apply(X,1,'-',colMeans(X))) # subtract the column means form columns of X
Y2 <- t(apply(Y2,1,'*',1/stds)) # devide by the standard deviation in Y2

# Herefter laves der en raekke plots som visualiserer data:
i = 1
j = 2
threshold = 0.9

titles = list('Zero-mean and unit variance')
s <- svd(Y2)
s$u <- -s$u # we flip U and V to obtain comparable visualizations
s$v <- -s$v # and it doesn't impact the PCA.
rho <- s$d ^ 2 / sum(s$d ^ 2)
U <- s$u
S <- diag(s$d)
V <- s$v
Z <- U %*% S
  
  # Plot projection of data onto chosen principal components
  plot(c(min(Z[ , i]), max(Z[ , i])), c(min(Z[ , j]), max(Z[ , j])), 
       xlab=paste('PC',toString(i)), ylab=paste('PC',toString(j)), 
       main=paste(titles[1], '\nProjection'), type="n")
  
  # plot points for each sensor in separate colors
  cols <- colorRamps::matlab.like2(C) ;
  for(c in 0:C-1){
    points(Z[y==c, i], Z[y==c, j], pch=19,cex=2, col=alpha(cols[c+1],.33))
  }
  legend("bottomright", legend=classNames, fill = cols)
  
  # Plot coefficients in the PC-space
  plot(c(-1,1), c(-1,1),
       xlab=paste('PC',toString(i)), ylab=paste('PC',toString(j)), 
       type='n',
       main=paste(titles[1], '\nAttribute coefficients'))
  arrows(integer(M), integer(M),
         V[,i], V[,j],
         length=.1,
         col='blue')
  text(V[,i]*1.1,V[,j]*1.1, attributeNames, cex=1.5)
  # Add a unit circle
  th = seq(0, 2.1*pi, 0.1); lines(cos(th), sin(th))
  
  # Plot the variance explained
  xlimits <- c(1, M); 
  plot(cumsum(rho),
       type='o',
       xlab="Principal components", 
       ylab="Cum. var. explained",
       xlim=xlimits,
       ylim=c(.65,1.02),
       col='blue',
       main=paste(titles[1], '\nVariance explanied'))
  grid()
  lines(xlimits, c(threshold, threshold), lty='dashed')

