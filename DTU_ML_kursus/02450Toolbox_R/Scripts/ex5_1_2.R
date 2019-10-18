# exercise 5.1.2
source(file.path("Scripts", "ex5_1_1.R")) # get data
# if you do not already have the package "rpart" installed, install it with the command install.packages("rpart")
library(rpart)
Xdatframe <- data.frame(X)
colnames(Xdatframe) <- attributeNames
# Alle variablene i datasaette er til at starte med numeriske variable,
# men konverteres til at vaere kategoriske variable - hvoraf naesten alle af
# dem er binaere variable. 
# De splits som foretages er derfor binaere....? (kan man sige det?)
Xdatframe[,attributeNames] <- lapply(Xdatframe[,attributeNames] , factor)
# check that Xdatframe represents data as categorical variables
str(Xdatframe)
summary(Xdatframe)

# fit classification tree
# Den formel der indsaettes har alle de klasser som data kan grupperes til
# control er en liste af input som styrer detajljer i rpart algotitmen
# Params styrer hvilken split metode der benyttes - Her bliver der anvendt Gini
# splitting Criterion
mytree <-
  rpart(
    classNames[y + 1] ~ Body.temperature + Skin.cover + Gives.birth + Aquatic.creature + Aerial.creature + Has.legs + Hibernates ,
    data = Xdatframe,
    control = rpart.control( 
      minsplit = 1,
      minbucket = 0,
      cp = 0
    ),
    parms = list(split = 'gini'),
    method = "class"
  )

par(xpd=NA) # make room for text labels
plot(mytree) # plotter det beslutningstrae som er blevet fittet ovenfor
text(mytree, pretty = 0) # pretty = 0 makes attribute values show up as the 
# numerical values they take in the data matrix X instead of encoding using a, b, c, etc.
# inspect details of tree
# Der bliver som det første split splittet på featuren skin cover (mammal hvis skin cover = 0, 2, 3)
# Til sidst er alle classNames blevet skilt ud 
summary(mytree)
# For hver split beregnes impurity for splittet - Hvis der for 
# eks. en kategorisk variabel er flere muligheder beregnes impurity for hver split (stadig indenfor samme feature)
# og det split som giver den hoejeste impurity 
# Node number 1 er det første split - 'primary splits giver et overblik over hvilke 
# split der er blevet valgt inkl hvilke kategorier som er blevet splittet
# I det første split er det 0, 2 og 3 



