# Supervised learning - Classification 
# Der trænes modeller hvor outcome er en kategorisk variabel

# Classification with nearest neighbors:
# Nearest Neighbors - Measure similarity with distance.
# Distance i et feature space. 
# eucledian distance dist = sqrt((p1 - q1)^2 + (p1-p2)^2)
# KNN benytter denne teknik.
# pred <- knn(train_data, test_data, labels)
# 
# # Classification with nearest neighbors
# Classification er n?r man tr?ner en model hvor
# Det der l?res er kategorier.
# Ligesom andet supervised learning foreg?r dette ved at
# Man tr?ner en model ud fra data der allerede er kategoriseret.
# Nearest neighbors klassifier finder andre observationer som 
# et feature-space ligger t?t p? den p?. 
# Den g?r dette ved at m?le afstanden mellem dem, men hvor afstanden er mellem
# observationerne i et feature space. Her kan akserne p? feature space v?re 
# Koordinater, men det kan ogs? v?re eks farver.
# Afstanden mellem observationerne m?les her med euclidean afstands formel.
# Man kan her bruge modellen til at klassifiere observationer der ikke har 
# noget label. 
library(class)
pred <- knn(traindata, testdata, labels)
# For billeder kan dette eksempelvis g?res ved at kigge p? m?ngden af
# r?d, gr?n og bl? farve for hver pixel eller andre omr?der som et billede
# kan inddeles i. 
table(pred, truth)
mean(pred = truth) # Accuracy
# Hvor mange naboer der bruges til at klassificere med 
# er K. Default i R er 1 nabo. 
# Hvilken v?rdi af k som skal v?lges 
# afh?nger af data og hvor mange "noicy" observationer der findes
# i data.
pred <- knn(traindata, testdata, labels, k, prob)
# Knn antager at data er i en numerisk form. 
# for at kunne m?le afstanden mellem dem. 
# Her kan man bruge dummy-encoding (0/1) variable.
# Man skal her ogs? v?re opm?rksom p? at data er p? samme 
# skala - data skal v?re normaliseret. 
# man kan normalisere data p? flere forskellige m?der. 
# Man kan reskalere data p? mange m?der.
# eksempelvis (x-min(x) / (max(x) - min(x)))
# Herefter ligger data mellem 0 og 1. 

# Bayesian methods in classification:
# Estimerer sandsynligheder ud fra tdiligere observationer
# P(A) = Antal observationer med A / Antal observationer i alt
# P(B) = Antal observationer med B / Antal observationer i alt
# Joint probability n?r ting sker samtidig. 
# Kr?ver at observationer er afh?ngige. F.eks. 
# Lokation i forhold til tidspunkt p? dagen.
# P(A|B) = P(A and B) / P(B)
# Eks. P(work|evening) =  1 / 25 = 4%
# 1 procent sandsynlighed for at v?re p? arbejde om aftenen
# 25 procent for at det er aften. 
# Naive Bayes bygger p? netop dette. 
library(naivebayes)
model <- naive_bayes(fmla, tr?ningsdata)
pred <- predict(model, testdata)

# Hvis man har et datas?t med forskellige klasser som er afh?ngige
# kan man ud fra disse klasser med naive baysian 
# f? en sandsynlighed for at en given kombination af 
# klasser forekommer. 
# Det kan eksempelvis v?re om man er p? arbejde eller hjemme ud fra hvilken
# Type data det er.
# Compute P(A) 
p_A <- nrow(subset(where9am, where9am$location == "office"))/ nrow(where9am)
# Compute P(B)
p_B <- nrow(subset(where9am, where9am$daytype == "weekday")) / nrow(where9am)
# Compute the observed P(A and B)
p_AB <- nrow(subset(where9am, where9am$daytype == "weekday" & where9am$location == "office")) /
  nrow(where9am)
# Compute P(A | B) and print its value
p_A_given_B <- p_AB / p_B
p_A_given_B

# Ved at bruge type = "prob"  printer den sandsynglighederne for hver af klasserne
pred <- predict(model, testdata, type = "prob")
# Det er vigtigt i Naive Bayes at klasserne er afh?ngige. Hvis de er uafh?ngige
# Kan man ikke bruge dem sammen til at forudsige et event. 
# Naive Bayes algoritme antager uafh?nighed og udregner mere simple intersektion
# antagelser. 
# Antagelser med 0 sandsynlighed g?r at resten af intersektions ogs? bliver 0. 
# For at undg? dette laver algoritmen noget der hedder "Laplace correction" hvor
# der til hver intersektionen sansynlighed bliver lagt 1 oveni. S?ledes bliver en 
# sandsynlighed p? 0 eksempelvis 0.01 og man bevarer hermed de ?vrige led 
# ved ikke at gange med 0.
# dette sikre at der mellem alle klasser vil v?re en eller anden form for intersekt
# som g?r at det er muligt at forudsige alle kombinationer mellem klasserne. 
# Hvis man i tr?ningsdata ikke har observationer hvor man arbejder i weekenden 
# betyder dette ikke at det aldrig kan ske. ved at lave en Laplace correction
# sikre man at dette er muligt at udregne hvis det en dag skulle ske. 

# Binning er en metode man kan bruge til at lave kategorier ud fra 
# numerisk data - dette er n?dvendigt fordi Naive Bayes har sv?rt ved
# at h?ndtere numerisk data. 

# Logistisk regression 
# Binary predictions - Outcome i bin?r form. (yes, no) (1, 0) mm. 
# Her fitter man en logistik funktion til data i stedet for en lin?r linje
# som man g?r i line?r regression. 
model <- glm(formula = fmla, data, family = "binomial")
prob <- predict(model, testdata, type = "response")
pred <- ifelse (prob > 0.5, 1, 0)

# Det kan skabe problemer hvis det outcome man pr?ver at forudsige
# Kun optr?der sj?ldent. I disse tilf?lde kan det resultere i en bedre
# model hvis man pr?ver at forudsige det modsatte.
# ROC curve kan bruges til at visulisere om modellen er i stand til at skelne
# positive og negative grupper.
# Jo mere kurvet ROC kurven er i forhold til en lige linje, 
# jo bedre en klassifier er modellen, og jo bedre er den til at skelne 
# mellem klasserne.
# Area under the Curve (AUC) er et m?l man kan bruge til at kvatificere hvor
# god modellen er. AUC = 0.5 er et d?rligt m?l som betyder at modellen slet ikke
# kan skelne mellem klasserne. (ikke bedre end random chance).
# En perfekt model har et AUC p? 1. 

library(pROC)
ROC <- roc(truth, pred)
plot(ROC, col = "blue")
AUC(ROC)

# Alle inputs der bruges i regressionsmodeller skal v?re numeriske.
# Derfor er dummy-coding (One hot encoding) n?dvendigt til at h?ndtere
# alle kategoriske variable. 
# Desuden skal man ogs? tage sig af manglende v?rdier. Dette kan evt
# v?re at udelade observationerne eller ved at imputerer dem.
# Interaktioner mellem variable kan ogs? v?re en fordel 
# Alle tre b?r gennemg?es for at f? den bedst mulige model. 

# Feature selection (Automatic feature selection):
# Stepwise regression:
# Backward stepwise - starter med en model der indeholder alle variable, 
# og bliver ved indtil kun de vigtigste variable er med. 
# Forward stepwise g?r det samme, men starter med kun en enkelt variabel.
# Dette kan dog ende med at man har en model som er god, men som 
# ikke kan forklares eller som er counterintuitiv. 

# Specify a null model with no predictors
null_model <- glm(donated ~ 1, data = donors, family = "binomial")
# Specify the full model using all of the potential predictors
full_model <- glm(donated ~ ., data = donors, family = "binomial")
# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
# Estimate the stepwise donation probability
step_prob <- predict(step_model, type = "response")
# Plot the ROC of the stepwise model
library(pROC)
ROC <- roc(donors$donated, step_prob)
plot(ROC, col = "red")
auc(ROC)

# Decision trees / Classification trees:
# Root node, decision nodes og leaf nodes. 
# rpart er en r-pakke som kan bruges til at 
# bygge et classification tr?. 
library(rpart)
model <- rpart(fmla, data, method = "class")
predict <- predict(model, testdata, type = "class")
table(pred, truth)
accuracy <- mean(pred == truth)

library(rpar.plot)
rpart.plot(model)
rpart.plot(model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

# Et beslutningstr? laver split som laver de mest rene split.
# Jo flere beslutningssplit der bliver lavet 
# jo mere rene bliver splitene - indtil man ender ud i 
# et "leaf-node" som udelukkende indeholder en enkelt 
# klasse. 
# Ogs? med klassifikationstr?er er det vigtigt at dele sit
# data op i test og tr?ningsdata for at m?le modellens
# egentlige performance og for at undg? overfitting. 

# Pruning strategies;
# Pre-pruning (maksimalt dybde i tr?et eller minimum
# antal observationer i et split).
# Post-pruning - Her bliver grene med kun meget lidt p?virkning 
# p? modellens accuracy bliver sk?ret v?k her. 
# Begge metoder har til form?l at begr?nse tr?ernes omfang.

# Pre-pruning:
library(rpart)
prune_control <- rpart.control(maxdepth = 30, minsplit = 20)
model <- rpart(fmla, data, method = "class", control = prune_control)
# Post-pruning:
library(rpart)
model <- rpart(fmla, data, method = "class")
plotcp(model)
mode_pruned <- prune(model, cp = 0.20)

# Classifications trees kan blive kombineret til "skove"
# Random forest models. 
# Random forest er en ensemble model.
# En samling af mindre og simplere tr?er der bliver kombineret, 
# For at f? forskellige tr?er bliver hvert tr? bygget p? et random 
# subset af data, s?ledes at ingen af tr?erne er blevet bygget p? det 
# samme data.

library(randomForest)
model <- randomForest(fmla,
                      data, 
                      ntree = 500, # antal tr?er i skoven
                      mtry = sqrt(p)) # Antal predictors per tr?
predictions <- predict(model, testdata)


