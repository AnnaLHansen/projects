# Grafisk evaluering af modeller
# Truth vs. prediction plots, residual plots, Gain curve plot
# GainCurvePlot() funktion
# 
# 
fmla <- as.formula(blood_pressure ~ weight + age)
modellen <- lm(fmla, data = bloodpressure)

summary(data)
summary(modellen)
predictions <- predict(modellen)

#Truth vs predictions plot i ggplot
ggplot(data, aes(x = predictions, y = truth)) + 
  geom_point() + 
  geom_abline(color = "blue")

GainCurvePlot(data, "predictions", "truth", "modellen")

# Root Mean Squared Error (RMSE)
# RMSE = sqrt(mean((pred-truth)^2))
# Prediction error af din model på det specifikke data
# Linear regression er designet til at minimere RMSE
# Evaluering af RMSE kan gøres ved at kigge på Stanard Deviation (sd)
# Standard Deviation er den typiske forskel mellem en specifik observation og 
# gennemsnittet.
# Når RMSE er lavere end SD tyder det på at modellen esitmere bedre end 
# Et simpelt gennemsnit
# residuals (pred-truth) (kaldes også error)
error <- pred - truth
rmse <- sqrt(mean(error^2))
sd <- sd(truth)

# R^2 er et mål for hvor godt ens model forklare data
# 1 er perfekt fit
# 0 r dårligt fit
# R^2 = 1 - (sum((truth - pred)^2)) / (sum((truth - meanofftruth)^2))
# R^2 = 1 - Residuals sum of squares / Total sum of squares
# Total sum of squares er variansen af data
# Residual sum of squares er variansen som er forklaret med modellen. 
# En lav Residual sum of squares i forhold til Total sum of squares betyder
# at en stor del af variansen bliver forklaret med modellen, og R^2 bliver et tal
# tæt på 1.
# Hvis Residual sum of squares til gengæld er næsten lige så stor som 
# total sum of squares så bliver kun meget lidt varians forklaret med modellen
# og R^2 vil være tæt på 0.
error <- pred - truth
residualsumofsquares <- sum(error^2)
totalerror <- truth - mean(truth)
totalsumofsquares <- sum(totalerror^2)
r2 <- 1 - (residualsumofsquares/totalsumofsquares)
# For lm() modellr i R, kan man se R2 som en del af modellens summary
# For modeller som minimere residual sum of squares 
# gælder det også at:
# (gælder lineare modeller, gam modeller og nogle three-based modeller)
# correlation mellem pred og truth kaldes rho (p)
rho <- cor(pred, truth)
rho2 <- rho^2 # Hvilket er det samme som R2

# Målene gælder selvfølgelig kun for det data som der trænes på
# og ikke fremtidig data, da der her ikke vides hvor god
# modellen fitter på nyt og ukendt data.

# Generelt performer en model bedre på dens eget træningsdata
# end på nyt data. 
# Hvis man kun bruger træningsdata til at evaluere sin model
# kan det derfor give misvisende resultater. 
# Hvis modellen performer væsentligt dårligere på nyt data "overfitter"
# modellen.
# Når man har meget data er den bedste metode at splitte data op i træningsdata
# og testdata.
# Hvis man ikke har nok data til at gøre dette kan man krydsvalidere til at
# estimere modellens out-of-sample performance. 
# Ved krydsvalidering måles performance ikke på det datasæt som der er trænet 
# på og giver derfor et unbiased estimat af hvordan modellen vil fitte 
# til al data. 
library(vtreat)
splitPlan <- kWayCrossValidation(numberofrowsintrainingdata, numberoffoldsincrossvalidation)
#første fold viser hvilke rækker der skal trænes på og hvilke der skal testes på 
data$predcv <- 0
for (i in numberoffolds){
  split <- splitPlan[[1]]
  model <- lm(flma, data[split$train,])
  data$predcv[split$app] <- predict(model, data[split$app, ])  
}

# En fuld model trænes også
# rmse funktionen er en indbygget r funktion som kan bruges til at udregne 
# residual sum of squares
data$pred <- predict(lm(fmla, data = data))
rmse(data$pred, data$truth)
rmse(data$predcv, data$truth)

# Hvis alle de estimerede modelpermances ser godt ud
# kan man herefter få videre og træne sin endelige model
# på det fulde træningsdata. Denne models fremtidige performance kan man ikke
# måle, hvis man ikke har noget nyt data at måle på. 
# Man kan ved at opdele data i test og træning måle performance med RMSE og R2 ved
# at burge modellen på både test og træningsdata. 
# Hvis muligt kan man herefter plotte prediction mod truth

# Linear regression with categorical inputs
# I mange r modeller bliver categoriske variable 
# repræsenteret som N - 1 Indikator variable
# Det niveau som ikke bliver brugt kan ses som reference level
# One- hot encoding.
# Hvis der er for mange niveau kan dette blive et problem (eks. postnumre)
# Det kan bl.a. føre til at der kommer for mange variable i forhold til 
# observationer og hermed risiko for overfitting.
# Når man konvertere en kategorisk variabel således skal man også være opmærksom
# på at de ikke bliver behandlet forkert (punkter i et geometrisk space)


# Interaktioner mellem variable
# Hvis forholdet mellem variablene er additivt vil
# resultatet være summen mellem variablene.
# Interaktioner mellem variablene betyder at der er en forskel mellem 
# effekterne afhængig af niveauet på en variabel. 
# Et eksempel kan således være forskellen i plantevækst afhængig af om 
# planten er placeret i skygge eller sol. 
# Interaktioner i R kan skrives enten som y ~ a:b eller som y ~ I(a*b)

# Hvis man eks har en model hvor der er to input variable Metabol ~ Gastric + Sex 
# og at man kan se at der når man plotter gatric mod metabol med forskellige farver til Sex
# kan se at der er forskellige effekter af køn, tyder dette
# på at der er en interaktion mellem Gatric og sex

# For at bestemme hvilken model med hvilke input (med og uden interaktioner) som 
# giver den bedste model kan man kigge på Residual sum of squares (rmse) i 
# krydsvalidering


# # alcohol is in the workspace
# summary(alcohol)
# 
# # Both the formulae are in the workspace
# fmla_add
# fmla_interaction
# 
# # Create the splitting plan for 3-fold cross validation
# set.seed(34245)  # set the seed for reproducibility
# splitPlan <- kWayCrossValidation(nrow(alcohol), 3, NULL, NULL)
# 
# # Sample code: Get cross-val predictions for main-effects only model
# alcohol$pred_add <- 0  # initialize the prediction vector
# for(i in 1:3) {
#   split <- splitPlan[[i]]
#   model_add <- lm(fmla_add, data = alcohol[split$train, ])
#   alcohol$pred_add[split$app] <- predict(model_add, newdata = alcohol[split$app, ])
# }
# 
# # Get the cross-val predictions for the model with interactions
# alcohol$pred_interaction <- 0 # initialize the prediction vector
# for(i in 1:3) {
#   split <- splitPlan[[i]]
#   model_interaction <- lm(fmla_interaction, data = alcohol[split$train, ])
#   alcohol$pred_interaction[split$app] <- predict(model_interaction, newdata = alcohol[split$app, ])
# }
# 
# # Get RMSE
# alcohol %>% 
#   gather(key = modeltype, value = pred, pred_add, pred_interaction) %>%
#   mutate(residuals = Metabol - pred) %>%
#   group_by(modeltype) %>%
#   summarize(rmse = sqrt(mean(residuals^2)))


# Transformering af output kan nogle gange give bedre resultater
# end at forudsige selve outputtet. eks. log transformation. 
# Hvis data er fordelt med en lang hale og stor spredning i værdier (eks. 60-700K)
# er data lognormalt fordelt.
# Her vil gennemsnittet typsik også være højere end medianen.
# Det vil resultere i at man ved at forudsige gennemsnittet vil 
# forudsige højere værdier (overpredict).
# Mean value er ofte det som bliver predicted i regressionsmodeller.
# Er output logtransformeret, vil man her typisk se en normalfordeling 
# af data med medianer og gennemsnit der er tættere på hinanden samt en 
# smallere spredning af værdier.
# Hvis man kan se disse trends i data kan man i stedet for at fitte en model
# til det egentlige output, fitte en model til det logtransformerede output.
# Modellen kan herefter bruges til at forudsige med, 
# Og til sidst for at få output i sin originale form kan man 
# tage exp(logpred).
# I stedet for at reducere den faktiske error, reducerer man i stedet den relative error.
# Root mean squared relative errror (RMS-relative error)
# RMSRE = sqrt(mean(
# ((pred-truth)/(truth))^2
# ))
# residual = truth - pred
# relative error = residual / truth
# Den reltive error bliver mindre ved logtransformationsmodellen,
# mens den normale RMSE bliver en anelse højere.
# Den laveste RMSE er derfor ikke altid et udtryk for den bedste model.
# Hvad der er vigtigst afhænger af ens mål for projektet.
# Ligesom at man kan transformere output kan man også transformere input variablene.
# Der kan være mange grunde til dette. 
# En af grundene kunne være at man kan se en ikke linear trend i ens data, 
# som gør det svært at forudsige input udelukkende med  addititve variable som
# giver et linear fit. 

# # Make predictions and compare
# houseprice %>% 
#   mutate(pred_lin = predict(model_lin),       # predictions from linear model
#          pred_sqr = predict(model_sqr)) %>%   # predictions from quadratic model 
#   gather(key = modeltype, value = pred, pred_lin, pred_sqr) %>% # gather the predictions
#   ggplot(aes(x = size)) + 
#   geom_point(aes(y = price)) +                   # actual prices
#   geom_line(aes(y = pred, color = modeltype)) + # the predictions
#   scale_color_brewer(palette = "Dark2")

# # houseprice is in the workspace
# summary(houseprice)
# 
# # fmla_sqr is in the workspace
# fmla_sqr
# 
# # Create a splitting plan for 3-fold cross validation
# set.seed(34245)  # set the seed for reproducibility
# splitPlan <- kWayCrossValidation(nrow(houseprice), 3, NULL, NULL)
# 
# # Sample code: get cross-val predictions for price ~ size
# houseprice$pred_lin <- 0  # initialize the prediction vector
# for(i in 1:3) {
#   split <- splitPlan[[i]]
#   model_lin <- lm(price ~ size, data = houseprice[split$train,])
#   houseprice$pred_lin[split$app] <- predict(model_lin, newdata = houseprice[split$app,])
# }
# 
# # Get cross-val predictions for price as a function of size^2 (use fmla_sqr)
# houseprice$pred_sqr <- 0 # initialize the prediction vector
# for(i in 1:3) {
#   split <- splitPlan[[i]]
#   model_sqr <- lm(fmla_sqr, data = houseprice[split$train, ])
#   houseprice$pred_sqr[split$app] <- predict(model_sqr, newdata = houseprice[split$app, ])
# }
# 
# # Gather the predictions and calculate the residuals
# houseprice_long <- houseprice %>%
#   gather(key = modeltype, value = pred, pred_lin, pred_sqr) %>%
#   mutate(residuals = price - pred)
# 
# # Compare the cross-validated RMSE for the two models
# houseprice_long %>% 
#   group_by(modeltype) %>% # group by modeltype
#   summarize(rmse = sqrt(mean(residuals^2)))

# Ovenstående kode sammenligner i krydsvalidering to forskellige modeller
# Med RMSE som evaluerings mål. 
# Det viser at den transformerede udgave af input på størrelse
# Giver et lavere residual sum of squeares end modellen med den 
# lineare sammenhæng.

# Regression i ikke-lineare situationer.
# - Logistik regression til at forudsige sandsynligheder
# - Poisson og Quasipoisson regressioner
# - Gam prediktioner

# For generalixed linear model (glm) kan goodness of fit
# Hvor god modellen er estimeres ved pseudoR2
# pseudoR2 = 1 - deviance/null.deviance
logistik_model <- glm(fmla, data, family = binomial)
perf <- glance(logistik_model)
pseudoR2 <- 1 - perf$deviance /perf$null.deviance
prediction <- predict(logistik_model, data, type="response")
GainCurvePlot(data, "pred", "truth", "titletilplottet")

# Countdata - forudsige "antal"/"counts" med enten
# Poisson eller quasipoission regression. 
# Disse er også glm modeller med family poisson eller quasipoisson
# Poisson antager at mean(truth) = var(truth) mens
# at quasipoisson kan bruges hvis disse er meget forskellige.

# generalized additive models (GAM)
# S() i GAM angiver at man vil aafprøve et ikke lineært
# fit mens variablen angivet udelukkende som sig selv angiver
# et linær fit.

# Ensemble and desicion trees:
# Det er svært for beslutningstræer at udtrykke linære sammenhæng
# Flere splits kan vise flere nyancer men der er en risiko for overfitting
# En ensemble model er en model som består af flere forskellige beslutningstræer
# Dette giver som regel bedre modeller (eks random forest og gradient boosting trees)
# Random forest:
# Random forest bygger flere beslutningstræer
# reducerer risikoen for overfitting og giver samtidig en bedre model
library(ranger)
fmla 
random_forest_model <- ranger(fmla,
                              data,
                              num.tress = 500,
                              respect.unordered.factors ="order",
                              seed = 42)

prediction <- ranger::predict(model, data)$predictions
# Xgboost for eksempel kan ikke håndtere kategoriske variable
# og at håndtere kategoriske variable med One-hot encoding er derfor nødvendigt
# vtreat pakken kan gøre dette.
# Pakken rydder dog også op i missing data for alle numeriske og categoriske værdier
# Pakken laver en "treatment" plan som er en plan for hvordan man sikkert får 
# "hot one" encoded dit data. Det er "designTreatmentsZ()" funktionen som gør dette
# Selve konverteringen sker med funktionen "prepare()". 
# Prepare() bruges også til at konverterer fremtidig data til one hot encoding.
# Outputtet af designmTreatmentsz er en dataframe som indeholder 
# niveauerne og navnet på de nye variable.
# Funktionen håndterer desuden også hvis der optræder nye kategorier i 
# testdata som ikke findes i træningsdata.

library(vtreat)
# Create the treatment plan
treatplan <- designTreatmentsZ(data, variablesomskal_onehotencodes)
# # Examine the scoreFrame
# (scoreFrame <- treatplan %>%
#     use_series(scoreFrame) %>%
#     select(varName, origName, code))
# 
# # We only want the rows with codes "clean" or "lev"
# (newvars <- scoreFrame %>%
#     filter(code %in% c("clean", "lev")) %>%
#     use_series(varName))
(dframe.treat <- prepare(treatplan, data, varRestriction = newvars))

# Gradient boosting machines:
# How to fit gradient boosting models med xgboost pakken. 
# xgb.cv krydsvaliderer med et højt antal runder/træer som fittes
# For hvert træ kan Residual sum of squares ses i xbg.cv()$evaluation.log 
# nbest er det antal af træer som minimere RSME. 
# Xgboost() kan herefter køres med nrounds = nbest
# for at få den endelige model.

#Her antages det at "data" allerede har været igennem one hot
#encoding - det samme gælder for test data
cv <- xgb.cv(data = as.matrix(data),
             label = data$y-variablen, # Outcome variablen
             objective = "reg:linear", # reg:linear for linærregression
             rounds = 100, # Maksimale antal træer der fittes
             nfold = 5, # Antal folds i krydsvalideringen
             eta = 0.3, # learning rate
             depth = 6) # Den maksimale dybde af hvert træ

evallog <- as.data.frame(cv$evaluation_log)
nrounds <- which.min(evallog$test_rmse_mean) # Det optimale antal af træer

model <- xgboost(data = as.matrix(data),
                 label = data$y-variablen,
                 nrounds = nrounds,
                 objective = "reg:linaer",
                 eta = 0.3,
                 depth = 6) 

prediction <- predict(model, testdata)
ggplot(data, aes(x = pred, y = truth)) +
  geom_point() + 
  geom_abline()


# Modeller til at håndtere ikke lineære tendenser.
# Her kan man bruge bl.a. quassipoisson som glm modeller, 
# gam modeller, randomforest eller gradient boosting modeller. 
