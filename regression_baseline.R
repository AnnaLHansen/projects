library(dplyr)
library(caret)
library(cvTools)
train <- readRDS("~/projects/train_standard.rds")


set.seed(42)
CV <- cvTools::cvFolds(nrow(train), K=10)

tabel <- data.frame("krydsvalideringsfold" = rep(1:10),
                    "traeningserror" = rep(NA),
                    "testerror" = rep(NA))

for (fold in 1:10){

  traening <- train %>%
    filter(CV$which != fold)
  
  traening_x <- traening[, -1]
  traening_y <- traening[, 1]
  
  test <- train %>%
    filter(CV$which == fold)

  test_x <- test[, -1]
  test_y <- test[, 1]
  
  model <- lm(fremskreven_pris_M2 ~ enhed.antalbadevaerelser + enhed.antalvandskylledetoiletter + bolig_areal + bolig_alder + EV_NN_M2 + NN_m2pris_mean + 
                NN_m2pris_max + NN_afstand_max + NN_m2pris_stdrel + ydervaegsmateriale_gasbeton + tagtype_fibercement + taet_paa_kyst, data = traening)
  
  praediktion_testsaet <- predict(model, test_x)
  praediktion_traeningssaet <- predict(model, traening_x)
  
  # Compute squared error without using the input data at all
  tabel$traeningserror[fold] <- sum((praediktion_traeningssaet - mean(traening_y))^2)
  tabel$testerror[fold] <- sum((praediktion_testsaet - mean(traening_y))^2)
  
}

########################################
########################################
########################################

library(dplyr)
library(caret)
library(cvTools)
library(glmnet)
train <- readRDS("~/projects/train_standard.rds")


set.seed(42)
CV <- cvTools::cvFolds(nrow(train), K=10)

tabel2 <- data.frame("krydsvalideringsfold" = rep(1:10),
                    "lambda" = rep(NA),
                    "testerror" = rep(NA))

for (fold in 1:10){
  
  traening <- train %>%
    filter(CV$which != fold)
  
  traening_x <- as.matrix(traening[, c("enhed.antalbadevaerelser", "enhed.antalvandskylledetoiletter", "bolig_areal", "bolig_alder",
                             "EV_NN_M2", "NN_m2pris_mean", "NN_m2pris_max", "NN_afstand_max", "NN_m2pris_stdrel", "ydervaegsmateriale_gasbeton",
                             "tagtype_fibercement", "taet_paa_kyst")])
  traening_y <- traening[, 1]
  
  test <- train %>%
    filter(CV$which == fold)
  
  test_x <- as.matrix(test[, c("enhed.antalbadevaerelser", "enhed.antalvandskylledetoiletter", "bolig_areal", "bolig_alder",
                     "EV_NN_M2", "NN_m2pris_mean", "NN_m2pris_max", "NN_afstand_max", "NN_m2pris_stdrel", "ydervaegsmateriale_gasbeton",
                     "tagtype_fibercement", "taet_paa_kyst")])
  test_y <- test[, 1]
  
  lambda <- 10^seq(-3, 3, length = 100)
  model <- cv.glmnet(x = traening_x, y = traening_y, nfolds = 10, alpha = 0)
  
  praediktion_testsaet <- predict(model, test_x)
  praediktion_traeningssaet <- predict(model, traening_x)
  
  tabel2$lambda[fold] <- model$lambda.1se
  tabel2$testerror[fold] <- sum((praediktion_testsaet - mean(test_y))^2) #mse mean sum of squares

  
}


########################################
########################################
########################################

library(dplyr)
library(caret)
library(cvTools)
library(glmnet)
library(neuralnet)
train <- readRDS("~/projects/train_standard.rds")


set.seed(42)
CV <- cvTools::cvFolds(nrow(train), K=10)

tabel3 <- data.frame("krydsvalideringsfold" = rep(1:10),
                     "hidden_notes" = rep(NA),
                     "testerror" = rep(NA))

for (fold in 1:10){
  
  traening <- train %>%
    filter(CV$which != fold)
  
  traening_x <- as.matrix(traening[, c("enhed.antalbadevaerelser", "enhed.antalvandskylledetoiletter", "bolig_areal", "bolig_alder",
                                       "EV_NN_M2", "NN_m2pris_mean", "NN_m2pris_max", "NN_afstand_max", "NN_m2pris_stdrel", "ydervaegsmateriale_gasbeton",
                                       "tagtype_fibercement", "taet_paa_kyst")])
  traening_y <- traening[, 1]
  
  test <- train %>%
    filter(CV$which == fold)
  
  test_x <- as.matrix(test[, c("enhed.antalbadevaerelser", "enhed.antalvandskylledetoiletter", "bolig_areal", "bolig_alder",
                               "EV_NN_M2", "NN_m2pris_mean", "NN_m2pris_max", "NN_afstand_max", "NN_m2pris_stdrel", "ydervaegsmateriale_gasbeton",
                               "tagtype_fibercement", "taet_paa_kyst")])
  test_y <- test[, 1]
  

  fmla <- as.formula(fremskreven_pris_M2 ~ enhed.antalbadevaerelser + enhed.antalvandskylledetoiletter + bolig_areal + bolig_alder + EV_NN_M2 + NN_m2pris_mean + 
                NN_m2pris_max + NN_afstand_max + NN_m2pris_stdrel + ydervaegsmateriale_gasbeton + tagtype_fibercement + taet_paa_kyst)
  # Fit neural network to training set
  MSEBest = Inf
  besthidden <- ""
  for(h in 1:4){ # Number of re-trains of neural network
    model <- neuralnet(fmla,
                       traening,
                       hidden=h, # Number of hidden units
                       rep = 1, # Number of reruns
                       linear.output=TRUE,
                       err.fct='sse')

    mse <- sum((unlist(netwrk$net.result)-mean(test_y))^2)
    if(mse<MSEBest){
      bestnet <- netwrk
      MSEBest <- mse
      besthidden <- h
    }
  }
  
  praediktion_testsaet <- neuralnet::compute(model, test_x)
  tabel3$hidden_units[fold] <- besthidden
  tabel3$testerror[fold] <- sum((unlist(praediktion_testsaet$net.result) - mean(test_y))^2) # mse mean sum of squares
  
  
}




