library(caret)
library(glmnet)
library(dplyr)
train <- readRDS("~/projects/train_standard.rds")

# Herunder sammenlignes to forskellige metoder til evaluere en model på.
# Den første er en model hvor der simpelt opsplittes i et test og træningsdatasæt
# modellen trænes på det ene og evalueres på det andet. 
# Den anden metode er krydsvalidering hvor denne opsplitning sker flere gange
# med det formål at få det mest rigtige indblik i den egentlige 
# performance af modellen. 
# Ved den første metode kan man være uheldig at ramme ind i enkelte observationer 
# som forvrider det egentlige billede.

# Data opsplittes i test og træningsdata med 70% af data i træningsdata
set.seed(42)
partition <- createDataPartition(y = train$fremskreven_pris_M2, p = 0.7, list = F)
trainingdata = train[partition, ]
test <- train[-partition, ]
# Herefter opsplittes videre træningsdata i yderligere 80:20.
# Denne opdeling sker således at der er et træningssæt og et valideringssæt 
# og derefter et yderige ukendt testsæt som der kan testes på.
set.seed(42)
partitiontraining <- createDataPartition(y = trainingdata$fremskreven_pris_M2, p = 0.8, list = F)
training <- trainingdata[partitiontraining, ]
validation <- trainingdata[-partitiontraining, ]
# Herefter er det vigtigt at evaluere hvilke variable som skal med i modellen. 
# En metode som kan bruges er at gennemgå p-værdierne i regressionsmodellen. 
# Men med mange variable vil dette tage lang tid. 
# En anden metode er at bruge lasso regularization. Lasso regresssion er implementeret til 
# at identificere de variable som er signifikante i modellen.
x <- model.matrix(~.,trainingdata[,-1])
y <- trainingdata$fremskreven_pris_M2
cv.out <- cv.glmnet(x,y,alpha=1,type.measure = "mse" ) # Lasso regression når alpha = 1
lambda_min <- cv.out$lambda.min
lambda_1se <- cv.out$lambda.1se
coef <- as.matrix(coef(cv.out,s=lambda_1se))
coef2 <- as.data.frame(as.table(coef))
coef2 <- coef2 %>%select(-Var2)%>%filter(Freq != 0)%>%rename(Variable = Var1, Coeficients = Freq)
# Alle de variable som har en ikke 0 frekvens 
# Er de variable som er vurderet til at have en betydning for kvadratmeterpriserne.

# Herefter laves en linear model med alle disse variable i.
model <- lm(fremskreven_pris_M2 ~ enhed.antalbadevaerelser + enhed.antalvandskylledetoiletter + bolig_areal + bolig_alder + EV_NN_M2 + NN_m2pris_mean + 
              NN_m2pris_max + NN_afstand_max + NN_m2pris_stdrel + ydervaegsmateriale_gasbeton + tagtype_fibercement + taet_paa_kyst, data = training)

# Modellens RMSE udregnes som et mål for hvor god modellen er på nyt data
p <- predict(model, validation)
error <- (p- validation$fremskreven_pris_M2)
RMSE_Model <- sqrt(mean(error^2))

ptest <- predict(model, test)
error1 <- (ptest- test$fremskreven_pris_M2)
RMSE_NewData <- sqrt(mean(error1^2))
Method <- c("Train/Test Split")
ModelRMSE <- c(RMSE_Model)
RMSENewData <- c(RMSE_NewData)

table1 <- data.frame(Method, ModelRMSE, RMSENewData)
# kable(table1) %>% kable_styling(c("striped", "bordered")) %>%column_spec(2:3, border_left = T)

# Herunder trænes en linar model med krydsvalidering for at evaluere performance
modelcv <- train(
  fremskreven_pris_M2 ~ enhed.antalbadevaerelser + enhed.antalvandskylledetoiletter + bolig_areal + bolig_alder + EV_NN_M2 + NN_m2pris_mean + 
    NN_m2pris_max + NN_afstand_max + NN_m2pris_stdrel + ydervaegsmateriale_gasbeton + tagtype_fibercement + taet_paa_kyst, data = training,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10
  )
)

RMSE_Modelcv <- modelcv$results$RMSE
pcv <- predict(modelcv, test)
errorcv <- (pcv- test$fremskreven_pris_M2)
RMSE_NewDatacv <- sqrt(mean(errorcv^2))
Method <- c("Crossvalidation")
ModelRMSE <- c(RMSE_Modelcv)
RMSENewData <- c(RMSE_NewDatacv)
table2 <- data.frame(Method, ModelRMSE, RMSENewData)
tablefinal <- dplyr::bind_rows(table1, table2)

predictions <- modelcv %>% predict(test)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test$fremskreven_pris_M2),
  Rsquare = R2(predictions, test$fremskreven_pris_M2)
)

# I dette tilfælde har begge metoder lige gode i forhold til hvor godt de fitter 
# på test data. Dette er formegentlig fordi der er nok observationer i data til at fange de
# egentlige trends selv med et enkelt trænings-validerings split. 
# Med andre og mindre datasæt vil man typisk se at modellen fra krydsvalideringen performer
# bedre på nyt data, fordi krydsvalideringen er med til at forhindre overfitting på 
# træningsdata

modelcv$results

# 
# train <- readRDS("~/projects/train_standard.rds")
# y <- train$fremskreven_pris_M2
# x <- train[, 2:39]
# xname <- colnames(x)
# fmla <- as.formula(paste("y ~ ", paste(xname, collapse= "+")))
# # Estimate model parameters
# w_est = lm(fmla, data=x)
# # Make a scatter plot of predicted versus true values 
# y_est = w_est$fitted.values
# plot(
#   y,
#   y_est,
#   xlab = 'kvm pris (true)',
#   ylab = 'kvm pris (estimated)',
#   main = 'kvadratmeterpriser',
#   pch = 20
# )
# 
# # Make a histogram of the residual error
# hist(y-y_est, breaks=41, main="Residual error")


### Introducerer herefter en model med regularisering
library(glmnet)

lambda <- 10^seq(-3, 3, length = 100)
set.seed(42)
modelcv <- train(
  fremskreven_pris_M2 ~ enhed.antalbadevaerelser + enhed.antalvandskylledetoiletter + bolig_areal + bolig_alder + EV_NN_M2 + NN_m2pris_mean + 
    NN_m2pris_max + NN_afstand_max + NN_m2pris_stdrel + ydervaegsmateriale_gasbeton + tagtype_fibercement + taet_paa_kyst, data = training,
  method = "glmnet",
  trControl = trainControl(
    method = "cv", number = 10
  ),
  tuneGrid = expand.grid(alpha = 0,lambda = lambda)
)

plot(modelcv$results$lambda, modelcv$results$RMSE)
# Model coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)
# Make predictions
predictions <- ridge %>% predict(test)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test$fremskreven_pris_M2),
  Rsquare = R2(predictions, test$fremskreven_pris_M2)
)

# alpha0fit <- cv.glmnet(x = xtrain,
#                        y = ytrain,
#                        type.measure = "mse",
#                        alpha = 0,
#                        family = "gaussian")
# 
# alpha0predict <- predict(alpha0fit,
#                          s = alpha0fit$lambda.1se,
#                          newx = xtest)
#
# Den optimale værdi for lambda er sat i 
# lambda.1se som giver den simpleste model
# med færrest parametre
# mean squared error for prediktionen:
#
# mean((ytest - alpha0predict)^2)
# plot(alpha0fit,
#      xvar = "lambda",
#      label = TRUE)
# alpha0fit$lambda.1se # Den brugte lambda vaerdi
# 
# 
# op <- par(mfrow=c(1, 2))
# plot(alpha0fit$glmnet.fit, "norm",   label=TRUE)
# plot(alpha0fit$glmnet.fit, "lambda", label=TRUE)
# par(op)


# Each curve corresponds to a variable. It shows the path of its coefficient against 
# the ℓ1-norm of the whole coefficient vector at as λ varies. The axis above 
# indicates the number of nonzero coefficients at the current λ,
# which is the effective degrees of freedom (df ) for the lasso. Users may also
# wish to annotate the curves; this can be done by setting label = TRUE in the plot command.
# cvfit = cv.glmnet(x, y, nfolds = 10)
# plot(cvfit)
# cvfit$lambda.min
# 
# fit = glmnet(x,
#              y, 
#              alpha = 0.2,
#              weights = c(rep(1,107542),rep(2,107541)), 
#              nlambda = 20)
# plot(fit, xvar = "lambda", label = TRUE)


###########################################
#### ANN (Artificial Neural Network) ######
###########################################
set.seed(42)
modelcv <- train(
  fremskreven_pris_M2 ~ enhed.antalbadevaerelser + enhed.antalvandskylledetoiletter + bolig_areal + bolig_alder + EV_NN_M2 + NN_m2pris_mean + 
    NN_m2pris_max + NN_afstand_max + NN_m2pris_stdrel + ydervaegsmateriale_gasbeton + tagtype_fibercement + taet_paa_kyst, data = training,
  method = "nnet",
  trControl = trainControl(
    method = "cv", number = 10
  ),
  trace=TRUE, maxit=10, linout = 1 # Maxit er her kun sat til 1 iteration
)

predictions <- modelcv %>% predict(test)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test$fremskreven_pris_M2),
  Rsquare = R2(predictions, test$fremskreven_pris_M2)
)


# library(neuralnet)
# 
# y_train <- train[, colnames(train) == "fremskreven_pris_M2"]
# attributeNames <- colnames(train)[colnames(train) != "fremskreven_pris_M2" ]
# (fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse= "+"))))
# # Fit neural network to training set
# MSEBest = Inf
# for(t in 1:1){ # Number of re-trains of neural network
#   netwrk = neuralnet(fmla, 
#                      train,
#                      hidden=2, # Number of hidden units
#                      act.fct='tanh',
#                      linear.output=TRUE, 
#                      err.fct='sse');
#   
#   mse <- sum((unlist(netwrk$net.result)-y_train)^2)
#   
#   if(mse<MSEBest){
#     bestnet <- netwrk
#     MSEBest <- mse
#   }
# }
# 
# modFit_1 <- train(v_response ~., method="nnet", trControl=cvCtrl, data=df_train, trace=TRUE, maxit=1000, linout = 1)
#
# model <- train(RT..seconds.~., data = cadets, 
#                method = "nnet", trControl = ctrl,
#                linout = TRUE)
