#########################################################
###################### PCA ##############################
#########################################################

train <- readRDS("C:/Users/alhan/projects/train.rds")
train <- train[train$ejendomsgruppe %in% c(
  "pcl_byzone",
  "pcl_landzone",
  "pcl_uzone",
  "pcl_somzone",
  "rkh_byzone",
  "rkh_somzone",
  "rkh_uzone",
  "rkh_landzone"
), ]
train <- standardize(train)
train_pca <- prcomp(train)
# summary(train_pca)
# str(train_pca)

autoplot(train_pca, loadings = TRUE)

s <- svd(train)
rho <- ((s$d)^2)/(sum((s$d)^2))
par(mar = c(5, 4, 4, 4) + 0.3)
plot(rho,
     type='o',
     main="Varians forklaret med principal components",
     xlab="Principal components",
     ylab="Forklaret varians",
     col='deepskyblue')
par(new = TRUE)
plot(cumsum(rho),
     type='o',
     col='darkorchid',
     axes = FALSE,
     ylab = "",
     xlab = "")
legend("topleft",
       c("varians","kommulativ varians"),
       cex=.8,
       bty = 'n',
       fill=c("deepskyblue","darkorchid")
)
axis(side=4)
mtext("Kummulativ varians", side=4, line=3)

#############################################################
############ Simpel linear model ############################
#############################################################

train <- readRDS("C:/Users/alhan/projects/train_standard.rds")
fmla <- as.formula(fremskreven_pris_M2 ~ bolig_areal + EV_NN_M2 + bolig_alder)
# Implement the 3-fold cross-fold plan with vtreat
k <- 10 # Antallet af folds
splitPlan <- vtreat::kWayCrossValidation(nrow(train), k , dframe = NULL, y = NULL)
train$pred.cv <- 0

for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- lm(fmla, data = train[split$train, ])
  train$pred.cv[split$app] <- predict(model, newdata = train[split$app, ])
}

# Predict from a full model
train$pred <- predict(lm(fmla, data = train))

ggplot(data = train, aes(x = pred, y =  fremskreven_pris_M2)) +
  geom_point() +
  geom_abline(color = "blue")

ggplot(data = train, aes(x = pred.cv, y =  fremskreven_pris_M2)) +
  geom_point() +
  geom_abline(color = "green")

rmse <- mltools::rmse(train$pred, train$fremskreven_pris_M2)
rmse.cv <- mltools::rmse(train$pred.cv, train$fremskreven_pris_M2)
rsq <- function (x, y) cor(x, y) ^ 2
r_squared <- rsq(x = train$pred, y = train$fremskreven_pris_M2)
r_squared.cv <- rsq(x = train$pred.cv, y = train$fremskreven_pris_M2)

#########################################
########### Regulariseret model #########
#########################################

  train <- readRDS("C:/Users/alhan/projects/train_standard.rds")
  y <- as.matrix(train$fremskreven_pris_M2)[1:200000,]
  x <- as.matrix(train[, 2:39])[1:200000,]
  fit = glmnet(x, y, alpha = 0, nlambda = 100)
  plot(fit, xvar = "lambda", label = TRUE)
  # Each curve corresponds to a variable. It shows the path of its coefficient against 
  # the ℓ1-norm of the whole coefficient vector at as λ varies. The axis above 
  # indicates the number of nonzero coefficients at the current λ,
  # which is the effective degrees of freedom (df ) for the lasso. Users may also
  # wish to annotate the curves; this can be done by setting label = TRUE in the plot command.

    lambda <- cv.glmnet(x, y)
    cvfit = cv.glmnet(x, y, nfolds = 10, lambda = lambda$lambda)
    plot(cvfit)

    x_predict <-  as.matrix(train[, 2:39])[200001:215083,]
    y_predict <- as.matrix(train$fremskreven_pris_M2)[200001:215083,]
    a <- glmnet::predict.glmnet(object = fit, newx = x_predict)

    
    ###########################################
    #### ANN (Artificial Neural Network) ######
    ###########################################
    
    library(neuralnet)
    train <- readRDS("C:/Users/alhan/projects/train_standard.rds")
    y_train <- train[, colnames(train) == "fremskreven_pris_M2"]
    attributeNames <- colnames(train)[colnames(train) != "fremskreven_pris_M2" ]
    (fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse= "+"))))
    # Fit neural network to training set
    MSEBest = Inf
    for(t in 1:1){ # Number of re-trains of neural network
      netwrk = neuralnet(fmla, 
                         train,
                         hidden=2, # Number of hidden units
                         act.fct='tanh',
                         linear.output=TRUE, 
                         err.fct='sse');
      
      mse <- sum((unlist(netwrk$net.result)-y_train)^2)
      
      if(mse<MSEBest){
        bestnet <- netwrk
        MSEBest <- mse
      }
    }
  
    
