# Classification model: 
# twoClasssummary sættes hvis man ønsker at bruge AUC mål:
# myControl <- trainControl(
#   method = "cv",
#   number = 10,
#   summaryFunction = twoClassSummary, #defaultSummary,
#   classProbs = TRUE, # IMPORTANT!
#   verboseIter = TRUE
# )
#model <- train(Class ~., data = Sonar, method = "glm", trControl = myControl)

# Random forest model
#model <- train(
#   quality ~ .,
#   tuneLength = 1,
#   data = wine, 
#   method = "ranger",
#   trControl = trainControl(
#     method = "cv", 
#     number = 5, 
#     verboseIter = TRUE
#   )



# model <- train(
#   y ~ ., 
#   data = overfit,
#   tuneGrid = expand.grid(
#     alpha = 0:1,
#     lambda =  seq(0.0001, 1, length = 20)
#   ),
#   method = "glmnet",
#   trControl = myControl
# )
# print(model)
# max(model$results$ROC)
# Plot modellen - den alpha med det højeste ROC er den bedste...

# knn_model <- train(
#   x = breast_cancer_x, 
#   y = breast_cancer_y,
#   method = "glm",
#   trControl = myControl,
#   preProcess = "knnImpute" # "medianImpute"

# model_list <- list(item1 = model_glmnet, item2 = model_rf)
# # Pass model_list to resamples(): resamples
# resamples <- resamples(model_list)
# bwplot(resamples, metric = "ROC")