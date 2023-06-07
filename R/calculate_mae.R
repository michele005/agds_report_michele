#function to calculate MAE on the test set for a given k
calculate_mae <- function(k) {
  #train KNN model
  knn_fit <- train(VPD_F ~ ., data = train, method = "knn",
                   trControl = trainControl(method = "cv", number = 10),
                   tuneGrid = expand.grid(k = k))
  
  #make predictions on the test set
  knn_pred <- predict(knn_fit, newdata = test)
  
  #calculate MAE on the test set
  mae <- mean(abs(knn_pred - test$VPD_F))
  
  #return MAE
  return(mae)
}