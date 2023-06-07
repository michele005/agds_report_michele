find_optimal_k <- function(k_values) {
  
  #initialize variables
  best_k <- NULL
  best_mae <- Inf
  
  #iterate over k values
  for (k in k_values) {
    #train KNN model
    knn_fit <- train(VPD_F ~ ., data = train, method = "knn",
                     trControl = trainControl(method = "cv", number = 10),
                     tuneGrid = expand.grid(k = k))
    
    #make predictions on the test set
    knn_pred <- predict(knn_fit, newdata = test)
    
    #calculate MAE on the test set
    mae <- mean(abs(knn_pred - data$VPD_F))
    
    #update best MAE and corresponding k
    if (mae < best_mae) {
      best_mae <- mae
      best_k <- k
    }
  }
  
  #return the optimal k
  return(best_k)
}