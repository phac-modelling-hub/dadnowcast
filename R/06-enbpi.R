#' Ensemble batch prediction intervals and prediction metrics
#' 
#' @param dadnow A dadnow object.
#' @param k The number of steps ahead to predict.
#' @param train_window The number of days to use for training.
#' @param level The prediction interval level.
#' @param batches The number of batches to use for training.
#'
#' @returns A dadnow object with the ensemble predictions and intervals added.
#' @export
enbpi <- function(dadnow, model, k = NULL, train_window = 30, level = 0.95) {
  if (is.null(k)) k <- length(dadnow$y_nowcast)
  
  train_indices <- sample(1:(length(dadnow$X_train) - k - train_window), batches, replace = FALSE)

  preds <- vector(mode = "list", length = batches)
  rmse <- vector(mode = "numeric", length = batches)
  mae <- vector(mode = "numeric", length = batches)
  mre <- vector(mode = "numeric", length = batches)
  for (i in 1:batches) {
    # Get the training data
    X_train <- dadnow$X_train[train_indices[i]:(train_indices[i] + train_window - 1), ]
    y_train <- dadnow$y_train[train_indices[i]:(train_indices[i] + train_window - 1)]
    
    # Get the test data
    X_test <- dadnow$X_train[(train_indices[i] + train_window):(train_indices[i] + k + train_window), ]
    y_test <- dadnow$y_train[(train_indices[i] + train_window):(train_indices[i] + k + train_window)]

    # Fit the model
    preds[[i]] <- dispatch_model(model = dadnow$model, x_train = X_train, y_train = y_train, x_nowcast = X_test, params = dadnow$params)
    rmse[i] <- sqrt(mean((dadnow$y_test - preds[[i]]$prediction)^2))
    mae[i] <- mean(abs(dadnow$y_test - preds[[i]]$prediction))
    mre[i] <- mean(((dadnow$y_test - preds[[i]]$prediction) / (dadnow$y_test + 0.1))^2)
  }

  preds_mat <- matrix(unlist(preds), nrow = k, byrow = FALSE)
  preds_ci <- apply(preds_mat, 1, function(x) quantile(x, c((1 - level)/2, 1 - (1 - level)/2)))

  list(enbpi = preds_ci, rmse = rmse, mae = mae, mre = mre)
}
