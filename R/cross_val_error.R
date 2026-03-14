cross_val_error <- function(X_train, y_train, folds, model, params = list()) {
  evals <- list()
  for (i in unique(folds)) {
    X_train_fold <- X_train[folds != i, ]
    y_train_fold <- y_train[folds != i]
    X_test_fold <- X_train[folds == i, ]
    y_test_fold <- y_train[folds == i]
    
    nowcast <- dispatch_model(model)(
      X_train = X_train_fold,
      Y_train = y_train_fold,
      X_nowcast = X_test_fold,
      params = params
    )
    
    evali <- data.frame(
      "rmse" = rmse(y_test_fold, nowcast$prediction),
      "mae" = mae(y_test_fold, nowcast$prediction),
      "mre" = mre(y_test_fold, nowcast$prediction)
    )
    
    evals[[i]] <- evali
  }

  evals <- do.call(rbind, evals)
  evals <- apply(evals, 2, mean)
  eval <- data.frame(
    "formula" = deparse(formula),
    "model" = model,
    "params" = paste0(names(params), params, collapse = "_"),
    "rmse" = evals["rmse"],
    "mae" = evals["mae"],
    "mre" = evals["mre"]
  )

  return(eval)
}

mae <- function(y_test, nowcast) {
  mean(abs(y_test - nowcast), na.rm = TRUE)
}

rmse <- function(y_test, nowcast) {
  sqrt(mean((y_test - nowcast)^2, na.rm = TRUE))
}

mre <- function(y_test, nowcast) {
  mean(((y_test - nowcast) / (y_test + 0.1))^2, na.rm = TRUE)
}