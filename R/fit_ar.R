#' Fit an AR model on given data and make predictions for a given set of data with options for using an AR, ARX, or ARIMA model
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Current data of X_train for which there is no Y_train data, used to make nowcasts
#' @param params A named list containing additional parameters: `p` Integer indicating the degree of the AR model, and `n.ahead` integer indicating the number of predictions to be make
#'
#' @returns Arima object, predictions, and fitted values of the model on the training data

fit_ar <- function(Y_train, X_train = NULL, X_nowcast = NULL, params = list(p = 1)) {
  p <- params$p
  
  n <- nrow(data.frame(X_nowcast))
  
  # create the AR model
  AR_mod <- arima(Y_train, order = c(p, 0, 0))
  
  # create the predictions
  preds <- predict(AR_mod, n)
  
  # put the predictions into a data frame and add 95% confidence bands
  predictions <- data.frame(prediction = as.numeric(preds$pred), lower = as.numeric(preds$pred - 1.96 * preds$se), upper = as.numeric(preds$pred + 1.96 * preds$se))
  
  # find the fitted values of the model
  fitVals <- Y_train - AR_mod$residuals
  
  # assemble the output in the correct format
  list(model = AR_mod, prediction = predictions, fitted_values = fitVals)
}
