#' Fit an AR model on given data and make predictions for a given set of data with options for using an AR or ARX model
#'
#' @param XTrain Training data for the explanatory variables in the model
#' @param YTrain Training data for the response variable
#' @param XNowcast Data to make predictions bases on
#' @param p Integer indicating the number of parameters for the AR model
#' @param n.ahead Integer indicating the number of predictions to be make
#' @param arx Indicates if the model to be used is ARX or AR
#'
#' @returns Linear model object and predictions
#' @export

fit_AR <- function(Y_train, X_train = NULL, X_nowcast = NULL, p = 1, n.ahead = 1, arx = "AR") {
  if (n.ahead >= nrow(data.frame(X_nowcast))) {
    n <- n.ahead
  } else {
    n <- nrow(data.frame(X_nowcast))
  }

  if (arx == "AR") {
    AR_mod <- arima(Y_train, order = c(p,0,0))
    
    predictions <- predict(AR_mod,n)
  } else {
    Y_train <- rnorm(10)
    X_train <- cbind(rnorm(10),rnorm(10))
    AR_mod <- arima(Y_train, order = c(p,0,0), xreg = X_train)
    
    predictions <- predict(AR_mod,n,X_nowcast)
  }

  
  list(model = AR_mod, prediction = predictions)
  
}