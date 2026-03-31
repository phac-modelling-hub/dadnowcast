#' Fit an AR model on given data and make predictions for a given set of data with options for using an AR, ARX, or ARIMA model
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Current data of X_train for which there is no Y_train data, used to make nowcasts
#' @param params A named list containing additional parameters: `p` Integer indicating the degree of the AR model, and `n.ahead` integer indicating the number of predictions to be make
#'
#' @returns Arima object, predictions, and fitted values of the model on the training data

fit_ar <- function(Y_train, X_train = NULL, X_nowcast = NULL, params = list(p = 1, n.ahead = 1)) {
  # this is just a wrapper function that calls the fit_arx model with the proper parameters
  fit_arx(Y_train, X_train, X_nowcast, params)
}
