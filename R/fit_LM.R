#' Fit a linear model on given data and make predictions for a given set of data
#' 
#' @param XTrain Training data for the explanatory variables in the model
#' @param YTrain Training data for the response variable
#' @param XNowcast Data to make predictions bases on
#' 
#' @returns Linear model object and predictions
#' @export

fit_LM <- function(YTrain,XTrain, XNowcast){
  
  full_data <- as.data.frame(cbind(YTrain,XTrain))

  fitted_LM <- lm(YTrain ~ ., data = full_data)
  # fitted_LM
  XNowcast <- as.data.frame(XNowcast)
  predicted_LM <- predict(fitted_LM, XNowcast)
  
  list(fitted_LM, predicted_LM)
}