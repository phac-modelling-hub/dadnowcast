#' Print a dadnow object
#'
#' @param dadnow A dadnow object.
#'
#' @returns A dadnow object invisibly.
#' @export
print.dadnow <- function(dadnow) {
  cat("Formula:", deparse(dadnow$formula), "\n")
  cat("Date column:", dadnow$date_col, "\n")

  cat("\nTest set evaluation metrics:\n")
  print(dadnow$eval)
  if (any(dadnow$y_test == 0)) {
    cat("Note: There are zeros in the test set, which affects the mean relative error.\n")
  }
  
  #trained_models <- names(dadnow)[grepl("nowcast_", names(dadnow))]
  #if (length(trained_models) == 0) trained_models <- "None"
  #cat("Nowcasted models:", gsub("nowcast_", "", trained_models), "\n")

  #for (model in trained_models) {
  #  cat(paste0(model, ":"))
  #  print(dadnow[[model]])
  #  cat("\n")
  #}

  invisible(dadnow)
}

#' Summarize a dadnow object
#' 
#' Currently a placeholder.
#'
#' @param dadnow A dadnow object.
#'
#' @returns A dadnow object invisibly.
#' @export
summary.dadnow <- function(dadnow) {
  cat("dadnow object\n")
  cat("Formula:", deparse(dadnow$formula), "\n")
  cat("Model:", dadnow$model, "\n")
  cat("Date column:", dadnow$date_col, "\n")
  cat("Order:", dadnow$order, "\n")
  cat("Require imputation:", dadnow$require_imputation, "\n")

  invisible(dadnow)
}
