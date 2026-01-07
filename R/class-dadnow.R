print.dadnow <- function(dadnow) {
  cat("dadnow object\n")
  cat("Formula:", dadnow$formula, "\n")
  cat("Model:", dadnow$model, "\n")
  cat("Date column:", dadnow$date_col, "\n")
  cat("Order:", dadnow$order, "\n")
  cat("Require imputation:", dadnow$require_imputation, "\n")
}

summary.dadnow <- function(dadnow) {
  cat("dadnow object\n")
  cat("Formula:", dadnow$formula, "\n")
  cat("Model:", dadnow$model, "\n")
  cat("Date column:", dadnow$date_col, "\n")
  cat("Order:", dadnow$order, "\n")
  cat("Require imputation:", dadnow$require_imputation, "\n")
}
