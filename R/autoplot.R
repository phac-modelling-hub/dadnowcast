#' Plot the nowcasts
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.dadnow <- function(dadnow, last_n = NULL) {
  plot_data <- data.frame(
    x = dadnow$data$dates_train,
    y = c(dadnow$data$y_train, dadnow$data$y_test),
    method = "Training Data"
  )
  if (is.null(last_n)) last_n <- length(dadnow$data$dates_nowcast)
  if (last_n >= nrow(plot_data)) last_n <- nrow(plot_data) - 1
  plot_data <- plot_data[(nrow(plot_data)- last_n):nrow(plot_data), ] 

  plot_data <- rbind(plot_data, 
    data.frame(
      x = dadnow$data$dates_nowcast,
      y = dadnow$predictions,
      method = dadnow$model_id
    )
  )

  ggplot2::ggplot(data = plot_data) +
    ggplot2::aes(x = x, y = y, colour = method) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Date", y = "Prediction", colour = "Model")
}

#' Plot the nowcasts
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.multidadnow <- function(multidadnow, last_n = NULL) {
  plot_data <- data.frame(
    x = multidadnow$models[[1]]$prepped_data$dates_train,
    y = c(
      multidadnow$models[[1]]$prepped_data$y_train,
      multidadnow$models[[1]]$prepped_data$y_test),
    method = "Training Data"
  )
  if (is.null(last_n)) last_n <- length(dadnow$data$dates_nowcast)
  if (last_n >= nrow(plot_data)) last_n <- nrow(plot_data) - 1
  plot_data <- plot_data[(nrow(plot_data)- last_n):nrow(plot_data), ] 

  for (i in 1:length(multidadnow$models)) {
    plot_data <- rbind(plot_data, 
      data.frame(
        x = multidadnow$models[[i]]$prepped_data$dates_nowcast,
        y = multidadnow$models[[i]]$predictions,
        method = multidadnow$models[[i]]$model_id
      )
    )
  }

  ggplot2::ggplot(data = plot_data) +
    ggplot2::aes(x = x, y = y, colour = method) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Date", y = "Prediction", colour = "Model")
}



