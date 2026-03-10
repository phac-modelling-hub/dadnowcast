#' Plot the nowcasts
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.dadnow <- function(dadnow, last_n = NULL) {
  plot_data <- data.frame(
    x = dadnow$dates_train,
    y = c(dadnow$y_train, dadnow$y_test),
    method = "Training Data"
  )
  if (is.null(last_n)) last_n <- length(dadnow$dates_nowcast)
  if (last_n >= nrow(plot_data)) last_n <- nrow(plot_data) - 1
  plot_data <- plot_data[(nrow(plot_data)- last_n):nrow(plot_data), ] 

  nowcast_models <- names(dadnow)[grepl("nowcast_", names(dadnow))]
  if (length(nowcast_models) > 0) {
    for (i in seq_along(nowcast_models)) {
      plot_data <- rbind(
        plot_data,
        data.frame(
          x = dadnow$dates_nowcast,
          y = dadnow[[nowcast_models[i]]]$prediction,
          method = gsub("(nowcast_)", "",nowcast_models[i])
        )
      )
    }
  }

  ggplot2::ggplot(data = plot_data) +
    ggplot2::aes(x = x, y = y, colour = method) +
    ggplot2::geom_line()
}
