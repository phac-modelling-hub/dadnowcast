
#' Plot the nowcasts
#'
#' @param multidadnow A multidadnow object.
#' @param last_n The number of most recent observations to plot (in addition to the nowcasts). Defaults to all.
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.multidadnow <- function(
    multidadnow, 
    se = FALSE, 
    last_n = Inf, 
    alpha = 0.12) 
{
  plot_data <- get_data(multidadnow, include_training = TRUE)
  plot_data <- plot_data[!is.na(plot_data[, multidadnow$response]), ]
  
  # Get the training data, and only plot the last n observations.
  training_data <- plot_data[plot_data$model == "Training", ]
  if (last_n < nrow(training_data)) {
    training_data <- training_data[(nrow(training_data) - last_n):nrow(training_data), ]
  }
  
  # Remove columns so that the training data is plotted all facets.
  training_data$model    <- NULL
  training_data$formula  <- NULL
  training_data$model_id <- NULL
  training_data$params   <- NULL
  
  
  plot_data <- plot_data[plot_data$model != "Training", ]
  
  # Bare object. We want the nowcasts to be
  # the top layer, so they come later.
  g <- ggplot2::ggplot() + 
    ggplot2::theme(
      panel.grid = element_line(color = 'grey97')
    )
  
  # Uncertainty ribbons
  if (se) {
    g <- g +
      ggplot2::geom_ribbon(
        data = plot_data,
        mapping = ggplot2::aes(
          x = !!sym(multidadnow$date_col),
          ymin = pi_lower,
          ymax = pi_upper,
          colour = model_id,
          fill   = model_id,
          group  = paste0(model, params, formula)
        ),
        alpha = alpha,
        linewidth = 0.1,
        linetype = "solid"
      )
  }

  # Past data
  if (last_n > 0) {
    g <- g +
      ggplot2::geom_step(
        data = training_data,
        mapping = ggplot2::aes(
          x = !!sym(multidadnow$date_col),
          y = !!sym(multidadnow$response)
        ),
        colour = "grey",
        inherit.aes = FALSE
      )
  }
  
  g <- g + ggplot2::geom_line(
      data = plot_data,
      mapping = ggplot2::aes(
        x = !!sym(multidadnow$date_col),
        y = !!sym(multidadnow$response),
        colour = model_id,
        group = paste0(model, params, formula)
      ),
      linewidth = 0.7
    ) +
      ggplot2::labs(x = "", y = "Prediction")#, colour = "Model")

  return(g)
}



