#' Create plot to compare forecast performance
#'
#' \code{compare_forecasts_performance} is a function to create a plot which
#' displays the forecast performance of one or more forecast models, based on a
#' selected performance summary variable. For demo purposes, sensitive figures
#' can be hidden from the audience.
#'
#' @param accuracy_overview A tibble containing an overview of the overall
#'   forecast accuracy of each forecast model in the main_forecasting_table,
#'   limited to data for a single group.
#' @param fc_models A character vector specifying which forecast models to
#'   display.
#' @param demo_mode Boolean, which is to be set to TRUE if any potentially
#'   sensitive figures should be hidden from the audience for demo purposes, or
#'   set to FALSE if all figures can safely be displayed.
#'
#' @return A plotly object displaying performance per forecast model.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @import ggplot2
#' @importFrom scales pretty_breaks
#' @importFrom ggthemes theme_calc
#' @importFrom plotly ggplotly layout
#' @importFrom tstools check_data_format get_plot_colors
#'
#' @examples
#' accuracy_overview <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    create_main_forecasting_table() %>%
#'    head(10) %>%
#'    add_fc_models_to_main_forecasting_table(
#'       periods_ahead = 12,
#'       fc_methods = c("linear")
#'    ) %>%
#'    get_forecast_accuracy_overview()
#' compare_forecasts_performance(
#'    accuracy_overview = accuracy_overview,
#'    fc_models = c("fc_linear_trend", "fc_linear_trend_seasonal")
#' )
compare_forecasts_performance <- function(accuracy_overview, fc_models = c(), demo_mode = FALSE) {
  # Check accuracy_overview
  tstools::check_data_format(
    data = accuracy_overview,
    func_name = "compare_forecasts_performance",
    req_cols = c(
      "grouping", "fc_periods_ahead", "fc_model", "n_data_point", 
      "MAE", "MAPE", "MASE", 
      "min", "q1", "metric", "q3", "max", "sd", "order"
    ),
    unique_value_cols = "grouping"
  )
  # Check fc_models
  available_fc_models <- unique(accuracy_overview$fc_model)
  invalid_fc_models <- fc_models[!fc_models %in% available_fc_models]
  if (length(invalid_fc_models) > 0) {
    message <- paste0("The following specified fc_models are not available in the supplied accuracy_overview fc_models column:\n", paste0("\t", invalid_fc_models, collapse = "\n"))
    stop(message)
  }
  if (length(fc_models) == 0) stop("The specified fc_models does not contain any fc_models to be plotted ... \n")
  # Create plot_data
  plot_data <- accuracy_overview %>%
    dplyr::filter(fc_model %in% fc_models) %>% 
    dplyr::mutate(
      lower_bound = pmax((metric - 2*sd), 0),
      upper_bound = metric + 2*sd
    ) %>% 
    dplyr::select(grouping, fc_model, fc_periods_ahead, n_data_point, lower_bound, metric, upper_bound)
  # Create colours for plot
  lines <- sort(unique(plot_data$fc_model))
  colors <- tstools::get_plot_colors(n_colors = length(lines))
  colors <- setNames(colors, lines)
  # Create format function
  format_as <- function(x) format(round(x, 2), nsmall = 2, big.mark = ",", scientific = F)
  format_as_axis <- function(x) format(round(x, 0), nsmall = 0, big.mark = ",", scientific = F)
  # Specify information on evaluation metric
  eval_metric <- "UNKNOWN"
  if (all(accuracy_overview$MAE == accuracy_overview$metric)) eval_metric <- "Mean Absolute Error (MAE)"
  if (all(accuracy_overview$MAPE == accuracy_overview$metric)) eval_metric <- "Mean Absolute Percentage Error (MAPE)"
  if (all(accuracy_overview$MASE == accuracy_overview$metric)) eval_metric <- "Mean Absolute Scaled Error (MASE)"
  # Create tooltip text
  if (demo_mode) {
    tooltip_text <- paste0("
      paste0(
        'Model: ', fc_model,
        '<br>Forecast horizon: ', fc_periods_ahead, ' month(s) ahead',
        '<br>',
        '<br>Data summary: ",eval_metric,"',
        '<br>Data points: ', n_data_point, ' observation(s)',
        '<br>Group: ', grouping
      )
    ")
  } else {
    tooltip_text <- paste0("
      paste0(
        'Model: ', fc_model,
        '<br>Forecast horizon: ', fc_periods_ahead, ' month(s) ahead',
        '<br>Upper bound: ', format_as(upper_bound),
        '<br>Forecast error: ', format_as(metric),
        '<br>Lower bound: ', format_as(lower_bound),
        '<br>',
        '<br>Data summary: ",eval_metric,"',
        '<br>Data points: ', n_data_point, ' observation(s)',
        '<br>Group: ', grouping
      )
    ")
  }
  
  # Create the plot
  plot <- suppressWarnings(
    ggplot2::ggplot(plot_data, ggplot2::aes(x = fc_periods_ahead, y = metric, color = fc_model, fill = fc_model)) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_bound, ymax = upper_bound), linetype = 2, alpha = 0.25) +
      ggplot2::geom_point(ggplot2::aes_string(text = tooltip_text), size = 0.5) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), labels = format_as_axis) +
      ggthemes::theme_calc() +
      ggplot2::xlab("Forecast horizon") +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank()
      )
  )
  # Hide y-axis if required
  if (demo_mode) {
    plot <- plot + 
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(), 
        axis.ticks.y = ggplot2::element_blank()
      )
  }
  # Transform to plotly and return
  plot %>% 
    plotly::ggplotly(tooltip = "text") %>% 
    plotly::layout(legend = list(x = 100, y = 0.5)) %>% 
    return()
}