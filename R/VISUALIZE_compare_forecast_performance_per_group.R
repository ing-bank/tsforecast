#' Create plot to compare forecast performance per group
#'
#' \code{compare_forecast_performance_per_group} is a function to create a plot
#' which compares the forecast performance of a single forecast model for every
#' available group, based on a selected performance summary variable. For demo
#' purposes, sensitive figures can be hidden from the audience.
#'
#' @param accuracy_overview A tibble containing an overview of the overall
#'   forecast accuracy for each group in the main_forecasting_table, limited to
#'   data for a single forecast model.
#' @param fc_model A character string specifying which forecast model to compare
#'   accross the available groups.
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
#' @importFrom ggthemes theme_calc
#' @importFrom plotly ggplotly layout
#' @importFrom tstools get_plot_colors
#'
#' @examples
#' accuracy_overview <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    create_main_forecasting_table(
#'       min_train_periods = 180
#'    ) %>%
#'    add_fc_models_to_main_forecasting_table(
#'       periods_ahead = 12,
#'       fc_methods = c("linear")
#'    ) %>% 
#'    get_forecast_accuracy_overview()
#' compare_forecast_performance_per_group(
#'    accuracy_overview = accuracy_overview,
#'    fc_model = "fc_linear_trend_seasonal"
#' )
compare_forecast_performance_per_group <- function(accuracy_overview, fc_model = "", demo_mode = FALSE) {
  # Check accuracy_overview
  tstools::check_data_format(
    data = accuracy_overview,
    func_name = "compare_forecast_performance_per_group",
    req_cols = c(
      "grouping", "fc_periods_ahead", "fc_model", "n_data_point", 
      "MAE", "MAPE", "MASE", 
      "min", "q1", "metric", "q3", "max", "sd", "order"
    )
  )
  # Check fc_model
  if (length(fc_model) > 1) stop("Only a single fc_model can be specified for comparing groups ... \n")
  if (!fc_model %in% accuracy_overview$fc_model) {
    message <- paste0("The specified fc_model '",fc_model,"' is not available in the supplied accuracy_overview fc_models column ... \n")
    stop(message)
  }
  model_to_compare <- fc_model
  # Create plot_data
  plot_data <- accuracy_overview %>%
    dplyr::filter(fc_model == model_to_compare) %>% 
    dplyr::mutate(
      lower_bound = pmax((metric - 2*sd), 0),
      upper_bound = metric + 2*sd
    ) %>% 
    dplyr::select(grouping, fc_model, fc_periods_ahead, n_data_point, lower_bound, metric, upper_bound)
  # Create colours for plot
  lines <- sort(unique(plot_data$grouping))
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
        'Group: ', grouping,
        '<br>Forecast horizon: ', fc_periods_ahead, ' month(s) ahead',
        '<br>',
        '<br>Data summary: ",eval_metric,"',
        '<br>Data points: ', n_data_point, ' observation(s)',
        '<br>Model: ', fc_model
      )
    ")
  } else {
    tooltip_text <- paste0("
      paste0(
        'Group: ', grouping,
        '<br>Forecast horizon: ', fc_periods_ahead, ' month(s) ahead',
        '<br>Upper bound: ', format_as(upper_bound),
        '<br>Forecast error: ', format_as(metric),
        '<br>Lower bound: ', format_as(lower_bound),
        '<br>',
        '<br>Data summary: ",eval_metric,"',
        '<br>Data points: ', n_data_point, ' observation(s)',
        '<br>Model: ', fc_model
      )
    ")
  }
  # Create the plot
  plot <- suppressWarnings(
    ggplot2::ggplot(plot_data, ggplot2::aes(x = fc_periods_ahead, y = metric, color = grouping, fill = grouping)) +
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