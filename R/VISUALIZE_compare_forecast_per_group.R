#' Create plot to compare forecast per group
#'
#' \code{compare_forecast_per_group} is a function to create a plot which
#' compares the forecasted values of a single forecast model for every available
#' group. For demo purposes, sensitive figures can be hidden from the audience.
#'
#' @param main_forecasting_table A tibble containing a single row per group and
#'   several columns of data required for time series forecasting, which has
#'   been created using the \code{create_main_forecasting_table} function and
#'   which has been extended with the fc_models and fc_errors columns using the
#'   \code{add_fc_models_to_main_forecasting_table} function.
#' @param fc_model A character string specifying which forecast model to compare
#'   accross the available groups.
#' @param demo_mode Boolean, which is to be set to TRUE if any potentially
#'   sensitive figures should be hidden from the audience for demo purposes, or
#'   set to FALSE if all figures can safely be displayed.
#'
#' @return A plotly object displaying a comparison of forecasts per group.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @import ggplot2
#' @importFrom ggthemes theme_calc
#' @importFrom plotly ggplotly layout
#' @importFrom tstools get_plot_colors period_to_last_day
#'
#' @examples
#' tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    create_main_forecasting_table() %>%
#'    dplyr::filter(ts_split_date == 200503) %>%
#'    add_fc_models_to_main_forecasting_table(
#'       periods_ahead = 12,
#'       fc_methods = c("linear")
#'    ) %>%
#'    compare_forecast_per_group(
#'       fc_model = "fc_linear_trend_seasonal"
#'    )
compare_forecast_per_group <- function(main_forecasting_table, fc_model = "", demo_mode = FALSE) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "compare_forecast_per_group",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "fc_errors"
    )
  )
  # Check available rows per group
  groups <- main_forecasting_table %>% 
    dplyr::select(grouping) %>% 
    dplyr::distinct() %>% 
    dplyr::pull()
  if (nrow(main_forecasting_table) != length(groups)) stop("The specified main_forecasting_table should be filtered and contain only a single row per group ... \n")
  # Check fc_model
  if (length(fc_model) > 1) stop("Only a single fc_model can be specified for comparing groups ... \n")
  if (!fc_model %in% main_forecasting_table$fc_errors[[1]]$fc_model) {
    message <- paste0("The specified fc_model '",fc_model,"' is not available in the supplied main_forecasting_table fc_errors column ... \n")
    stop(message)
  }
  model_to_compare <- fc_model
  # Extract forecasts from fc_error data
  forecasts <- dplyr::bind_rows(
      main_forecasting_table$fc_errors
    ) %>% 
    dplyr::filter(fc_model == model_to_compare) %>% 
    dplyr::transmute(
      grouping = grouping,
      period = period,
      fc_periods_ahead = fc_periods_ahead,
      fc_model = fc_model,
      value = fc_value
    )
  # Extract actuals from ts_objects
  actuals <- get_actuals_from_main_forecasting_table(
      main_forecasting_table = main_forecasting_table,
      for_plot = T
    ) %>% 
    dplyr::filter(period < min(forecasts$period))
  # Combine actuals and forecasts into plot data
  plot_data <- dplyr::bind_rows(
      actuals,
      forecasts
    ) %>% 
    dplyr::mutate(period = tstools::period_to_last_day(period))
  # Determine split date
  split_date <- tstools::period_to_last_day(main_forecasting_table$ts_split_date)
  # Create colours for plot
  lines <- sort(unique(plot_data$grouping))
  colors <- tstools::get_plot_colors(n_colors = length(lines))
  colors <- setNames(colors, lines)
  # Determine format function
  format_as <- function(x) format(round(x, 2), nsmall = 2, big.mark = ",", scientific = F)
  format_as_axis <- function(x) format(round(x, 0), nsmall = 0, big.mark = ",", scientific = F)
  # Create tooltip text
  if (demo_mode) {
    tooltip_text <- paste0("
      paste0(
        'Group: ', grouping,
        '<br>Period: ', format.Date(period, '%B %Y'),
        ifelse(grepl('actuals', fc_model), '', paste0('<br>Ahead: ', fc_periods_ahead, ' month(s)')),
        '<br>Type: ', fc_model
      )
    ")
  } else {
    tooltip_text <- paste0("
      paste0(
        'Group: ', grouping,
        '<br>Period: ', format.Date(period, '%B %Y'),
        ifelse(grepl('actuals', fc_model), '', paste0('<br>Ahead: ', fc_periods_ahead, ' month(s)')),
        '<br>Value: ', format_as(value),
        '<br>Type: ', fc_model
      )
    ")
  }
  # Create the plot
  plot <- suppressWarnings(
    ggplot2::ggplot(plot_data, ggplot2::aes(x = period, y = value, color = grouping)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes_string(text = tooltip_text), size = 0.5) +
    ggplot2::geom_vline(xintercept = as.numeric(split_date), linetype = 3) +
    ggplot2::scale_color_manual(values = colors) + 
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), labels = format_as_axis) +
    ggthemes::theme_calc() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
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