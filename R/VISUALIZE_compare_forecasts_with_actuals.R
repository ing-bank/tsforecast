#' Create plot to compare forecasts with actuals
#'
#' \code{compare_forecasts_with_actuals} is a function to create a plot which
#' compares the forecasted values with the actuals (if already available) for a
#' set of specified forecast models. For demo purposes, sensitive figures can be
#' hidden from the audience.
#'
#' @param main_forecasting_table A tibble containing a single row and several
#'   columns of data required for time series forecasting, which has been
#'   created using the \code{create_main_forecasting_table} function and which
#'   has been extended with the fc_models and fc_errors columns using the
#'   \code{add_fc_models_to_main_forecasting_table} function.
#' @param fc_models A character vector specifying which forecast models to
#'   display.
#' @param demo_mode Boolean, which is to be set to TRUE if any potentially
#'   sensitive figures should be hidden from the audience for demo purposes, or
#'   set to FALSE if all figures can safely be displayed.
#' @param show_original Boolean, which is set to TRUE to include the original
#'   actuals in the plot.
#'
#' @return A plotly object displaying both forecasts and actuals.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @import ggplot2
#' @importFrom ggthemes theme_calc
#' @importFrom plotly ggplotly layout
#' @importFrom tstools check_data_format get_plot_colors period_to_last_day
#'   unlist_if_required
#'
#' @examples
#' tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    create_main_forecasting_table() %>%
#'    head(1) %>%
#'    add_fc_models_to_main_forecasting_table(
#'       periods_ahead = 12,
#'       fc_methods = c("linear")
#'    ) %>%
#'    compare_forecasts_with_actuals(
#'       fc_models = c("fc_linear_trend", "fc_linear_trend_seasonal")
#'    )
compare_forecasts_with_actuals <- function(main_forecasting_table, fc_models = c(), demo_mode = FALSE, show_original = TRUE) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "compare_forecasts_with_actuals",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "fc_errors"
    ),
    unique_value_cols = c("grouping", "ts_split_date")
  )
  # Check fc_models
  available_fc_models <- unique(main_forecasting_table$fc_errors[[1]]$fc_model)
  invalid_fc_models <- fc_models[!fc_models %in% available_fc_models]
  if (length(invalid_fc_models) > 0) {
    message <- paste0("The following specified fc_models are not available in the supplied main_forecasting_table fc_errors column:\n", paste0("\t", invalid_fc_models, collapse = "\n"))
    stop(message)
  }
  if (length(fc_models) == 0) stop("The specified fc_models does not contain any fc_models to be plotted ... \n")
  # Extract actuals from ts_objects
  actuals <- get_actuals_from_main_forecasting_table(
    main_forecasting_table = main_forecasting_table,
    for_plot = T
  )
  # Filter out actuals_original if required
  if (!show_original) {
    actuals <- actuals %>% 
      dplyr::filter(fc_model != "actuals_original")
  }
  # Extract forecasts from fc_error data  
  forecasts <- main_forecasting_table %>% 
    dplyr::pull(fc_errors) %>% 
    tstools::unlist_if_required() %>% 
    dplyr::filter(fc_model %in% fc_models) %>% 
    dplyr::transmute(
      grouping = grouping,
      period = period,
      fc_periods_ahead = fc_periods_ahead,
      fc_model = fc_model,
      value = fc_value
    )
  # Combine actuals and forecasts into plot data
  plot_data <- dplyr::bind_rows(
      actuals,
      forecasts
    ) %>% 
    dplyr::filter(period <= max(forecasts$period)) %>% 
    dplyr::mutate(period = tstools::period_to_last_day(period))
  # Determine split date
  split_date <- tstools::period_to_last_day(main_forecasting_table$ts_split_date)
  # Get number of lines (excluding original actuals)
  lines <- sort(unique(plot_data$fc_model))
  lines <- lines[lines != "actuals_original"]
  # Create linetypes for plot
  linetypes <- setNames(rep("solid",length(lines)),lines)
  # Create colours for plot
  colors <- tstools::get_plot_colors(n_colors = length(lines) - 1)
  colors <- setNames(c("#000000",colors),lines)
  # Overwrite colour for original actuals
  if ("actuals_original" %in% actuals$fc_model) {
    linetypes <- c(linetypes, setNames("dotted", "actuals_original"))
    colors <- c(colors, setNames("#000000", "actuals_original"))
  }
  # Determine format function
  if (min(plot_data$value, na.rm = T) >= 0 & max(plot_data$value, na.rm = T) <= 1) {
    format_as_axis <- function(x) format(round(x, 2), nsmall = 0, big.mark = ",", scientific = F)
  } else {
    format_as_axis <- function(x) format(round(x, 0), nsmall = 0, big.mark = ",", scientific = F)
  }
  format_as <- function(x) format(round(x, 2), nsmall = 2, big.mark = ",", scientific = F)
  
  # Create tooltip text
  if (demo_mode) {
    tooltip_text <- paste0("
      paste0(
        'Type: ', fc_model,
        '<br>Period: ', format.Date(period, '%B %Y'),
        ifelse(grepl('actuals', fc_model), '', paste0('<br>Ahead: ', fc_periods_ahead, ' month(s)')),
        '<br>Group: ', grouping
      )
    ")
  } else {
    tooltip_text <- paste0("
      paste0(
        'Type: ', fc_model,
        '<br>Period: ', format.Date(period, '%B %Y'),
        ifelse(grepl('actuals', fc_model), '', paste0('<br>Ahead: ', fc_periods_ahead, ' month(s)')),
        '<br>Value: ', format_as(value),
        '<br>Group: ', grouping
      )
    ")
  }
  # Create the plot
  plot <- suppressWarnings(
    ggplot2::ggplot(plot_data, ggplot2::aes(x = period, y = value, color = fc_model, linetype = fc_model)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes_string(text = tooltip_text), size = 0.5) +
    ggplot2::geom_vline(xintercept = as.numeric(split_date), linetype = 3) +
    ggplot2::scale_color_manual(values = colors) + 
    ggplot2::scale_linetype_manual(values = linetypes) +
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