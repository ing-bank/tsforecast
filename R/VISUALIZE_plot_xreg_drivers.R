#' Create plot to see how selected external regressor influences variable of
#' interest
#'
#' \code{plot_xreg_driver} is a function to create a plot which shows how one
#' external regressor influences the variable of interest, ceteris paribus
#'
#' @param main_forecasting_table A tibble containing a single row per group and
#'   several columns of data required for time series forecasting, which has
#'   been created using the \code{create_main_forecasting_table} function and
#'   which has been extended with the fc_models and fc_errors columns using the
#'   \code{add_fc_models_to_main_forecasting_table} function. Note that this
#'   table should have the output of a multivariate analysis, otherwise the
#'   function does not run.
#' @param xreg A character that contains a string with the name of the external
#'   regressor to be plotted.
#' @param delta A numeric to look at how many deltas one wants for xreg.
#'   Applicable only in ML fc_models (e.g. trees and forests) where such deltas
#'   are taken into account when running a forecast.
#' @param granularity A numeric determining how fine one wants the plot to be.
#'   Higher numbers return better resolution plots, but will also increase run
#'   time.
#'
#' @return A plotly object displaying how the selected xreg drivers the variable of interest.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom tibble tibble
#' @import dplyr
#' @import ggplot2
#' @importFrom ggthemes theme_calc
#' @importFrom plotly ggplotly layout
#' @importFrom tstools check_data_format get_plot_colors
#'
#' @examples
#' tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company"),
#'       xreg_cols = c("spotprice", "gemprice")
#'    ) %>%
#'    create_main_forecasting_table() %>%
#'    dplyr::filter(ts_split_date == 200503) %>%
#'    add_fc_models_to_main_forecasting_table(
#'       periods_ahead = 12
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'    plot_xreg_drivers(
#'       xreg = "spotprice"
#'    )
plot_xreg_drivers <- function(main_forecasting_table, xreg = "", delta = 0, granularity = 50) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "plot_xreg_drivers",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "fc_models", "fc_errors"
    ),
    unique_value_cols = "ts_split_date"
  )
  # Check available groups
  groups <- main_forecasting_table %>% 
    dplyr::select(grouping) %>% 
    dplyr::distinct() %>% 
    dplyr::pull()
  if (nrow(main_forecasting_table) != length(groups)) stop("The specified main_forecasting_table should be filtered and contain only a single row per group ... \n")
  # Check that this is a multivariate dataset
  if (length(attr(main_forecasting_table$ts_object_train[[1]], "xreg_cols")) == 0) {
    stop("Analysis is univariate. You can only do this plot in multivariate analysis!")
  }
  # Check that xreg only has one argument
  if (length(xreg) != 1) {
    stop("Input xreg can only have one argument")
  }
  # Check that the xreg is valid
  if (!all(xreg %in% attr(main_forecasting_table$ts_object_train[[1]], "xreg_cols"))) {
    stop("Specified external regressor is not in the main_forecasting_table!")
  }
  # Check that granuality is numeric
  if (!is.numeric(granularity) | granularity < 0) {
    stop("Input granularity must be non-negative and numeric")
  }
  # Check that delta is numeric and not larger than max(seasonality)
  if (!is.numeric(delta)) {
    stop("Input delta must be numeric")
  }
  if (delta > max(attr(main_forecasting_table$ts_object_train[[1]], "seasonality")) | delta < 0) {
    stop(paste0("Input delta must be non-negative and not higher than maximum seasonality, which is",
                max(attr(main_forecasting_table$ts_object_train[[1]], " seasonality")))) 
  }
  # Get list of fc_models to run the exercise
  fc_models <- names(main_forecasting_table$fc_models[[1]])
  # If delta is not zero, re-adjust xreg name and only look at randomforest and trees
  if (delta != 0) {
    xreg <- paste0(xreg, "_delta", delta)
    forest <- fc_models[grepl("forest", fc_models)]
    rpart <- fc_models[grepl("rpart", fc_models)]
    ctree <- fc_models[grepl("ctree", fc_models)]
    fc_models <- c(rpart, ctree, forest)
  }
  # Create the data frame of candidate external regressors, fitted values and fc_model names
  main_fit_table <- tibble::tibble(
    xreg_value = vector(length = granularity * length(fc_models)),
    fitted = vector(length = granularity * length(fc_models)),
    fc_model = rep(fc_models, each = granularity)
  )
  # Create plot_data
  plot_data <- main_fit_table %>% 
    dplyr::slice(0)
  # Calculate fits for forest and tree fc_models
  if (any(grepl("forest", fc_models) | grepl("rpart", fc_models) | grepl("ctree", fc_models))) {
    ML_fit_table <- get_fitted_tree_forest_values(
      main_forecasting_table = main_forecasting_table,
      main_fit_table = main_fit_table,
      xreg = xreg
    )
    plot_data <- plot_data %>% 
      dplyr::bind_rows(ML_fit_table)
  }
  # Calculate fits for forecast fc_models
  if (any(grepl("linear", fc_models) | grepl("arima", fc_models) | grepl("nn", fc_models))) {
    forecast_fit_table <- get_fitted_forecast_values(
      main_forecasting_table = main_forecasting_table,
      main_fit_table = main_fit_table,
      xreg = xreg
    )
    plot_data <- plot_data %>% 
      dplyr::bind_rows(forecast_fit_table)
  }
  # Calculate fits for prophet fc_models
  if (any(grepl("prophet", fc_models))) {
    prophet_fit_table <- get_fitted_prophet_values(
      main_forecasting_table = main_forecasting_table,
      main_fit_table = main_fit_table,
      xreg = xreg
    )
    plot_data <- plot_data %>% 
      dplyr::bind_rows(prophet_fit_table)
  }
  # Calculate fits for recursive fc_models
  if (any(grepl("_rec_", fc_models))) {
    # TODO: TELL MEHMET TO IMPLEMENT THE plot_xreg_drivers() FUNCTION FOR THE RECURSIVE FORECAST MODELS!
  }
  # Create colors for plot
  lines <- unique(plot_data$fc_model)
  colors <- tstools::get_plot_colors(n_colors = length(lines))
  colors <- setNames(colors, lines)
  # Create tooltip_text
  tooltip_text <- paste0("
    paste0(
      'Group: ', main_forecasting_table$grouping[1],
      '<br>Regressor: ', xreg,
      '<br>Regressor Value: ', format_as(xreg_value),
      '<br>Predicted Value: ', format_as(fitted),
      '<br>Model: ', fc_model
    )
  ")
  
  format_as <- function(x) format(round(x, 2), nsmall = 2, big.mark = ",", scientific = F)
  format_as_axis <- function(x) format(round(x, 1), nsmall = 0, big.mark = ",", scientific = F)
  # Initial plot
  plot <- suppressWarnings(
    plot_data %>% 
      ggplot2::ggplot(ggplot2::aes(x = xreg_value, y = fitted, color = fc_model)) + 
      ggplot2::geom_line() + 
      ggplot2::geom_point(ggplot2::aes_string(text = tooltip_text), size = 0.1) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), labels = format_as_axis) +
      ggplot2::scale_color_manual(values = colors) +
      ggthemes::theme_calc() +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank()
      )
  )
  
  # Transform to plotly and return
  plot %>% 
    plotly::ggplotly(tooltip = "text") %>% 
    plotly::layout(legend = list(x = 100, y = 0.5)) %>% 
    return()
}
