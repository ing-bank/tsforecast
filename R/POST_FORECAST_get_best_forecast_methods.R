#' Get best forecast methods
#'
#' \code{get_best_forecast_methods} is a function to determine the n best
#' forecast methods from an (ordered) accuracy overview, when evaluating for a
#' specific minimum and maximum number of periods ahead forecast horizon. This
#' is determined separately for every grouping available within the accuracy
#' overview.
#'
#' @param accuracy_overview A tibble containing an overview of the overall
#'   forecast accuracy of each forecast model in the main_forecasting_table.
#' @param n A positive integer value indicating the number of best forecast
#'   methods to be returned.
#' @param min_periods_ahead A positive integer value indicating the minimum
#'   number of periods ahead to be used as forecast horizon for evaluating the
#'   forecast performance.
#' @param max_periods_ahead A positive integer value indicating the maximum
#'   number of periods ahead to be used as forecast horizon for evaluating the
#'   forecast performance.
#' @param filter_incomplete_fc Boolean, which is set to TRUE if all incomplete
#'   forecasts (which did run successfully for one or more split dates for a
#'   specific group) should be filtered before calculation, or set to FALSE if
#'   they should not be filtered.
#'
#' @return A tibble with an overview of the n best forecast methods for each
#'   grouping.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools check_data_format
#' @import dplyr
#'
#' @examples
#' accuracy_overview <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>% 
#'    create_main_forecasting_table(
#'       seasonal_periods = c(12,3),
#'       min_train_periods = 150
#'    ) %>%
#'    add_fc_models_to_main_forecasting_table(
#'       periods_ahead = 12,
#'       fc_methods = c("basic", "linear")
#'    ) %>%
#'    get_forecast_accuracy_overview()
#'get_best_forecast_methods(accuracy_overview)
get_best_forecast_methods <- function(accuracy_overview, n = 1, min_periods_ahead = 1, max_periods_ahead = Inf, filter_incomplete_fc = TRUE) {
  # Check accuracy_overview
  tstools::check_data_format(
    data = accuracy_overview,
    func_name = "get_best_forecast_methods",
    req_cols = c(
      "grouping", "fc_periods_ahead", "fc_model", "n_data_point", 
      "MAE", "MAPE", "MASE", 
      "min", "q1", "metric", "q3", "max", "sd", "order"
    )
  )
  # Check to make sure n is a non-negative whole number
  if (!(is.numeric(n) & n > 0 & n == suppressWarnings(as.integer(n)))) {
    message <- paste0("The parameter 'n' should be a positive integer value, instead of '",n,"' ... ")
    stop(message)
  }
  # Overwrite periods_ahead if invalid
  min_periods_ahead <- max(1, min_periods_ahead, na.rm = T)
  max_periods_ahead <- min(Inf, max_periods_ahead, na.rm = T)
  if (min_periods_ahead > max_periods_ahead) stop(paste0("The parameter min_periods_ahead (= ",min_periods_ahead,") can't be greater than the parameter max_periods_ahead (= ",max_periods_ahead,") ... \n"))
  
  # Filter incomplete forecasts if required
  if (filter_incomplete_fc) {
    complete_forecasts <- accuracy_overview %>% 
      dplyr::group_by(grouping, fc_model) %>% 
      dplyr::mutate(tot_n_data_points = sum(n_data_point)) %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(tot_n_data_points == max(tot_n_data_points)) %>% 
      dplyr::distinct(grouping, fc_model)
    accuracy_overview <- dplyr::inner_join(
      x = accuracy_overview,
      y = complete_forecasts,
      by = c("grouping", "fc_model")
    )
  }
  
  # Perform calculations and return
  accuracy_overview %>% 
    dplyr::filter(fc_periods_ahead >= min_periods_ahead) %>% 
    dplyr::filter(fc_periods_ahead <= max_periods_ahead) %>% 
    dplyr::group_by(grouping, fc_model) %>% 
    dplyr::summarize(
      metric = mean(metric),
      total_rank = sum(order),
      ranking = 1
    ) %>% 
    dplyr::arrange(grouping, abs(metric), total_rank) %>% 
    dplyr::mutate(ranking = cumsum(ranking)) %>%
    dplyr::ungroup() %>% 
    dplyr::filter(ranking <= n) %>% 
    dplyr::select(grouping, fc_model, ranking) %>% 
    return()
}