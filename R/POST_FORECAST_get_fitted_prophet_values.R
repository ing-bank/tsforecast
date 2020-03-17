#' Get fitted values of the prophet forecast models, post forecast
#'
#' \code{get_fitted_prophet_values} is a function that creates fitted values
#' from the prophet forecast model. The fitted values vary with respect to one
#' changing external regressor.
#'
#' @param main_forecasting_table A tibble containing a single row per group and
#'   several columns of data required for time series forecasting, which has
#'   been created using the \code{create_main_forecasting_table} function and
#'   which has been extended with the fc_models and fc_errors columns using the
#'   \code{add_fc_models_to_main_forecasting_table} function. Note that this
#'   table should have the output of a multivariate analysis.
#' @param main_fit_table A tibble containing information about the specific
#'   fc_model and external regressor values to be used as inputs
#' @param xreg A character that contains a string with the name of the external
#'   regressor to be plotted.
#'
#' @return A vector of fitted values
#'
#' @importFrom magrittr '%>%'
#' @importFrom prophet make_future_dataframe
#' @import dplyr
#'
#' @examples
#' main_forecasting_table <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company"),
#'       xreg_cols = c("spotprice", "gemprice")
#'    ) %>%
#'    create_main_forecasting_table() %>%
#'    dplyr::filter(ts_split_date == 200503) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'    add_fc_models_to_main_forecasting_table(
#'       periods_ahead = 12,
#'       fc_methods = "prophet",
#'       keep_fc_model_objects = T
#'    )
#' fc_models <- names(main_forecasting_table$fc_models[[1]])
#' main_fit_table <- tibble::tibble(
#'    xreg_value = vector(length = 100 * length(fc_models)),
#'    fitted = vector(length = 100 * length(fc_models)),
#'    fc_model = rep(fc_models, each = 100)
#' )
#' get_fitted_prophet_values(
#'    main_forecasting_table = main_forecasting_table,
#'    main_fit_table = main_fit_table,
#'    xreg = "spotprice"
#' )
get_fitted_prophet_values <- function(main_forecasting_table, main_fit_table, xreg = "") {
  # Extract min and max xreg values
  xreg_values <- main_forecasting_table$ts_object_train[[1]] %>% 
    ts_object_to_tibble() %>% 
    dplyr::pull(xreg)
  xreg_min <- min(xreg_values)
  xreg_max <- max(xreg_values)
  # Get granularity
  granularity <- nrow(main_fit_table)/length(unique(main_fit_table$fc_model))
  # Plug in vector of xreg candidates to predict on
  prophet_fit_table <- main_fit_table %>%
    dplyr::filter(grepl("prophet", fc_model)) %>% 
    dplyr::group_by(fc_model) %>% 
    dplyr::mutate(xreg_value = seq(from = xreg_min, to = xreg_max, length.out = granularity)) %>% 
    dplyr::ungroup()
  # Loop over each forecast model
  for (fc_model_select in unique(prophet_fit_table$fc_model)) {
    # Get prophet model object
    prophet_model <- main_forecasting_table$fc_models[[1]][[fc_model_select]]$model
    # Create fc_data for prophet
    prophet_fc_data <- prophet::make_future_dataframe(
      m = prophet_model, 
      periods = 1, 
      freq = "month",
      include_history = F
    )
    fit_data <- main_forecasting_table$ts_object_train[[1]] %>% 
      ts_object_to_tibble() %>% 
      dplyr::select(attr(main_forecasting_table$ts_object_train[[1]], "xreg_cols")) %>% 
      dplyr::summarise_all(mean) %>% 
      dplyr::bind_cols(
        x = prophet_fc_data,
        y = .
      ) %>% 
      dplyr::slice(rep(1:n(), each = granularity)) %>% 
      dplyr::mutate(!! dplyr::sym(xreg) := prophet_fit_table$xreg_value[1:granularity])
    # Get fitted value
    fc_value <- predict(prophet_model, fit_data)
    fc_value <- as.numeric(fc_value$yhat)
    # Put into fitted table
    prophet_fit_table <- prophet_fit_table %>% 
      dplyr::group_by(fc_model) %>% 
      dplyr::mutate(fitted = dplyr::case_when(
        fc_model == fc_model_select ~ fc_value,
        TRUE ~ as.numeric(fitted)
      )) %>% 
      dplyr::ungroup()
  }
  return(prophet_fit_table)
}