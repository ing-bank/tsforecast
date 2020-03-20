#' Get fitted values of the tree and forest forecast models, post forecast
#'
#' \code{get_fitted_tree_forest_values} is a function that creates fitted values
#' from the tree and forest forecast models. The fitted values vary with respect
#' to one changing external regressor.
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
#' @importFrom tidyr drop_na
#' @importFrom purrr map
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
#'       fc_methods = c("tree", "forest"),
#'       keep_fc_model_objects = T
#'    )
#' fc_models <- names(main_forecasting_table$fc_models[[1]])
#' main_fit_table <- tibble::tibble(
#'    xreg_value = vector(length = 100 * length(fc_models)),
#'    fitted = vector(length = 100 * length(fc_models)),
#'    fc_model = rep(fc_models, each = 100)
#' )
#' get_fitted_tree_forest_values(
#'    main_forecasting_table = main_forecasting_table,
#'    main_fit_table = main_fit_table,
#'    xreg = "spotprice"
#' )
get_fitted_tree_forest_values <- function(main_forecasting_table, main_fit_table, xreg = "") {
  # Set seed to enable reproduction of results
  set.seed(42)
  # Do calculations for ML models
  ML_fit_table <- main_fit_table %>% 
    dplyr::filter((grepl("forest", fc_model) | grepl("rpart", fc_model) | grepl("ctree", fc_model)) & !grepl("_rec_", fc_model))
  # Get ML_train_data
  ML_model <- sample(unique(ML_fit_table$fc_model), 1)
  ML_train_data <- main_forecasting_table$fc_models[[1]][[ML_model]]$ML_train_data
  # Extract min and max xreg values
  xreg_values <- ML_train_data %>% 
    dplyr::filter(!is.na(!! dplyr::sym(xreg))) %>% 
    dplyr::pull(xreg)
  xreg_min <- min(xreg_values)
  xreg_max <- max(xreg_values)
  # Extract granularity
  granularity <- main_fit_table %>% 
    dplyr::filter(fc_model == ML_model) %>% 
    nrow()
  # Initialize xreg vector and predicted fc_vector 
  ML_fit_table <- ML_fit_table %>% 
    dplyr::group_by(fc_model) %>% 
    dplyr::mutate(xreg_value = seq(from = xreg_min, to = xreg_max, length.out = granularity)) %>% 
    dplyr::ungroup()
  # Loop over each forecast model
  for (select_model in unique(ML_fit_table$fc_model)) {
    # Update ML_train_data from ML model objects
    ML_train_data <- main_forecasting_table$fc_models[[1]][[select_model]]$ML_train_data
    # Create sample data, using mean of dataset
    sample_data <- ML_train_data %>% 
      dplyr::select(-col_of_interest) %>% 
      select_if(~ is.factor(.) == FALSE) %>% 
      tidyr::drop_na() %>% 
      dplyr::summarise_all(mean)
    # For factors, use a random row
    sample_data <- ML_train_data %>% 
      dplyr::select_if(is.factor) %>% 
      tidyr::drop_na() %>% 
      dplyr::sample_n(1) %>% 
      dplyr::bind_cols(
        sample_data,
        .
      )
    # Make sure sample_data columns have the same class as ML_train_data for ctree
    if (grepl("ctree",select_model)) {
      # Get integer columns
      integer_cols <- ML_train_data %>%
        dplyr::select_if(is.integer) %>% 
        colnames()
      # Define dplyr::quosures to reset integer class
      reset_integer_class <- setNames(
        object = purrr::map(
          .x = dplyr::syms(integer_cols), 
          .f = ~dplyr::quo(as.integer(!!.x))
        ),
        nm = integer_cols
      )
      # Apply dplyr::quosures
      sample_data <- sample_data %>% 
        dplyr::mutate(!!! reset_integer_class)
    }
    # Increase sample_data rows
    sample_data <- sample_data %>% 
      dplyr::slice(rep(1:n(), each = granularity)) %>%
      dplyr::mutate(!! dplyr::sym(xreg) := ML_fit_table$xreg_value[1:granularity])
    # Get fitted values
    fitted_vector <- predict(main_forecasting_table$fc_models[[1]][[select_model]]$model, sample_data)
    last_value <- main_forecasting_table$ts_object_train[[1]][,"col_of_interest"] %>% 
      tail(1) %>% 
      as.numeric()
    fitted_vector <- last_value + as.vector(fitted_vector)
    # Using the new ML_fit_table, get fitted values
    ML_fit_table <- ML_fit_table %>% 
      dplyr::group_by(fc_model) %>% 
      dplyr::mutate(
        fitted = dplyr::case_when(
          fc_model == select_model ~ fitted_vector,
          TRUE ~ as.numeric(fitted)
        )
      ) %>% 
      dplyr::ungroup()
  }
  return(ML_fit_table)
}