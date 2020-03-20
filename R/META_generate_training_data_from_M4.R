#' Generate training data
#'
#' \code{generate_training_data_from_M4} is a function to generate training data
#' from the M4 time series forecasting competition data. The M4 data comes from
#' the M4comp2018 package, which contains 100.000 time series from different
#' domains. The available M4 time series are filtered to only the monthly time
#' series, corresponding to Finance data, which are between 5 and 15 years long.
#' The training data is used for training a Feature-based Forecast Model
#' Averaging (FFORMA) model.
#'
#' @param n A positive integer value indicating the number of sample time series
#'   to draw from the total set of M4 times series.
#' @param dir_fforma A character string specifying the path to the directory
#'   where the training data is stored to do FFORMA training.
#' @param verbose Boolean, which is set to TRUE if status updates are valued, or
#'   set to FALSE if they are not.
#'
#' @return Nothing.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#'
#' @examples
#' generate_training_data_from_M4()
generate_training_data_from_M4 <- function(n = 1, dir_fforma = file.path(getwd(),"data/fforma_training_data"), verbose = TRUE) {
  # Check to make sure n is a whole number
  if (!(is.numeric(n) && n == suppressWarnings(as.integer(n)) && n > 0)) {
    message <- paste0("The parameter 'n' should be a positive integer value, instead of '",delta,"' ... ")
    stop(message)
  }
  # Check dir_fforma
  if (!is.character(dir_fforma)) {
    message <- paste0("The parameter 'dir_fforma' should be a character string indicating a valid path to an existing directory ... \n")
    stop(message)
  }
  if (!dir.exists(dir_fforma)) {
    message <- paste0("It seems you do NOT have access to the specified location where the training data for FFORMA is stored:\n", dir_fforma, "\n")
    stop(message)
  }

  # Store only valid time series
  valid_M4 <- list()
  # Loop over each time series
  for (ts in M4comp2018::M4) {
    # Keep only monthly time series
    if (ts$period == "Monthly") {
      # Keep only finance related time series
      if (ts$type == "Finance") {
        # Keep only time series with between 5 to 15 years of data
        ts_length <- (ts$n + 18)
        if (ts_length >= 5*12 & ts_length <= 15*12) {
          valid_M4 <- append(
            x = valid_M4,
            values = list(ts)
          )
        }
      }
    }
  }
  
  # Randomly select subset of the valid_M4
  samples <- sample(1:length(valid_M4), n)
  # Loop over each sample
  for (s in samples) {
    # Get time series data
    sample_M4 <- valid_M4[[s]]
    # Define name of the sample
    sample_name <- paste0(sample_M4$st, "_", "Finance")
    # Only run if this sample does not already exist
    if (any(grepl(toupper(sample_name), list.files(dir_fforma)))) {
      # Message
      if (verbose) cat(paste0("\n",format(Sys.time(), "%H:%M:%S -")," TRAINING DATA FOR ", sample_M4$st, " ALREADY EXISTS! :)\n"))
    } else {
      # Extract time series data
      ts_data <- dplyr::bind_rows(
          ts_object_to_tibble(sample_M4$x),
          ts_object_to_tibble(sample_M4$xx)
        ) %>% 
        dplyr::mutate(
          st = sample_M4$st,
          type = "Finance"
        ) %>% 
        tstools::initialize_ts_forecast_data(
          date_col = "period",
          col_of_interest = "x",
          group_cols = c("st", "type")
        )
      # Message
      if (verbose) cat(paste0("\n",format(Sys.time(), "%H:%M:%S -")," GENERATING TRAINING DATA FOR ", sample_M4$st, "\n"))
      # Add all fc_models
      main_forecasting_table <- create_main_forecasting_table(
          data = ts_data,
          seasonal_periods = NULL,
          min_train_periods = 5 * 12 # At least 5 years
        ) %>% 
        add_fc_models_to_main_forecasting_table(
          periods_ahead = 3 * 12, # Forecast 3 years ahead
          allow_negative_fc = T,
          verbose = verbose,
          parallel = T
        )
      # Message
      if (verbose) cat(paste0(format(Sys.time(), "%H:%M:%S -")," STORING TRAINING DATA FOR ", sample_M4$st, "\n"))
      # Store main_forecasting_table for FFORMA training
      store_main_forecasting_table_for_fforma(
        main_forecasting_table = main_forecasting_table,
        name = sample_name
      )
    }
  }
}