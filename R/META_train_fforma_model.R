#' Train the FFORMA model
#'
#' \code{train_fforma_model} is a function to train a Feature-based Forecast
#' Model Averaging (FFORMA) model, which weights the forecasts from the
#' different available forecast models within the tsforecast package to
#' create a new ensamble forecast. The FFORMA model uses the characteristics
#' from the time series objects to predict which forecast model gives the best
#' forecast performance. After training on all available training sets within
#' the specified directory, the resulting FFORMA model is stored within the
#' package to be reused at runtime during time series forecasting.
#'
#' @param dir_fforma A character string specifying the path to the directory
#'   where the training data os stored to do FFORMA training.
#' @param verbose Boolean, which is set to TRUE if status updates are valued, or
#'   set to FALSE if they are not.
#'
#' @return A FFORMA meta learning model, trained according to the approach
#'   descibed in \href{https://robjhyndman.com/papers/fforma.pdf}{this paper}
#'   and
#'   \href{https://github.com/robjhyndman/M4metalearning/blob/master/docs/metalearning_example.md}{this
#'    example} on github.
#'
#' @importFrom magrittr '%>%'
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na
#' @import dplyr
#' @import caret
#' @import gbm
#' @importFrom usethis use_data
#' @importFrom tstools check_data_format
#'
#' @examples
#' train_fforma_model()
train_fforma_model <- function(dir_fforma = file.path(getwd(),"data/fforma_training_data"), verbose = TRUE) {
  # Check dir_fforma
  if (!is.character(dir_fforma)) {
    message <- paste0("The parameter 'dir_fforma' should be a character string indicating a valid path to an existing directory ... \n")
    stop(message)
  }
  if (!dir.exists(dir_fforma)) {
    message <- paste0("It seems you do NOT have access to the specified location where the training data for FFORMA is stored:\n", dir_fforma, "\n")
    stop(message)
  }
  # Get a list of all available training data
  files <- list.files(
    path = dir_fforma,
    pattern = "training_data_",
    full.names = T
  )
  # Create empty results table
  training_data <- tibble::tibble()
  # Loop over available files
  for (file in files) {
    # Message
    if (verbose) cat(paste0("Loading ", basename(file), "\n"))
    # Load data
    temp_data <- readRDS(file)
    # Check temp_data
    tstools::check_data_format(
      data = temp_data,
      func_name = "train_fforma_model",
      req_cols = c(
        "grouping", "ts_object_train", "best_fc_model", "ts_split_date"
      )
    )
    # Filter out 0's and sparse non-zero ts objects from temp_data
    empty_rows <- c()
    for (i in 1:nrow(temp_data)) {
      # Extract col of interest
      col_of_interest <- temp_data$ts_object_train[[i]] %>%
        ts_object_to_tibble() %>%
        dplyr::pull(col_of_interest)
      # Check whether there are sufficient number of non-zero values
      if (sum(col_of_interest != 0) < 36) {
        empty_rows <- c(i, empty_rows)
      }
    }
    # Take out the rows with insufficient data
    temp_data <- temp_data %>%
      dplyr::mutate(n_row = 1:n()) %>%
      dplyr::filter(!n_row %in% empty_rows) %>%
      dplyr::select(-n_row)
    # Add ts features
    temp_data <- temp_data %>%
     dplyr::select(grouping, ts_object_train, ts_split_date, best_fc_model) %>%
      dplyr::group_by(grouping, ts_split_date) %>%
      dplyr::mutate(
        features = purrr::pmap(
          .f = get_ts_features,
          .l = list(
            "ts_object" = ts_object_train
          )
        )
      ) %>%
      dplyr::ungroup() %>%
      # Unnest the features
      tidyr::unnest(features)
    # Add to training_data
    training_data <- dplyr::bind_rows(
      training_data,
      temp_data
    )
  }
  # Set seed for reproducibility
  set.seed(42)
  # Define fit control
  fit_control <- caret::trainControl(
    method = "repeatedcv",
    number = 2,
    repeats = 3
  )
  # Message
  if (verbose) cat("\nStarting Training:\n")
  # Train the model
  fforma_model <- caret::train(
    best_fc_model ~ ., 
    data = training_data %>% 
      dplyr::select(-grouping, -ts_object_train, -ts_split_date) %>% 
      tidyr::drop_na() %>% 
      filter_stationary_columns(),
    method = "gbm", 
    trControl = fit_control,
    na.action = na.pass,
    verbose = verbose
  )
  # Message
  if (verbose) cat("Finished Training! :)\n\n")
  # Display feature importance
  if (verbose) {
    fforma_model %>% 
      summary(plotit = F) %>% 
      dplyr::transmute(
        feature = factor(var, levels = rev(unique(var)), ordered = T),
        importance = rel.inf
      ) %>% 
      ggplot2::ggplot(ggplot2::aes(x = feature, y = importance)) +
      ggplot2::geom_bar(stat = 'identity') +
        ggplot2::coord_flip() +
        ggthemes::theme_calc()
    
  }
  # Store the model in the /data folder of the package
  usethis::use_data(fforma_model, overwrite = T)
}