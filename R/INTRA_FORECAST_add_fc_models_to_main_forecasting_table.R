#' Add fc models to main forecasting table
#'
#' \code{add_fc_models_to_main_forecasting_table} is a function to extend the
#' main forecasting table with an additional column, called fc_models, in which
#' all the specified forecast methods are stored after they have been run. The
#' forecast methods are run for each row of parameters and time series data in
#' the main forecasting table. The resulting fc_models column consists of a
#' named lists with forecast models and forecast data.
#'
#' @param main_forecasting_table A tibble containing several columns of data
#'   required for time series forecasting, which has been created using the
#'   \code{create_main_forecasting_table} function.
#' @param periods_ahead A positive integer value indicating the number of
#'   periods to forecast ahead.
#' @param fc_methods A character vector specifying the forecast methods to add.
#'   For more info \code{`?supported_fc_methods`}.
#' @param overwrite_fc Boolean, which is set to TRUE if all existing forecasts
#'   should be overwritten, or set to FALSE if only unavailable forecasts should
#'   be added. Needs to be set to true if the @param main_forecasting_table
#'   already has forecasts of different @param periods_ahead.
#' @param add_fc_errors Boolean, which is set to TRUE if
#'   \code{add_fc_errors_to_main_forecasting_table} should be run to add the
#'   fc_errors column as part of this function, or set to FALSE if it is not to
#'   be run (but to possibly be run separately later on).
#' @param allow_negative_fc Boolean, which is to be set to TRUE if negative
#'   forecast values are allowed, or set to FALSE if negative forecast values
#'   should be overwritten by a value of zero.
#' @param keep_fc_model_objects Boolean, which is set to TRUE in order to keep
#'   original fc_model objects in the main_forecasting_table after running the
#'   forecast. This is needed for scenario analysis in multivariate forecasting.
#'   However, it may lead to memory issues, as the main_forecasting_table
#'   increases in size.
#' @param verbose Boolean, which is set to TRUE if status updates are valued, or
#'   set to FALSE if they are not.
#' @param parallel Boolean, which is set to TRUE if each row of the
#'   main_forecast_table should be ran in parallel (using #logical cores - 1),
#'   or set to FALSE if they are not.
#' @param max_cores A positive integer value indicating the maximum number of
#'   cores to use for running the forecasts in parallel.
#'
#' @return A tibble containing several columns of data required for time series
#'   forecasting, extended with as an additional column, called fc_models, with
#'   named lists with forecast models and forecast data.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom purrr pmap
#' @importFrom tstools check_data_format unlist_if_required
#'   get_browser_to_use_for_shiny
#' @import dplyr
#' @importFrom parallel detectCores clusterEvalQ clusterMap
#' @import ParallelLogger
#'
#' @examples
#' data <- tstools::initialize_ts_forecast_data(
#'    data = dummy_gasprice,
#'    date_col = "year_month",
#'    col_of_interest = "gasprice",
#'    group_cols = c("state", "oil_company")
#' )
#' main_forecasting_table <- create_main_forecasting_table(
#'       data = data,
#'       seasonal_periods = c(12,3)
#'    ) %>%
#'    head(15)
#' main_forecasting_table <- add_fc_models_to_main_forecasting_table(
#'    main_forecasting_table,
#'    periods_ahead = 12,
#'    fc_methods = c("basic", "tree", "forest", "prophet")
#' )
add_fc_models_to_main_forecasting_table <- function(main_forecasting_table, periods_ahead = 12,
                                                    fc_methods = supported_fc_methods_uni_var(),
                                                    overwrite_fc = FALSE, add_fc_errors = TRUE, 
                                                    allow_negative_fc = FALSE, 
                                                    keep_fc_model_objects = FALSE, verbose = FALSE, 
                                                    parallel = TRUE, max_cores = Inf) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "add_fc_models_to_main_forecasting_table",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid"
    )
  )
  # Check to make sure periods_ahead is a non-negative whole number
  if (!(is.numeric(periods_ahead) & periods_ahead > 0 & periods_ahead == suppressWarnings(as.integer(periods_ahead)))) {
    message <- paste0("The parameter 'periods_ahead' should be a positive integer value, instead of '",periods_ahead,"' ... ")
    stop(message)
  }
  # Check specified forecast methods
  invalid_fc_methods <- fc_methods[!fc_methods %in% supported_fc_methods_uni_var()]
  if (length(invalid_fc_methods) > 0) {
    message <- paste0("The following specified fc_methods are not valid:\n", paste0("\t", invalid_fc_methods, collapse = "\n"))
    stop(message)
  }
  fc_methods <- match.arg(fc_methods, several.ok = T)
  # Check if forecasts exists in main_forecast_table, periods ahead is different from the previous forecasts and overwrite_fc is false.
  if ("periods_ahead" %in% names(attributes(main_forecasting_table))) {
    prev_periods_ahead <- attributes(main_forecasting_table)$periods_ahead
    if ((overwrite_fc == FALSE) & (prev_periods_ahead != periods_ahead)) {
      stop("The main_forecasing_table has forecasts with different periods_ahead, overwrite_fc needs to be set to TRUE")
    }
  }
  
  # Define log_file path
  log_file <- file.path(tempdir(), paste0(format(Sys.time(),"%Y%m%d_h%Hm%Ms%S"),"_forecasting_log.txt"))
  # Delete log_file with contents of possible previous run
  unlink(log_file)
  # Delete previous attached loggers
  ParallelLogger::clearLoggers()
  # Init default file logger in current dir and console logger
  ParallelLogger::addDefaultFileLogger(log_file)
  ParallelLogger::addDefaultConsoleLogger()
  
  # Determine if univariate or multivariate
  n_vars <- main_forecasting_table$ts_object_train[1] %>% 
    tstools::unlist_if_required() %>% 
    attr("xreg_cols") %>% 
    length()
  model_type <- ifelse(n_vars == 0, "univariate", "multivariate")
  
  # Initialize fc_models column in main_forecasting_table
  if (overwrite_fc | !("fc_models" %in% colnames(main_forecasting_table))) {
    main_forecasting_table <- main_forecasting_table %>% 
      dplyr::mutate(fc_models = list(list()))
  }
    
  # Prepare generic forecast data
  fc_data <- main_forecasting_table %>%
    dplyr::transmute(
      grouping,
      ts_split_date,
      ts_object_train,
      ts_object_valid,
      fc_models,
      periods_ahead = periods_ahead,
      periods_history = train_length,
      fc_methods = list(fc_methods),
      keep_fc_model_objects = keep_fc_model_objects,
      verbose = verbose
    ) %>% 
    dplyr::group_by(grouping) %>% 
    dplyr::mutate(
      keep_fc_model_objects = (keep_fc_model_objects & (ts_split_date == max(ts_split_date)))
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-grouping, -ts_split_date)
  
  # In case of univariate models
  if (model_type == "univariate") {
    # Remove irrelevant parameters if univariate
    fc_data <- fc_data %>% 
      dplyr::select(-ts_object_valid, -keep_fc_model_objects)
    # Specify forecast function to use
    fc_function <- add_all_univariate_forecast_models
  }
    
  # In case of multivariate models
  if (model_type == "multivariate") {
    # Filter non-multivariate fc_methods
    multivariate_fc_methods <- fc_methods[fc_methods %in% supported_fc_methods_multi_var()]
    if (length(multivariate_fc_methods) == 0) stop(paste0("None of the specified fc_methods (",paste0(fc_methods, collapse = ", "),") is valid for multivariate forecasting ..."))
    # Overwrite fc_methods with only multivariate ones
    fc_data <- fc_data %>% 
      dplyr::mutate(
        fc_methods = list(multivariate_fc_methods)
      )
    # Specify forecast function to use
    fc_function <- add_all_multivariate_forecast_models
  }
  
  # Check if parallel would be faster
  if (parallel) {
    # Check if parallel will be faster
    parallel <- decide_on_parallel_run(
      fc_methods = fc_methods,
      nrows = nrow(main_forecasting_table)
    )
    # Create and show message
    if (verbose) {
      if (parallel) {
        message <- paste(format(Sys.time(), "%H:%M:%S -"),"PARALLEL FORECAST LOGGING HAS STARTED")
      } else {
        message <- paste(format(Sys.time(), "%H:%M:%S -"),"PREFERENCE FOR PARALLEL FORECAST RUN IS IGNORED BECAUSE IT'S NOT EXPECTED TO BE FASTER")
      }
      ParallelLogger::logInfo(message)
    }
  }
  
  # If execution in parallel is required
  if (parallel) {
    # Enable logging if required
    if (verbose) {
      # Open log file in terminal
      if (Sys.info()[['sysname']] == "Windows") {
        termId <- rstudioapi::terminalExecute(
          command = paste0('powershell -command "& {Get-Content ', log_file,' -Wait}"')
        )
      }
      # Specify setting to lower level functions
      fc_data <- fc_data %>%
        dplyr::mutate(parallel = parallel)
    }
    # Use tryCatch to make sure cluster is always stopped
    tryCatch(
      expr = {
        # Create forecast cluster using number of available cores minus one
        fc_cluster <- ParallelLogger::makeCluster(min((parallel::detectCores() - 1), max_cores))
        # Load required library in each cluster when using prophet
        if ("prophet" %in% fc_methods) {
          # Needs to be loaded, because of issue https://github.com/facebook/prophet/issues/285
          parallel::clusterEvalQ(
            cl = fc_cluster,
            expr = {
              library(rstan)
            }
          )
        }
        
        # Run forecast function
        new_main_forecasting_table <- fc_data %>% 
          dplyr::mutate(
            new_fc_models = purrr::lift(
              ..f = parallel::clusterMap, 
              cl = fc_cluster
            )(
              .x = .,
              fun = fc_function
            )
          )
      },
      finally = {
        # Show error message
        if (!exists("new_main_forecasting_table")) {
          ParallelLogger::logError(paste0(bgRed("\nERROR"), " for running forecast methods in ", bold("parallel:\n")))
          ParallelLogger::logError(paste0("\t", fc_cluster, collapse = "\n"))
        }
        # Always end with stopping the cluster ...
        ParallelLogger::stopCluster(fc_cluster)
        # Finalize logging if verbose
        if (verbose) {
          message <- paste(format(Sys.time(), "%H:%M:%S -"),"PARALLEL FORECAST LOGGING HAS FINISHED")
          ParallelLogger::logInfo(message)
        }
      }
    )
    
  # Otherwise, non-parallel execution is used
  } else {
    # Run forecast function
    new_main_forecasting_table <- fc_data %>% 
      dplyr::mutate(
        new_fc_models = purrr::pmap(
          .l = .,
          .f = fc_function
        )
      )
  }
    
  # Add new forecast models to main_forecasting_table
  main_forecasting_table <- dplyr::bind_cols(
      main_forecasting_table %>% 
        dplyr::select(-fc_models),
      new_main_forecasting_table %>% 
        dplyr::select(fc_models = new_fc_models)
    )
    
  # Add fc_errors column if required
  if (add_fc_errors) {
    main_forecasting_table <- add_fc_errors_to_main_forecasting_table(
      main_forecasting_table = main_forecasting_table,
      allow_negative_fc = allow_negative_fc
    )
  }

  # Set attributes of main_forecasting_table
  attr(main_forecasting_table, "periods_ahead") <- periods_ahead
  
  # Kill terminal (if running)
  if (exists("termId")) {
    rstudioapi::terminalKill(termId)
  }
  # Launch code to start log viewer
  if (verbose) {
    if (Sys.info()[["sysname"]] == "Windows") {
      log_file <- normalizePath(log_file, winslash = "/")
      browser_path <- normalizePath(tstools::get_browser_to_use_for_shiny(), winslash = "/")
    } else {
      # For Linux
      browser_path <- tstools::get_browser_to_use_for_shiny()
    }
    start_log_viewer_command <- paste0("R -e \"options(shiny.launch.browser=TRUE, browser='", browser_path, "');ParallelLogger::launchLogViewer('", log_file, "')\"")
    rstudioapi::terminalExecute(
      command = start_log_viewer_command
    )
  }
  
  # Return results
  return(main_forecasting_table)
}