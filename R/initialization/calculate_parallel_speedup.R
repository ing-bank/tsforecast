
# TO RECALIBRATE THE run_parallel FUNCTION
# 1: Open tsforecast Project file
# 2: Load package (Ctrl + Shift + L)
# 3: Source this script

library(tidyverse)

calculate_parallel_speedup <- function(n_cores = c(1, 4), fc_methods = supported_fc_methods_uni_var(),
                                        run_nrows = c(1, 100), iterations = 3) {
  # Create dataset to run calculations on
  main_forecasting_table <- datasets::AirPassengers %>% 
    ts_object_to_tibble() %>% 
    dplyr::transmute(
      passengers = x,
      dataset = "AirPassengers"
    ) %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "period",
      col_of_interest = "passengers",
      group_cols = "dataset"
    ) %>% 
    create_main_forecasting_table()
  # Create empty tibble to store the results
  results <- tibble::tibble()
  # Perform grid search
  for (iteration in 1:iterations) {
    for (cores in n_cores) {
      for (fcm in fc_methods) {
        if (fcm == "svm") {
          recursive <- TRUE
        } else if (fcm %in% c("tree", "forest")) {
          recursive <- c(TRUE, FALSE)
        } else {
          recursive <- FALSE
        }
        for (rec in recursive) {
          for (nrows in run_nrows) {
            # Sample some rows
            sample_rows <- sample(
              x = 1:nrow(main_forecasting_table), 
              size = max(nrows, cores),
              replace = T
            )
            # Filter these sampled rows
            temp_fc_table <- main_forecasting_table[sample_rows,]
            # Determine time of running forecasts
            time <- system.time({
              add_fc_models_to_main_forecasting_table(
                main_forecasting_table = temp_fc_table,
                fc_methods = if (rec) c(fcm, "recursive") else fcm,
                parallel = (cores > 1),
                max_cores = cores
              )
            })
            # Combine results of each run
            results <- dplyr::bind_rows(
              results,
              tibble::tibble(
                n_cores = as.character(cores),
                fc_method = fcm,
                recursive = rec,
                parallel = (cores > 1),
                nrows = max(nrows, cores),
                iteration = iteration,
                time = as.numeric(time)[3]
              )
            )
            # Print for status update
            print(tail(results, 1))
          }
        }
      }
    }
  }
  # Return results
  return(results)
}



# Run function to get results to determine initialization time
results_for_initialization <- calculate_parallel_speedup(run_nrows = 1)

# FOR TEMP STORAGE/RECOVERY
if (FALSE) {
  write.csv2(results_for_initialization, "C:/GIT/results_for_initialization.csv", row.names = F)
  results_for_initialization <- read.csv2("C:/GIT/results_for_initialization.csv")
}

# Decide on initialization time
model_for_initialization <- lm(time ~ parallel + fc_method + recursive - 1, data = results_for_initialization)
summary(model_for_initialization)



# Run function to get results to determine time per row and forecast method
results_for_nrows <- calculate_parallel_speedup(run_nrows = 100)

# FOR TEMP STORAGE/RECOVERY
if (FALSE) {
  write.csv2(results_for_nrows, "C:/GIT/results_for_nrows.csv", row.names = F)
  results_for_nrows <- read.csv2("C:/GIT/results_for_nrows.csv")
}

# Decide on runtime per model and nrows for parallel
model_for_nrows_parallel <- lm(time ~ n_cores:fc_method:nrows + recursive:fc_method:nrows - 1, data = results_for_nrows %>% dplyr::filter(parallel == T))
summary(model_for_nrows_parallel)

# Decide on runtime per model and nrows for non-parallel
model_for_nrows_non_parallel <- lm(time ~ fc_method:nrows + recursive:fc_method:nrows - 1, data = results_for_nrows %>% dplyr::filter(parallel == F))
summary(model_for_nrows_non_parallel)



# NOW UPDATE THE decide_on_parallel_run() FUNCTION!


