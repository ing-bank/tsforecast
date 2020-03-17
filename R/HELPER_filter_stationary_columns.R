#' Filter stationary columns from the data
#'
#' \code{filter_stationary_columns} is a function to filter columns which have
#' only a single unique value from the data. This is done mostly before
#' modelling, because these stationary columns contain no information that can
#' be used to create a model.
#'
#' @param data A data.table, data.frame or tibble object containing the data to
#'   be used for modelling, from which the stationary columns need to be
#'   filtered.
#'
#' @return A tibble with only non-stationary columns.
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools check_data_format
#' @import dplyr
#'
#' @examples
#' dummy_gasprice %>% 
#'    filter_stationary_columns() %>% 
#'    colnames()
#' dummy_gasprice %>% 
#'    dplyr::filter(state == "New York") %>% 
#'    filter_stationary_columns() %>% 
#'    colnames()
filter_stationary_columns <- function(data) {
  # Check data
  tstools::check_data_format(
    data = data,
    func_name = "filter_stationary_columns"
  )
  # Keep only non-stationary columns
  cols_to_remove <- data %>% 
    # Make sure the data is not grouped
    dplyr::ungroup() %>% 
    # Select only columns with more than one unique value
    dplyr::select_if(function(x) dplyr::n_distinct(x) <= 1) %>% 
    # Get column names
    colnames()
  # Never remove column of interest
  cols_to_remove <- cols_to_remove[cols_to_remove != "col_of_interest"]
  # Remove stationary columns and return
  data %>% 
    dplyr::select(-cols_to_remove) %>% 
    return()
}