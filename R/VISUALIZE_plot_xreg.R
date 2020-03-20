#' Create plot to compare select external regressors to variable of interest
#'
#' \code{plot_xreg} is a function to create a plot which compares selected
#' external regressors with the variable of interest
#'
#' @param main_forecasting_table A tibble containing a single row per group and
#'   several columns of data required for time series forecasting, which has
#'   been created using the \code{create_main_forecasting_table} function and
#'   which has been extended with the fc_models and fc_errors columns using the
#'   \code{add_fc_models_to_main_forecasting_table} function. Note that this
#'   table should have the output of a multivariate analysis, otherwise the
#'   function does not run.
#' @param xreg A vector that contains strings with the names of the external
#'   regressors to be plotted.
#'
#' @return A plotly object displaying multiple plots.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @import ggplot2
#' @importFrom ggthemes theme_calc
#' @importFrom plotly ggplotly layout
#' @importFrom tstools check_data_format get_plot_colors period_to_last_day
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
#'       periods_ahead = 12,
#'       fc_methods = c("linear")
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'    plot_xreg(
#'       xreg = "spotprice"
#'    )
plot_xreg <- function(main_forecasting_table, xreg = "") {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "plot_xreg",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "fc_errors"
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
  # Check that the xreg_cols are valid
  if (!all(xreg %in% attr(main_forecasting_table$ts_object_train[[1]], "xreg_cols"))) {
    stop("Specified external regressors are not in the main_forecasting_table!")
  }
  # Take out dataset from main_forecasting_table
  valid_data <- main_forecasting_table$ts_object_valid[[1]] %>% 
    ts_object_to_tibble()
  data <- main_forecasting_table$ts_object_train[[1]] %>% 
    ts_object_to_tibble() %>% 
    dplyr::bind_rows(valid_data) %>% 
    dplyr::mutate(period = tstools::period_to_last_day(period))
  
  # Get date when col_of_interest becomes NA
  vertical_line_date <- main_forecasting_table$ts_split_date %>% 
    tstools::period_to_last_day()
  
  # Create plot data
  xreg_select <- c(xreg, "col_of_interest", "period")
  
  plot_data <- data %>%
    dplyr::select(dplyr::matches(paste(xreg_select, collapse = "|")))
  # Get rid of potential variables that got included from name similarities
  plot_data <- plot_data[xreg_select] %>% 
    reshape2::melt(value.name = "original_value", "period") %>%
    dplyr::group_by(variable) %>% 
    # Normalize between 0 and 1
    dplyr::mutate(value = (original_value - min(original_value, na.rm = T))/(max(original_value, na.rm = T) - min(original_value, na.rm = T))) %>% 
    dplyr::ungroup()
  
  # Get number of lines and ING plot colors
  lines <- plot_data %>%
    dplyr::select(variable) %>% 
    dplyr::pull() %>% 
    unique() %>% 
    as.character()
  # Take out col_of_interest in lines
  lines <- lines[!grepl("col_of_interest", lines)]
  # Put back col_of_interest as first argument in lines
  lines <- c("col_of_interest", lines)
  colors <- tstools::get_plot_colors(n_colors = length(lines) - 1)
  colors <- setNames(c("#000000",colors),lines)
  
  # Determine format function
  format_as <- function(x) format(round(x, 2), nsmall = 2, big.mark = ",", scientific = F)
  format_as_axis <- function(x) format(round(x, 3), nsmall = 0, big.mark = ",", scientific = F)
  
  # Create tooltip text
  tooltip_text <- paste0("paste0(
    'Variable: ', variable,
    '<br>Period: ', format.Date(period, '%B %Y'),
    '<br>Normalized Value: ', format_as(value),
    '<br>Original Value: ', format_as(original_value)
  )") 
  # Create the plot
  plot <- suppressWarnings(
    ggplot2::ggplot(plot_data, ggplot2::aes(x = period, y = value, color = variable)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes_string(text = tooltip_text), size = 0.5) +
      ggplot2::geom_vline(xintercept = as.numeric(vertical_line_date), linetype = 3) +
      ggplot2::scale_color_manual(values = colors) + 
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), labels = format_as_axis) +
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