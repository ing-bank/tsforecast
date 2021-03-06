% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/HELPER_get_actuals_from_main_forecasting_table.R
\name{get_actuals_from_main_forecasting_table}
\alias{get_actuals_from_main_forecasting_table}
\title{Get actuals from main_forecasting_table.}
\usage{
get_actuals_from_main_forecasting_table(main_forecasting_table, for_plot = F)
}
\arguments{
\item{main_forecasting_table}{A tibble containing several columns of data
required for time series forecasting, which has been created using the
\code{create_main_forecasting_table} function.}

\item{for_plot}{Boolean, which is to be set to TRUE if specific
transformations should be performed to make the data ready for one of the
plotting functions available within the package, or set to FALSE if these
additional transformation should not be applied.}
}
\value{
A tibble containing the actual values of the column of interest per
  period.
}
\description{
\code{get_actuals_from_main_forecasting_table} is a function to extract the
actuals from the main forecasting table, e.g. to be used for plotting.
}
\examples{
main_forecasting_table <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
   ) \%>\%
   create_main_forecasting_table(
      data = data,
      seasonal_periods = c(12,3)
   )
get_actuals_from_main_forecasting_table(main_forecasting_table)
}
