% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/META_apply_fforma_model.R
\name{apply_fforma_model}
\alias{apply_fforma_model}
\title{Apply the FFORMA model}
\usage{
apply_fforma_model(
  fc_models,
  ts_object_train,
  verbose = FALSE,
  log_message = ""
)
}
\arguments{
\item{ts_object_train}{A time series object, which contains only the training
data.}

\item{fc_errors}{A tibble with data on the fc errors for all the forecast
models in fc_models.}
}
\value{
A tibble with data on the fc errors for all the forecast models in
  fc_models, which has been extended with the FFORMA meta model forecasts and
  fc errors.
}
\description{
\code{apply_fforma_model} is a function to apply the FFORMA meta model (which
has been trained using the \code{train_fforma_model} function) to a set of
model forecasts comming from a single row of the main_forecasting_table.
}
\examples{
ts_object_train <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
   ) \%>\% 
   dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") \%>\% 
   tstools::transform_data_to_ts_object()
fc_models <- add_all_univariate_forecast_models(
   ts_object_train = ts_object_train,
   periods_ahead = 12,
   verbose = T
)
apply_fforma_model(
   ts_object_train = ts_object_train,
   fc_models = fc_models
)
}
