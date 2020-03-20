
context("decompose_ts_object_for_ML")

test_that("check decompose_ts_object_for_ML with all valid input combinations", {
  xregs <- list("spotprice", "gemprice", c("spotprice", "gemprice"), NULL)
  stationary <- c(TRUE, FALSE)
  features <- c(TRUE, FALSE)
  xreg_deltas <- c(TRUE, FALSE)
  for (xreg_cols in xregs) {
    for (filter_stationary in stationary) {
      for (filter_date_features in features) {
        for (add_xreg_deltas in xreg_deltas) {
          function_input <- tstools::initialize_ts_forecast_data(
            data = dummy_gasprice,
            date_col = "year_month",
            col_of_interest = "gasprice",
            group_cols = c("state", "oil_company"),
            xreg_cols = xreg_cols
          ) %>% 
            dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
            tstools::transform_data_to_ts_object()
          function_output <- decompose_ts_object_for_ML(
            ts_object = function_input,
            filter_stationary = filter_stationary,
            filter_date_features = filter_date_features,
            add_xreg_deltas = add_xreg_deltas
          )
          expect_equal(
            object = class(function_output),
            expected = c("tbl_df", "tbl", "data.frame")
          )
          expect_equal(
            object = nrow(function_output),
            expected = 191
          )
          if (filter_stationary) {
            if (filter_date_features) {
              # Both TRUE
              expect_equal(
                object = ncol(function_output),
                expected = 23 + (length(xreg_cols) * (1 + add_xreg_deltas*12))
              )
            } else {
              # features FALSE, stationary TRUE
              expect_equal(
                object = ncol(function_output),
                expected = 24 + (length(xreg_cols) * (1 + add_xreg_deltas*12))
              )
            }
          } else {
            if (filter_date_features) {
              # features TRUE, stationary FALSE
              expect_equal(
                object = ncol(function_output),
                expected = 29 + (length(xreg_cols) * (1 + add_xreg_deltas*12))
              )
            } else {
              # Both FALSE
              expect_equal(
                object = ncol(function_output),
                expected = 30 + (length(xreg_cols) * (1 + add_xreg_deltas*12))
              )
            }
          }
        }
      }
    }
  }
})

test_that("check decompose_ts_object_for_ML with invalid data input", {
  stationary <- c(TRUE, FALSE)
  features <- c(TRUE, FALSE)
  xreg_deltas <- c(TRUE, FALSE)
  for (filter_stationary in stationary) {
    for (filter_date_features in features) {
      for (add_xreg_deltas in xreg_deltas) {
        expect_error(
          decompose_ts_object_for_ML(
            ts_object = dummy_gasprice,
            filter_stationary = filter_stationary,
            filter_date_features = filter_date_features,
            add_xreg_deltas = add_xreg_deltas
          )
        )
      }
    }
  }
})
