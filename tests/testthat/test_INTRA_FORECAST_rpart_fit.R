
context("rpart_fit")

test_that("check rpart_fit with valid inputs", {
  xregs <- list("spotprice", "gemprice", c("spotprice", "gemprice"), NULL)
  for (xreg_cols in xregs) {
    function_input <- tstools::initialize_ts_forecast_data(
        data = dummy_gasprice,
        date_col = "year_month",
        col_of_interest = "gasprice",
        group_cols = c("state", "oil_company"),
        xreg_cols = xreg_cols
      ) %>% 
      dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
      tstools::transform_data_to_ts_object() %>% 
      decompose_ts_object_for_ML() %>% 
      dplyr::mutate(col_of_interest = col_of_interest - dplyr::lag(col_of_interest)) %>% 
      dplyr::filter(!is.na(col_of_interest))
    function_output <- rpart_fit(
      ML_data = function_input,
      minsplit = 20,
      maxdepth = 30,
      cp = 0.1,
      xval = 10
    )
    expect_equal(class(function_output), "numeric")
    expect_equal(round(function_output, 0), 100)
  }
})

test_that("check rpart_fit with invalid RPART parameter inputs", {
  xregs <- list("spotprice", "gemprice", c("spotprice", "gemprice"), NULL)
  param.minsplit <- list("10", 10)
  param.maxdepth <- c(-1, 15)
  param.cp <- list("0.002", 0.002)
  for (xreg_cols in xregs) {
    for (minsplit in param.minsplit) {
      for (maxdepth in param.maxdepth) {
        for (cp in param.cp) {
          function_input <- tstools::initialize_ts_forecast_data(
              data = dummy_gasprice,
              date_col = "year_month",
              col_of_interest = "gasprice",
              group_cols = c("state", "oil_company"),
              xreg_cols = xreg_cols
            ) %>% 
            dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
            tstools::transform_data_to_ts_object() %>% 
            decompose_ts_object_for_ML() %>% 
            dplyr::mutate(col_of_interest = col_of_interest - dplyr::lag(col_of_interest)) %>% 
            dplyr::filter(!is.na(col_of_interest))
          if (!(minsplit == 10 & maxdepth == 15 & cp == 0.002)) {
            expect_error(
              rpart_fit(
                ML_data = function_input,
                minsplit = minsplit,
                maxdepth = maxdepth,
                cp = cp,
                xval = 10
              )
            ) 
          }
        }
      }
    }
  }
})
