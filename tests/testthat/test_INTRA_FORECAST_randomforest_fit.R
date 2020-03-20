
context("randomforest_fit")

test_that("check randomforest_fit with valid inputs", {
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
      dplyr::filter(!is.na(col_of_interest)) %>% 
      tidyr::drop_na()
    function_output <- randomforest_fit(
      ML_data = function_input,
      mtry = 8,
      nodesize = 5,
      ntree = 500
    )
    expect_equal(class(function_output), "numeric")
    expect_true(function_output >= 35)
    expect_true(function_output <= 55)
  }
})

test_that("check randomforest_fit with invalid parameter inputs", {
  xregs <- list("spotprice", "gemprice", c("spotprice", "gemprice"), NULL)
  param.mtry <- c(8)
  param.nodesize <- list("5", 5)
  param.ntree <- c(-500, 500)
  for (xreg_cols in xregs) {
    for (mtry in param.mtry) {
      for (nodesize in param.nodesize) {
        for (ntree in param.ntree) {
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
            dplyr::filter(!is.na(col_of_interest)) %>% 
            tidyr::drop_na()
          if (!(mtry == 8 & nodesize == 5 & ntree == 500)) {
            expect_error(
              randomforest_fit(
                ML_data = function_input,
                mtry = mtry,
                nodesize = nodesize,
                ntree = ntree
              )
            )
          }
        }
      }
    }
  }
})
