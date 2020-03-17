
context("ctree_fit")

test_that("check ctree_fit with valid inputs", {
  xregs <- list("spotprice", "gemprice", c("spotprice", "gemprice"), NULL)
  param.testtype <- c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")
  param.teststat <- c("quad", "max")
  for (xreg_cols in xregs) {
    for (testtype in param.testtype) {
      for (teststat in param.teststat) {
        function_input <- tstools::initialize_ts_forecast_data(
            data = dummy_gasprice,
            date_col = "year_month",
            col_of_interest = "gasprice",
            group_cols = c("state", "oil_company"),
            xreg_cols = xreg_cols
          ) %>% 
          dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
          tstools::transform_data_to_ts_object() %>% 
          decompose_ts_object_for_ML(filter_date_features = T) %>% 
          dplyr::mutate(col_of_interest = col_of_interest - dplyr::lag(col_of_interest)) %>% 
          dplyr::filter(!is.na(col_of_interest)) %>% 
          tidyr::drop_na()
        function_output <- ctree_fit(
          ML_data = function_input,
          minsplit = 20,
          mincriterion = 0.95,
          minbucket = 7,
          testtype = testtype,
          teststat = teststat,
          nresample = 9999
        )
        expect_equal(class(function_output), "numeric")
        expect_true(function_output >= 70)
        expect_true(function_output <= 110)
      }
    }
  }
})

test_that("check ctree_fit with invalid CTREE parameter inputs", {
  xregs <- list("spotprice", "gemprice", c("spotprice", "gemprice"), NULL)
  param.minsplit <- c(-10, 10)
  param.mincriterion <- c(-1.5, 0.975)
  param.minbucket <- c(-3, 5)
  param.testtype <- c("Bonferroni", "Bunny Rabbits")
  param.teststat <- c("quad", "quadcore")
  param.nresample <- c(-99, 9999)
  for (xreg_cols in xregs) {
    for (minsplit in param.minsplit) {
      for (mincriterion in param.mincriterion) {
        for (minbucket in param.minbucket) {
          for (testtype in param.testtype) {
            for (teststat in param.teststat) {
              for (nresample in param.nresample) {
                function_input <- tstools::initialize_ts_forecast_data(
                    data = dummy_gasprice,
                    date_col = "year_month",
                    col_of_interest = "gasprice",
                    group_cols = c("state", "oil_company"),
                    xreg_cols = xreg_cols
                  ) %>% 
                  dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
                  tstools::transform_data_to_ts_object() %>% 
                  decompose_ts_object_for_ML(filter_date_features = T) %>% 
                  dplyr::mutate(col_of_interest = col_of_interest - dplyr::lag(col_of_interest)) %>% 
                  dplyr::filter(!is.na(col_of_interest)) %>% 
                  tidyr::drop_na()
                if (!(minsplit == 10 & mincriterion == 0.975 & minbucket == 5 & testtype == "Bonferroni" & teststat == "quad" & nresample == 9999)) {
                  expect_error(
                    suppressWarnings(
                      ctree_fit(
                        ML_data = function_input,
                        minsplit = minsplit,
                        mincriterion = mincriterion,
                        minbucket = minbucket,
                        testtype = testtype,
                        teststat = teststat,
                        nresample = nresample
                      )
                    )
                  )
                }
              }
            }
          }
        }
      }
    }
  }
})
