
context("filter_stationary_columns")

test_that("check filter_stationary_columns with or without stationary columns", {
  function_input <- dummy_gasprice
  function_output <- dummy_gasprice %>% 
    filter_stationary_columns()
  expect_equal(function_input, function_output)
  expect_true(ncol(function_input) == ncol(function_output))
  expect_true(ncol(function_output) == 6)
  function_output <- dummy_gasprice %>% 
    dplyr::filter(state == "New York") %>% 
    filter_stationary_columns()
  expect_true(ncol(function_input) > ncol(function_output))
  expect_true(ncol(function_output) == 5)
  function_output <- function_output %>% 
    dplyr::filter(oil_company == "CompanyA") %>% 
    filter_stationary_columns()
  expect_true(ncol(function_input) > ncol(function_output))
  expect_true(ncol(function_output) == 4)
  
  function_output <- function_input %>% 
    dplyr::filter(state == "New York") %>% 
    dplyr::filter(oil_company == "CompanyA") %>% 
    filter_stationary_columns()
  expect_true(ncol(function_input) > ncol(function_output))
  expect_true(ncol(function_output) == 4)
})

test_that("check filter_stationary_columns with invalid inputs", {
  expect_error(
    filter_stationary_columns(
      data = "potato"
    )
  )
  expect_error(
    filter_stationary_columns(
      data = 42
    )
  )
})
