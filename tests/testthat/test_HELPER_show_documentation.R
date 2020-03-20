
context("show_documentation")

test_that("check show_documentation for correctly loading README", {
  function_output <- show_documentation(
    topic = "README",
    test_mode = T
  )
  expect_true(file.exists(function_output))
})

test_that("check show_documentation for correctly loading forcast models topic", {
  function_output <- show_documentation(
    topic = "forecast_models",
    test_mode = T
  )
  expect_true(file.exists(function_output))
})

test_that("check show_documentation directly", {
  expect_null(
    show_documentation(
      topic = "README",
      test_mode = F
    )
  )
  expect_null(
    show_documentation(
      topic = "forecast_models",
      test_mode = F
    )
  )
})

test_that("check show_documentation with invalid inputs", {
  expect_error(
    show_documentation(
      topic = "potato"
    )
  )
  expect_error(
    show_documentation(
      topic = 42
    )
  )
})
