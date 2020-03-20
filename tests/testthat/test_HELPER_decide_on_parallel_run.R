
context("decide_on_parallel_run")

test_that("check decide_on_parallel_run with valid inputs", {
  function_output <- decide_on_parallel_run(
    fc_methods = "basic",
    nrows = 100
  )
  expect_is(function_output, "logical")
  expect_false(function_output)
  function_output <- decide_on_parallel_run(
    fc_methods = "prophet",
    nrows = 100
  )
  expect_is(function_output, "logical")
  expect_true(function_output)
  function_output <- decide_on_parallel_run(
    fc_methods = "basic",
    nrows = 1000
  )
  expect_is(function_output, "logical")
  expect_true(function_output)
  function_output <- decide_on_parallel_run(
    fc_methods = "prophet",
    nrows = 5
  )
  expect_is(function_output, "logical")
  expect_false(function_output)
  function_output <- decide_on_parallel_run(
    fc_methods = c("basic", "prophet"),
    nrows = 10
  )
  expect_is(function_output, "logical")
  expect_true(function_output)
})

test_that("check decide_on_parallel_run with invalid inputs", {
  expect_error(
    decide_on_parallel_run(
      fc_methods = "potato"
    )
  )
  expect_error(
    decide_on_parallel_run(
      fc_methods = c("basic", "potato")
    )
  )
  expect_error(
    decide_on_parallel_run(
      fc_methods = list()
    )
  )
  expect_error(
    decide_on_parallel_run(
      fc_methods = "basic",
      nrows = -42
    )
  )
  expect_error(
    decide_on_parallel_run(
      fc_methods = "basic",
      nrows = 4.2
    )
  )
})
