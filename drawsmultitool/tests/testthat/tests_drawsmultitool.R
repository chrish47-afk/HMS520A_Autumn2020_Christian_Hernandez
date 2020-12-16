# Tests
#library(testthat)

test_that("publish_column_cell", {
  #expect_equal(publish_column_cell(col_name, "pct"))
  #expect_equal(publish_column_cell(col_name, "mean"))
  expect_error(publish_column_cell(col_name = pct))
  expect_error(publish_column_cell(decimals = "a"))
})

# draws_percent_change
#test_that("draws_percent_change", {
#})

# draws_mean
#test_that("draws_mean", {
#})
