

test_that("publish_column_cell", {

  expect_error(publish_column_cell(col_name = pct))
  expect_error(publish_column_cell(decimals = "a"))
})

test_that("draws_percent_change",{
  expect_error(draws_percent_change(year_start = 1990))
  expect_error(drwas_percent_change(year_end = 2015))

})


# Avoid testing simple code that you're confident will work. Instead focus your time one code that you're not sure about, is fragile, or had complicated interdependecies.
