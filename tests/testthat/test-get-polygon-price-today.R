library(testthat)

test_that("returns dataframe", {
  a1 <- get_polygon_price_today('SNAP')
  expect_is(a1,'data.frame')
})

test_that("unrecognized character strings return message", {
  a1 <- get_polygon_price_today('sdfhlk')
  expect_is(a1,'character')
})

#testthat::test_file('/Users/simit.patel/Documents/polygoniodev/tests/testthat/test-get-polygon-price-today.R')
