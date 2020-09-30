library(testthat)        # load testthat package
library(sixjupiterinternal)       # load our package

# Test whether the output is a data frame
test_that("get-benzing-earnings returns dataframe", {
  r1 <- get_polygon_daily_prices('AAPL')
  expect_is(r1,'data.frame')
})
test_that("retired or tickers without active data return empty tibble", {
  r1 <- get_polygon_daily_prices('LNKD',from = '2020-02-01')
  expect_equal(nrow(r1), 0)
})

test_that("unidentified tickers or nonequity strings return empty tibble", {
  r1 <- get_polygon_daily_prices('dfsdfd',to = Sys.Date() - 2)
  expect_equal(nrow(r1), 0)
})

test_that("9 columns", {
  r1 <- get_polygon_daily_prices('AAPL',to = Sys.Date() - 2)
  expect_equal(ncol(r1), 9)
})

test_that("ticker col", {
  r1 <- get_polygon_daily_prices('AAPL',to = Sys.Date() - 2)
  expect_true(!is.null(r1$ticker))
})

#testthat::test_file('/Users/simit.patel/Documents/polygoniodev/tests/testthat/test-get-polygon-daily-prices.R')
