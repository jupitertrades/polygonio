library(testthat)

test_that("ticker-news()", {
  a1 <- ticker_news('SNAP')
  expect_is(a1,'data.frame')
})



#testthat::test_file('/Users/simit.patel/Documents/polygoniodev/tests/testthat/test-get-polygon-price-today.R')
