#' @title Get all US Stock Prices
#'
#' @description Show a metric plotted over time as a line chart.
#'
#' @return Dataframe with last price, volume for the curernt day, and percent change
#' since open. 
#'
#'@examples
#' \dontrun{
#'all_us_stock_prices(metric_df)
#'}
#'
#' @export all_us_stock_prices
#' @import dplyr
all_us_stock_prices <- function() {
  snapshot <- glue::glue("https://api.polygon.io/v2/snapshot/locale/us/markets/stocks/tickers?apiKey={Sys.getenv('polygon')}") %>% 
    httr::GET() %>% httr::content() %>% purrr::pluck('tickers')
  ticker_list <- purrr::map_chr(snapshot,'ticker')
  today_ohlc <- purrr::map_dfr(snapshot,'day') %>% select(-vw)
  names(today_ohlc) <- c('close','high','low','open','volume')
  pct_change <- purrr::map_dbl(snapshot,'todaysChangePerc')
  current_snapshot <- today_ohlc %>% 
    mutate(ticker = ticker_list, daily_percent_change = pct_change) %>% 
    select(c(ticker,everything()))
  return(current_snapshot)
}
  