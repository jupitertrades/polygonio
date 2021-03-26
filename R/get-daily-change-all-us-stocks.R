get_daily_change_all_us_stocks <- function() {
  options(scipen = 999)
  polygon_list <- glue::glue("https://api.polygon.io/v2/snapshot/locale/us/markets/stocks/tickers?apiKey={Sys.getenv('polygon')}&limit=50000") %>%
    httr::GET() %>% httr::content()
  if(class(polygon_list) != 'list' |is.null(polygon_list$tickers)) {
    return(tibble::tibble())
  }
  snapshot_slim <- polygon_list %>% purrr::pluck('tickers') 
  tickers_list <- snapshot_slim %>% map2_chr('ticker',purrr::pluck) 
  todays_pct_change_list <- snapshot_slim %>% map2_dbl('todaysChange',purrr::pluck)
  change_df <- tibble::tibble(ticker = tickers_list,todays_pct_change = todays_pct_change_list)
  return(change_df)
}