get_polygon_price_today <- function(symbol) {
  options(scipen = 999)
  polygon_list <- glue::glue("https://api.polygon.io/v2/snapshot/locale/us/markets/stocks/tickers/{symbol}?apiKey={Sys.getenv('polygon')}") %>%
    httr::GET() %>% httr::content()
  if(polygon_list$status == 'NotFound') {
    return('ticker not found. please confirm it is a real, currently listed, US ticker.')
  }
  polygon_df <- polygon_list %>% purrr::pluck('ticker') %>% purrr::pluck('day') %>% dplyr::bind_rows() %>%
    rename(close = c, high = h, low = l, open = o,volume = v, volume_weighted_avg_price = vw) %>%
    mutate(ticker = symbol) %>% select(ticker, everything())
  return(polygon_df)
}
