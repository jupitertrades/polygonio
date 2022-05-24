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

snapshot_us_stocks <- function() {
  options(scipen = 999)
  polygon_list <- glue::glue("https://api.polygon.io/v2/snapshot/locale/us/markets/stocks/tickers?apiKey={Sys.getenv('polygon')}&limit=50000") %>%
    httr::GET() %>% httr::content()
  if(class(polygon_list) != 'list' |is.null(polygon_list$tickers)) {
    return(tibble::tibble())
  }
  tickers_list <- polygon_list %>% map2_chr('ticker',purrr::pluck) 
  snapshot_data <- polygon_list %>% purrr::pluck('tickers') %>% 
    map2('day',purrr::pluck) %>% bind_rows() %>% mutate(ticker = tickers_list)
  
  names(snapshot_data) <- c('close','high','low','open','volume','volume_weighted_price','ticker')
  snapshot_data_named <- snapshot_data %>% select(c(ticker,open,high,low,close,
                                                    volume_weighted_price,volume)) %>% 
    mutate(time = Sys.time())
  return(snapshot_data_named)
}
