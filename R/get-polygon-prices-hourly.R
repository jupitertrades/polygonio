get_polygon_hourly_prices <- function(ticker,from = Sys.Date()-61, to = Sys.Date()) {
  options(scipen = 999)
  if(!lubridate::is.Date(from) | !lubridate::is.Date(to)){
    if(lubridate::is.Date(from %>% as.Date()) == FALSE | lubridate::is.Date(to %>% as.Date) == FALSE) {
      stop("from or to arguments need to be of class date. Try entering them in YYYY-MM-DD format within
           as an object of class Date. Like so: from = as.Date('2020-02-20')")
    } else {
      from = as.Date(from)
      to <- as.Date(to)
    }
  }
  polygon_list <- glue::glue("https://api.polygon.io/v2/aggs/ticker/{ticker}/range/1/hour/{from}/{to}?sort=asc&apiKey={Sys.getenv('polygon')}&limit=50000") %>%
    httr::GET() %>% httr::content()
  if(class(polygon_list) != 'list' | length(polygon_list$results) == 0) {
    return(tibble::tibble())
  }
  polygon_df <- polygon_list %>% purrr::pluck('results') %>% dplyr::bind_rows() %>%
    dplyr::mutate(t = t/1000, t = as.POSIXct(t, origin = '1970-01-01')) %>%
    dplyr::rename (date = t, open = o, high = h, low = l, close = c, volume = v,
                   volume_weighted_avg_price = vw) %>% select(-n) %>%
    dplyr::mutate(ticker = ticker) %>%
    dplyr::select(c(ticker,date,open,high,low,close,volume_weighted_avg_price,volume))
  return(polygon_df)
}
