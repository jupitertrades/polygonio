get_polygon_daily_prices <- function(ticker,from = Sys.Date()-135, to = Sys.Date()) {
  options(scipen = 999)
  polygon_list <- glue::glue("https://api.polygon.io/v2/aggs/ticker/{ticker}/range/1/day/{from}/{to}?sort=asc&apiKey={Sys.getenv('polygon')}") %>%
    httr::GET() %>% httr::content()
    if(class(polygon_list) != 'list' | polygon_list$resultsCount == 0) {
      return(tibble::tibble())
    }
  if(is.null(polygon_list$results)) {
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

