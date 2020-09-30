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
  date_set <- seq.Date(from = from, to = to, by = 9)
  date_partner <- rep(1:(length(date_set)/2+1), each = 2,length.out = length(date_set))
  date_table <- date_set %>% tibble::as_tibble() %>% dplyr::mutate(pair = date_partner)
  if(nrow(date_table) %% 2 > 0) {
    date_table <- dplyr::bind_rows(date_table,tibble::tibble(value = to,pair = max(date_partner)))
  }
  poly_inner <- function(date_pair) {
    date_values <- date_table %>% dplyr::filter(pair == date_pair)
    polygon_list <- glue::glue("https://api.polygon.io/v2/aggs/ticker/{ticker}/range/1/hour/{date_values$value[1]}/{date_values$value[2]}?sort=desc&apiKey={Sys.getenv('polygon')}") %>%
      httr::GET() %>% httr::content()
    if(class(polygon_list) != 'list' | length(polygon_list$results) == 0) {
      return(tibble::tibble())
    }
    polygon_df <- polygon_list %>% purrr::pluck('results') %>% dplyr::bind_rows() %>%
      dplyr::mutate(t = t/1000, t = as.POSIXct(t, origin = '1970-01-01'))
    return(polygon_df)
  }
  return_set <- purrr::map_dfr(1:(max(date_partner)),poly_inner)
  if(nrow(return_set) == 0) {
    return(return_set)
  }
  if(nrow(return_set) > 0) {
    return_set <- return_set %>% dplyr::mutate(ticker = ticker) %>%
      dplyr::rename (date = t, open = o, high = h, low = l, close = c, volume = v,
                     volume_weighted_avg_price = vw, trade_count = n)
  }
  return(return_set)
}
