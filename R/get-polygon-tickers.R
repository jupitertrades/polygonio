get_polygon_tickers <- function() {
  get_ticker_loop <- function(i) {
    print(i)
    polygon_list <- glue::glue("https://api.polygon.io/v2/reference/tickers?apiKey={Sys.getenv('polygon')}&page={i}") %>%
      httr::GET() %>% httr::content() %>% pluck('tickers')
    tickers <- polygon_list %>% map2('ticker',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
    name_full <- polygon_list %>% map2('name',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
    market <- polygon_list %>% map2('market',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
    locale <- polygon_list %>% map2('locale',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
    type <- polygon_list %>% map2('type',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
    currency <- polygon_list %>% map2('currency',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
    poly_ref_tibble <- tibble::tibble(ticker = tickers, name_full = name_full, market = market, locale = locale,
                              type = type, currency = currency)
  return(poly_ref_tibble)
  }
  full_polygon_ticker <- purrr::map_dfr(1:3000,purrr::possibly(get_ticker_loop,otherwise =tibble::tibble()))
  return(full_polygon_ticker)
}
