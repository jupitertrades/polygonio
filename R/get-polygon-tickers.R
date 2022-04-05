get_polygon_tickers <- function() {
  polygon_response <- glue::glue("https://api.polygon.io/v3/reference/tickers?apiKey={Sys.getenv('polygon')}&limit=1000") %>%
    httr::GET() %>% httr::content()
  polygon_path <- polygon_response$next_url
  polygon_list <- polygon_response %>% pluck('results')
  tickers <- polygon_list %>% map2('ticker',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
  name_full <- polygon_list %>% map2('name',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
  market <- polygon_list %>% map2('market',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
  locale <- polygon_list %>% map2('locale',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
  exchange <- polygon_list %>% map2('primary_exchange',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
  type <- polygon_list %>% map2('type',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
  currency <- polygon_list %>% map2('currency',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
  poly_ref_tibble <- tibble::tibble(ticker = tickers, name_full = name_full, market = market, locale = locale,
                                    primary_exchange = exchange, type = type, currency = currency)
  while(!is.null(polygon_path)) {
    polygon_response <- glue::glue("{polygon_path}&apiKey={Sys.getenv('polygon')}&limit=1000") %>% httr::GET() %>% httr::content()
    polygon_list <- polygon_response %>% pluck('results')
    tickers <- polygon_list %>% map2('ticker',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
    name_full <- polygon_list %>% map2('name',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
    market <- polygon_list %>% map2('market',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
    locale <- polygon_list %>% map2('locale',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
    exchange <- polygon_list %>% map2('primary_exchange',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
    type <- polygon_list %>% map2('type',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
    currency <- polygon_list %>% map2('currency',purrr::pluck) %>% modify_if(is.null,~as.character(NA)) %>% flatten_chr()
    loop_tibble <- tibble::tibble(ticker = tickers, name_full = name_full, market = market, locale = locale,
                                  primary_exchange = exchange, type = type, currency = currency)
    poly_ref_tibble <- bind_rows(poly_ref_tibble,loop_tibble)
    polygon_path <- polygon_response$next_url
  }
  return(poly_ref_tibble)
}
