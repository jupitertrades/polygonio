get_ticker_details <- function(ticker) {
  details_list <- glue::glue("https://api.polygon.io/v1/meta/symbols/{ticker}/company?&apiKey={Sys.getenv('polygon')}") %>%
    httr::GET() %>% httr::content()
  description <- details_list$description
  employee_count <- details_list$employees
  url <- details_list$url
  ceo <- details_list$ceo
  name <- details_list$name
  ticker <- details_list$symbol
  details_frame <- tibble::tibble(ticker = ticker, name = name, url = url, description = description,
                          employee_count = employee_count, ceo = ceo)
  return(details_frame)
}

get_stock_market_status <- function() {
  status <- glue::glue("https://api.polygon.io/v1/marketstatus/now?&apiKey={Sys.getenv('polygon')}") %>%
    httr::GET() %>% httr::content()
  return(status$market)
}
