get_ticker_details <- function(ticker) {
  details_list <- glue::glue("https://api.polygon.io/v1/meta/symbols/{ticker}/company?&apiKey={Sys.getenv('polygon')}") %>%
    httr::GET() %>% httr::content()
  details_list$tags <- details_list %>% glue::glue_collapse(sep = ",",last = ",")
}
