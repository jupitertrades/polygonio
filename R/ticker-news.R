library(dplyr)

ticker_news <- function(ticker,since = 3) {
  
  api_path <- glue::glue("https://api.polygon.io/v2/reference/news?ticker={toupper(ticker)}&published_utc={Sys.Date()-since}&apiKey={Sys.getenv('polygon')}&limit=100")
  news_set <- httr::GET(api_path) %>% httr::content()
  titles <- news_set$results %>% purrr::map_chr('title', purrr::pluck)
  author <- news_set$results %>% purrr::map_chr('author', purrr::pluck)
  url <- news_set$results %>% purrr::map_chr('article_url', purrr::pluck)
  time <- news_set$results %>% purrr::map_chr('published_utc', purrr::pluck)
  description <- news_set$results %>% purrr::map_chr('description', purrr::pluck)
  frame <- tibble::tibble(title = titles, author = author, url = url, description = description, time = time)
  return(frame)
}