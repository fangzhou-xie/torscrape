# main function in this package

#' scrape webpages with Tor proxy
#' 
#' @param urls, character vector, a character vector of urls to be scraped
#' @param async, bool (default FALSE), to use async request or not
#' @param max_retry, int (default 5), how many times to allow for repeating
#' @param random_ua, bool (default TRUE), whether to use Randomuseragent::random_useragent() or not
#' @param callback, function (default return entire request contenty), take request content as input and return a single character as output
#' @export
scrape <- function(urls, async = FALSE, max_retry = 3, random_ua = TRUE, callback = NULL) {
  if (async) {
    scrape_async(urls, max_retry, random_ua, callback)
  } else {
    scrape_serial(urls, max_retry, random_ua, callback)
  }
}

scrape_serial <- function(urls, max_retry, random_ua, callback) {
  
  # init try num
  try <- 1
  urls_remain <- urls
  url_df <- tibble::tibble(url = character(0), content = character(0)) # return df if not settting db name, otherwise write to db
  
  safe_curl <- purrr::safely(curl::curl_fetch_memory)
  
  if (is.null(callback)) {
    callback <- function(req) {rawToChar(req$content)}
  }
  
  h <- curl::new_handle(proxy = "socks5://localhost:9050", maxage_conn = 10, failonerror = TRUE) # , fresh_connect = 1)
  if (random_ua) {
    curl::handle_setheaders(h, "Cache-Control" = "no-cache", "User-Agent" = Randomuseragent::random_useragent())
  } else {
    curl::handle_setheaders(h, "Cache-Control" = "no-cache")
  }
  
  while(try <= max_retry & length(urls_remain) > 0) {
    reqs <- purrr::map(urls_remain, ~safe_curl(.x, handle = h))
    reqs <- purrr::map(reqs, "result")
    
    # test all NULL reqs behavior
    reqs_valid_ids <- which(lapply(reqs, FUN = Negate(is.null)) == TRUE)
    reqs_valid_content <- purrr::map_chr(reqs[reqs_valid_ids], callback)
    
    # TODO: add callback here
    url_df <- rbind(url_df, tibble::tibble(url = urls_remain[reqs_valid_ids], content = reqs_valid_content))
    
    # urls_remain <- if(any(reqs_isvalid)) urls_remain[-reqs_isvalid] else urls_remain
    urls_remain <- setdiff(urls_remain, urls_remain[reqs_valid_ids])
    if (length(urls_remain) == 0) {
      break
    }
    try <- try + 1
  }
  
  # append missing ones at the end
  url_df <- rbind(url_df, tibble::tibble(url = urls_remain, content = rep(NA_character_, length(urls_remain))))
  url_df
}

scrape_async <- function(urls, max_retry, random_ua, callback) {
  # init try num
  try <- 1
  urls_remain <- urls
  url_df <- tibble::tibble(url = character(0), content = character(0)) # return df if not settting db name, otherwise write to db
  
  if (is.null(callback)) {
    callback <- function(req) {rawToChar(req$content)}
  }
  
  dn <- function(url) {
    function(req) {
      reqs[[url]] <<- req
    }
  }
  
  while(try <= max_retry & length(urls_remain) > 0) {
    reqs <- list()
    pool <- curl::new_pool()
    
    purrr::map(urls_remain, function(x) {
      h <- curl::new_handle(proxy = "socks5://localhost:9050", maxage_conn = 10, failonerror = TRUE) # , fresh_connect = 1)
      if (random_ua) {
        curl::handle_setheaders(h, "Cache-Control" = "no-cache", "User-Agent" = Randomuseragent::random_useragent())
      } else {
        curl::handle_setheaders(h, "Cache-Control" = "no-cache")
      }
      curl::curl_fetch_multi(x, done = dn(x), pool = pool, handle = h)
    })
    # execute the pool
    curl::multi_run(pool = pool)
    purrr::map(reqs, "result")
    
    # test all NULL reqs behavior
    reqs_valid_ids <- urls_remain %in% names(reqs)
    reqs_valid_content <- purrr::map_chr(reqs, callback)
    
    # TODO: add callback here
    url_df <- rbind(url_df, tibble::tibble(url = names(reqs), content = reqs_valid_content))
    
    urls_remain <- setdiff(urls_remain, urls_remain[reqs_valid_ids])
    if (length(urls_remain) == 0) {
      break
    }
    # print(urls_remain)
    try <- try + 1
  }
  
  # append missing ones at the end
  url_df <- rbind(url_df, tibble::tibble(url = urls_remain, content = rep(NA_character_, length(urls_remain))))
  url_df
}


# read file into single string
# readChar(fn, file.info(fn)$size)

# read single string as dataframe
# read.table(text = readr::read_file("d.csv"), sep = ",", header = TRUE)
# read.table(text = "a,b\n1,4\n2,5\n3,6\n", sep = ",", header = TRUE)
