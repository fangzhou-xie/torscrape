# main function in this package

#' scrape webpages with Tor proxy
#' 
#' @param urls, character vector, a character vector of urls to be scraped
#' @param async, bool (default FALSE), to use async request or not
#' @param max_retry, int (default 5), how many times to allow for repeating
#' @param wait, float (default 0.1), wait time per call
#' @param random_ua, bool (default TRUE), whether to use Randomuseragent::random_useragent() or not
#' @param callback, function (default return entire request contenty), take request content as input and return a single character as output
#' @export
scrape <- function(urls, async = FALSE, max_retry = 3, wait = 0.1, random_ua = TRUE, callback = NULL) {
  if (async) {
    scrape_async(urls, max_retry, wait, random_ua, callback)
  } else {
    scrape_serial(urls, max_retry, wait, random_ua, callback)
  }
}

scrape_serial <- function(urls, max_retry, wait, random_ua, callback) {
  
  # init try num
  try <- 1
  urls_remain <- urls
  url_df <- tibble::tibble(url = character(0), content = character(0)) # return df if not settting db name, otherwise write to db
  
  if (is.null(callback)) {
    callback <- function(req) {rawToChar(req$content)}
  }
  
  h <- curl::new_handle(proxy = "socks5://localhost:9050", maxage_conn = 10) # , fresh_connect = 1)
  if (random_ua) {
    curl::handle_setheaders(h, "Cache-Control" = "no-cache", "User-Agent" = Randomuseragent::random_useragent())
  } else {
    curl::handle_setheaders(h, "Cache-Control" = "no-cache")
  }
  
  while(try <= max_retry & length(urls_remain) > 0) {
    reqs <- purrr::map(urls_remain, function(x) {
      req <- curl::curl_fetch_memory(x, handle = h)
      Sys.sleep(wait)
      req
    })
    
    reqs_valid <- purrr::map_lgl(reqs, function(x) {
      # print(x$status_code)
      x$status_code == 200
    })
    
    reqs_content <- purrr::map_chr(reqs[reqs_valid], callback)
    
    # TODO: add callback here
    url_df <- rbind(url_df, tibble::tibble(url = urls_remain[reqs_valid], content = reqs_content))
    
    urls_remain <- if(any(reqs_valid)) urls_remain[-reqs_valid] else urls_remain
    try <- try + 1
  }
  
  # append missing ones at the end
  url_df <- rbind(url_df, tibble::tibble(url = urls_remain, content = rep(NA_character_, length(urls_remain))))
  url_df
}

scrape_async <- function(urls, max_retry, wait, random_ua, callback) {
  # init try num
  try <- 1
  urls_remain <- urls
  url_df <- tibble::tibble(url = character(0), content = character(0)) # return df if not settting db name, otherwise write to db
  
  if (is.null(callback)) {
    callback <- function(req) {rawToChar(req$content)}
  }
  
  done <- function(url) {
    function(req) {
      reqs[[url]] <<- req
    }
  }
  
  while(try <= max_retry & length(urls_remain) > 0) {
    reqs <- list()
    pool <- curl::new_pool()
    
    purrr::map(urls_remain, function(x) {
      h <- curl::new_handle(proxy = "socks5://localhost:9050", maxage_conn = 10) # , fresh_connect = 1)
      if (random_ua) {
        curl::handle_setheaders(h, "Cache-Control" = "no-cache", "User-Agent" = Randomuseragent::random_useragent())
      } else {
        curl::handle_setheaders(h, "Cache-Control" = "no-cache")
      }
      curl::curl_fetch_multi(x, done = done(x), pool = pool, handle = h)
    })
    # execute the pool
    curl::multi_run(pool = pool)
    
    reqs_valid <- purrr::map_lgl(names(reqs), function(x) {
      # print(x$status_code)
      reqs[[x]]$status_code == 200
    })
    
    reqs_content <- purrr::map_chr(reqs[names(reqs)[reqs_valid]], callback)
    
    # TODO: add callback here
    url_df <- rbind(url_df, tibble::tibble(url = names(reqs)[reqs_valid], content = reqs_content))
    
    urls_remain <- if(any(reqs_valid)) urls_remain[-reqs_valid] else urls_remain
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
