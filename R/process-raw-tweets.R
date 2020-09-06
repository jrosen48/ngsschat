# process-raw-tweets

process_raw_tweets <- function(f) {
  # d <- read_csv(f)
  # 
  # d$status_id <- get_tweet_id(d$status_url)
  # 
  # o <- rtweet::lookup_statuses(d$status_id) # requires a token; see rtweet::create_token()
  # 
  # # first processed in 2019
  # write_csv(rtweet::flatten(o), "data-raw/rtweet-processed-tweets.csv") 
  
  d <- read_csv("data-raw/rtweet-processed-tweets.csv")
  d$created_at <- lubridate::with_tz(d$created_at, "EST")
  d$status_id <- get_tweet_id(d$status_url)
  d$screen_name <- tolower(d$screen_name)
  d
}

create_orig <- function(d) {
  orig <- d %>% 
    dplyr::filter(created_at >= lubridate::ymd("2014-08-01"),
                  created_at <= lubridate::ymd("2015-07-31")) %>% 
    arrange(created_at)
}

create_orig_post <- function(d) {
  orig_post <- d %>% 
    filter(created_at >= lubridate::ymd("2015-08-01"),
           created_at <= lubridate::ymd("2016-07-31")) %>% 
    arrange(created_at)
}
