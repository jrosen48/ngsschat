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
  d$status_id <- get_tweet_id(orig$status_url)
  
}

# read_rds("data-raw/all-ngsschat-tweets.rds") %>% 
# nrow() # 25493 - 25380 = 113 tweet difference in ~ two years between present and original processing
# nrow(o) # 25380

# these match
# d %>% nrow()

# readd(orig_all) %>% nrow()

# these match

create_orig <- function(d) {
  orig <- d %>% 
    filter(created_at >= lubridate::ymd("2014-08-01"),
           created_at <= lubridate::ymd("2015-07-31")) %>% 
    arrange(created_at)
}

# orig1 <- readd(orig)
# 
# nrow(orig)
# nrow(orig1)

# these match

create_orig_post <- function(d) {
  orig_post <- d %>% 
    filter(created_at >= lubridate::ymd("2015-08-01"),
           created_at <= lubridate::ymd("2016-07-31")) %>% 
    arrange(created_at)
}

# nrow(orig_post)
# 
# readd(orig_post) %>% nrow()
