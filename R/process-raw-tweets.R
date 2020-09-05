# process-raw-tweets

d <- read_csv("data-raw/original-storify-data.csv")

d$status_id <- get_tweet_id(d$status_url)

o <- rtweet::lookup_statuses(d$status_id)

write_csv(rtweet::flatten(o), "data-raw/rtweet-processed-tweets.csv")

read_rds("data-raw/all-ngsschat-tweets.rds") %>% 
  nrow() # 25493 - 25380 = 113 tweet difference in ~ two years

nrow(o) # 25380
