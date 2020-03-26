# proc processed data

# Loading packages

library(tidyverse)
library(lubridate)
library(rTAGS)
library(rtweet)
library(osfr) # devtools::install_github("centerforopenscience/osfr")   # only need to run once
library(nominatim)

get_tweet_id <- function (t) {
  t <- str_extract(t, "[0-9]{6,}")
  t
}

# uploading initial data
# library(osfr)
# to upload
project <- osf_retrieve_node("https://osf.io/9ex7k/")
# osf_upload(project, "0_orig_tweets.csv")
# osf_upload(project, "ngsschat-code-profiles.csv")

# Getting data 

osf_retrieve_file("https://osf.io/j5a28/") %>% 
  osf_download(path = "0_orig_tweets.csv", overwrite = FALSE)

osf_retrieve_file("https://osf.io/fuh8z/") %>% 
  osf_download(path = "ngsschat-code-profiles.csv", overwrite = FALSE)

# Loading data

orig_tweets <- read_csv("0_orig_tweets.csv")
p <- read_csv("ngsschat-code-profiles.csv")

# orig_tweets$ID <- str_split(orig_tweets$status_url, "/") %>%
#   map(~ .[[6]]) %>%
#   unlist()
# 
# orig_rtweet <- rtweet::lookup_tweets(orig_tweets$ID)
# write_rds(orig_rtweet, "all-ngsschat-tweets.rds")
# # scraped_tweets <- read_csv("raw-tweets.csv")
# # rtweet_tweets <- lookup_statuses(as.character(scraped_tweets$id)) # for caching

orig_rtweet <- read_rds("all-ngsschat-tweets.rds")

orig_rtweet$status_id <- get_tweet_id(orig_rtweet$status_url)

# data_frame(lll = lll, )
# orig_rtweet %>% count(is_retweet)

# Loading new data

orig_rtweet <- mutate(orig_rtweet, screen_name = tolower(screen_name))

orig_rtweet$day <- round_date(orig_rtweet$created_at, "day")

orig_rtweet %>% 
  filter(day >= '2014-08-01' & day < '2015-07-31') %>% 
  nrow() # let's use this one - 2014-2015 school year

dd <- orig_rtweet %>% 
  filter(day >= '2014-08-01' & day < '2015-07-31')

dd <- proc_tweets(dd)

users <- rtweet::users_data(dd)

users <- users %>% 
  distinct(screen_name, .keep_all = TRUE) %>% 
  mutate(screen_name = tolower(screen_name))

users_n_tweets <- dd %>% 
  filter(!is_retweet) %>% 
  count(screen_name) %>% 
  mutate(screen_name = tolower(screen_name)) %>% 
  rename(n_tweets = n) %>% 
  select(screen_name, n_tweets)

users_n_days <- dd %>% 
  filter(!is_retweet) %>% 
  count(screen_name, day) %>% 
  count(screen_name) %>% 
  mutate(screen_name = tolower(screen_name)) %>% 
  rename(n_days = n) %>% 
  select(screen_name, n_days)

users <- users %>% 
  left_join(users_n_tweets) %>% 
  left_join(users_n_days)

p <- p %>% 
  filter(Code != 0) %>% 
  mutate(screen_name = tolower(screen_name)) %>% 
  select(screen_name, code = Code, code_category = Category)

users <- users %>% 
  left_join(p, by = "screen_name")

users <- users %>%
  filter(n_tweets > 1)

# ds_edge <- create_edgelist(dd)

write_rds(dd, "rtweet-tweets-14-15.rds")
#write_csv(ds_edge, "ds-edge-14-15.csv")
#write_csv(ds_edge,'ngsschat-edgelist-14-15.csv')
ds_edge <- read_csv("ngsschat-edgelist-14-15.csv") # this is an edgelist created from the rtweet data

#ds_edge <- read_csv("ds-edge-14-15.csv")
ds_edge %>% count(edge_type)

# joining new data
X2_tweets <- read_csv("proc-data/2_tweets.csv")
X2_tweets$status_id <- get_tweet_id(X2_tweets$tweet_link)

x <- X2_tweets %>% 
  select(status_id, rtNames, favNames)

xx <- x %>% 
  filter(status_id %in% dd$status_id) %>% 
  filter(!is.na(rtNames) | !is.na(favNames)) %>% 
  left_join(select(dd, screen_name, status_id))

favNames <- str_split(xx$favNames, "\\*")
rtNames <- str_split(xx$rtNames, "\\*")

d1 <- tibble(screen_name = xx$screen_name, favNames) %>% 
  mutate(edge_type = "favorite") %>% unnest() %>% filter(!is.na(favNames)) %>% 
  select(receiver = screen_name, sender = favNames, edge_type)

d2 <- tibble(screen_name = xx$screen_name, rtNames) %>% 
  mutate(edge_type = "retweet") %>% unnest() %>% filter(!is.na(rtNames)) %>% 
  select(receiver = screen_name, sender = rtNames, edge_type)

da <- bind_rows(d1, d2)

ds_edge <- ds_edge %>% 
  mutate_if(is.character, tolower)

da <- da %>% 
  mutate_if(is.character, tolower)

ds_edge <- bind_rows(ds_edge, da)

ds_edge <- ds_edge %>% 
  filter(sender %in% users$screen_name | receiver %in% users$screen_name)

# Adding user data

ddd <- add_users_data(ds_edge, users)

ddd %>% count(edge_type)
ddd %>% count(edge_type) %>% summarize(n_sum = sum(n))

ddd <- ddd %>% 
  filter(n_tweets_sender > 1 & n_tweets_receiver > 1)

ddd %>% count(edge_type)
ddd %>% count(edge_type) %>% summarize(n_sum = sum(n))

# users$screen_name %in% ds_edge$sender | users$screen_name %in% ds_edge$receiver
# users <- users[-which(!(users$screen_name %in% ds_edge$sender | users$screen_name %in% ds_edge$receiver)), ]
x <- c(unique(ddd$sender), unique(ddd$receiver))

# ui <- x %>% 
#   rtweet::lookup_users(token = token)
# 
# uii <- ui %>% 
#   select(screen_name, location) %>% 
#   left_join(users)
# 
# write_csv(uii, "uii.csv") # where does this go?
# library(nominatim)
# l <- as.list(users$location) %>%
#   purrr::map(osm_geocode, key = MQ_API_KEY)
#  
# write_rds(l, "all-geocoded-locs.rds")

ddd <- ddd %>% 
  filter(n_tweets_sender > 1 & n_tweets_receiver > 1) %>% 
  mutate(
    code_sender = ifelse(is.na(code_sender), 11, code_sender),
    code_receiver = ifelse(is.na(code_receiver), 11, code_receiver)) %>%
  mutate(group_sender = recode(code_sender,
                               `1` = "Teacher", 
                               `2` = "Administrator",
                               `3` = "Administrator",
                               `4` = "Researcher",
                               `5` = "Other",
                               `8` = "Other",
                               `9` = "Other",
                               `10` = "Unclear",
                               `6` = "Organization",
                               `7` = "Organization",
                               `11` = "Unclear"),
         group_receiver = recode(code_receiver,
                                 `1` = "Teacher", 
                                 `2` = "Administrator",
                                 `3` = "Administrator",
                                 `4` = "Researcher",
                                 `5` = "Other",
                                 `8` = "Other",
                                 `9` = "Other",
                                 `10` = "Unclear",
                                 `6` = "Organization",
                                 `7` = "Organization",
                                 `11` = "Unclear"))

users <- mutate(users,
                code = ifelse(is.na(code), 11, code)) %>% 
  mutate(group = recode(code,
                        `1` = "Teacher", 
                        `2` = "Administrator",
                        `3` = "Administrator",
                        `4` = "Researcher",
                        `5` = "Other",
                        `8` = "Other",
                        `9` = "Other",
                        `10` = "Unclear",
                        `6` = "Organization",
                        `7` = "Organization",
                        `11` = "Unclear")) 

users$day_created <- lubridate::round_date(lubridate::ymd_hms(users$account_created_at), "day")
users$years_on_twitter <- (lubridate::ymd("2014-08-01", tz = "UTC") - users$day_created) / 365

ddd <- ddd %>% 
  mutate(interaction_type = ifelse(edge_type %in% c("mention", "reply"), "conversing",
                                   ifelse(edge_type %in% c("retweet", "quotes", "favorite"), "endorsing", NA)))



# Writing the five products

orig_rtweet %>% 
  filter(day >= '2013-08-01' & day <= '2014-07-31') %>% # 13-14
  filter(!is_retweet) %>% 
  flatten() %>% 
  write_csv("orig-pre-14.csv")

orig_rtweet %>% 
  filter(day >= '2015-08-01' & day <= '2016-07-31') %>% # 15-16
  filter(!is_retweet) %>% 
  flatten() %>% 
  write_csv("orig-post-15.csv")

write_rds(dd, 'ngsschat-tweets-14-15.rds')
write_rds(users, "users-to-analyze.rds")
write_rds(ddd, "edgelist-to-analyze.rds")

# Uploading the products

project <- osf_retrieve_node("https://osf.io/9ex7k/")

MAPS_API_KEY <- 'AIzaSyDJex1zLFQDb9y0ghovbSPevMlGUVCCjPc'
users$geocoded_location <- mapsapi::mp_geocode(users$location, key = MAPS_API_KEY)
write_rds(users, 'geocoded-locations.rds')

#osf_upload(project, 'prepare-data.R', overwrite = TRUE)
osf_upload(project, 'geocoded-locations.rds', overwrite = TRUE)
osf_upload(project, "all-ngsschat-tweets.rds", overwrite = TRUE)
osf_upload(project, "ngsschat-tweets-14-15.rds", overwrite = TRUE)
osf_upload(project, "orig-pre-14.csv", overwrite = TRUE)
osf_upload(project, "orig-post-15.csv", overwrite = TRUE)
osf_upload(project, "users-to-analyze.csv", overwrite = TRUE)
osf_upload(project, "edgelist-to-analyze.csv", overwrite = TRUE)