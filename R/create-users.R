# create-users

create_users <- function(orig, professional_role_codes) {
  
  users <- rtweet::users_data(orig)
  
  users_n_tweets <- orig %>% 
    filter(!is_retweet) %>% 
    count(screen_name) %>% 
    mutate(screen_name = tolower(screen_name)) %>% 
    rename(n_tweets = n) %>% 
    select(screen_name, n_tweets)
  
  users <- users %>% 
    mutate(screen_name = tolower(screen_name)) %>% 
    distinct(screen_name, .keep_all = TRUE)
  
  users <- users %>% 
    left_join(users_n_tweets)
  
  professional_role_codes <- professional_role_codes %>% 
    filter(Code != 0) %>% 
    mutate(screen_name = tolower(screen_name)) %>% 
    select(screen_name, code = Code, code_category = Category)
  
  users <- users %>% 
    left_join(professional_role_codes, by = "screen_name")
  
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
  
  users <- users %>%
    filter(n_tweets > 1)
  
  users %>% 
    filter(screen_name != "etnyresci") # this user w/ two tweets was removed from the dataset inadvertently 
  
}