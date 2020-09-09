get_replies_recursive <- function(statuses, existing_data = NULL) {
  statuses <- statuses[!is.na(statuses)] # this line removes the NAs, which are very common because most Tweets are not replies
  new_data <- lookup_statuses(statuses)
  
  print(paste0("Accessed ", nrow(new_data), " new Tweets"))
  
  new_statuses <- new_data$reply_to_status_id[!is.na(new_data$reply_to_status_id)]
  
  if (is.null(existing_data)) {
    out_data <- new_data
  } else {
    out_data <- rbind(new_data, existing_data)
  }
  
  if (length(new_statuses) > 1) {
    # this is the key line where the function calls itself, but passing new statuses
    get_replies_recursive(new_statuses, existing_data = out_data)
  } else {
    print(paste0("Accessed ", nrow(out_data), " new Tweets in total"))
    return(out_data)
  }
}

thread_finder <- function(status_id, d, out_statuses = NULL) {
  
  status_is_a_reply_to <- d[d$status_id == status_id, ]$reply_to_status_id
  
  status_is_a_reply_to <- ifelse(length(status_is_a_reply_to) == 0, NA, status_is_a_reply_to)
  
  if (!is.na(status_is_a_reply_to)) {
    
    if (is.null(out_statuses)) {
      out_statuses <- c(status_id, status_is_a_reply_to)  
    } else {
      out_statuses <- c(out_statuses, status_is_a_reply_to)
    }
    
    thread_finder(status_is_a_reply_to, d, out_statuses)
    
  } else {
    #out_statuses <- c(out_statuses, status_id)
    return(as.character(out_statuses))
  }
}

remove_short_threads <- function(thread, d, i) {
  same_thread <- which(str_detect(d$thread_string, thread))
  
  same_thread_df <- d[same_thread, "thread_string"]
  
  same_thread_df <- same_thread_df %>%
    mutate(length_of_string = nchar(thread_string)) %>%
    arrange(desc(length_of_string))
  
  #print(same_thread)
  
  print(i)
  
  the_longest_thread <- pull(same_thread_df[1, "thread_string"])
  
  #print(str_c("thread: ", thread))
  #print(str_c("longest: ", the_longest_thread))
  
  if (!(the_longest_thread == thread)) {
    print("searching recursively")
    remove_short_threads(the_longest_thread, d = d, i = i)
  } else {
    the_longest_thread
  }
}

process_conversation_thread_codes <- function(d) {

  #d <- drake::readd(orig_all)
  ## ------------------------------------------------------------------------------------------------------------
  #o <- get_replies_recursive(drake::readd(orig_all)$reply_to_status_id)
  #write_rds(o, "data/recursively-searched-replies.rds")
  o <- read_rds("data/recursively-searched-replies.rds")
  #o$status_id <- o$status_url %>% stringr::str_split("/") %>% purrr::map(~.[[6]]) %>% unlist()
  dd <- rbind(d, o)
  
  ## ------------------------------------------------------------------------------------------------------------
  dd <- dd %>% 
    mutate(day = lubridate::round_date(created_at, "day")) 
  
  d1415 <- dd %>% 
    filter(day < as.POSIXct(as.Date(c("2015-08-01"))),
           day >= as.POSIXct(as.Date("2014-07-31")))
  
  ## ------------------------------------------------------------------------------------------------------------
  d1415 <- d1415 %>% 
    distinct(status_id, .keep_all = TRUE)
  
  d1415 <- d1415 %>% 
    arrange(desc(created_at))
  
  ## ------------------------------------------------------------------------------------------------------------
  o <- purrr::map(d1415$status_id, thread_finder, d = dd) # neither of the inputs have an e
  
  ## ------------------------------------------------------------------------------------------------------------
  dx <- tibble(ID = 1:length(o))
  
  dx$thread <- o
  
  dxx <- dx %>% 
    unnest(thread)
  
  has_an_e <- dxx$thread %>% str_detect("e") %>% which()
  
  dxx <- dxx[-has_an_e, ] # this is removing the 
  
  dxx <- dxx %>% 
    group_by(ID) %>% 
    mutate(thread = as.character(thread)) %>% 
    mutate(thread_string = toString(as.character(thread)))
  
  dxx <- dxx %>% 
    select(ID, thread_string) %>% 
    distinct(thread_string, .keep_all = TRUE) %>% 
    ungroup()
  
  dxx$row_number <- 1:nrow(dxx)
  
  o <- map2(.x = dxx$thread_string, .f = remove_short_threads, d = dxx, .y = 1:nrow(dxx))
  
  ## ------------------------------------------------------------------------------------------------------------
  oo <- unique(o)
  ox <- tibble(ID = 1:length(oo))
  ox$thread_string = map_chr(oo, ~.)
  
  ## ------------------------------------------------------------------------------------------------------------
  tj <- ox %>%  
    select(-ID) %>% 
    left_join(dxx) %>% 
    select(ID, thread_string) %>% 
    mutate(thread_string = str_split(thread_string, ", ")) %>% 
    unnest(thread_string) %>% 
    rename(status_id = thread_string)
  
  tjj <- tj %>% 
    group_by(status_id) %>% 
    summarize(id_string = toString(ID)) %>% 
    mutate(id_string = str_split(id_string, ", ")) %>% 
    unnest(id_string) %>% 
    mutate(status_id = as.character(status_id)) %>% 
    mutate(id_string = str_pad(id_string, 4, pad = "0"))
  
  ## ------------------------------------------------------------------------------------------------------------
  mx <- d1415 %>% 
    left_join(tjj) %>% 
    arrange(created_at) %>% 
    mutate(id_factor = as.integer(fct_inorder(as.factor(id_string), ordered = TRUE))) %>% 
    mutate(id_factor = str_pad(as.character(id_factor), 4, pad = "0"))
  
  mxx <- mx %>% distinct(id_factor, status_id, .keep_all = TRUE)
  
  ## ------------------------------------------------------------------------------------------------------------
  mxx <- mxx %>% 
    mutate(date = lubridate::ymd(lubridate::round_date(created_at, "day")),
           time = hms::as_hms(mxx$created_at))
  
  mxx <- mxx %>% 
    select(status_id, date, time, screen_name, text, reply_to_status_id, is_quote, is_retweet, favorite_count, retweet_count, mentions_screen_name, quoted_status_id, quoted_screen_name, retweet_status_id, status_url, id_string, id_factor)
  
  mxx <- mxx %>% 
    mutate(code = NA) %>% 
    select(code, everything())
  
  mxx <- mxx %>% 
    mutate(code = if_else(str_detect(text, "^[aA][1-9]"), "answer",
                          ifelse(str_detect(text, "^[qQ][1-9]"), "question",
                                 ifelse(!is.na(id_string), "thread", "unclear"))))
  
  ## ------------------------------------------------------------------------------------------------------------
  mxx
}
