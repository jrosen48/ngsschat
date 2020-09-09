
process_qual_codes <- function(data, users, dd) {

  data <- readd(all_unfiltered_coded_threads)
  users <- readd(users)
  dd <- readd(dd)
  
  du <- filter(data, code == "unclear")
  
  duu <- data %>% 
    filter(!(status_id %in% du$status_id),
           code %in% c("question", "answer")) %>% 
    mutate(code = "TS")
  
  #ddb <- dd
  
  codes <- dd %>%
    filter(Flag == 1) %>% 
    mutate(ID = as.character(ID))
  
  dd <- dd %>% 
    dplyr::select(ID, code = Code)
  
  # dd[2619:nrow(dd), "ID"] <- str_c("uc-", 1:1319)
  
  # dd[2619:nrow(dd), ]
  
  d <- data %>% 
    filter(!(status_id %in% duu$status_id)) %>% 
    mutate(ID = as.character(as.integer(id_factor))) %>% 
    dplyr::select(-code)
  
  d <- d %>% 
    left_join(dd)
  
  dub <- du %>%
    mutate(ID = str_c("uc-", 1:1318)) %>% # I had to reduce this by 1
    dplyr::select(-code) %>%
    left_join(dd)
  
  d <- d %>% 
    filter(!(status_id %in% duu$status_id),
           !(status_id %in% dub$status_id)) %>% 
    bind_rows(duu) %>% 
    bind_rows(dub)
  
  d[is.na(d$ID), "ID"] <- str_c("qa-", 1:2197)
  
  # write_csv(select(d, -mentions_screen_name), "all-unfiltered-coded-threads.csv")
  
  dx <- d %>% 
    group_by(status_id) %>% 
    summarize(code_string = toString(code))
  
  d_reduced_to_one <- dx %>% 
    separate(code_string, into = c("var1", "var2")) %>% 
    mutate(var = case_when(
      var1 == var2 ~ var1,
      is.na(var2) ~ var1,
      var1 == "TS" & var2 == "SB" ~ "SB",
      var1 == "SB" & var2 == "TS" ~ "SB",
      var1 == "OT" & var2 == "TS" ~ "TS",
      var1 == "OT" & var2 == "SB" ~ "SB",
      var1 == "TS" & var2 == "OT" ~ "TS",
      var1 == "SB" & var2 == "OT" ~ "SB",
      var1 == "TF" & var2 != "TF" ~ "TF",
      var1 != "TF" & var2 == "TF" ~ "TF",
      TRUE ~ 'NA'
    )) %>% 
    rename(code = var) %>% 
    dplyr::select(status_id, code)
  
  u <- users %>% 
    mutate(screen_name = tolower(screen_name))
  
  dj <- d %>% # d is ALL of the coded threads; dj w/ select vars
    dplyr::select(status_id, screen_name, ID, date, time)
  
  dj <- dj %>%
    mutate(screen_name = tolower(screen_name)) %>% 
    filter(screen_name %in% u$screen_name)
  
  do <- d_reduced_to_one %>% # dx is codes
    left_join(dj)
  
  do %>% 
    mutate(date_time = lubridate::ymd_hms(paste(date, time)))
  
}
