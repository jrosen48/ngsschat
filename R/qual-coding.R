library(tidyverse)
library(googlesheets)

d <- read_rds("~/ngsschat-shiny/mxx.rds")

duu <- d %>% 
  filter(!(status_id %in% du$status_id),
         code %in% c("question", "answer")) %>% 
  mutate(code = "TS")

du <- read_rds("~/ngsschat-shiny/mxxx.rds")

dd <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQBjqZwruucVVv6Xtmrgdprm283DT4g-M9W1JHb9d0MKlYcQZyQvxG_Fq2JFhIstUYf7oBqNt3BjTtb/pub?output=csv',
               col_types = cols(
                 Coder = col_character(),
                 ID = col_character(),
                 Code = col_character(),
                 Notes = col_character(),
                 Flag = col_double()
               ))

#ddb <- dd

codes <- dd %>%
  filter(Flag == 1) %>% 
  mutate(ID = as.character(ID))

dd <- dd %>% 
  select(ID, code = Code)

# dd[2619:nrow(dd), "ID"] <- str_c("uc-", 1:1319)

dd[2619:nrow(dd), ]

d <- d %>% 
  filter(!(status_id %in% duu$status_id)) %>% 
  mutate(ID = as.character(as.integer(id_factor))) %>% 
  select(-code)

d <- d %>% 
  left_join(dd)

# codes %>% 
#   left_join(d) %>% 
#   distinct(ID, .keep_all = T) %>% 
#   count(ID, code) %>% 
#   mutate(ID = as.integer(ID)) %>% 
#   arrange(ID) %>% 
#   clipr::write_clip()

dub <- du %>%
  mutate(ID = str_c("uc-", 1:1319)) %>%
  select(-code) %>%
  left_join(dd)

d <- d %>% 
  filter(!(status_id %in% duu$status_id),
         !(status_id %in% dub$status_id)) %>% 
  bind_rows(duu) %>% 
  bind_rows(dub)

dx <- d %>% 
  group_by(status_id) %>% 
  summarize(code_string = toString(code))

dx <- dx %>% 
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
  select(status_id, code)

r <- read_rds('ngsschat-tweets-14-15.rds')

dx <- dx %>% 
  filter(status_id %in% r$status_id)

dj <- d %>% 
  select(status_id, screen_name, ID, date, time) %>% 
  distinct(status_id, .keep_all = TRUE)

do <- dx %>% 
  left_join(dj)

do %>% 
  mutate(date_time = lubridate::ymd_hms(paste(date, time))) %>% 
  write_csv("qual-coded-tweets.csv")
