#-----------------------
# 1. setting up
#-----------------------

setwd("~/Dropbox/research/ngsschat") # change to local dir

options(stringsAsFactors = F)

# makes sure to install these with install.packages() if they aren't already installed

library(rvest)
library(dplyr)
library(stringr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(XML)
library(httr)

# library(plyr) # careful (this often masks other functions so plyr::func() is used instead)

#-----------------------
# 2. getting urls and parsing storifies # need to check how comprehensive the chats are
#-----------------------

urls <- read_html("https://ngsschat.wikispaces.com/") %>% # this is a wiki with links to the 51 storifies
      html_nodes("a") %>% # gets all anchor tags
      html_attr("href") # gets all links

urls

tmp_index <- grep("storify", urls)

storify_urls_all <- urls[tmp_index]

out_df <- data.frame(url = storify_urls_all)

for (i in 1:nrow(out_df)){
      split_url <- parse_url(out_df$url[i])
      out_df$url[i] <- paste0(as.character(split_url$scheme), "://", as.character(split_url$hostname), "/", as.character(split_url$path)) # not saving some urls (but still works for api_url)
      out_df$api_url[i] <- paste0("https://api.storify.com/v1/stories/", as.character(split_url$path)) # makes a url
      temp_dat <- fromJSON(out_df$api_url[i], simplifyDataFrame = T)
      out_df$num_tweets[i] <- (temp_dat$content$stats$elements$text + temp_dat$content$stats$elements$quote + temp_dat$content$stats$elements$image + temp_dat$content$stats$elements$video + temp_dat$content$stats$elements$link + temp_dat$content$stats$elements$other) # number of tweets in story
      out_df$n_pages[i] <- ceiling(out_df$num_tweets[i] / 50) # number of pages, to be used later
      out_df$date[i] <- str_split(temp_dat$content$date$created, "T")[[1]][1]
      print(paste0("Processed Storify ", i, " from ", out_df$date[i], " with ", out_df$num_tweets[i], " tweets"))
}

str(out_df)

View(out_df)

out_df <- out_df[56:60, ]

# write.csv(out_df, "~/dropbox/research/ngsschat/storify-meta.csv") to save out_df

#-----------------------
# 3. processing tweets from storifies
#-----------------------

# function for handling broken links by adding NA to tweet row

missing_func <- function(temp_link){
      out <- tryCatch({
            temp_vec <- read_html(temp_link)
            temp_vec <- html_nodes(temp_vec, ".tweet-text") # extracts tweet
            my_output_vec[k] <- html_text(temp_vec) # saves text to vector for page
      },
      error = function(cond){
            message(cond)
            my_output_vec[k] <- NA
      },
      finally = print(paste0("Parsing HTML for Storify ", i, " page ", j, ", tweet ", k))
      )
      return(out)
}

str(out_df)

for (i in (1:nrow(out_df))){ # this iterates through the storifies

      temp_page_num <- 1:out_df$n_pages[i] # temp_page_num
      temp_urls_to_process <- paste0(out_df$api_url[i], "?page=", temp_page_num, "&per_page=50") # temp_urls_to_process

      out_text <- list() # for output for text
      out_time <- list() # for output for time
      out_username <- list() # for output for username

      for (j in 1:length(temp_urls_to_process)){ # this selects one page of one storify
            temp_dat <- fromJSON(temp_urls_to_process[j])

            temp_link <- ifelse(grepl("http:", temp_dat$content$elements$permalink) | grepl("https:", temp_dat$content$elements$permalink),
                                temp_dat$content$elements$permalink, paste0("http:", temp_dat$content$elements$permalink)) # gets links and corrects broken links

            temp_link_time <- temp_dat$content$elements$posted_at

            my_output_vec <- vector() # makes vector for text output for page
            my_output_vec_time <- vector() # makes vector for text output for page

            for (k in 1:length(temp_link)){ # this processes one page of one storify
                  my_output_vec[k] <- missing_func(temp_link[k])

                  parsed_time <- temp_link_time[k] # timestamp

                  # these are all temps
                  date <- str_split(parsed_time, "T") # date is t1[[1]][1]
                  time <- str_split(date[[1]][2], "\\.") # time is time[[1]][1]
                  date_time <- paste0(date[[1]][1], " ", time[[1]][1])
                  my_output_vec_time[k] <- parse_date_time(date_time, "ymd hms", tz = "UST")
            }

            out_time[[j]] <- my_output_vec_time
            out_username[[j]] <- temp_dat$content$elements$source$username # username
            out_text[[j]] <- my_output_vec # text
      }

      out_data <- data.frame(time = .POSIXct(unlist(out_time)), username = unlist(out_username), text = unlist(out_text))
      write.csv(out_data, paste0("~/dropbox/research/ngsschat/data/new_data/", i, ".csv"), row.names = F) # change to local dir
}

# str(out_df)
# str(out_data)
# out_data$url <- out
# write.csv(out_data, "out_data.csv")

the_files <- list.files("data/new_data")
the_files

google_files <- the_files[stringr::str_detect(tolower(the_files), "ngsschat")]

google_files

out_files <- list()
for (i in 1:length(google_files)){
      out_files[[i]] <- read.csv(paste0("data/new_data/", google_files[i]), header = T)
      print(str(out_files[[i]]))
}

length(google_files)
length(out_files)

google_files
str(out_data)

file1 <- out_files[[1]]
str(file1)
file1 <- select(file1, time = X1, username = X.FredEnde, text = X.ehoffman.Welcome..Elissa...NGSSchat)
file1$time <- rep("10-6-16", nrow(file1))
file2 <- out_files[[2]]
str(file2)
file2 <- select(file2, time = X1, username = X.TdiShelton, text = Welcome.to..NGSSchat..We.re.so.excited.you.re.joining.us..Please.remember.to.include..NGSSchat.in.your.tweets.)
file2$time <- rep("11-3-16", nrow(file2))
file3 <- out_files[[3]]
str(file3)
file3 <- select(file3, time = X.FredEnde, username = X.FredEnde, text = Welcome.to.all.new..NGSSchat.particpants...Aloha.)
file3$time <- rep("7-21-16", nrow(file3))
file4 <- out_files[[4]]
str(file4)
file4 <- select(file4, time = Screen.Name, username = Screen.Name, text =Tweet.Text)
file4$time <- rep("7-7-16", nrow(file4))
file5 <- out_files[[5]]
str(file5)
file5 <- select(file5, time = X.FredEnde, username = X.FredEnde, text =Welcome.to..NGSSchat..We.re.so.excited.you.re.joining.us..Please.remember.to.include..NGSSchat.in.your.tweets.)
str(file5)
file5$time <- rep("8-18-16", nrow(file5))
file6 <- out_files[[6]]
str(file6)
file6 <- select(file6, time = X.FredEnde, username = X.FredEnde, text =Q1..In.education..how.would.you.define..feedback....NGSSchat)
file6$time <- rep("11-17-16", nrow(file6))
file7 <- out_files[[7]]
str(file7)
file7 <- select(file7, time = X9.16.2016, username = X.TdiShelton, text =Welcome.to..NGSSchat..We.re.so.excited.you.re.joining.us..Please.remember.to.include..NGSSchat.in.your.tweets.)
file7$time <- rep("9-15-16", nrow(file7))
file8 <- out_files[[8]]
str(file8)
file8 <- select(file8, time = Screen.Name, username = Screen.Name, text = Tweet.Text)
file8$time <- rep("6-17-16", nrow(file8))

# the new storify files

other_files <- the_files[!stringr::str_detect(tolower(the_files), "ngsschat")]
other_files

out_files <- list()
for (i in 1:length(other_files)){
      print(paste0("data/new_data/", other_files[i]))
      out_files[[i]] <- read.csv(paste0("data/new_data/", other_files[i]), header = T)
}

other_files_df <- plyr::ldply(out_files)
str(other_files_df)
other_files_df$time <- lubridate::ymd_hms(other_files_df$time)

# The mongo

df <- read.csv("~/Dropbox/research/ngsschat/data/all_storify_data.csv")
str(df)
df$time <- lubridate::mdy_hm(df$time)
df <- rbind(df, other_files_df)
df$time <- lubridate::round_date(df$time, unit = "day")

str(df)

# Merging files

the_list <- list(file1, file2, file3, file4, file5, file6, file7, file8)
the_df <- plyr::ldply(the_list)
str(the_df)
the_df$time <- lubridate::mdy(the_df$time)
table(the_df$time)

file_names <- dir("~/dropbox/research/ngsschat/data/new_data", pattern =".csv") # # change to local dir
out_ls <- list()
for (i in 1:length(file_names)){
      out_ls[[i]] <- read.csv(paste0("storify-data/", file_names[i]), header = T, stringsAsFactors = F)
}

dat <- plyr::ldply(out_ls) # need to have plyr installed
write.csv(dat, "all_storify_data_new.csv") # change to local dir
