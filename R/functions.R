create_time_series <- function(orig_all) {
  p <- orig_all %>% 
    filter(!is_retweet) %>% 
    ts_plot() +
    theme_bw() +
    xlab("Day") +
    ylab("Number of Original Tweets") +
    xlim(c(as.POSIXct(as.Date("2012-01-01")), as.POSIXct(as.Date("2017-12-31")))) +
    geom_rect(aes(xmin = as.POSIXct(as.Date(c("2015-08-01"))), 
                  xmax =
                    as.POSIXct(as.Date(c("2016-07-31"))),
                  ymin = 0,
                  ymax = 925),
              fill = "gray55", color = "gray55", alpha = 0.025) +
    geom_rect(aes(xmin = as.POSIXct(as.Date(c("2014-07-31"))), 
                  xmax = as.POSIXct(as.Date(c("2015-07-31"))),
                  ymin = 0,
                  ymax = 925),
              fill = "gray90", alpha = .025) +
    theme(text = element_text(family = "Times"))
  
  ggsave("img/ngsschat-n-tweets.png", width = 7, height = 5)
  
  p
}

create_location_plot_and_return_users <- function(l, state_data) {
  
  US <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  
  s <- st_join(l, US)
  
  states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  states <- cbind(states, st_coordinates(st_centroid(states)))
  states$ID <- tools::toTitleCase(states$ID)
  
  sel_sgbp <- st_within(l, US)
  sel_sgbp_l <- unlist(sel_sgbp)
  
  s
  
  states$index <- 1:49
  
  counts <- sel_sgbp_l %>%
    table() %>% 
    as_tibble() %>% 
    set_names(c("index", "n")) %>% 
    mutate(index = as.integer(index))
  
  states <- states %>% 
    left_join(counts)
  
  state_data <- state_data %>% 
    dplyr::select(-1) %>% 
    rename(ID = State) %>% 
    dplyr::select(ID, Early:Not)
  
  state_data <- state_data %>% 
    mutate(adoption = case_when(
      Early_star == 1 ~ 2,
      Early== 1 ~1,
      Near_star == 1 ~4,
      Near == 1 ~ 3,
      # Late_star == 1 ~ 6,
      # Late == 1 ~ 5,
      Not == 1 ~ 7
    ))
  
  states <- states %>% 
    left_join(state_data)
  
  states_points <- st_centroid(states)
  
  states_points$n <- ifelse(is.na(states_points$n), 0, states$n)
  
  p <- ggplot(states) +
    geom_sf(aes(fill = as.factor(adoption))) +
    ggrepel::geom_label_repel(
      data = states_points,
      aes(label = n, geometry = geometry),
      stat = "sf_coordinates",
      point.padding = 0,
      min.segment.length = .025,
      family = "Times",
      size = 6
    ) +
    coord_sf(crs = st_crs(3347), default =TRUE) +
    theme_void() +
    scale_fill_manual(NULL, values = c('#1f78b4','#a6cee3',
                                       '#33a02c','#b2df8a',
                                       'white'),
                      labels = c("Early NGSS", "Early NGSS-aligned", "Late NGSS", "Late NGSS-aligned", "N/A")) +
    theme(legend.position = "top", text = element_text(family = "Times", size = 22)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("img/adoption-over-time.png", width = 11, height = 11)
  
  states
}

create_sociogram <- function(edge, users) {
  
  # Conversing
  edge_ss <- filter(edge, 
                    tolower(sender) %in% tolower(users$screen_name) & tolower(receiver) %in% tolower(users$screen_name)) %>% 
    mutate(sender = tolower(sender),
           receiver = tolower(receiver)) %>% 
    filter(n_tweets_sender >= 10,
           n_tweets_receiver >= 10,
           sender != "Other",
           receiver != "Other") %>% 
    dplyr::select(sender, receiver, interaction_type) %>% 
    filter(interaction_type == "conversing")
  
  users_ss <- users %>% 
    mutate(screen_name = tolower(screen_name)) %>% 
    filter(n_tweets >= 10) %>% 
    dplyr::select(screen_name, n_days, n_tweets, years_on_twitter, group) %>% 
    mutate(group = ifelse(group == "Unclear", "Other", group))
  
  weights <- edge_ss %>% 
    count(sender, receiver) %>% 
    rename(weight = n) 
  
  edge_ss <- edge_ss %>% 
    left_join(weights, by = c("sender", "receiver"))
  
  graph <- tidygraph::tbl_graph(users_ss, edge_ss, directed = FALSE)
  
  # plot using ggraph
  p1 <- graph %>% 
    mutate(Popularity = tidygraph::centrality_degree(mode = 'in')) %>% 
    ggraph(layout = 'kk') + 
    geom_edge_link(alpha = .01, 
                   arrow = arrow(length = unit(2, 'mm')),
                   end_cap = circle(2.5, 'mm')) +
    geom_node_point(aes(shape = group, size = Popularity)) +
    theme_graph() +
    scale_size_continuous(guide = FALSE) +
    scale_shape(NULL, 
                guide = guide_legend(override.aes = list(size=5))) +
    theme(legend.position = 'top') +
    ggtitle("Conversations") +
    theme(text = element_text(family = 'Times', size = 20),
          plot.title = element_text(family = 'Times', hjust = .5))
  
  p1
  
  ggsave("img/conversations.png", width = 8, height = 6)
}

create_descriptive_stats <- function(users, orig, states) {
  
  users <- users %>% 
    mutate(group = ifelse(group == "Unclear", "Other", group))
  
  users_ss <- users %>% 
    dplyr::select(screen_name, 
           group, n_tweets,years_on_twitter)
  
  orig <- dplyr::select(orig, -user_id)
  
  n_tweeters_by_group <- orig %>% 
    left_join(users_ss) %>% 
    filter(!is_retweet) %>% 
    count(group, screen_name) %>% 
    count(group) %>% 
    rename(n_tweeters = n) %>% 
    mutate(n_prop = n_tweeters / sum(n_tweeters)) %>% 
    arrange(desc(n_tweeters))
  
  n_tweets_by_group <- orig %>% 
    left_join(users_ss) %>% 
    filter(!is_retweet) %>% 
    count(group, screen_name) %>% 
    group_by(group) %>% 
    summarize(sum_n_tweets = sum(n))
  
  n_years_by_group <- orig %>% 
    left_join(users_ss) %>% 
    filter(!is_retweet) %>% 
    group_by(group) %>% 
    summarize(mean_years = mean(years_on_twitter),
              sd_years= sd(years_on_twitter),
              mean_sd_years = str_c(round(mean_years, 3), " (", round(sd_years, 3), ")"))
  
  # this is individual engagement by group - probably what we want to report
  fin_df <- orig %>% 
    filter(!is_retweet) %>% 
    left_join(users) %>% 
    semi_join(users) %>% 
    count(group, screen_name) %>% 
    group_by(group) %>% 
    summarize(median_n_tweets = median(n),
              mean_n_tweets = mean(n), 
              sd_n_tweets = sd(n)) %>% 
    right_join(n_tweeters_by_group) %>% 
    right_join(n_tweets_by_group) %>% 
    right_join(n_years_by_group) %>% 
    arrange(desc(sum_n_tweets)) %>% 
    mutate_if(is.numeric, round, 3) %>%
    mutate(mean_sd_tweets = str_c(mean_n_tweets, " (", sd_n_tweets, ")")) %>% 
    dplyr::select(group, n_tweeters, sum_n_tweets, median_n_tweets, mean_sd_tweets, mean_sd_years)
  
  fin_df %>% 
    summarize_if(is.numeric, sum)
  fin_df %>% 
    summarize_if(is.numeric, mean)
  fin_df %>% 
    summarize_if(is.numeric, sd)
  
  fin_df
}

proc_users_data_for_locations <- function(users, l, states) {
  US <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  
  lint <- st_within(l, US) %>% as.integer()
  
  states$index <- 1:49
  
  ind_counts <- lint %>%
    as_tibble() %>% 
    set_names("index")
  
  counts <- states %>% 
    dplyr::select(index, ID) %>% 
    right_join(ind_counts) %>% 
    dplyr::select(index, ID)
  
  users$state <- counts$ID
  
  users <- states %>% 
    dplyr::select(ID, adoption) %>% 
    rename(state = ID) %>%
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    right_join(users) %>% 
    mutate(adoption_cont = case_when(
      adoption <= 2 ~ 1,
      adoption <= 4 ~ 2,
      adoption <= 6 ~ 3,
      adoption == 7 ~ 4
    ),
    adoption_tri = case_when(
      adoption <= 2 ~ "early",
      # adoption <= 4 ~ "middle",
      adoption <= 6 ~ "late",
      adoption == 7 ~ "not",
      is.na(adoption) ~ "missing"
    ),
    adoption_tri = factor(adoption_tri, levels = c("not", "early", "late", "missing")))
  
  users
}


prepare_for_influence <- function(orig_pre, orig_post, users, edge) {
  n_days <- orig_pre %>% 
    mutate(screen_name = tolower(screen_name)) %>% 
    filter(!is_retweet) %>% 
    count(screen_name, day) %>% 
    count(screen_name) %>% 
    dplyr::select(screen_name, pre_n_days = n)
  
  orig_prep <- orig_pre %>% 
    mutate(screen_name = tolower(screen_name)) %>% 
    filter(!is_retweet) %>% 
    count(screen_name) %>% 
    dplyr::select(screen_name, pre_n = n) %>% 
    left_join(n_days)
  
  n_days <- orig_post %>% 
    mutate(screen_name = tolower(screen_name)) %>% 
    filter(!is_retweet) %>% 
    count(screen_name, day) %>% 
    count(screen_name) %>% 
    dplyr::select(screen_name, post_n_days = n) 
  
  orig_postp <- orig_post %>% 
    mutate(screen_name = tolower(screen_name)) %>% 
    filter(!is_retweet) %>% 
    count(screen_name) %>% 
    dplyr::select(screen_name, post_n = n) %>% 
    left_join(n_days)
  
  d_for_influence <- users %>% 
    left_join(orig_prep) %>% 
    left_join(orig_postp) %>% 
    distinct(screen_name, .keep_all = TRUE) %>% 
    dplyr::select(screen_name, pre_n, pre_n_days, n_tweets, post_n, post_n_days) %>% 
    filter(n_tweets > 1) %>% 
    mutate_all(replace_na, 0)
  
  orig_pre <- rename(orig_pre, sender = screen_name)
  
  edge <- edge %>% 
    mutate(sender = tolower(sender),
           receiver = tolower(receiver))
  
  influence_endorsing <- edge %>% 
    filter(interaction_type == "endorsing") %>% 
    count(sender, receiver) %>% 
    left_join(rename(orig_prep, sender = screen_name)) %>% 
    mutate(exposure = n * pre_n) %>% 
    group_by(receiver) %>% 
    summarize(exposure_sum_end = sum(exposure, na.rm = TRUE)) %>% 
    rename(screen_name = receiver) %>% 
    right_join(d_for_influence) %>% 
    mutate(exposure_sum_end = replace_na(exposure_sum_end, 0)) %>% 
    left_join(users) %>% 
    mutate(group= ifelse(group %in% c("Other", "Unclear", "Uncoded"), "Other", group))
  
  influence_endorsing_n <- edge %>% 
    filter(interaction_type == "endorsing") %>% 
    count(sender, receiver) %>% 
    left_join(rename(orig_prep, sender = screen_name)) %>% 
    mutate(exposure = n) %>% 
    group_by(receiver) %>% 
    summarize(exposure_sum_end = sum(exposure, na.rm = TRUE)) %>% 
    rename(screen_name = receiver) %>% 
    right_join(d_for_influence) %>% 
    mutate(exposure_sum_end = replace_na(exposure_sum_end, 0)) %>% 
    left_join(users) %>% 
    mutate(group= ifelse(group %in% c("Other", "Unclear", "Uncoded"), "Other", group))
  
  influence_conversing <- edge %>% 
    filter(interaction_type == "conversing") %>% 
    count(sender, receiver) %>% 
    left_join(rename(orig_prep, sender = screen_name)) %>% 
    mutate(exposure = n * pre_n) %>% 
    group_by(receiver) %>% 
    summarize(exposure_sum_conv = sum(exposure, na.rm = TRUE)) %>% 
    rename(screen_name = receiver) %>% 
    right_join(d_for_influence) %>% 
    mutate(exposure_sum_conv = replace_na(exposure_sum_conv, 0)) %>% 
    left_join(users) %>% 
    mutate(group= ifelse(group %in% c("Other", "Unclear", "Uncoded"), "Other", group))
  
  influence_conversing_n <- edge %>% 
    filter(interaction_type == "conversing") %>% 
    count(sender, receiver) %>% 
    left_join(rename(orig_prep, sender = screen_name)) %>% 
    mutate(exposure = n) %>% 
    group_by(receiver) %>% 
    summarize(exposure_sum_conv = sum(exposure, na.rm = TRUE)) %>% 
    rename(screen_name = receiver) %>% 
    right_join(d_for_influence) %>% 
    mutate(exposure_sum_conv = replace_na(exposure_sum_conv, 0)) %>% 
    left_join(users) %>% 
    mutate(group= ifelse(group %in% c("Other", "Unclear", "Uncoded"), "Other", group))
  
  sum_conv <- edge %>% 
    filter(interaction_type == "conversing") %>% 
    count(sender, receiver) %>% 
    group_by(receiver) %>% 
    summarize(sum_conv = sum(n))
  
  sum_end <- edge %>% 
    filter(interaction_type == "endorsing") %>% 
    count(sender, receiver) %>% 
    group_by(receiver) %>% 
    summarize(sum_end = sum(n))
  
  influence <- influence_endorsing %>% 
    left_join(influence_conversing) %>% 
    left_join(users) %>% 
    mutate(group = ifelse(group %in% c("Other", "Unclear", "Uncoded"), "Other", group))
  
  influence$group <- fct_relevel(as.factor(influence$group), "Other")
  
  influence_n <- influence_endorsing_n %>% 
    left_join(influence_conversing_n) %>% 
    left_join(users) %>% 
    mutate(group = ifelse(group %in% c("Other", "Unclear", "Uncoded"), "Other", group))
  
  influence_n$group <- fct_relevel(as.factor(influence_n$group), "Other")
  
  influence
}

fix_codes <- function(coded_threads) {
  # for the two tweets codes that didn't get coded correctly, somehow
  coded_threads <- mutate(coded_threads,
                          code = if_else(is.na(code), "SB", code))
  coded_threads
}

prepare_coded_threads <- function(coded_threads, influence) {

  coded_threads <- coded_threads %>% 
    filter(!str_detect(ID, "uc"))
  
  coded_threads <- coded_threads %>% 
    arrange(ID, date_time)
  
  coded_threads %>% count(code)
  coded_threads$status_id <- as.character(coded_threads$status_id)
  
  coded_threads <- coded_threads %>% 
    mutate(code = ifelse(is.na(code), "TS", code))
  
  coded_threads <- coded_threads %>% 
    mutate(screen_name = tolower(screen_name)) %>% 
    left_join(dplyr::select(influence, screen_name, n_tweets, adoption_tri, group))
  
  safe_log <- function(x) {
    ifelse(x == 0, 0, log(x))
  }
  
  sum_n_tweets <- coded_threads %>% 
    group_by(ID) %>% 
    summarize(sum_n_tweets = sum(safe_log(n_tweets)))
  
  coded_threads <- coded_threads %>% 
    left_join(sum_n_tweets) %>% 
    mutate(sum_n_tweets = ifelse(is.na(ID), n_tweets, sum_n_tweets))
  
  coded_threads <- coded_threads %>% 
    mutate(ID = if_else(is.na(ID), str_c("qa-", as.character(row_number())), ID))
  
  coded_threads %>% 
    janitor::tabyl(code) %>% 
    arrange(desc(n))
  
  tm <-coded_threads %>% 
    janitor::tabyl(code) %>% 
    arrange(desc(n))
  
  cst <- chisq.test(tm$n)
  
  coded_threads
}

calc_thread_stats <- function(coded_threads, influence) {
  
  # which groups participated
  n_groups <- coded_threads %>% 
    count(ID, group, code) %>% 
    count(ID, group, code) %>% 
    group_by(ID, code) %>% 
    summarize(n_groups = sum(n))
  
  coded_threads %>% 
    count(ID, group, code) %>% 
    count(ID, group, code) %>% 
    spread(group, n, fill = 0) %>% 
    dplyr::select(ID:Teacher) %>% 
    gather(key, val, -ID, -code) %>% 
    group_by(code, key) %>% 
    summarize(mean_val = mean(val)) %>% 
    spread(key, mean_val) %>% 
    ungroup() %>% 
    dplyr::select(Code = code, Teacher, Administrator, Researcher, Organization, Other) %>% 
    filter(Code != "RT") %>% clipr::write_clip()
  
  tm <- coded_threads %>% 
    count(ID, group, code) %>% 
    count(ID, group, code) %>% 
    group_by(ID, code) %>% 
    summarize(sum_n_groups = sum(n))
  
  tm$code <- as.factor(tm$code)
  m <- glm(sum_n_groups ~ code, data = tm, family = "poisson")
  g <- glht(m, mcp(code="Tukey"))
  summary(m)
  summary(g)
  
  # length 2 - same res
  sum_n_groups <- coded_threads %>% 
    count(ID, status_id, code) %>% 
    count(ID, status_id, code) %>% 
    group_by(ID, code) %>% 
    summarize(sum_n_groups = sum(n))
  
  coded_threads %>% 
    count(ID, status_id, code) %>% 
    count(ID, status_id, code) %>% 
    group_by(ID, code) %>% 
    summarize(sum_n_groups = sum(n)) %>% 
    group_by(code) %>% 
    summarize(mean_n = mean(sum_n_groups),
              sd_n = sd(sum_n_groups))
  
  tm <- coded_threads %>% 
    count(ID, status_id, code) %>% 
    count(ID, status_id, code) %>% 
    group_by(ID, code) %>% 
    summarize(sum_n_groups = sum(n))
  
  tm$code <- as.factor(tm$code)
  m <- glm(sum_n_groups ~ code, data = tm, family = "poisson")
  g <- glht(m, mcp(code="Tukey"))
  summary(m)
  summary(g)
  
  # m individuals participated
  ind_participated <- coded_threads %>% 
    count(ID, screen_name, code) %>% 
    count(ID, screen_name, code) %>% 
    group_by(ID, code) %>% 
    summarize(ind_participated = sum(n))
  
  coded_threads %>% 
    count(ID, screen_name, code) %>% 
    count(ID, screen_name, code) %>% 
    group_by(ID, code) %>% 
    summarize(sum_n_groups = sum(n)) %>% 
    group_by(code) %>% 
    summarize(mean_n = mean(sum_n_groups),
              sd_n = sd(sum_n_groups))
  
  tm <- coded_threads %>% 
    count(ID, screen_name, code) %>% 
    count(ID, screen_name, code) %>% 
    group_by(ID, code) %>% 
    summarize(sum_n_groups = sum(n))
  
  tm$code <- as.factor(tm$code)
  m <- glm(sum_n_groups ~ code, data = tm, family = "poisson")
  g <- glht(m, mcp(code="Tukey"))
  summary(m)
  summary(g)
  
  # m groups participated
  n_groups <- coded_threads %>% 
    count(ID, group, code) %>% 
    count(ID, group, code) %>% 
    group_by(ID, code) %>% 
    summarize(n_groups = sum(n))
  
  coded_threads %>% 
    count(ID, group, code) %>% 
    count(ID, group, code) %>% 
    group_by(ID, code) %>% 
    summarize(sum_n_groups = sum(n)) %>% 
    group_by(code) %>% 
    summarize(mean_n = mean(sum_n_groups),
              sd_n = sd(sum_n_groups))
  
  tm <- coded_threads %>% 
    count(ID, group, code) %>% 
    count(ID, group, code) %>% 
    group_by(ID, code) %>% 
    summarize(sum_n_groups = sum(n))
  
  tm$code <- as.factor(tm$code)
  m <- glm(sum_n_groups ~ code, data = tm, family = "poisson")
  g <- glht(m, mcp(code="Tukey"))
  summary(m)
  summary(g)
  
  # n adoption statuses
  n_adoption <-
    coded_threads %>% 
    count(ID, adoption_tri, code) %>% 
    count(ID, adoption_tri, code) %>% 
    group_by(ID, code) %>% 
    summarize(n_adoption = sum(n))
  
  coded_threads %>% 
    count(ID, adoption_tri, code) %>% 
    count(ID, adoption_tri, code) %>% 
    group_by(ID, code) %>% 
    summarize(sum_n_groups = sum(n)) %>% 
    group_by(code) %>% 
    summarize(mean_n = mean(sum_n_groups),
              sd_n = sd(sum_n_groups))
  
  tm <- coded_threads %>% 
    count(ID, adoption_tri, code) %>% 
    count(ID, adoption_tri, code) %>% 
    group_by(ID, code) %>% 
    summarize(sum_n_groups = sum(n))
  
  tm$code <- as.factor(tm$code)
  m <- glm(sum_n_groups ~ code, data = tm, family = "poisson")
  g <- glht(m, mcp(code="Tukey"))
  summary(m)
  summary(g)
  
  # n individuals participated in at least one
  one_or_more <- function(x) {
    sum(if_else(x > 0, 1, 0))
  }
  
  coded_threads %>% 
    count(screen_name, code) %>% 
    spread(code, n, fill = 0) %>% 
    semi_join(influence) %>% 
    summarize_if(is.numeric, one_or_more) %>% 
    gather(key, val) %>% 
    mutate(prop = val / 247)
  
  tm <- coded_threads %>% 
    count(screen_name, code) %>% 
    spread(code, n, fill = 0) %>% 
    semi_join(influence) %>% 
    summarize_if(is.numeric, one_or_more) %>% 
    gather(key, val)
  
  cst <- chisq.test(tm$val)
  cst
  tm
  cst$stdres
}

prep_influence_for_modeling <- function(coded_threads, influence) {
  length_of_thread <- coded_threads %>% 
    count(ID) %>% 
    rename(length_of_ind_thread = n)
  
  coded_tweets <- coded_threads %>% 
    count(screen_name, code) %>% 
    spread(code, n, fill = 0) %>% 
    dplyr::select(screen_name, SB, TF, TS) %>% 
    mutate(screen_name=tolower(screen_name))
  
  safe_log <- function(x) {
    ifelse(x == 0, 0, log(x))
  }
  
  coded_tweets_sum <- coded_threads %>% 
    left_join(length_of_thread) %>% 
    #mutate(mean_n_tweets = sum_n_tweets) %>% 
    mutate(mean_n_tweets = sum_n_tweets/length_of_ind_thread) %>% 
    group_by(screen_name, code) %>% 
    summarize(n = sum(mean_n_tweets)) %>% 
    spread(code, n, fill = 0) %>% 
    ungroup() %>% 
    dplyr::select(screen_name, SB, TF, TS, OT) %>% 
    mutate(screen_name=tolower(screen_name)) %>% 
    set_names(c("screen_name", "SB_sum", "TF_sum", "TS_sum", "OT_sum"))
  
  # coded_threads <- coded_threads %>% 
  #   left_join(length_of_thread)
  # 
  # coded_threads %>% 
  #   mutate(mean_n_tweets = sum_n_tweets/length_of_ind_thread)
  
  # coded_tweets_sum <- coded_threads %>%
  #   group_by(screen_name, code, ID) %>% 
  #   summarize(sum_sum_n_tweets = first(sum_n_tweets)) %>% 
  #   #summarize(sum_sum_n_tweets = sum(sum_sum_n_tweets)) %>% 
  #   spread(code, sum_sum_n_tweets, fill = 0) %>%
  #   dplyr::select(screen_name, SB, TF, TS) %>%
  #   ungroup() %>% 
  #   mutate(screen_name=tolower(screen_name))
  
  influence <- influence %>%
    left_join(coded_tweets, by = "screen_name") %>% 
    left_join(coded_tweets_sum, by = "screen_name")
}


