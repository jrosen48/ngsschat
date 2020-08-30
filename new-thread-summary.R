params <-
  list(coded_threads = "", influence = "")

## ----setup, include=FALSE, eval = FALSE---------------------------------------------
## knitr::opts_chunk$set(echo = TRUE, warning=FALSE)
## coded_threads <- params$coded_threads
## influence <- params$influence


## ---- eval = FALSE------------------------------------------------------------------
## coded_threads <- drake::readd(proc_coded_threads)
## influence <- drake::readd(influence)


## -----------------------------------------------------------------------------------
library(tidyverse)
library(cowplot)
# 11127 tweets in threads; 8,384 unique tweets

d <- read_csv("all-unfiltered-coded-threads.csv", col_types = cols(ID = col_character()))
d %>% filter(is.na(ID))

u <- read_csv('data/users-to-analyze.csv') %>% 
  select(-code)

i <- drake::readd(influence) %>% 
  select(screen_name, adoption_tri)

d <- d %>% mutate(screen_name = tolower(screen_name))
dd <- d %>% 
  left_join(u) %>% 
  left_join(i)

dd <- dd %>% 
  mutate(group = if_else(is.na(group), "Unclear", group))

dd <- dd %>% 
  mutate(group = if_else(group == "Unclear", "Other", group))

dd <- dd %>% 
  filter(code != "RT")


## -----------------------------------------------------------------------------------
f <- function(edgelist, users_data_from_rtweet){
  users_prepped <- dplyr::mutate(users_data_from_rtweet,
                                 screen_name = tolower(screen_name)
  )
  users_prepped <- dplyr::select(users_prepped,
                                 screen_name,
                                 tidyselect::everything()
  )
  users_prepped <- dplyr::distinct(users_prepped,
                                   screen_name, .keep_all = TRUE)
  senders_prepped <- dplyr::mutate(edgelist,
                                   screen_name = tolower(sender)
  )
  
  ## edit all sender variable names to have "_sender" in them
  names(users_prepped)[2:length(users_prepped)] <-
    stringr::str_c(names(users_prepped), "_sender")[2:length(users_prepped)]
  edgelist_with_senders_data <- dplyr::left_join(senders_prepped,
                                                 users_prepped,
                                                 by = "screen_name")
  
  ## change the name of screen_name back to sender
  receivers_prepped <- dplyr::mutate(edgelist_with_senders_data,
                                     sender = screen_name,
                                     screen_name = tolower(receiver)
  )
  
  ## would be nice to not have to do this again! (it is because of the names issue - an easy fix)
  users_prepped <- dplyr::mutate(users_data_from_rtweet,
                                 screen_name = tolower(screen_name)
  )
  users_prepped <- dplyr::select(users_prepped,
                                 screen_name,
                                 tidyselect::everything()
  )
  users_prepped <- dplyr::distinct(users_prepped,
                                   screen_name, .keep_all = TRUE)
  
  ## edit all sender variable names to have "_receiver" in them
  names(users_prepped)[2:length(users_prepped)] <-
    stringr::str_c(names(users_prepped), "_receiver")[2:length(users_prepped)]
  
  edgelist_with_all_users_data <- dplyr::left_join(receivers_prepped,
                                                   users_prepped,
                                                   by = "screen_name")
  edgelist_with_all_users_data <- dplyr::select(edgelist_with_all_users_data, -screen_name)
  edgelist_with_all_users_data
}

tm<-dd %>% 
  filter(!(str_detect(ID, "uc") | str_detect(ID, "qa")))

create_reply_edges <- function(d) {
  o <- d %>% 
    select(receiver = screen_name) %>% 
    mutate(sender = lead(receiver),
           receiver = tolower(receiver),
           sender = tolower(sender)) 
  
  o %>% 
    filter(!is.na(sender)) %>% 
    select(sender, receiver)
}

tm

tm$group<- ifelse(tm$group == "Unclear", "Other", tm$group)

ts <- tm %>% 
  filter(code == "TS") %>% split(.$ID)
tf <- tm %>% 
  filter(code == "TF")%>% split(.$ID)
sb <- tm %>% 
  filter(code == "SB")%>% split(.$ID)
ot <- tm %>% 
  filter(code == "OT")%>% split(.$ID)

sbtf <- tm %>% 
  filter(code == "SB" | code == "TF") %>% split(.$ID)

pts <- map_df(ts, create_reply_edges) %>% f(u) %>% mutate(var = "ts")
ptf <- map_df(tf, create_reply_edges) %>% f(u) %>% mutate(var = "tf")
psb <- map_df(sb, create_reply_edges) %>% f(u) %>% mutate(var = "sb")
pot <- map_df(ot, create_reply_edges) %>% f(u) %>% mutate(var = "ot")

#pot <- map_df(sbtf, create_reply_edges) %>% f(u) %>% mutate(var = "sbtf")

edgelist <- bind_rows(pts, ptf, psb, pot)

write_csv(edgelist, "edgelist.csv")

uu <- filter(u, n_tweets >= 1)
uu$group<- ifelse(u$group == "Unclear", "Other", u$group)

u <- filter(u, n_tweets >= 10)
u$group<- ifelse(u$group == "Unclear", "Other", u$group)

library(tidygraph)
library(ggraph)
library(igraph)

graph_from_data_frame(pts[, 1:2]) %>% 
  degree(mode = "in") %>% sd()

graph_from_data_frame(pts[, 1:2]) %>% graph.density()

e <- select(pts, sender, receiver) %>% filter(sender %in% u$screen_name & receiver %in% u$screen_name)
ptsg <- tbl_graph(select(u, name = screen_name, group), edges= e)
g <- tidygraph::as.igraph(ptsg)
g <- igraph::delete.vertices(g, degree(g)==0)
ptsg <- as_tbl_graph(g)

p <- select(ptf, sender, receiver) %>% filter(!(sender %in% uu$screen_name & receiver %in% uu$screen_name))
p <- p %>%
  gather(key, val) %>%
  select(screen_name = val)

e <- select(ptf, sender, receiver) %>% filter(sender %in% uu$screen_name & receiver %in% uu$screen_name)
ptfg <- tbl_graph(nodes = select(uu, name = screen_name, group), e)
g <- tidygraph::as.igraph(ptfg)
g <- igraph::delete.vertices(g, degree(g)==0)
ptfg <- as_tbl_graph(g)

e <- select(pot, sender, receiver) %>% filter(sender %in% uu$screen_name & receiver %in% uu$screen_name)
potg <- tbl_graph(nodes = select(uu, name = screen_name, group), e)
g <- tidygraph::as.igraph(potg)
g <- igraph::delete.vertices(g, degree(g)==0)
potg <- as_tbl_graph(g)

e <- select(psb, sender, receiver) %>% filter(sender %in% u$screen_name & receiver %in% u$screen_name)
psbg <- tbl_graph(nodes = select(u, name = screen_name, group), e)
g <- tidygraph::as.igraph(psbg)
g <- igraph::delete.vertices(g, degree(g)==0)
psbg <- as_tbl_graph(g)

e <- select(pts, sender, receiver) %>% filter(sender %in% u$screen_name & receiver %in% u$screen_name)
ptsg <- tbl_graph(nodes = select(uu, name = screen_name, group), e)
g <- tidygraph::as.igraph(ptsg)
g <- igraph::delete.vertices(g, degree(g)==0)
ptsg <- as_tbl_graph(g)

## -----------------------------------------------------------------------------------
p1 <- ptsg %>%
  mutate(Popularity = tidygraph::centrality_degree(mode = 'in'),
         Popularity = Popularity/2) %>%
  ggraph::ggraph(layout = "kk") +
  geom_edge_link(alpha = .075, 
                 arrow = arrow(length = unit(2, 'mm')),
                 end_cap = circle(2.5, 'mm')) +
  geom_node_point(aes(shape = group, size = Popularity, color = group)) +
  theme_graph() +
  scale_size_continuous(guide = FALSE) +
  scale_shape_discrete("", breaks = c("Teacher", "Administrator", "Researcher", "Organization", "Other"),
                       limits = c("Teacher", "Administrator", "Researcher", "Organization", "Other")) +
  scale_color_brewer("", type = "qual", palette = 2, breaks = c("Teacher", "Administrator", "Researcher", "Organization", "Other"),
                     limits = c("Teacher", "Administrator", "Researcher", "Organization", "Other")) +
  ggtitle("Transactional") +
  theme(text = element_text(family = 'Times', size = 15),
        plot.title = element_text(family = 'Times', hjust = .5),
        legend.direction = "horizontal",
        legend.text=element_text(size=13)) +
  scale_color_manual("", values = c('#2e242c', '#b45b1f', '#1fb45b', '#b41f78', '#1f78b4'),
                     breaks = c("Teacher", "Administrator", "Researcher", "Organization", "Other"),
                     limits = c("Teacher", "Administrator", "Researcher", "Organization", "Other"))

p2 <- psbg %>% 
  mutate(Popularity = tidygraph::centrality_degree(mode = 'in'),
         Popularity = Popularity/2) %>%
  ggraph::ggraph(layout = "kk") +
  geom_edge_link(alpha = .025, 
                 arrow = arrow(length = unit(2, 'mm')),
                 end_cap = circle(2.5, 'mm')) +
  geom_node_point(aes(shape = group, size = Popularity, color = group)) +
  theme_graph() +
  scale_size_continuous(guide = FALSE) +
  scale_shape_discrete("", breaks = c("Teacher", "Administrator", "Researcher", "Organization", "Other"),
                       limits = c("Teacher", "Administrator", "Researcher", "Organization", "Other")) +
  scale_color_brewer("", type = "qual", palette = 2, breaks = c("Teacher", "Administrator", "Researcher", "Organization", "Other"),
                     limits = c("Teacher", "Administrator", "Researcher", "Organization", "Other")) +
  ggtitle("Sense-making") +
  theme(text = element_text(family = 'Times', size = 15),
        plot.title = element_text(family = 'Times', hjust = .5)) +
  scale_color_manual("", values = c('#2e242c', '#b45b1f', '#1fb45b', '#b41f78', '#1f78b4'),
                     breaks = c("Teacher", "Administrator", "Researcher", "Organization", "Other"),
                     limits = c("Teacher", "Administrator", "Researcher", "Organization", "Other"))

p3 <- ptfg %>% 
  mutate(Popularity = tidygraph::centrality_degree(mode = 'in'),
         Popularity = Popularity/2) %>%
  ggraph::ggraph(layout = "kk") +
  geom_edge_link(alpha = .15, 
                 arrow = arrow(length = unit(2, 'mm')),
                 end_cap = circle(2.5, 'mm')) +
  geom_node_point(aes(shape = group, size = Popularity, color = group)) +
  theme_graph() +
  scale_size_continuous(guide = FALSE) +
  scale_shape_discrete("", breaks = c("Teacher", "Administrator", "Researcher", "Organization", "Other"),
                       limits = c("Teacher", "Administrator", "Researcher", "Organization", "Other")) +
  scale_color_brewer("", type = "qual", palette = 2, breaks = c("Teacher", "Administrator", "Researcher", "Organization", "Other"),
                     limits = c("Teacher", "Administrator", "Researcher", "Organization", "Other")) +
  ggtitle("Transformational") +
  theme(text = element_text(family = 'Times', size = 15),
        plot.title = element_text(family = 'Times', hjust = .5)) + 
  scale_color_manual("", values = c('#2e242c', '#b45b1f', '#1fb45b', '#b41f78', '#1f78b4'),
                     breaks = c("Teacher", "Administrator", "Researcher", "Organization", "Other"),
                     limits = c("Teacher", "Administrator", "Researcher", "Organization", "Other"))

#   mutate(Popularity = tidygraph::centrality_degree(mode = 'in'),
#          Popularity = Popularity/2) %>%
#   ggraph::ggraph(layout = "kk") +
#   geom_edge_link(alpha = .15,
#                  arrow = arrow(length = unit(2, 'mm')),
#                  end_cap = circle(2.5, 'mm')) +
#   geom_node_point(aes(shape = group, size = Popularity, color = group)) +
#   theme_graph() +
#   scale_size_continuous(guide = FALSE) +
#   scale_shape_discrete("", breaks = c("Teacher", "Administrator", "Researcher", "Organization", "Other"),
#                        limits = c("Teacher", "Administrator", "Researcher", "Organization", "Other"), guide = F) +
#   scale_color_brewer("", type = "qual", palette = 2, breaks = c("Teacher", "Administrator", "Researcher", "Organization", "Other"),
#                      limits = c("Teacher", "Administrator", "Researcher", "Organization", "Other"),guide = F) +
#   ggtitle("Off-topic Conversations") +
#   theme(text = element_text(family = 'Times', size = 15),
#         plot.title = element_text(family = 'Times', hjust = .5)) +
#   theme(legend.position = "bottom")

## -----------------------------------------------------------------------------------

library(cowplot)
legend <- get_legend(p1)

prow <- plot_grid(p1 + theme(legend.position = "none"), 
                  p2 + theme(legend.position = "none"), 
                  p3 + theme(legend.position = "none"), nrow = 1, rel_widths = c(1, .825, .675))

plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .075))

ggsave("all-ints-plots.png", width = 10, height = 5.5)
ggsave("all-ints-plots.png", width = 10, height = 10)


## ---- eval = FALSE------------------------------------------------------------------
## all_year <- read_rds("data/ngsschat-tweets-14-15.rds")
## sn <- all_year$screen_name %>% unique()
## 
## sno <- dd %>%
##   #filter(!(str_detect(ID, "uc") | str_detect(ID, "qa"))) %>%
##   pull(screen_name) %>%
##   unique()
## 
## i$screen_name %in% sno


## -----------------------------------------------------------------------------------
dd %>% 
  janitor::tabyl(code)


## -----------------------------------------------------------------------------------
dd %>% 
  filter(!(str_detect(ID, "uc") | str_detect(ID, "qa"))) %>% 
  janitor::tabyl(code)


## -----------------------------------------------------------------------------------
dd %>% 
  filter(str_detect(ID, "uc") | str_detect(ID, "qa")) %>% 
  count(str_detect(ID, "uc"))


## -----------------------------------------------------------------------------------
# mean
n_group_mean <- dd %>% 
  count(ID, group, code) %>% 
  spread(group, n, fill = 0) %>% 
  dplyr::select(ID:Teacher) %>% 
  gather(key, val, -ID, -code) %>% 
  group_by(code, key) %>% 
  summarize(mean_val = mean(val)) %>% 
  spread(key, mean_val) %>% 
  ungroup() %>% 
  arrange(desc(Administrator)) %>% 
  dplyr::select(Code = code, Teacher, Administrator, Researcher, Organization, Other) %>% 
  mutate_if(is.double, round, 2)

# sd
n_group_sd <- dd %>% 
  count(ID, group, code) %>% 
  spread(group, n, fill = 0) %>% 
  dplyr::select(ID:Teacher) %>% 
  gather(key, val, -ID, -code) %>% 
  group_by(code, key) %>% 
  summarize(sd_val = sd(val)) %>% 
  spread(key, sd_val) %>% 
  ungroup() %>% 
  dplyr::select(Code = code, Teacher, Administrator, Researcher, Organization, Other) %>% 
  mutate_if(is.double, round, 2) %>% 
  set_names(c("Code", str_c("sd_", names(.[2:6]))))

n_group_mean
n_group_sd


## -----------------------------------------------------------------------------------
dd <- dd %>% 
  mutate(adoption_tri = as.character(adoption_tri)) %>% 
  mutate(adoption_tri_c = if_else(is.na(adoption_tri), "missing", adoption_tri))

# mean
n_adoption_mean <- dd %>% 
  filter(!(str_detect(ID, "uc") | str_detect(ID, "qa"))) %>% 
  count(ID, adoption_tri_c, code) %>% 
  spread(adoption_tri_c, n, fill = 0) %>% 
  dplyr::select(ID:not) %>% 
  gather(key, val, -ID, -code) %>% 
  group_by(code, key) %>% 
  summarize(mean_val = mean(val)) %>% 
  spread(key, mean_val) %>% 
  ungroup() %>% 
  mutate_if(is.double, round, 2)

# sd
n_adoption_sd <- dd %>% 
  filter(!(str_detect(ID, "uc") | str_detect(ID, "qa"))) %>% 
  count(ID, adoption_tri_c, code) %>% 
  spread(adoption_tri_c, n, fill = 0) %>% 
  dplyr::select(ID:not) %>% 
  gather(key, val, -ID, -code) %>% 
  group_by(code, key) %>% 
  summarize(sd_val = sd(val)) %>% 
  spread(key, sd_val) %>% 
  ungroup() %>% 
  mutate_if(is.double, round, 2) %>% 
  set_names(c("Code", str_c("sd_", names(.[2:5]))))

n_adoption_mean
n_adoption_sd


## -----------------------------------------------------------------------------------
dd %>% 
  filter(!(str_detect(ID, "uc") | str_detect(ID, "qa"))) %>% 
  count(ID, code) %>% 
  group_by(code) %>% 
  summarize(mean_length = mean(n),
            sd_length = sd(n))


## -----------------------------------------------------------------------------------
one_or_more <- function(x) {
  sum(if_else(x > 0, 1, 0))
}

dd %>% 
  filter(!(str_detect(ID, "uc") | str_detect(ID, "qa"))) %>% 
  count(screen_name, code) %>% 
  spread(code, n, fill = 0) %>% 
  semi_join(i) %>% 
  summarize_if(is.numeric, one_or_more) %>% 
  gather(key, val) %>% 
  mutate(prop = val / 247)


## -----------------------------------------------------------------------------------
dd %>% 
  filter(!(str_detect(ID, "uc") | str_detect(ID, "qa"))) %>% 
  count(ID, screen_name, code) %>% 
  count(ID, screen_name, code) %>% 
  group_by(ID, code) %>% 
  summarize(sum_n_groups = sum(n)) %>% 
  group_by(code) %>% 
  summarize(mean_n = mean(sum_n_groups),
            sd_n = sd(sum_n_groups))



## -----------------------------------------------------------------------------------
dd %>% 
  filter(!(str_detect(ID, "uc") | str_detect(ID, "qa"))) %>% 
  count(ID, group, code) %>% 
  count(ID, group, code) %>% 
  group_by(ID, code) %>% 
  summarize(sum_n_groups = sum(n)) %>% 
  group_by(code) %>% 
  summarize(mean_n = mean(sum_n_groups),
            sd_n = sd(sum_n_groups))


## -----------------------------------------------------------------------------------
dd %>% 
  filter(!(str_detect(ID, "uc") | str_detect(ID, "qa"))) %>% 
  count(ID, adoption_tri, code) %>% 
  count(ID, adoption_tri, code) %>% 
  group_by(ID, code) %>% 
  summarize(sum_n_groups = sum(n)) %>% 
  group_by(code) %>% 
  summarize(mean_n = mean(sum_n_groups),
            sd_n = sd(sum_n_groups))

