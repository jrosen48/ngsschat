library(tidyverse)
library(rTAGS)
library(brms)
library(igraph)

e <- read_csv("edgelist.csv")

g <- graph_from_data_frame(e)

dc <- degree(g, mode = "in") %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  set_names("sender", "degree_in")

# dc <- e %>% 
#   count(sender) %>% 
#   arrange(desc(n)) %>% 
#   set_names("sender", "degree_in")

dc %>% 
  as_tibble() %>% 
  arrange(desc(degree_in))

edge <- read_csv("edgelist.csv") %>% 
  filter(var == "ts")

i <- drake::readd(influence) %>% 
  select(-code)

# g <- graph_from_data_frame(edge)
# 
# dci <- degree(g, mode = "in") %>% 
#   as.data.frame() %>% 
#   rownames_to_column() %>% 
#   set_names("sender", "degree_in")

tm <- edge %>% 
  left_join(dc) %>% 
  group_by(receiver) %>% 
  summarize(exposure_sum = sum(degree_in)) %>% 
  rename(screen_name = receiver)

# dc <- degree(g, mode = "in") %>% 
#   as.data.frame() %>% 
#   rownames_to_column() %>% 
#   set_names("screen_name", "degree_in")
# 
# dc %>% 
#   left_join(u) %>% 
#   mutate(group = if_else(group == "Unclear", "Other", group)) %>% 
#   group_by(group) %>% 
#   summarize(in_degree = mean(degree_in),
#             in_degree_sd = sd(degree_in)) %>% 
#   arrange(desc(in_degree))

i <- i %>% 
  left_join(tm) %>% 
  mutate(exposure_sum = ifelse(is.na(exposure_sum), 0, exposure_sum))

edge_sb <- read_csv("edgelist.csv") %>% 
  filter(var == "sb" | var == "tf")

# g_sb <- graph_from_data_frame(edge_sb)
# 
# dci_sb <- degree(g_sb, mode = "in") %>% 
#   as.data.frame() %>% 
#   rownames_to_column() %>% 
#   set_names("sender", "degree_in")

tm <- edge_sb %>% 
  left_join(dc) %>% 
  group_by(receiver) %>% 
  summarize(exposure_sum_sb = sum(degree_in)) %>% 
  rename(screen_name = receiver)

i <- i %>% 
  left_join(tm) %>% 
  mutate(exposure_sum = ifelse(is.na(exposure_sum), 0, exposure_sum),
         exposure_sum_sb = ifelse(is.na(exposure_sum_sb), 0, exposure_sum_sb))

select(i, exposure_sum, exposure_sum_sb)
i$exposure_sum_std <- as.vector(scale(i$exposure_sum))
i$exposure_sum_sb_std <- as.vector(scale(i$exposure_sum_sb))
#i$group <- as.character(i$group)
i$group <- fct_relevel(as.factor(i$group), "Other")
i$group

i %>% 
  group_by(group) %>% 
  summarize(mean_post_n = mean(post_n),
            median_post_n = median(post_n),
            median_n_tweets = median(n_tweets),
            post_n_users = sum(post_n > 0),
            sum_users = n(),
            prop_n_users = post_n_users / sum_users,
            sum_pre_n = sum(pre_n > 0),
            prop_pre_n = sum_pre_n / sum_users) %>% 
  arrange(desc(prop_n_users))

mts <- glm(post_n ~ 1 +
            exposure_sum_std+ exposure_sum_sb_std + group,
          data = i,
          family = 'poisson')

sjPlot::tab_model(mts)
library(multcomp)

g <- multcomp::glht(mts, multcomp::mcp(group="Tukey"))
g
msb <- glm(post_n ~ 1 +
             exposure_sum_sb_std++ group + pre_n,
           data = i,
           family = 'poisson')
sjPlot::tab_model(mts, show.std = T)
sjPlot::tab_model(msb, show.std = T)

# library(lavaan)
# 
# x <- dummies::dummy(i$group) %>% as_tibble()
# names(x) <- c("other", "admin", "org", "res", "teach")
# i <- bind_cols(x, i)
# model <- '
#   # regressions
#   post_n ~ exposure_sum_std + exposure_sum_sb_std + admin + org + res + teach + pre_n
# '
# 
# summary(sem(model, data = i), std = T)

m1 <- glm(post_n~ 1,
          data = i,
          family = 'poisson')
sjPlot::tab_model(m1, show.std = T)

m1b <- glm(post_n_days~ 1,
          data = i,
          family = 'poisson')
sjPlot::tab_model(m1b, show.std = T)

margins::margins(m1b)

x <- sjPlot::plot_model(m1, type = "pred")
x
sjPlot::tab_model(m1, show.std = T)
slibrary(margins)
x$group$data
margins(m1, i)
modelbased::estimate_means(m1)
modelbased::estimate_contrasts(m1)
modelbased::estimate_link(m1)
modelbased::estimate_response(m1, transform = "none")
modelbased::estimate_response(m1) %>% 
  as_tibble() %>% 
  group_by(group) %>% 
  summarize(mean = mean(Predicted),
            sd = sd(Predicted))

i %>% 
  group_by(group) %>% 
  summarize(mean = mean(post_n),
            sd = sd(post_n))
