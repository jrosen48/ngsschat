edge <- read_csv("edgelist.csv")

u <- read_csv('data/users-to-analyze.csv') %>% 
  select(-code)
# brms-tmp

dc <- edge %>%
  select(sender, receiver, var) %>% 
  filter(var == "sb" | var == "tf")

g <- graph_from_data_frame(dc)

m <- as_adjacency_matrix(g, sparse = FALSE) # sender is row, receiver is column

t <- m %>% 
  as.data.frame() %>% 
  rownames_to_column("sender") %>% 
  gather(receiver, val, -sender) %>% 
  as_tibble()

tt <- rTAGS::add_users_data(t, u)

tt$group_receiver <- ifelse(tt$group_receiver == "Unclear", "Other", tt$group_receiver)
tt$group_sender <- ifelse(tt$group_sender == "Unclear", "Other", tt$group_sender)

tt$group_receiver <- fct_relevel(as.factor(tt$group_receiver), "Other")
tt$group_sender <- fct_relevel(as.factor(tt$group_sender), "Other")

tt$dic <- ifelse(tt$val > 0, 1, 0)
tt$same <- ifelse(tt$group_sender == tt$group_receiver, 1, 0)
tt$diff <- ifelse(tt$group_sender != tt$group_receiver, 1, 0)

mts <- brm(val ~ 1 +
             group_sender + group_receiver +
             same +
             (1|sender) + (1|receiver),
           iter = 4000, chains = 4, cores = 4,
           family = 'poisson',
           data = tt)

write_rds(mts, "mts.rds")

mts