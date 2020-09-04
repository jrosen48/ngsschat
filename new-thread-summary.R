params <-
  list(coded_threads = "", influence = "")

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

pts <- map_df(ts, create_reply_edges) %>% transform_edgelist(u) %>% mutate(var = "ts")
ptf <- map_df(tf, create_reply_edges) %>% transform_edgelist(u) %>% mutate(var = "tf")
psb <- map_df(sb, create_reply_edges) %>% transform_edgelist(u) %>% mutate(var = "sb")
pot <- map_df(ot, create_reply_edges) %>% transform_edgelist(u) %>% mutate(var = "ot")

edgelist <- bind_rows(pts, ptf, psb, pot)

write_csv(edgelist, "edgelist.csv")

uu <- filter(u, n_tweets >= 1)
uu$group<- ifelse(u$group == "Unclear", "Other", u$group)

u <- filter(u, n_tweets >= 10)
u$group<- ifelse(u$group == "Unclear", "Other", u$group)

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
library(cowplot)
legend <- get_legend(p1)

prow <- plot_grid(p1 + theme(legend.position = "none"), 
                  p2 + theme(legend.position = "none"), 
                  p3 + theme(legend.position = "none"), nrow = 1, rel_widths = c(1, .825, .675))

plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .075))

# ggsave("all-ints-plots.png", width = 10, height = 5.5)
ggsave("all-ints-plots.png", width = 10, height = 10)
