# graphTweets demo -----

library(graphTweets)
library(rtweet)
library(igraph)
library(echarts4r) # for plots

tweets <- search_tweets("#rstats", n = 1000, include_rts = FALSE, token = twitter_token)

# Sys.setlocale("LC_TIME", "English")

tweets <- search_tweets("#rstats")

library(graphTweets)

# simple network
tweets %>% 
  gt_edges(text, screen_name, status_id) %>% # get edges
  gt_nodes %>% # get nodes
  gt_graph %>% # build igraph object
  plot(.)

# igraph
tweets %>% 
  gt_edges(text, screen_name, status_id) %>% 
  gt_graph() -> graph

class(graph)

# list
tweets %>% 
  gt_edges(text, screen_name, status_id) %>% 
  gt_collect() -> edges

names(edges)

tweets %>% 
  gt_edges(text, screen_name, status_id) %>% 
  gt_nodes() %>% 
  gt_collect() -> graph

lapply(graph, nrow) # number of edges and nodes

lapply(graph, names) # names of data.frames returned

tweets %>% 
  gt_edges(text, screen_name, status_id) %>% 
  gt_nodes(meta = TRUE) %>%  # add meta data
  gt_collect() -> graph

tweets %>% 
  gt_edges(text, screen_name, status_id, datetime = "created_at") %>% 
  gt_nodes(meta = TRUE) %>% 
  gt_collect() -> graph

tweets %>% 
  gt_edges(text, screen_name, status_id, datetime = "created_at") %>% 
  gt_nodes(meta = TRUE) %>% 
  gt_collect() -> gt

# replace NAs
gt$nodes$name <- ifelse(is.na(gt$nodes$name), gt$nodes$nodes, gt$nodes$name)
gt$nodes$followers_count <- ifelse(is.na(gt$nodes$followers_count), 0, gt$nodes$followers_count)

e_charts() %>% 
  e_graph_gl() %>% # use graph GL for performances
  e_graph_edges(gt$edges, source, target) %>% 
  e_graph_nodes(gt$nodes, name, followers_count, n_edges) 

tweets %>% 
  gt_edges(text, screen_name, status_id) %>% 
  gt_graph() -> g

# communities
wc <- walktrap.community(g)
V(g)$color <- membership(wc)

# plot
# tons of arrguments because defaults are awful
plot(g, 
     layout = igraph::layout.fruchterman.reingold(g), 
     vertex.color = V(g)$color,
     vertex.label.family = "sans",
     vertex.label.color = hsv(h = 0, s = 0, v = 0, alpha = 0.0),
     vertex.size = igraph::degree(g), 
     edge.arrow.size = 0.2, 
     edge.arrow.width = 0.3, edge.width = 1,
     edge.color = hsv(h = 1, s = .59, v = .91, alpha = 0.7),
     vertex.frame.color="#fcfcfc")


# twitteR ----

library(twitteR)
library(igraph)
library(stringr)

tweets <- twitteR::searchTwitter('#rstats', n = 1000)
df <- twitteR::twListToDF(tweets)


