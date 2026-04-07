library(tidyverse)
library(igraph)

transfers.euro <- readRDS("~/R/EuropeanSoccerNetwork/Data/transfers.euro.rds")

edges <- transfers.euro %>%
  group_by(from.id, to.id) %>%
  summarise(
    total.fee = sum(fee, na.rm = TRUE),
    num.transfers = n(),
    mean.age = mean(Age),
    total.MV = sum(Market.value, na.rm = TRUE)
  ) %>%
  rename(from = from.id, to = to.id)


nodes <- teamLeagues %>% 
  rename(id = teamid,
         name = team) %>% 
  select(id, name, league)


g <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)


write_graph(g, "euro_transfers.graphml", format = "graphml")

