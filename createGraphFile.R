library(tidyverse)
library(igraph)
library(sf)

# Load data -----
transfers.euro <- readRDS("~/R/EuropeanSoccerNetwork/Data/transfers.euro.rds")
dist_matrix <- readRDS("~/R/EuropeanSoccerNetwork/Data/dist_matrix.rds")
nodedataexternal <- readRDS("~/R/EuropeanSoccerNetwork/Data/nodedataexternal.rds")
teamLeagues <- readRDS("~/R/EuropeanSoccerNetwork/Data/teamLeagues.rds")


# Create Graph file -----
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
  inner_join(nodedataexternal) %>% 
  rename(id = teamid,
         name = team,
         ClubMV = totMarketVal) %>% 
  select(id, everything())


g <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

write_graph(g, "Data/euro_transfers.graphml", format = "graphml")

league_matrix <- outer(nodes$league, nodes$league, FUN = "==") * 1
rownames(league_matrix) <- nodes$id
colnames(league_matrix) <- nodes$id

saveRDS(league_matrix, "Data/league_matrix.rds")



save(g, league_matrix, dist_matrix, file = "Data/allData.RData")
