library(tidyverse)
library(tidygraph)
library(igraph)
library(bibliometrix)
library(tosr)
library(here)
library(lubridate)
library(sjrdata)
library(openxlsx)
library(zoo)


asn_wos_scopus_csv <-
  read_csv("output/asn_wos_scopus.csv")

asn_wos_scopus_tbl_graph <-
  as_tbl_graph(asn_wos_scopus_csv,
               directed = FALSE) |>
  activate(nodes) |>
  mutate(components = group_components(type = "weak")) |>
  filter(components == 1) |>
  mutate(degree = centrality_degree(),
         community = as.factor(group_louvain()))

asn_wos_scopus_tbl_graph |>
  activate(nodes) |>
  data.frame() |>
  count(community, sort = TRUE) |>
  slice(1:50)
