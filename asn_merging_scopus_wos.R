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


edgelist_scopus_authors_csv <-
  read_csv("output/asn_scopus_264777_3.csv")

edgelist_wos_authors_csv <-
  read_csv("output/asn_wos_3237711_4.csv") |>
  select(-doi)

edgelist_author_cocitation_wos_scopus <-
  edgelist_wos_authors_csv |>
  bind_rows(edgelist_scopus_authors_csv) |>
  add_count(Source, Target) |>
  unique() |>
  dplyr::rename(weight = n)


write_csv(edgelist_author_cocitation_wos_scopus,
          "output/asn_wos_scopus.csv")
