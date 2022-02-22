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


scopus_wos_final <-
  read_csv("output/scopus_wos_final.csv")


data_tidied_scopus_main <-
  scopus_wos_final |>
  filter(database %in% "scopus") |>
  select(SR, AU, PY) |>
  separate_rows(AU, sep = ";")

edgelist_scopus_dummy <-
  tibble(Source = as.character(),
         Target = as.character(),
         year = as.numeric())

for (i in data_tidied_scopus_main$SR) {

  df_1 <- data_tidied_scopus_main |>
    filter(SR %in% i) |>
    dplyr::rename(year = PY)

  if (dim(df_1)[1] >= 2) {

    df_2 <- df_1 |>
      select(AU) |>
      pull() |>
      combn(2, simplify = FALSE) |>
      as_tibble(.name_repair = "minimal") |>
      t() |>
      data.frame() |>
      dplyr::rename(Source = 1,
                    Target = 2)

    df_3 <-
      df_2 |>
      bind_cols(df_1 |>
                  select(year) |>
                  unique())

    edgelist_scopus_dummy <-
      edgelist_scopus_dummy |>
      bind_rows(df_3)

  }

}

edgelist_scopus_dummy |>
  write_csv("output/asn_wos.csv")
