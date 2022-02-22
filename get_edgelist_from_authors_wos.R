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


author_1 <- # 4273 3 there a mistake with the duplicate values.
  wos_author |> # 190
  filter(!is.na(PY)) |>
  dplyr::rename(year = PY,
                doi = DI) |>
  bind_rows(wos_author_ref |>
              mutate(year = as.numeric(year))) |> # 4104
  unique() |>
  filter(!(doi %in% "10.3109/13880209.2014.922589" &
             year == 2014)) # this is a duplicate value with != year
edgelist_wos_authors <- data.frame(Source = character(),
                                   Target = character(),
                                   doi = character(),
                                   year = as.numeric(),
                                   stringsAsFactors = FALSE)

table_ids <- table(author_1$doi)
table_ids_0 <- data.frame(table_ids)
table_ids_1 <- table_ids_0[table_ids_0$Freq >= 2,]
list_ids_1 <- unique(table_ids_1$Var1)

for (i in list_ids_1) {
  df_1 = author_1[author_1$doi == i,] |>
    filter(!is.na(doi))
  df_2 = combn(df_1$author, 2, simplify = FALSE)
  df_3 = data.frame((t(data.frame(df_2))), i)
  colnames(df_3) = c("Source", "Target", "doi")
  df_4 <- df_3 |> bind_cols(df_1 |> select(year) |> unique())
  edgelist_wos_authors = rbind(edgelist_wos_authors, df_4)
}
