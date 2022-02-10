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


asn_scopus_1 <-
  scopus_wos_final |>
  filter(database == "scopus") |>
  mutate(ID_TOS = str_extract(SR, ".*,"))

asn_scopus_ref <-
  asn_scopus_1 |>
  select(CR) |>
  separate_rows(CR, sep = "; ") |>
  mutate(lastname = sub("\\., .*", "", CR),
         lastname = sub(",", "", lastname),
         lastname = sub("\\.", "", lastname),
         year = str_extract(CR, "\\(([0-9]{4})\\)"),
         year = str_remove_all(year, "\\(|\\)")) |>
  mutate(lastname = str_replace(lastname,
                                pattern = "\\.",
                                replacement = ""),
         ID_TOS = paste0(lastname, ", ", year, ",")) |>
  select(ID_TOS, CR)

edgelist_scopus_ref_dummy <-
  tibble(Source = as.character(),
         Target = as.character(),
         year = as.character())
for (i in 1:length(asn_scopus_ref$CR)) {

  df_1 <-
    asn_scopus_ref |>
    select(CR) |>
    slice(i) |>
    mutate(year_2 = str_extract(CR, "\\([0-9]{4}\\)"),
           year_1 = str_remove(year_2, "\\("),
           year = str_remove(year_1, "\\)"),
           authors_2 = str_remove(CR,
                                  "\\([0-9]{4}\\) .*"),
           authors_1 = str_extract(authors_2,
                                   ".*\\.,"),
           authors = str_replace_all(authors_1,
                                     "\\.",
                                     replacement = "")) |>
    select(authors, year) |>
    separate_rows(authors, sep = ", ") |>
    mutate(authors = str_replace(authors, ",", ""))

  df_2 <-
    df_1 |>
    select(authors) |>
    pull() |>
    zoo::rollapply(2, by=2, c) |>
    as_tibble() |>
    unite(authors, sep = " ") |>
    mutate(authors = str_replace(authors, "(?<= [[:alpha:]]).*", ""))

  if (dim(df_2)[1] >= 2) {

    df_3 <-
      df_2 |>
      pull() |>
      combn(2, simplify = FALSE) |>
      as_tibble(.name_repair = "minimal") |>
      t() |>
      data.frame() |>
      dplyr::rename(Source = 1,
                    Target = 2)

    df_4 <-
      df_3 |>
      bind_cols(df_1 |>
                  select(year) |>
                  unique())

    edgelist_scopus_ref_dummy <-
      edgelist_scopus_ref_dummy |>
      bind_rows(df_4)

  }
}

edgelist_scopus_ref <- edgelist_scopus_ref_dummy |>
  filter(!str_detect(Source, "[0-9]"),
         !str_detect(Target, "[0-9]")) |>
  mutate(year = as.numeric(year))

edgelist_scopus_ref |> write_csv("output/asn_scopus.csv")
