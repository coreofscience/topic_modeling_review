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
library(XML)
library(plyr)


scopus_wos_final <-
  read_csv("output/scopus_wos_final.csv")


DOI <-
  scopus_wos_final |>
  filter(database %in% "wos") |>
  select(CR) |>
  separate_rows(CR, sep = ";") |> # 1650 rows
  filter(str_detect(CR, "DOI"))  |>   # 1300 rows
  mutate(DOI = str_remove(CR, ".*DOI ")) |>
  distinct(DOI)    # 1049 rows
##########################################################################
#### Extracci?n de la informaci?n de los art?culos de las referencias ####
##########################################################################
references <- data.frame(DI = character(),
                         PU = character(),
                         SO = character(),
                         J9 = character(),
                         PD = character(),
                         PY = character(),
                         TI = character(),
                         AF = character(),
                         stringsAsFactors = FALSE)
authors <- data.frame(doi = character(),
                      author = character(),
                      year = character(),
                      month = character(),
                      stringsAsFactors = FALSE)
for (i in DOI$DOI) {
  doi <- i
  url <- paste0("http://api.crossref.org/works/", doi, ".xml")
  xml_data_1 = try(xmlParse(url), silent = TRUE);
  if (class(xml_data_1) == "try-error") {
    next
  } else  {
    xml_data_2 <- xmlToList(xml_data_1)

    notfound =try(as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi"]][[".attrs"]]) == "journal_article", silent = TRUE);
    if (class(notfound) == "try-error"){
      next
    }else{

      if (as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi"]][[".attrs"]]) == "journal_article"){

        # PUBLISHER-NAME


        if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["crm-item"]])){
          PU <- as.character(NA)
        } else {

          publisher0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["crm-item"]])
          publisher <- data.frame(publisher0)
          if(nrow(publisher) == 0){
            PU <- as.character(NA)
          }else{
            PU <- as.character(publisher$text[1])
          }
        }

        # JOURNAL FULL TITLE


        if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["full_title"]])){
          SO <- as.character(NA)
        } else {

          journal0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["full_title"]])
          journal <- data.frame(journal0)
          if(nrow(journal) == 0){
            SO <- as.character(NA)
          }else{
            SO <- as.character(journal[1,1])
          }
        }

        # JOURNAL ABBREV TITLE


        if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["abbrev_title"]])){
          J9 <- as.character(NA)
        } else {

          journal0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["abbrev_title"]])
          journal <- data.frame(journal0)
          if(nrow(journal) == 0){
            J9 <- as.character(NA)
          }else{
            J9 <- as.character(journal[1,1])
          }
        }

        # MONTH


        if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["month"]])){
          PD <- as.character(NA)
        } else {

          month0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["month"]])
          month <- data.frame(month0)
          if(nrow(month) == 0){
            PD <- as.character(NA)
          }else{
            PD <- as.character(month[1,1])
          }
        }

        # YEAR


        if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["year"]])){
          PY <- as.character(NA)
        } else {

          Year0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["year"]])
          Year <- data.frame(Year0)
          if(nrow(Year) == 0){
            PY <- as.character(NA)
          }else{
            PY <- as.character(Year[1,1])
          }
        }

        # TITLE


        if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["titles"]][["title"]])){
          TI <- as.character(NA)
        } else {

          title0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["titles"]][["title"]])
          title <- try(data.frame(title0), silent = TRUE);

          if(class(title) == "try-error"){
            titlex <- try(ldply(title0, data.frame), silent = TRUE);
            if(class(titlex) == "try-error"){
              TI <- as.character(NA)
            }else{
              TI0 <- as.character(titlex[1,2])
              TI <- trimws(TI0)
            }
          }else{
            if(nrow(title) == 0){
              TI <- as.character(NA)
            }else{
              TI <- as.character(title[1,1])
            }
          }
        }

        # CONTRIBUTORS


        if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]]))
        {
          AF <- as.character(NA)
        } else {

          author_ref <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]])

          author_1_ref <- ldply(author_ref, data.frame)
          author_2_ref <- author_1_ref[author_1_ref$.attrs == "author", c(2,3) ]

          authorss <- data.frame(doi= doi, author = paste0(author_2_ref$surname, ", ", author_2_ref$given_name), year = PY, month = PD)

          authors = rbind(authors, authorss)
          authorss$author <- trim(authorss$author)
          AF <- as.character(paste(authorss$author, collapse = ";   "))
        }

        references0 <- data.frame(DI = doi, PU = PU, SO = SO, J9 = J9, PD = PD, PY = PY, TI = TI, AF = AF)
        references = rbind(references, references0)

      }else {
        next}
    }
  }
}
authors$month <- sub("^[0]+", "", authors$month) # Elimina los ceros a la izquierda en los meses
##########################################################################
#### Separaci?n de los nombres de los autores de los art?culos de WoS ####
##########################################################################
wos_author <- # 190 3
  scopus_wos_final |>
  filter(database %in% "wos") |>
  select(DI, AU, PY) |>
  separate_rows(AU, sep = ";") |>
  dplyr::rename(author = AU)
#################################################################
#### Uni?n de los datos de WoS y de las referencias de estos ####
#################################################################
authors$author <- gsub("(?<=, [[:alpha:]]).*", "", authors$author, perl=TRUE)
wos_author_ref <- # 4104 3
  authors |>
  mutate(author = str_replace(author, ",", "")) |>
  select(doi, author, year) |>
  mutate(author = str_to_upper(author))

write_csv(wos_author_ref,
          "output/asn_wos_refs.csv")
