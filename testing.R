con <- DBI::dbConnect(RSQLite::SQLite(),
                      here("topic_modeling_review",
                           "topic_modeling.db")
)
