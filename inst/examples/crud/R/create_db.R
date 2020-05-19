setup_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  add_book_table(con)
  con
}

## Exists so tests get a clear db for tests
clear_db <- function() {
  DBI::dbRemoveTable(con, "books")
  add_book_table(con)
}

add_book_table <- function(con) {
  DBI::dbExecute(con,
                 "CREATE TABLE books(id INTEGER PRIMARY KEY ASC,
                 title UNIQUE NOT NULL,
                 author,
                 setting)")
}
