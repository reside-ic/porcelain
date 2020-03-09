setup_db <- function() {
  path <- file.path("inst/crud-db.sqlite")
  if (file.exists(path)) {
    file.remove(path)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), "inst/crud-db.sqlite")
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
                 title UNIQUE,
                 author,
                 setting)")
}
