con <- setup_db()

add_book <- function(book) {
  book_df <- as_data_frame(jsonlite::fromJSON(book))
  DBI::dbAppendTable(con, "books", book_df)
  id <- DBI::dbGetQuery(con, sprintf("SELECT id FROM books WHERE title = '%s'",
                                     book_df$title))[[1]]
  list(id = jsonlite::unbox(id))
}

get_book_details <- function(title, details = NULL) {
  if (is.null(details)) {
    details <- "*"
  }
  title <- URLdecode(title)
  details <- vapply(details, URLdecode, character(1))
  data <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s FROM books WHERE title = '%s'",
    paste(details, collapse = ", "),
    title))
  if (nrow(data) == 0) {
    stop(sprintf("Could not find book with title '%s'.", title))
  }
  lapply(data, jsonlite::unbox)
}
