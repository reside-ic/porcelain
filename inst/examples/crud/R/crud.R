con <- setup_db()

add_book <- function(book) {
  browser()
  DBI::dbAppendTable(con, "books", as_data_frame(book))
  id <- DBI::dbGetQuery(con, sprintf("SELECT id FROM books WHERE title = '%s'",
                                     book$title))[[1]]
  list(id = id)
}

get_book_details <- function(title, details = NULL) {
  if (is.null(details)) {
    details <- "*"
  }
  data <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s FROM books WHERE title = '%s'",
    paste(details, collapse = ", "),
    title))
  if (nrow(data) == 0) {
    stop(sprintf("Could not find book with title '%s'.", title))
  }
  data
}
