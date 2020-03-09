context("crud")

test_that("can add a new book to the db", {
  clear_db()
  book <- list(
    title = "The Picture of Dorian Gray",
    author = "Oscar Wilde",
    setting = "London"
  )
  id <- add_book(book)
  expect_equal(id, list(id = 1))

  out <- DBI::dbGetQuery(con,
                         "SELECT * FROM books WHERE author = 'Oscar Wilde'")
  expect_equal(nrow(out), 1)
})

test_that("get book details", {
  clear_db()
  book <- list(
    title = "The Picture of Dorian Gray",
    author = "Oscar Wilde",
    setting = "London"
  )
  add_book(book)

  details <- get_book_details("The Picture of Dorian Gray",
                              details = c("author", "setting"))

  expect_equal(details, data_frame(author = "Oscar Wilde", setting = "London"))

  details <- get_book_details("The Picture of Dorian Gray",
                              details = c("author"))
  expect_equal(details, data_frame(author = "Oscar Wilde"))

  details <- get_book_details("The Picture of Dorian Gray")
  expect_equal(details, data_frame(id = 1,
                                   title = "The Picture of Dorian Gray",
                                   author = "Oscar Wilde",
                                   setting = "London"))

  expect_error(get_book_details("missing", details = "author"),
               "Could not find book with title 'missing'.")
})
