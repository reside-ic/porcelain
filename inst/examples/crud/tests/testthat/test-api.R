context("api")

test_that("can hit create endpoint", {
  api <- build_api()
  book_body <- '{
    "title": "The Picture of Dorian Grey",
    "author": "Oscar Wilde",
    "setting": "London"
  }'
  res <- api$request("POST", "/books",
                     body = book_body,
                     content_type = "application/json")
  res <- pkgapi:::from_json(res)
  expect_equal(res$status, 200)
  expect_equal(res$data$id, 1)
  out <- DBI::dbGetQuery(con, "SELECT * FROM books")
  expect_equal(nrow(out), 1)


  res <- api$request("GET",
                     "/books/The%20Picture%20of%20Dorian%20Gray?details=author")
  res <- pkgapi:::from_json(res)
  expect_equal(res$status, 200)
  expect_equal(names(res$data), "author")
  expect_equal(res$data$author, "Oscar Wilde")

  res <- api$request(
    "GET",
    "/books/The%20Picture%20of%20Dorian%20Gray?details=author&details=setting")
  res <- pkgapi:::from_json(res)
  expect_equal(names(res$data), c("author", "setting"))
  expect_equal(res$data$author, "Oscar Wilde")
  expect_equal(res$data$setting, "London")
})
