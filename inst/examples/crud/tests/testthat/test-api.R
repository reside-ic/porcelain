context("api")

test_that("create endpoint works as expected", {
  clear_db()
  endpoint <- create_endpoint()
  res <- endpoint$run(list(
    title = "The Picture of Dorian Gray",
    author = "Oscar Wilde",
    setting = "London"
  ))

  expect_equal(res$status_code, 200)
  expect_null(res$errors)
  expect_equal(res$data$id, jsonlite::unbox(1L))
})

test_that("details endpoint works as expected", {
  clear_db()
  endpoint <- details_endpoint()
  res <- endpoint$run("The Picture of Dorian Gray", c("author", "setting"))

  expect_equal(res$status_code, 500)
  expect_null(res$value$data)
  expect_length(res$value$errors, 1)
  expect_equal(res$value$errors[[1]]$error, jsonlite::unbox("SERVER_ERROR"))
  expect_equal(res$value$errors[[1]]$detail, jsonlite::unbox(
    "Could not find book with title 'The Picture of Dorian Gray'."))

  create <- create_endpoint()
  res <- create$run(list(
    title = "The Picture of Dorian Gray",
    author = "Oscar Wilde",
    setting = "London"
  ))

  res <- endpoint$run("The Picture of Dorian Gray",
                      details = c("author", "setting"))

  expect_equal(res$status_code, 200)
  expect_null(res$errors)
  expect_equal(names(res$data), c("author", "setting"))
  expect_equal(res$data$author, jsonlite::unbox("Oscar Wilde"))
  expect_equal(res$data$setting, jsonlite::unbox("London"))
})

test_that("api works as expected", {
  api <- build_api()
  expect_equal(names(api$routes), c("books", "book"))
  expect_equal(api$routes$books$verbs, "POST")
  expect_equal(names(api$routes$book), "<title>")
  expect_equal(api$routes$book$`<title>`$verbs, "GET")
})
