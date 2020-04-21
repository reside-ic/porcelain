context("endpoint")

test_that("endpoints reject unconsumed dots", {
  returning <- pkgapi_returning_json("String", "schema")
  expect_error(
    pkgapi_endpoint$new(
      "GET", "/path", function() "hello", returning = returning,
      a = "unconsumed"),
    "Unconsumed dot arguments: character (a)",
    fixed = TRUE)

  expect_error(
    pkgapi_endpoint$new(
      "GET", "/path", function() "hello", returning = returning,
      a = "unconsumed", 1L),
    "Unconsumed dot arguments: character (a), integer (unnamed argument)",
    fixed = TRUE)

  expect_error(
    pkgapi_endpoint$new(
      "GET", "/path", function() "hello", returning = returning,
      1L),
    "Unconsumed dot arguments: integer (unnamed argument)",
    fixed = TRUE)
})


test_that("Can serve endpoint directly", {
  square <- function(n) {
    jsonlite::unbox(n * n)
  }
  endpoint <- pkgapi_endpoint$new(
    "GET", "/square", square,
    returning = pkgapi_returning_json("Number", "schema"),
    pkgapi_input_query(n = "numeric"),
    validate = TRUE)

  cmp <- pkgapi$new()$handle(endpoint)$request("GET", "/square", list(n = 3))
  res <- endpoint$request(list(n = 3))
  res$headers[["Date"]] <- cmp$headers[["Date"]]
  expect_equal(cmp, res)
})
