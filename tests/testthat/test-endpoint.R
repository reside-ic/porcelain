test_that("endpoints reject unconsumed dots", {
  returning <- porcelain_returning_json("String", "schema")
  expect_error(
    porcelain_endpoint$new(
      "GET", "/path", function() "hello", returning = returning,
      a = "unconsumed"),
    "Unconsumed dot arguments: character (a)",
    fixed = TRUE)

  expect_error(
    porcelain_endpoint$new(
      "GET", "/path", function() "hello", returning = returning,
      a = "unconsumed", 1L),
    "Unconsumed dot arguments: character (a), integer (unnamed argument)",
    fixed = TRUE)

  expect_error(
    porcelain_endpoint$new(
      "GET", "/path", function() "hello", returning = returning,
      1L),
    "Unconsumed dot arguments: integer (unnamed argument)",
    fixed = TRUE)
})


test_that("Can serve endpoint directly", {
  square <- function(n) {
    jsonlite::unbox(n * n)
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/square", square,
    returning = porcelain_returning_json("Number", "schema"),
    porcelain_input_query(n = "numeric"),
    validate = TRUE)

  cmp <- porcelain$new()$handle(endpoint)$request("GET", "/square", list(n = 3))
  res <- endpoint$request(list(n = 3))
  res$headers[["Date"]] <- cmp$headers[["Date"]]
  expect_equal(cmp, res)
})
