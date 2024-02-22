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

  cmp <- porcelain$new()$handle(endpoint)$request("GET", "/square", list(n = 3),
                                                  request_id = "123")
  res <- endpoint$request(list(n = 3), request_id = "123")
  res$headers[["Date"]] <- cmp$headers[["Date"]]
  expect_equal(cmp, res)
})


test_that("target must be a function", {
  expect_error(porcelain::porcelain_endpoint$new(
    "GET", "/",
    porcelain::porcelain_input_query(a = "numeric", b = "numeric"),
    returning = porcelain::porcelain_returning_binary()),
    "'target' must be a function")
})
