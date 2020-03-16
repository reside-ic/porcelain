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
