## This is the only way we can easily test that porcelain talks to
## plumber correctly really, is by bringing up a plumber server on a
## background process and testing that we can talk to it. This is a
## separate set of tests to the ones in test-background.R which
## primarily test our background server handler (which we use here).

test_that("Can serve simple api", {
  create <- function() {
    hello <- function() {
      jsonlite::unbox("hello")
    }
    endpoint <- porcelain::porcelain_endpoint$new(
      "GET", "/", hello,
      returning = porcelain::porcelain_returning_json(),
      validate = TRUE)
    api <- porcelain::porcelain$new()
    api$handle(endpoint)
    api
  }

  bg <- porcelain_background$new(create)
  bg$start()
  r <- bg$request("GET", "/")

  expect_equal(httr::status_code(r), 200)
  h <- httr::headers(r)
  expect_equal(h[["content-type"]], "application/json")
  expect_equal(h[["x-porcelain-validated"]], "false")

  expect_mapequal(httr::content(r),
                  list(status = "success", "errors" = NULL, data = "hello"))
})


test_that("Convert errors in queries correctly", {
  create <- function() {
    target_sqrt <- function(x) {
      if (x < 0) {
        porcelain::porcelain_stop("'x' must be positive")
      }
      jsonlite::unbox(sqrt(x))
    }
    endpoint <- porcelain::porcelain_endpoint$new(
      "GET", "/sqrt", target_sqrt,
      porcelain::porcelain_input_query(x = "numeric"),
      returning = porcelain::porcelain_returning_json())
    api <- porcelain::porcelain$new()
    api$handle(endpoint)
    api
  }

  bg <- porcelain_background$new(create)
  bg$start()

  ## Thrown from the endpoint
  r <- bg$request("GET", "/sqrt", query = list(x = -5))
  expect_equal(httr::status_code(r), 400)
  expect_mapequal(httr::content(r),
                  list(status = "failure",
                       errors = list(
                         list(error = "ERROR",
                              detail = "'x' must be positive")),
                       data = NULL))

  ## Thrown from the filter
  r <- bg$request("GET", "/sqrt", query = list(y = -5))
  expect_equal(httr::status_code(r), 400)
  expect_mapequal(
    httr::content(r),
    list(status = "failure",
         errors = list(
           list(error = "INVALID_INPUT",
                detail = "query parameter 'x' is missing but required")),
         data = NULL))

  ## Also thrown from the filter:
  r <- bg$request("GET", "/sqrt", query = list(x = TRUE))
  expect_equal(httr::status_code(r), 400)
  expect_mapequal(
    httr::content(r),
    list(status = "failure",
         errors = list(
           list(error = "INVALID_INPUT",
                detail = paste("Error parsing query parameter 'x':",
                               "Could not convert 'TRUE' into a numeric"))),
         data = NULL))
})
