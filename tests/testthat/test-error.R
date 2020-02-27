context("error")


test_that("construct error", {
  expect_equal(
    pkgapi_error_data(list(a = NULL)),
    list(list(error = jsonlite::unbox("a"), detail = NULL)))
  expect_equal(
    pkgapi_error_data(list(a = "b")),
    list(list(error = jsonlite::unbox("a"), detail = jsonlite::unbox("b"))))
  expect_equal(
    pkgapi_error_data(list(a = "b", c = NULL)),
    list(list(error = jsonlite::unbox("a"), detail = jsonlite::unbox("b")),
         list(error = jsonlite::unbox("c"), detail = NULL)))
  expect_error(
    pkgapi_error_data(list(a = 1)),
    "All error details must be character or NULL")
})


test_that("error can be constructed", {
  err <- expect_error(
    pkgapi_error(c(a = "error")), class = "pkgapi_error")
  expect_equal(err$data, list(list(error = jsonlite::unbox("a"),
                                   detail = jsonlite::unbox("error"))))
  expect_equal(err$status_code, 400L)
})


test_that("Catch an error in a json endpoint", {
  hello <- function() {
    pkgapi_error(c("an-error" = "An error has occured"))
  }
  err <- get_error(hello())

  endpoint <- pkgapi_endpoint_json$new("GET", "/", hello, "String", "schema")
  expect_error(endpoint$target(), class = "pkgapi_error")

  res <- endpoint$run()
  expect_equal(res, pkgapi_process_error(err))
  expect_equal(res$status_code, 400L)
  expect_equal(res$content_type, "application/json")

  expect_equal(res$value$errors[[1]]$error,
               jsonlite::unbox("an-error"))
  expect_equal(res$value$errors[[1]]$detail,
               jsonlite::unbox("An error has occured"))
})


test_that("Catch error in a binary endpoint", {
  binary <- function() {
    pkgapi_error(c("an-error" = "An error has occured"))
  }
  err <- get_error(binary())

  endpoint <- pkgapi_endpoint_binary$new("GET", "/binary", binary)
  expect_error(endpoint$target(), class = "pkgapi_error")

  res <- endpoint$run()
  expect_equal(res, pkgapi_process_error(err))
  expect_equal(res$status_code, 400L)
  expect_equal(res$content_type, "application/json")

  expect_equal(res$value$errors[[1]]$error,
               jsonlite::unbox("an-error"))
  expect_equal(res$value$errors[[1]]$detail,
               jsonlite::unbox("An error has occured"))
})


test_that("Uncaught error", {
  hello <- function() {
    stop("Unexpected error!", call. = FALSE)
  }
  err <- get_error(hello())
  endpoint <- pkgapi_endpoint_json$new("GET", "/", hello, "String", "schema")

  res <- endpoint$run()
  expect_equal(res, pkgapi_process_error(err))
  expect_equal(res$status_code, 500L)
  expect_equal(res$content_type, "application/json")

  expect_equal(res$value$errors[[1]]$error,
               jsonlite::unbox("SERVER_ERROR"))
  expect_equal(res$value$errors[[1]]$detail,
               jsonlite::unbox("Unexpected error!"))
})


test_that("Uncaught error from the api", {
  hello <- function() {
    stop("unexpected error!")
  }
  endpoint <- pkgapi_endpoint_json$new("GET", "/", hello, "String", "schema")
  pr <- pkgapi$new()
  pr$handle(endpoint)
  res <- test_call(pr, "GET", "/")

  expect_equal(res$status, 500L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, endpoint$run()$body)
})


test_that("Catch error from the api", {
  hello <- function() {
    pkgapi_error(c("an-error" = "An error has occured"))
  }
  endpoint <- pkgapi_endpoint_json$new("GET", "/", hello, "String", "schema")
  pr <- pkgapi$new()
  pr$handle(endpoint)

  res <- test_call(pr, "GET", "/")
  expect_equal(res$status, 400L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, endpoint$run()$body)
})


## this is not actually super likely but seems worth checking for as
## we do move pass our error handler along.
test_that("Error during serialisation", {
  mock_endpoint <- R6::R6Class(
    inherit = pkgapi_endpoint_json,
    public = list(
      process = function(...) {
        ret <- super$process(...)
        ret$content_type <- NULL
        ret
      }
    ))

  hello <- function() {
    jsonlite::unbox("hello")
  }
  endpoint <- mock_endpoint$new("GET", "/", hello, "String", "schema")
  val <- endpoint$run()

  ## First, work our what the error should look like:
  err <- get_error(pkgapi_do_serialize_pass(val, plumber_response()))
  cmp <- pkgapi_process_error(err, FALSE)
  expect_equal(cmp$value$errors[[1]]$error, jsonlite::unbox("SERVER_ERROR"))
  expect_equal(cmp$value$errors[[1]]$detail, jsonlite::unbox(err$message))

  ## Then, get this from the endpoint,
  req <- NULL
  res <- plumber_response()
  ans <- pkgapi_serialize_pass(val, req, res, pkgapi_error_handler)

  expect_equal(res$status, 500L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, cmp$body)

  ## All the way from the api:
  pr <- pkgapi$new()
  pr$handle(endpoint)
  res_api <- test_call(pr, "GET", "/")
  expect_equal(res_api$status, 500L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, res$body)
})
