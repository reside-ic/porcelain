context("pkgapi")

test_that("wrap endpoint", {
  hello <- function() {
    jsonlite::unbox("hello")
  }
  endpoint <- pkgapi_endpoint_json$new("GET", "/", hello, "String", "schema")
  expect_is(endpoint, "pkgapi_endpoint")
  expect_is(endpoint, "pkgapi_endpoint_json")
  expect_equal(endpoint$content_type, "application/json")
  expect_equal(endpoint$schema, "String")
  expect_identical(endpoint$target, hello)

  res <- endpoint$run()
  expect_is(res, "pkgapi_response")
  expect_setequal(names(res),
                  c("status_code", "content_type", "body", "data", "value"))
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$body, to_json_string(res$value))
  expect_equal(res$data, hello())
  expect_equal(res$value, response_success(hello()))
})


test_that("validate schema", {
  hello <- function() {
    jsonlite::unbox(1)
  }
  endpoint <- pkgapi_endpoint_json$new("GET", "/", hello, "String", "schema")
  res <- endpoint$run()

  expect_is(res, "pkgapi_response")
  expect_equal(res$status_code, 500L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$body, to_json_string(res$value))
  expect_is(res$error, "pkgapi_validation_error")

  expect_equal(to_json_string(response_success(hello())), res$error$json)
  err <- get_error(pkgapi_validate(res$error$json, endpoint$validator, TRUE))
  expect_equal(err, res$error)

  ## TODO: test the actual contents of the error, but wait until we
  ## stabilise.  Particularly that we have a VALIDATION_ERROR here.
})


test_that("wrap raw output", {
  binary <- function() {
    as.raw(0:255)
  }
  endpoint <- pkgapi_endpoint_binary$new("GET", "/binary", binary)
  expect_is(endpoint, "pkgapi_endpoint")
  expect_is(endpoint, "pkgapi_endpoint_binary")
  expect_equal(endpoint$content_type, "application/octet-stream")
  expect_identical(endpoint$target, binary)

  res <- endpoint$run()
  expect_is(res, "pkgapi_response")
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/octet-stream")
  expect_equal(res$body, binary())
})


test_that("build api - json endpoint", {
  hello <- function() {
    jsonlite::unbox("hello")
  }
  endpoint <- pkgapi_endpoint_json$new("GET", "/", hello, "String", "schema")
  pr <- pkgapi$new()
  pr$handle(endpoint)

  res <- test_call(pr, "GET", "/")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, as.character(endpoint$run()$body))
})


test_that("build api - binary endpoint", {
  binary <- function() {
    as.raw(0:255)
  }
  endpoint <- pkgapi_endpoint_binary$new("GET", "/binary", binary)
  pr <- pkgapi$new()
  pr$handle(endpoint)

  res <- test_call(pr, "GET", "/binary")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/octet-stream")
  expect_equal(res$body, binary())
})


test_that("use routing parameter", {
  square <- function(n) {
    jsonlite::unbox(n * n)
  }
  endpoint <- pkgapi_endpoint_json$new("GET", "/square/<n:int>", square,
                                       "Number", "schema")

  ## endpoint directly:
  res <- endpoint$run(4)
  expect_equal(res$status_code, 200)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, jsonlite::unbox(16))
  expect_equal(res$value, response_success(res$data))
  expect_equal(res$body, to_json_string(res$value))

  ## Through the api
  pr <- pkgapi$new()$handle(endpoint)
  res_api <- test_call(pr, "GET", "/square/4")
  expect_equal(res_api$status, 200)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, res$body)
})
