context("pkgapi")

test_that("wrap endpoint", {
  hello <- function() {
    jsonlite::unbox("hello")
  }
  endpoint <- pkgapi_endpoint_json("GET", "/", hello, TRUE, "String", "schema")
  expect_is(endpoint, "pkgapi_endpoint")
  expect_equal(endpoint$content_type, "application/json")
  expect_identical(endpoint$target, hello)

  res <- endpoint$run()
  expect_is(res, "pkgapi_response")
  expect_setequal(names(res),
                  c("status_code", "content_type", "body", "data"))
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$body, to_json_string(response_success(res$data)))
  expect_equal(res$data, hello())

  expect_true(validator_response_success(res$body))
})


test_that("wrap raw output", {
  binary <- function() {
    as.raw(0:255)
  }
  endpoint <- pkgapi_endpoint_binary("GET", "/binary", binary)
  expect_is(endpoint, "pkgapi_endpoint")
  expect_equal(endpoint$content_type, "application/octet-stream")
  expect_identical(endpoint$target, binary)

  res <- endpoint$run()
  expect_is(res, "pkgapi_response")
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/octet-stream")
  expect_equal(res$body, binary())
  expect_equal(res$data, binary())
})


test_that("build api - json endpoint", {
  hello <- function() {
    jsonlite::unbox("hello")
  }
  endpoint <- pkgapi_endpoint_json("GET", "/", hello, TRUE, "String", "schema")
  pr <- pkgapi$new()
  pr$handle(endpoint)

  res <- pr$request("GET", "/")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, as.character(endpoint$run()$body))
})


test_that("build api - binary endpoint", {
  binary <- function() {
    as.raw(0:255)
  }
  endpoint <- pkgapi_endpoint_binary("GET", "/binary", binary, TRUE)
  pr <- pkgapi$new()
  pr$handle(endpoint)

  res <- pr$request("GET", "/binary")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/octet-stream")
  expect_equal(res$body, binary())
})


test_that("use routing parameter", {
  square <- function(n) {
    jsonlite::unbox(n * n)
  }
  endpoint <- pkgapi_endpoint_json("GET", "/square/<n:int>", square,
                                   TRUE, "Number", "schema")

  ## endpoint directly:
  res <- endpoint$run(4)
  expect_equal(res$status_code, 200)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, jsonlite::unbox(16))
  expect_equal(res$body, to_json_string(response_success(res$data)))

  ## Through the api
  pr <- pkgapi$new()$handle(endpoint)
  res_api <- pr$request("GET", "/square/4")
  expect_equal(res_api$status, 200)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, res$body)
})


test_that("use query parameter", {
  square <- function(n) {
    n <- as.numeric(n)
    jsonlite::unbox(n * n)
  }
  endpoint <- pkgapi_endpoint_json("GET", "/square", square,
                                   TRUE, "Number", "schema")

  ## endpoint directly:
  res <- endpoint$run(4)
  expect_equal(res$status_code, 200)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, jsonlite::unbox(16))
  expect_equal(res$body, to_json_string(response_success(res$data)))

  ## Through the api
  pr <- pkgapi$new()$handle(endpoint)
  res_api <- pr$request("GET", "/square", c(n = 4))
  expect_equal(res_api$status, 200)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, res$body)
})
