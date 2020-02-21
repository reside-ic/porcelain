context("pkgapi")

test_that("wrap endpoint", {
  hello <- function() {
    jsonlite::unbox("hello")
  }
  endpoint <- pkgapi_endpoint_json(hello, "String", "schema")
  expect_is(endpoint, "pkgapi_endpoint")
  expect_equal(endpoint$returns, "json")
  expect_equal(endpoint$schema, "String")
  expect_identical(endpoint$handler, hello)

  res <- endpoint$wrapped()
  expect_setequal(names(res), c("data", "value", "body", "content_type"))
  expect_equal(res$data, hello())
  expect_equal(res$value, response_success(hello()))
  expect_equal(res$body, to_json(res$value))
  expect_equal(res$content_type, "application/json")
})


test_that("validate schema", {
  hello <- function() {
    jsonlite::unbox(1)
  }
  endpoint <- pkgapi_endpoint_json(hello, "String", "schema")
  expect_error(endpoint$wrapped(), class = "pkgapi_validation_error")
  e <- tryCatch(endpoint$wrapped(), error = identity)
  expect_is(e, "pkgapi_validation_error")

  expect_setequal(names(e), c("message", "errors", "result"))
  expect_is(e$message, "character")
  expect_is(e$errors, "data.frame")

  expect_setequal(names(e$result), c("data", "value", "body", "content_type"))
  expect_equal(e$result$data, hello())
  expect_equal(e$result$value,
               list(status = jsonlite::unbox("success"),
                    errors = NULL,
                    data = hello()))
  expect_equal(e$result$body, to_json(e$result$value))
  expect_equal(e$result$content_type, "application/json")
})


test_that("wrap raw output", {
  binary <- function() {
    as.raw(0:255)
  }
  endpoint <- pkgapi_endpoint_binary(binary)
  expect_is(endpoint, "pkgapi_endpoint")
  expect_equal(endpoint$returns, "binary")
  expect_identical(endpoint$handler, binary)

  res <- endpoint$wrapped()
  expect_setequal(names(res), c("body", "content_type"))
  expect_equal(res$body, binary())
  expect_equal(res$content_type, "application/octet-stream")
})


test_that("build api - json endpoint", {
  hello <- function() {
    jsonlite::unbox("hello")
  }
  endpoint <- pkgapi_endpoint_json(hello, "String", "schema")
  pr <- pkgapi$new()
  pr$handle("GET", "/hello", endpoint)

  res <- test_call(pr, "GET", "/hello")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, as.character(endpoint$wrapped()$body))
})


test_that("build api - binary endpoint", {
  binary <- function() {
    as.raw(0:255)
  }
  endpoint <- pkgapi_endpoint_binary(binary)
  pr <- pkgapi$new()
  pr$handle("GET", "/binary", endpoint)

  res <- test_call(pr, "GET", "/binary")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/octet-stream")
  expect_equal(res$body, binary())
})


test_that("find schema root", {
  handler <- pkgapi_endpoint_json # important thing is that it is in the our ns
  expect_equal(schema_root(".", handler), normalizePath("."))
  expect_equal(schema_root(NULL, handler),
               system_file("schema", package = "pkgapi"))
  expect_error(schema_root(tempfile(), handler))
})
