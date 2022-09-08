test_that("wrap endpoint", {
  hello <- function() {
    jsonlite::unbox("hello")
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)

  expect_s3_class(endpoint, "porcelain_endpoint")
  expect_equal(endpoint$returning$content_type, "application/json")
  expect_identical(endpoint$target, hello)

  res <- endpoint$run()
  expect_s3_class(res, "porcelain_response")
  expect_setequal(names(res),
                  c("status_code", "content_type", "body", "data", "headers",
                    "validated"))
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$body, to_json_string(response_success(res$data)))
  expect_equal(res$data, hello())
  expect_equal(res$headers, NULL)
  expect_true(res$validated)

  expect_true(validator_response_success(res$body))
})


test_that("wrap raw output", {
  binary <- function() {
    as.raw(0:255)
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/binary", binary,
    returning = porcelain_returning_binary(),
    validate = TRUE)

  expect_s3_class(endpoint, "porcelain_endpoint")
  expect_equal(endpoint$returning$content_type, "application/octet-stream")
  expect_identical(endpoint$target, binary)

  res <- endpoint$run()
  expect_s3_class(res, "porcelain_response")
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/octet-stream")
  expect_equal(res$body, binary())
  expect_equal(res$data, binary())
  expect_equal(res$headers, NULL)
})


test_that("build api - json endpoint", {
  hello <- function() {
    jsonlite::unbox("hello")
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)
  pr <- porcelain$new()
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
  endpoint <- porcelain_endpoint$new(
    "GET", "/binary", binary,
    returning = porcelain_returning_binary(),
    validate = TRUE)
  pr <- porcelain$new()
  pr$handle(endpoint)

  res <- pr$request("GET", "/binary")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/octet-stream")
  expect_equal(res$body, binary())
})


test_that("throw error", {
  target_sqrt <- function(x) {
    if (x < 0) {
      porcelain_stop("'x' must be positive")
    }
    jsonlite::unbox(sqrt(x))
  }

  endpoint <- porcelain_endpoint$new(
    "GET", "/sqrt", target_sqrt,
    porcelain_input_query(x = "numeric"),
    returning = porcelain_returning_json("Number", "schema"))

  res <- endpoint$run(-1)

  expect_equal(res$status_code, 400)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$value$status, jsonlite::unbox("failure"))
  expect_null(res$value$data)
  expect_equal(res$value$errors,
               list(list(error = jsonlite::unbox("ERROR"),
                         detail = jsonlite::unbox("'x' must be positive"))))
  expect_equal(res$body, to_json_string(res$value))
  expect_s3_class(res$error, "porcelain_error")
  expect_equal(res$error$status_code, 400)
})


test_that("allow plain plumber endpoints to be used", {
  hello <- function() {
    jsonlite::unbox("hello")
  }
  pr <- porcelain$new()
  pr$handle("GET", "/", hello)
  res <- pr$request("GET", "/")
  expect_equal(res$status, 200)
  expect_equal(res$body, structure('"hello"', class = "json"))
})


test_that("disallow additional arguments with a pkgapendpoint", {
  endpoint <- porcelain_endpoint$new(
    "GET", "/", function() jsonlite::unbox("hello"),
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)
  pr <- porcelain$new()
  expect_error(
    pr$handle(endpoint, "/hello"),
    "If first argument is a 'porcelain_endpoint' no others allowed")
})


test_that("404 handler", {
  p <- porcelain$new()
  res <- p$request("GET", "/somewhere")
  expect_equal(res$status, 404)
  expect_equal(res$headers[["Content-Type"]], "application/json")

  cmp <- list(
    status = jsonlite::unbox("failure"),
    errors = porcelain_error_data(
      list(NOT_FOUND = list(detail = "Resource not found"))),
    data = NULL)
  expect_equal(res$body, to_json(cmp))
})


test_that("headers can be added to output", {
  binary <- function() {
    as.raw(0:255)
  }
  binary_with_header <- function() {
    data <- binary()
    porcelain_add_headers(data, list("Content-Disposition" = "new_file.txt"))
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/binary", binary_with_header,
    returning = porcelain_returning_binary(),
    validate = TRUE)

  expect_s3_class(endpoint, "porcelain_endpoint")
  expect_equal(endpoint$returning$content_type, "application/octet-stream")
  expect_identical(endpoint$target, binary_with_header)

  res <- endpoint$run()
  expect_s3_class(res, "porcelain_response")
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/octet-stream")
  expect_equal(res$body, binary())
  expect_equal(res$data, binary_with_header())
  expect_equal(res$headers, list("Content-Disposition" = "new_file.txt"))
})

test_that("build api - headers", {
  binary <- function() {
    as.raw(0:255)
  }
  binary_with_header <- function() {
    data <- binary()
    porcelain_add_headers(data, list("Content-Disposition" = "new_file.txt"))
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/binary", binary_with_header,
    returning = porcelain_returning_binary(),
    validate = TRUE)
  pr <- porcelain$new()
  pr$handle(endpoint)

  res <- pr$request("GET", "/binary")
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/octet-stream")
  expect_equal(res$headers[["Content-Disposition"]], "new_file.txt")
  expect_equal(res$body, binary())
})

test_that("build api - dupe headers throws error", {
  binary <- function() {
    as.raw(0:255)
  }
  binary_with_header <- function() {
    data <- binary()
    porcelain_add_headers(data, list("Content-Type" = "image/png"))
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/binary", binary_with_header,
    returning = porcelain_returning_binary(),
    validate = TRUE)
  pr <- porcelain$new()
  pr$handle(endpoint)

  res <- pr$request("GET", "/binary")
  expect_equal(res$status, 500L)
  body <- from_json(res$body)
  expect_equal(body$status, "failure")
  expect_equal(body$errors[[1]]$error, "SERVER_ERROR")
  expect_equal(body$errors[[1]]$detail, paste0(
    "Can't add header 'Content-Type' with value 'image/png'. Header already ",
    "exists with value 'application/octet-stream'."))
})


test_that("build api - text endpoint", {
  text <- function() {
    "some text"
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/text", text,
    returning = porcelain_returning_text(),
    validate = TRUE)

  res <- endpoint$run()
  expect_equal(res$status_code, 200)
  expect_equal(res$content_type, "text/plain")
  expect_null(res$headers)
  expect_true(res$validated)
  expect_equal(res$data, "some text")

  res_api <- endpoint$request()
  expect_equal(res_api$status, 200)
  expect_equal(res_api$headers[["Content-Type"]], "text/plain")
  expect_equal(res_api$body, "some text")
})
