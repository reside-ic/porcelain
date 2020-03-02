context("input")


test_that("Validate query parameters", {
  q <- pkgapi_input_query(a = "numeric", b = "numeric")
  res <- pkgapi_input_validator_query(q, formals(function(a, b) NULL))
  expect_equal(res(list(a = "1", b = "2")), list(a = 1, b = 2))

  err <- expect_error(res(list(a = "1", b = "x")), class = "pkgapi_error")
  expect_match(
    err$message,
    "Error parsing query parameter 'b': Could not convert 'x'",
    fixed = TRUE)

  err <- expect_error(res(list(a = "1", b = "2", c = "3")),
                      class = "pkgapi_error")
  expect_match(
    err$message,
    "Recieved extra query parameters: 'c'",
    fixed = TRUE)
})


test_that("Can use single query parameter", {
  square <- function(n) {
    jsonlite::unbox(n * n)
  }
  endpoint <- pkgapi_endpoint$new(
    "GET", "/square", square,
    returning = pkgapi_returning_json("Number", "schema"),
    input_query = pkgapi_input_query(n = "numeric"),
    validate = TRUE)

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


test_that("Can validate query parameters from plumber, throwing nice errors", {
  multiply <- pkgapi_endpoint$new(
    "GET", "/multiply", function(a, b) jsonlite::unbox(a * b),
    input_query = pkgapi_input_query(a = "numeric", b = "numeric"),
    returning = pkgapi_returning_json())

  expect_equal(multiply$inputs(list(a = "1", b = "2")),
               list(a = 1, b = 2))

  api <- pkgapi$new()
  api$handle(multiply)

  res <- api$request("GET", "/multiply", c(a = 1, b = 2))
  expect_equal(res$status, 200)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, multiply$run(1, 2)$body)

  res <- api$request("GET", "/multiply", c(a = 1, b = "x"))
  expect_equal(res$status, 400)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  dat <- jsonlite::fromJSON(res$body, FALSE)
  expect_equal(dat$errors[[1]]$error, "INVALID_QUERY")
  expect_match(
    dat$errors[[1]]$detail,
    "Error parsing query parameter 'b': Could not convert 'x'",
    fixed = TRUE)
})


test_that("use routing parameter", {
  power <- function(n, m = 2) {
    jsonlite::unbox(n^m)
  }
  endpoint <- pkgapi_endpoint$new(
    "GET", "/power/<m:int>", power,
    returning = pkgapi_returning_json("Number", "schema"),
    input_query = pkgapi_input_query(n = "numeric"),
    validate = TRUE)

  ## endpoint directly:
  res <- endpoint$run(n = 4, m = 3)
  expect_equal(res$status_code, 200)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, jsonlite::unbox(64))
  expect_equal(res$body, to_json_string(response_success(res$data)))

  ## Through the api
  pr <- pkgapi$new()$handle(endpoint)
  res_api <- pr$request("GET", "/power/3", c(n = 4))
  expect_equal(res_api$status, 200)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, res$body)
})


test_that("use body", {
  mean_rds <- function(x) {
    jsonlite::unbox(mean(unserialize(x)))
  }
  endpoint <- pkgapi_endpoint$new(
    "POST", "/mean", mean_rds,
    returning = pkgapi_returning_json("Number", "schema"),
    input_body = pkgapi_input_body_binary("x"),
    validate = TRUE)

  data <- runif(10)
  payload <- serialize(data, NULL)

  ## endpoint directly:
  res <- endpoint$run(x = payload)
  expect_equal(res$status_code, 200)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, jsonlite::unbox(mean(data)))
  expect_equal(res$body, to_json_string(response_success(res$data)))

  ## Through the api
  pr <- pkgapi$new()$handle(endpoint)
  res_api <- pr$request("POST", "/mean", body = payload)
  expect_equal(res_api$status, 200)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, res$body)
})
