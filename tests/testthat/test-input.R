context("input")


test_that("logical validator works", {
  expect_true(pkgapi_input_validator_logical("TRUE"))
  expect_true(pkgapi_input_validator_logical("True"))
  expect_true(pkgapi_input_validator_logical("true"))
  expect_true(pkgapi_input_validator_logical("T"))
  expect_false(pkgapi_input_validator_logical("FALSE"))
  expect_false(pkgapi_input_validator_logical("False"))
  expect_false(pkgapi_input_validator_logical("false"))
  expect_false(pkgapi_input_validator_logical("F"))

  expect_error(pkgapi_input_validator_logical("1"),
               "Could not convert '1' into a logical")
  expect_error(pkgapi_input_validator_logical("maybe"),
               "Could not convert 'maybe' into a logical")
})


test_that("integer validator works", {
  expect_equal(pkgapi_input_validator_integer("1"), 1L)
  expect_equal(pkgapi_input_validator_integer("-100"), -100L)

  expect_error(pkgapi_input_validator_integer("one"),
               "Could not convert 'one' into an integer")
  expect_error(pkgapi_input_validator_integer("string"),
               "Could not convert 'string' into an integer")

  expect_error(pkgapi_input_validator_integer("1.4"),
               "Could not convert '1.4' into an integer (loses precision)",
               fixed = TRUE)
})


test_that("integer validator works", {
  expect_equal(pkgapi_input_validator_numeric("1"), 1)
  expect_equal(pkgapi_input_validator_numeric("-100"), -100)
  expect_equal(pkgapi_input_validator_numeric("1.23"), 1.23)
  expect_equal(pkgapi_input_validator_numeric("1e-5"), 1e-5)

  expect_error(pkgapi_input_validator_numeric("one"),
               "Could not convert 'one' into a numeric")
  expect_error(pkgapi_input_validator_numeric("string"),
               "Could not convert 'string' into a numeric")
})


test_that("string validator", {
  expect_equal(pkgapi_input_validator_string("1"), "1")
  expect_equal(pkgapi_input_validator_string("TRUE"), "TRUE")
  expect_equal(pkgapi_input_validator_string("string"), "string")
  expect_error(pkgapi_input_validator_string(letters), "must be a scalar")
})


test_that("Validate query parameters", {
  q <- pkgapi_input_query(a = "numeric", b = "numeric")
  args <- formals(function(a, b) NULL)
  res <- pkgapi_input_validator_simple(q, args, "query")
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

  expect_equal(multiply$inputs(NULL, list(a = "1", b = "2"), NULL),
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
  expect_equal(dat$errors[[1]]$error, "INVALID_INPUT")
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


test_that("use text routing parameter", {
  operation <- function(type, x, y) {
    op <- switch(type,
           "plus" = `+`,
           "minus" = `-`)
    jsonlite::unbox(op(x, y))
  }
  endpoint <- pkgapi_endpoint$new(
    "GET", "/operation/<type>", operation,
    returning = pkgapi_returning_json("Number", "schema"),
    input_query = pkgapi_input_query(x = "numeric", y = "numeric"),
    validate = TRUE)

  res <- endpoint$run(type = "plus", x = 3, y = 4)
  expect_equal(res$status_code, 200)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, jsonlite::unbox(7))
  expect_equal(res$body, to_json_string(response_success(res$data)))

  ## Through the api
  pr <- pkgapi$new()$handle(endpoint)
  res_api <- pr$request("GET", "/operation/plus", c(x = 3, y = 4))
  expect_equal(res_api$status, 200)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, res$body)
})


test_that("Can use an optional logical parameter", {
  hello <- function(positive = TRUE) {
    jsonlite::unbox(if (positive) 1 else -1)
  }
  endpoint <- pkgapi_endpoint$new(
    "GET", "/hello", hello,
    returning = pkgapi_returning_json("Number", "schema"),
    input_query = pkgapi_input_query(positive = "logical"),
    validate = TRUE)

  ## endpoint directly:
  resf <- endpoint$run(FALSE)
  expect_equal(resf$status_code, 200)
  expect_equal(resf$content_type, "application/json")
  expect_equal(resf$data, jsonlite::unbox(-1))
  expect_equal(resf$body, to_json_string(response_success(resf$data)))

  rest <- endpoint$run()
  expect_equal(rest$status_code, 200)
  expect_equal(rest$content_type, "application/json")
  expect_equal(rest$data, jsonlite::unbox(1))
  expect_equal(rest$body, to_json_string(response_success(rest$data)))

  ## Through the api
  pr <- pkgapi$new()$handle(endpoint)
  resf_api <- pr$request("GET", "/hello", c(positive = FALSE))
  expect_equal(resf_api$status, 200)
  expect_equal(resf_api$headers[["Content-Type"]], "application/json")
  expect_equal(resf_api$body, resf$body)

  pr <- pkgapi$new()$handle(endpoint)
  rest_api <- pr$request("GET", "/hello")
  expect_equal(rest_api$status, 200)
  expect_equal(rest_api$headers[["Content-Type"]], "application/json")
  expect_equal(rest_api$body, rest$body)
})


test_that("use binary body", {
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


test_that("Use json body", {
  square <- function(n) {
    x <- jsonlite::fromJSON(n)
    jsonlite::unbox(x * x)
  }
  endpoint <- pkgapi_endpoint$new(
    "POST", "/square", square,
    returning = pkgapi_returning_json("Number", "schema"),
    input_body = pkgapi_input_body_json("n", "Number", "schema"),
    validate = TRUE)

  data <- 3
  payload <- to_json_string(jsonlite::unbox(data))

  ## endpoint directly:
  res <- endpoint$run(n = payload)
  expect_equal(res$status_code, 200)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, jsonlite::unbox(9L))
  expect_equal(res$body, to_json_string(response_success(res$data)))

  ## Through the api
  pr <- pkgapi$new()$handle(endpoint)
  res_api <- pr$request("POST", "/square", body = payload)
  expect_equal(res_api$status, 200)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, res$body)
})


test_that("inputs must match function args", {
  square <- function(n) {
    jsonlite::unbox(n * n)
  }
  expect_error(pkgapi_endpoint$new(
    "GET", "/square", square,
    returning = pkgapi_returning_json("Number", "schema"),
    input_query = pkgapi_input_query(m = "numeric"),
    validate = TRUE),
    "Argument 'm' (used in query) missing from the target function",
    fixed = TRUE)
})


test_that("No duplicated args allowed", {
  square <- function(n) {
    jsonlite::unbox(n * n)
  }
  expect_error(pkgapi_endpoint$new(
    "GET", "/square/<n>", square,
    returning = pkgapi_returning_json("Number", "schema"),
    input_query = pkgapi_input_query(n = "numeric"),
    validate = TRUE),
    "Duplicated parameter names: 'n' (in path), 'n' (in query)",
    fixed = TRUE)
})
