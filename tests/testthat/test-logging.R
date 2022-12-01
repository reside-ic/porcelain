test_that("Can log", {
  skip_if_not_installed("lgr")
  hello <- function() {
    jsonlite::unbox("hello")
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)

  logger <- test_logger("can-log")

  pr <- porcelain$new(logger = logger)
  pr$handle(endpoint)

  pr$request("GET", "/")

  log <- test_logger_read(logger)
  expect_length(log, 4)

  expect_equal(log[[1]][c("caller", "msg")],
               list(caller = "postroute", msg = "request GET /"))

  expect_equal(
    log[[2]][c("caller", "msg", "method", "path", "query", "headers",
               "endpoint")],
    list(caller = "postroute", msg = "request", method = "GET", path = "/",
         query = list(), headers = list(), endpoint = "/"))

  expect_equal(log[[3]][c("caller", "msg", "endpoint")],
               list(caller = "postserialize",
                    msg = "response GET / => 200 (49 bytes)",
                    endpoint = "/"))
  datetime_pattern <- "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"
  expect_match(log[[3]]$request_received, datetime_pattern)
  expect_match(log[[3]]$elapsed, "\\d+ \\w+")
  expect_type(log[[3]]$elapsed_ms, "integer")

  expect_equal(
    log[[4]][c("caller", "msg", "method", "path", "query", "headers",
               "body", "endpoint")],
    list(caller = "postserialize", msg = "response", method = "GET", path = "/",
         query = list(), headers = list(),
         body = '{"status":"success","errors":null,"data":"hello"}',
         endpoint = "/"))
})


test_that("Can log from endpoint with path variable", {
  skip_if_not_installed("lgr")
  id <- function(type, id) {
    jsonlite::unbox(id)
  }
  endpoint_path <- porcelain_endpoint$new(
    "GET", "/path/<type>/<id>", id,
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)
  param <- function(param) {
    jsonlite::unbox(param)
  }

  logger <- test_logger("can-log")

  pr <- porcelain$new(logger = logger)
  pr$handle(endpoint_path)

  pr$request("GET", "/path/value/123")

  log <- test_logger_read(logger)
  expect_length(log, 4)

  expect_equal(log[[1]][c("caller", "msg")],
               list(caller = "postroute", msg = "request GET /path/value/123"))

  expect_equal(
    log[[2]][c("caller", "msg", "method", "path", "query", "headers",
               "endpoint")],
    list(caller = "postroute", msg = "request", method = "GET",
         path = "/path/value/123", query = list(), headers = list(),
         endpoint = "/path/<type>/<id>"))

  expect_equal(log[[3]][c("caller", "msg", "endpoint")],
               list(caller = "postserialize",
                    msg = "response GET /path/value/123 => 200 (47 bytes)",
                    endpoint = "/path/<type>/<id>"))
  datetime_pattern <- "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"
  expect_match(log[[3]]$request_received, datetime_pattern)
  expect_match(log[[3]]$elapsed, "\\d+ \\w+")
  expect_type(log[[3]]$elapsed_ms, "integer")

  expect_equal(
    log[[4]][c("caller", "msg", "method", "path", "query", "headers",
               "body", "endpoint")],
    list(caller = "postserialize", msg = "response", method = "GET",
         path = "/path/value/123", query = list(), headers = list(),
         body = '{"status":"success","errors":null,"data":"123"}',
         endpoint = "/path/<type>/<id>"))
})


test_that("Can log from endpoint with query param", {
  skip_if_not_installed("lgr")
  param <- function(param) {
    jsonlite::unbox(param)
  }
  input_query <- porcelain_input_query(param = "string")
  endpoint_query <- porcelain_endpoint$new(
    "GET", "/query", param,
    input_query,
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)

  logger <- test_logger("can-log")

  pr <- porcelain$new(logger = logger)
  pr$handle(endpoint_query)

  pr$request("GET", "/query", c(param = "value"))

  log <- test_logger_read(logger)
  expect_length(log, 4)

  expect_equal(log[[1]][c("caller", "msg")],
               list(caller = "postroute", msg = "request GET /query"))

  expect_equal(
    log[[2]][c("caller", "msg", "method", "path", "query", "headers",
               "endpoint")],
    list(caller = "postroute", msg = "request", method = "GET", path = "/query",
         query = list(param = "value"), headers = list(),
         endpoint = "/query"))

  expect_equal(log[[3]][c("caller", "msg", "endpoint")],
               list(caller = "postserialize",
                    msg = "response GET /query => 200 (49 bytes)",
                    endpoint = "/query"))
  datetime_pattern <- "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"
  expect_match(log[[3]]$request_received, datetime_pattern)
  expect_match(log[[3]]$elapsed, "\\d+ \\w+")
  expect_type(log[[3]]$elapsed_ms, "integer")

  expect_equal(
    log[[4]][c("caller", "msg", "method", "path", "query", "headers",
               "body", "endpoint")],
    list(caller = "postserialize", msg = "response", method = "GET",
         path = "/query", query = list(param = "value"), headers = list(),
         body = '{"status":"success","errors":null,"data":"value"}',
         endpoint = "/query"))
})


test_that("binary output is converted to safe trace output", {
  skip_if_not_installed("lgr")
  endpoint <- porcelain_endpoint$new(
    "GET", "/", function() as.raw(0:255),
    returning = porcelain_returning_binary(),
    validate = TRUE)

  logger <- test_logger("can-log-binary")

  pr <- porcelain$new(logger = logger)
  pr$handle(endpoint)
  res <- pr$request("GET", "/")

  log <- test_logger_read(logger)
  expect_length(log, 4)
  expect_equal(log[[4]]$body, "<binary body (256 bytes)>")
})


test_that("log errors in a useful way", {
  hello <- function() {
    porcelain_stop("An error has occured", "an-error")
  }

  logger <- test_logger("error-single")

  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json("String", "schema"))

  pr <- porcelain$new(logger = logger)
  pr$handle(endpoint)
  res <- pr$request("GET", "/")

  log <- test_logger_read(logger)
  expect_length(log, 5)

  expect_equal(log[[4]]$msg, "error")
  expect_equal(log[[4]]$errors,
               list(list(error = "an-error", detail = "An error has occured")))
  expect_equal(log[[4]]$caller, "postserialize")
})


test_that("log multiple errors into a single log entry", {
  hello <- function() {
    porcelain_stop("An error has occured", "an-error",
                   list(ERROR1 = "message1", ERROR2 = "message2"))
  }

  logger <- test_logger("error-multiple")
  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json("String", "schema"))

  pr <- porcelain$new(logger = logger)
  pr$handle(endpoint)
  res <- pr$request("GET", "/")

  log <- test_logger_read(logger)
  expect_length(log, 5)
  expect_equal(log[[4]]$msg, "error")
  expect_equal(log[[4]]$errors,
               list(list(error = "ERROR1", detail = "message1"),
                    list(error = "ERROR2", detail = "message2")))
})


test_that("log incoming POST body", {
  hello <- function(body) {
    "hello"
  }

  logger <- test_logger("incoming-post")

  endpoint <- porcelain_endpoint$new(
    "POST", "/", hello,
    porcelain_input_body_json("body"),
    returning = porcelain_returning_json("String", "schema"))

  pr <- porcelain$new(logger = logger)
  pr$handle(endpoint)
  body <- '{"a": 1, "b": 2}'
  res <- pr$request("POST", "/", body = body)

  log <- test_logger_read(logger)
  expect_length(log, 4)
  expect_equal(log[[2]]$body, body)
})


test_that("Can construct logger with automatic name", {
  skip_if_not_installed("mockery")
  mock_package_name <- mockery::mock("foo")
  mockery::stub(porcelain_logger, "package_name", mock_package_name)
  logger <- porcelain_logger()
  mockery::expect_called(mock_package_name, 1)
  expect_equal(logger$name, "foo")
})


test_that("Can construct logger that logs to file", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  logger <- porcelain_logger(name = "porcelain/tests/file", path = tmp)
  logger$log("info", "hello")
  expect_true(file.exists(tmp))
  logs <- jsonlite::stream_in(file(tmp), verbose = FALSE)
  expect_equal(logs$msg, "hello")
  expect_equal(logs$logger, "porcelain/tests/file")
})


test_that("Can construct logger that logs to console", {
  logger <- porcelain_logger(name = "porcelain/tests/console")
  msg <- capture.output(logger$log("info", "hello"))
  expect_equal(jsonlite::parse_json(msg)$msg, "hello")
})
