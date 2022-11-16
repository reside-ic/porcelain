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
    log[[2]][c("caller", "msg", "method", "path", "query", "headers")],
    list(caller = "postroute", msg = "request", method = "GET", path = "/",
         query = list(), headers = list()))

  expect_equal(log[[3]][c("caller", "msg")],
               list(caller = "postserialize",
                    msg = "response GET / => 200 (49 bytes)"))
  datetime_pattern <- "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"
  expect_match(log[[3]]$request_received, datetime_pattern)
  expect_match(log[[3]]$elapsed, "\\d+ \\w+")
  expect_type(log[[3]]$elapsed_ms, "integer")

  expect_equal(
    log[[4]][c("caller", "msg", "method", "path", "query", "headers",
               "body")],
    list(caller = "postserialize", msg = "response", method = "GET", path = "/",
         query = list(), headers = list(),
         body = '{"status":"success","errors":null,"data":"hello"}'))
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
