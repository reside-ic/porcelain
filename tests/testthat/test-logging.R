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
  err <- get_error(hello())

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
})


test_that("log multiple errors into a single log entry", {
  hello <- function() {
    porcelain_stop("An error has occured", "an-error",
                   list(ERROR1 = "message1", ERROR2 = "message2"))
  }
  err <- get_error(hello())

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
