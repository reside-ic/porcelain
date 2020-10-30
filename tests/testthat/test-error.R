context("error")


test_that("construct error", {
  expect_equal(
    porcelain_error_data(list(a = NULL)),
    list(list(error = jsonlite::unbox("a"), detail = NULL)))
  expect_equal(
    porcelain_error_data(list(a = list(detail = "b"))),
    list(list(error = jsonlite::unbox("a"), detail = jsonlite::unbox("b"))))
  expect_equal(
    porcelain_error_data(list(a = list(detail = "b"), c = NULL)),
    list(list(error = jsonlite::unbox("a"), detail = jsonlite::unbox("b")),
         list(error = jsonlite::unbox("c"), detail = NULL)))
  trace <- c(jsonlite::unbox("this is"), jsonlite::unbox("the trace"))
  expect_equal(
    porcelain_error_data(list(a = list(
                                detail = "test",
                                key = jsonlite::unbox("fake_key"),
                                trace = trace
                              ))),
    list(list(error = jsonlite::unbox("a"), detail = jsonlite::unbox("test"),
              key = jsonlite::unbox("fake_key"), trace = trace)))
  expect_error(
    porcelain_error_data(list(a = list(detail = 1))),
    "All error details must be character or NULL")
})


test_that("error can be constructed", {
  err <- expect_error(
    porcelain_error(list(a = list(detail = "error"))),
    class = "porcelain_error")
  expect_equal(err$data, list(list(error = jsonlite::unbox("a"),
                                   detail = jsonlite::unbox("error"))))
  expect_equal(err$status_code, 400L)
  expect_match(err$message, "porcelain_error:\\s+\\* a: error")
})


test_that("Catch an error in a json endpoint", {
  hello <- function() {
    porcelain_stop("An error has occured", "an-error")
  }
  err <- get_error(hello())

  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json())
  expect_error(endpoint$target(), class = "porcelain_error")

  res <- endpoint$run()
  expect_equal(res, porcelain_process_error(err))
  expect_equal(res$status_code, 400L)
  expect_equal(res$content_type, "application/json")

  expect_equal(res$value$errors[[1]]$error,
               jsonlite::unbox("an-error"))
  expect_equal(res$value$errors[[1]]$detail,
               jsonlite::unbox("An error has occured"))

  expect_true(validator_response_failure(res$body))
})


test_that("Catch error in a binary endpoint", {
  binary <- function() {
    porcelain_stop("An error has occured", "an-error")
  }
  err <- get_error(binary())

  endpoint <- porcelain_endpoint$new(
    "GET", "/binary", binary,
    returning = porcelain_returning_binary())
  expect_error(endpoint$target(), class = "porcelain_error")

  res <- endpoint$run()
  expect_equal(res, porcelain_process_error(err))
  expect_equal(res$status_code, 400L)
  expect_equal(res$content_type, "application/json")

  expect_equal(res$value$errors[[1]]$error,
               jsonlite::unbox("an-error"))
  expect_equal(res$value$errors[[1]]$detail,
               jsonlite::unbox("An error has occured"))

  expect_true(validator_response_failure(res$body))
})


test_that("Uncaught error", {
  hello <- function() {
    stop("Unexpected error!", call. = FALSE)
  }
  err <- get_error(hello())
  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)

  res <- endpoint$run()
  expect_equal(res, porcelain_process_error(err))
  expect_equal(res$status_code, 500L)
  expect_equal(res$content_type, "application/json")

  expect_equal(res$value$errors[[1]]$error,
               jsonlite::unbox("SERVER_ERROR"))
  expect_equal(res$value$errors[[1]]$detail,
               jsonlite::unbox("Unexpected error!"))

  expect_true(validator_response_failure(res$body))
})


test_that("Uncaught error from the api", {
  hello <- function() {
    stop("unexpected error!")
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)
  pr <- porcelain$new()
  pr$handle(endpoint)
  res <- pr$request("GET", "/")

  expect_equal(res$status, 500L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, endpoint$run()$body)

  expect_true(validator_response_failure(res$body))
})


test_that("Catch error from the api", {
  hello <- function() {
    porcelain_stop("An error has occured", "an-error")
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)
  pr <- porcelain$new()
  pr$handle(endpoint)

  res <- pr$request("GET", "/")
  expect_equal(res$status, 400L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, endpoint$run()$body)

  expect_true(validator_response_failure(res$body))
})


## this is not actually super likely but seems worth checking for as
## we do move pass our error handler along.
test_that("Error during serialisation", {
  hello <- function() {
    jsonlite::unbox("hello")
  }
  returning <- porcelain_returning_json()
  returning$content_type <- NULL
  endpoint <- porcelain_endpoint$new("GET", "/", hello, returning = returning)
  val <- endpoint$run()

  ## First, work our what the error should look like:
  err <- get_error(porcelain_do_serialize_pass(val, plumber_response()))
  cmp <- porcelain_process_error(err)
  expect_equal(cmp$value$errors[[1]]$error, jsonlite::unbox("SERVER_ERROR"))
  expect_equal(cmp$value$errors[[1]]$detail, jsonlite::unbox(err$message))

  ## Then, get this from the endpoint,
  req <- NULL
  res <- plumber_response()
  ans <- porcelain_serialize_pass(val, req, res, porcelain_error_handler)

  expect_equal(res$status, 500L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, cmp$body)

  ## All the way from the api:
  pr <- porcelain$new()
  pr$handle(endpoint)
  res_api <- pr$request("GET", "/")
  expect_equal(res_api$status, 500L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, res$body)

  expect_true(validator_response_failure(res_api$body))
})

test_that("Catch error from the api with additional args", {
  hello <- function() {
    porcelain_stop("An error has occured", "an-error",
                   key = jsonlite::unbox("fake_key"),
                   trace = c(jsonlite::unbox("the"), jsonlite::unbox("trace")))
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)
  pr <- porcelain$new()
  pr$handle(endpoint)

  res <- pr$request("GET", "/")
  expect_equal(res$status, 400L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, endpoint$run()$body)

  expect_true(validator_response_failure(res$body))

  body <- jsonlite::parse_json(res$body)
  expect_equal(body$errors[[1]]$error, "an-error")
  expect_equal(body$errors[[1]]$detail, "An error has occured")
  expect_equal(body$errors[[1]]$key, "fake_key")
  expect_equal(body$errors[[1]]$trace, list("the", "trace"))
})

test_that("porcelain_stop forms errors correctly", {
  err <- get_error(porcelain_stop(
    errors = c("ERROR" = "First message", "ERROR2" = "2nd message")))
  expect_equal(err$data, list(
                           list(
                             error = jsonlite::unbox("ERROR"),
                             detail = jsonlite::unbox("First message")
                           ),
                           list(
                             error = jsonlite::unbox("ERROR2"),
                             detail = jsonlite::unbox("2nd message")
                           )
                         ))
  expect_equal(err$status_code, 400)
})

test_that("porcelain throws error from malformed additional args", {
  expect_error(porcelain_stop("msg", "ERROR", NULL, 400L, "another_arg"),
               "'... args' must be named")
})
