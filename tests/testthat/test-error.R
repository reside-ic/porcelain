context("error")


test_that("construct error", {
  mock_key <- mockery::mock("fake_key", cycle = TRUE)
  with_mock("ids::proquint" = mock_key, {
    expect_equal(
      pkgapi_error_data(list(a = NULL)),
      list(list(error = jsonlite::unbox("a"), detail = NULL,
                key = jsonlite::unbox("fake_key"))))
    expect_equal(
      pkgapi_error_data(list(a = "b")),
      list(list(error = jsonlite::unbox("a"), detail = jsonlite::unbox("b"),
                key = jsonlite::unbox("fake_key"))))
    expect_equal(
      pkgapi_error_data(list(a = "b", c = NULL)),
      list(list(error = jsonlite::unbox("a"), detail = jsonlite::unbox("b"),
                key = jsonlite::unbox("fake_key")),
           list(error = jsonlite::unbox("c"), detail = NULL,
                key = jsonlite::unbox("fake_key"))))
    value <- "test"
    trace <- c(jsonlite::unbox("this is"), jsonlite::unbox("the trace"))
    attr(value, "trace") <- trace
    z <- pkgapi_error_data(list(a = value))
    expect_equal(
      pkgapi_error_data(list(a = value)),
      list(list(error = jsonlite::unbox("a"), detail = jsonlite::unbox("test"),
                key = jsonlite::unbox("fake_key"), trace = trace)))
    expect_error(
      pkgapi_error_data(list(a = 1)),
      "All error details must be character or NULL")
  })
})


test_that("error can be constructed", {
  mock_key <- mockery::mock("fake_key", cycle = TRUE)
  with_mock("ids::proquint" = mock_key, {
    err <- expect_error(
      pkgapi_error(c(a = "error")), class = "pkgapi_error")
  })
  expect_equal(err$data, list(list(error = jsonlite::unbox("a"),
                                   detail = jsonlite::unbox("error"),
                                   key = jsonlite::unbox("fake_key"))))
  expect_equal(err$status_code, 400L)
  expect_match(err$message, "pkgapi_error:\\s+\\* a: error")
})


test_that("Catch an error in a json endpoint", {
  detail <- "An error has occured"
  attr(detail, "trace") <- c(jsonlite::unbox("the"),
                             jsonlite::unbox("trace"))
  hello <- function() {
    pkgapi_stop(message = detail, "an-error")
  }

  endpoint <- pkgapi_endpoint$new(
    "GET", "/", hello,
    returning = pkgapi_returning_json())
  expect_error(endpoint$target(), class = "pkgapi_error")

  mock_key <- mockery::mock("fake_key", cycle = TRUE)
  with_mock("ids::proquint" = mock_key, {
    err <- get_error(hello())
    res <- endpoint$run()
  })
  expect_equal(res, pkgapi_process_error(err))
  expect_equal(res$status_code, 400L)
  expect_equal(res$content_type, "application/json")

  expect_equal(res$value$errors[[1]]$error,
               jsonlite::unbox("an-error"))
  expect_equal(res$value$errors[[1]]$detail,
               jsonlite::unbox("An error has occured"))
  expect_equal(res$value$errors[[1]]$key,
               jsonlite::unbox("fake_key"))
  expect_equal(res$value$errors[[1]]$trace,
               c(jsonlite::unbox("the"), jsonlite::unbox("trace")))

  expect_true(validator_response_failure(res$body))
})


test_that("Catch error in a binary endpoint", {
  binary <- function() {
    pkgapi_error(c("an-error" = "An error has occured"))
  }

  endpoint <- pkgapi_endpoint$new(
    "GET", "/binary", binary,
    returning = pkgapi_returning_binary())
  expect_error(endpoint$target(), class = "pkgapi_error")

  mock_key <- mockery::mock("fake_key", cycle = TRUE)
  with_mock("ids::proquint" = mock_key, {
    err <- get_error(binary())
    res <- endpoint$run()
  })
  expect_equal(res, pkgapi_process_error(err))
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
  endpoint <- pkgapi_endpoint$new(
    "GET", "/", hello,
    returning = pkgapi_returning_json("String", "schema"),
    validate = TRUE)

  mock_key <- mockery::mock("fake_key", cycle = TRUE)
  with_mock("ids::proquint" = mock_key, {
    err <- get_error(hello())
    res <- endpoint$run()
    expect_equal(res, pkgapi_process_error(err))
  })
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
  endpoint <- pkgapi_endpoint$new(
    "GET", "/", hello,
    returning = pkgapi_returning_json("String", "schema"),
    validate = TRUE)
  pr <- pkgapi$new()
  pr$handle(endpoint)
  mock_key <- mockery::mock("fake_key", cycle = TRUE)
  with_mock("ids::proquint" = mock_key, {
    res <- pr$request("GET", "/")

    expect_equal(res$status, 500L)
    expect_equal(res$headers[["Content-Type"]], "application/json")
    expect_equal(res$body, endpoint$run()$body)
  })

  expect_true(validator_response_failure(res$body))
})


test_that("Catch error from the api", {
  hello <- function() {
    pkgapi_error(c("an-error" = "An error has occured"))
  }
  endpoint <- pkgapi_endpoint$new(
    "GET", "/", hello,
    returning = pkgapi_returning_json("String", "schema"),
    validate = TRUE)
  pr <- pkgapi$new()
  pr$handle(endpoint)

  mock_key <- mockery::mock("fake_key", cycle = TRUE)
  with_mock("ids::proquint" = mock_key, {
  res <- pr$request("GET", "/")
    expect_equal(res$status, 400L)
    expect_equal(res$headers[["Content-Type"]], "application/json")
    expect_equal(res$body, endpoint$run()$body)
  })

  expect_true(validator_response_failure(res$body))
})


## this is not actually super likely but seems worth checking for as
## we do move pass our error handler along.
test_that("Error during serialisation", {
  hello <- function() {
    jsonlite::unbox("hello")
  }
  returning <- pkgapi_returning_json()
  returning$content_type <- NULL
  endpoint <- pkgapi_endpoint$new("GET", "/", hello, returning = returning)
  val <- endpoint$run()

  ## First, work our what the error should look like:
  err <- get_error(pkgapi_do_serialize_pass(val, plumber_response()))
  mock_key <- mockery::mock("fake_key", cycle = TRUE)
  with_mock("ids::proquint" = mock_key, {
    cmp <- pkgapi_process_error(err)
  })
  expect_equal(cmp$value$errors[[1]]$error, jsonlite::unbox("SERVER_ERROR"))
  expect_equal(cmp$value$errors[[1]]$detail, jsonlite::unbox(err$message))
  expect_equal(cmp$value$errors[[1]]$key, jsonlite::unbox("fake_key"))

  ## Then, get this from the endpoint,
  req <- NULL
  with_mock("ids::proquint" = mock_key, {
    res <- plumber_response()
    ans <- pkgapi_serialize_pass(val, req, res, pkgapi_error_handler)
  })

  expect_equal(res$status, 500L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, cmp$body)

  ## All the way from the api:
  pr <- pkgapi$new()
  pr$handle(endpoint)
  with_mock("ids::proquint" = mock_key, {
    res_api <- pr$request("GET", "/")
  })
  expect_equal(res_api$status, 500L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$body, res$body)

  expect_true(validator_response_failure(res_api$body))
})

test_that("Catch error from the api with trace", {
  detail <- "An error has occured"
  attr(detail, "trace") <- c(jsonlite::unbox("the"),
                             jsonlite::unbox("trace"))
  hello <- function() {
    pkgapi_stop(message = detail, "an-error")
  }
  endpoint <- pkgapi_endpoint$new(
    "GET", "/", hello,
    returning = pkgapi_returning_json("String", "schema"),
    validate = TRUE)
  pr <- pkgapi$new()
  pr$handle(endpoint)

  mock_key <- mockery::mock("fake_key", cycle = TRUE)
  with_mock("ids::proquint" = mock_key, {
    res <- pr$request("GET", "/")
    expect_equal(res$status, 400L)
    expect_equal(res$headers[["Content-Type"]], "application/json")
    expect_equal(res$body, endpoint$run()$body)
  })

  expect_true(validator_response_failure(res$body))

  body <- jsonlite::parse_json(res$body)
  expect_equal(body$errors[[1]]$error, "an-error")
  expect_equal(body$errors[[1]]$detail, "An error has occured")
  expect_equal(body$errors[[1]]$key, "fake_key")
  expect_equal(body$errors[[1]]$trace, list("the", "trace"))
})
