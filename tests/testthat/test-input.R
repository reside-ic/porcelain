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


test_that("Can accept query parameters from plumber", {
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
