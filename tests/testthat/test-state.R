context("state")

test_that("use mutable state from an endpoint", {
  ## Free function that we want to bind
  target_increment <- function(counter, by = 1) {
    jsonlite::unbox(counter$increment(by))
  }

  x <- counter$new()
  endpoint <- porcelain_endpoint$new(
    "POST", "/increment", target_increment,
    porcelain_input_query(by = "numeric"),
    porcelain_state(counter = x),
    returning = porcelain_returning_json("Number", "schema"))

  expect_equal(
    endpoint$target(),
    jsonlite::unbox(1))
  expect_equal(
    endpoint$target(10),
    jsonlite::unbox(11))

  x$reset()
  res1 <- endpoint$run()
  expect_equal(res1$status_code, 200)
  expect_equal(res1$data, jsonlite::unbox(1))

  res2 <- endpoint$run(10)
  expect_equal(res2$status_code, 200)
  expect_equal(res2$data, jsonlite::unbox(11))

  x$reset()
  pr <- porcelain$new()$handle(endpoint)
  res1_api <- pr$request("POST", "/increment")
  res2_api <- pr$request("POST", "/increment", query = list(by = 10))

  expect_equal(res1_api$body, res1$body)
  expect_equal(res2_api$body, res2$body)
})


test_that("Validate state against target", {
  expect_error(
    porcelain_endpoint$new(
      "POST", "/increment", function(arg) NULL,
      porcelain_state(msg = 1),
      returning = porcelain_returning_json("Number", "schema")),
    "Argument 'msg' (used in state) missing from the target function",
    fixed = TRUE)
})


test_that("Mutiple state entries are not allowed", {
  expect_error(
    porcelain_endpoint$new(
      "GET", "/add", function(a, b, c) NULL,
      porcelain_state(a = 1),
      porcelain_state(b = 2),
      returning = porcelain_returning_json("Number", "schema")),
    "Only one 'porcelain_state' can be passed to an endpoint")
})
