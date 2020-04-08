context("state")

test_that("use mutable state from an endpoint", {
  ## Free function that we want to bind
  target_increment <- function(counter, by = 1) {
    jsonlite::unbox(counter$increment(by))
  }

  x <- counter$new()
  endpoint <- pkgapi_endpoint$new(
    "POST", "/increment", target_increment,
    pkgapi_input_query(by = "numeric"),
    pkgapi_state(counter = x),
    returning = pkgapi_returning_json("Number", "schema"))

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
  pr <- pkgapi$new()$handle(endpoint)
  res1_api <- pr$request("POST", "/increment")
  res2_api <- pr$request("POST", "/increment", query = list(by = 10))

  expect_equal(res1_api$body, res1$body)
  expect_equal(res2_api$body, res2$body)
})


test_that("Validate state against target", {
  expect_error(
    pkgapi_endpoint$new(
      "POST", "/increment", function(arg) NULL,
      pkgapi_state(msg = 1),
      returning = pkgapi_returning_json("Number", "schema")),
    "Argument 'msg' (used in state) missing from the target function",
    fixed = TRUE)
})
