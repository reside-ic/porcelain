context("error")

test_that("error can be constructed", {
  err <- expect_error(
    pkgapi_error(c(a = "error")), class = "pkgapi_error")
  expect_equal(err$data, list(list(error = jsonlite::unbox("a"),
                                   detail = jsonlite::unbox("error"))))
  expect_equal(err$status_code, 400L)
})


test_that("error case", {
  hello <- function() {
    pkgapi_error(c("an-error" = "An error has occured"))
  }
  ## Even better might not be to use the error handling here at all
  ## and have the endpint return this, because this is going to get
  ## annoying:
  err <- tryCatch(hello(), pkgapi_error = identity)

  endpoint <- pkgapi_endpoint_json(hello, "String", "schema")
  pr <- pkgapi$new()
  pr$handle("GET", "/error", endpoint)
  res <- test_call(pr, "GET", "/error")
  expect_equal(res$status, 400)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$body, to_json_string(response_failure(err$data)))
})
