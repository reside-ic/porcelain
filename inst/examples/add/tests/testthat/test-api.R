test_that("can add two numbers", {
  expect_equal(add(1, 2), jsonlite::unbox(3))
  expect_equal(add(3.14, 1.23), jsonlite::unbox(4.37))
})

test_that("add endpoint", {
  endpoint <- endpoint_add()
  res <- endpoint$run(1, 2)
  expect_equal(res$data, add(1, 2))
  expect_equal(res$status_code, 200)
  expect_equal(res$content_type, "application/json")

  obj <- api(validate = TRUE)
  res_api <- obj$request("GET", "/", list(a = 1, b = 2))
  expect_equal(res_api$body, res$body)
  expect_equal(res_api$headers[["X-Porcelain-Validated"]], "true")
})

test_that("api fails gracefully when given non-numeric input", {
  obj <- api(validate = TRUE)
  res <- obj$request("GET", "/", list(a = 1, b = "x"))
  expect_equal(res$status, 400)
  body <- jsonlite::fromJSON(res$body, simplifyDataFrame = FALSE)
  expect_equal(body$status, "failure")
  expect_equal(body$errors[[1]]$error, "INVALID_INPUT")
})
