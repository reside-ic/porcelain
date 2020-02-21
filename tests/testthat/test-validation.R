context("validation")

test_that("validate successful return", {
  root <- system_file("schema", package = "pkgapi")
  v <- pkgapi_validator("response-success", root)
  expect_true(v(to_json(response_success(NULL))))
  expect_true(v(to_json(response_success(1))))
})
