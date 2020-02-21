context("validation")

test_that("validate successful return", {
  root <- system_file("schema", package = "pkgapi")
  v <- pkgapi_validator("response-success", root)
  expect_true(v(to_json(response_success(NULL))))
  expect_true(v(to_json(response_success(1))))
})


test_that("validate errors", {
  root <- system_file("schema", package = "pkgapi")
  v <- pkgapi_validator("response-failure", root)

  e1 <- response_failure(c("ERROR" = "reason"))
  expect_equal(e1$errors, list(list(error = jsonlite::unbox("ERROR"),
                                    detail = jsonlite::unbox("reason"))))
  expect_true(v(to_json(e1)))

  e2 <- response_failure(list("ERROR" = NULL))
  expect_equal(e2$errors, list(list(error = jsonlite::unbox("ERROR"),
                                    detail = NULL)))
  expect_true(v(to_json(e2)))

  e3 <- response_failure(list("ERROR" = NULL, "OTHER" = "reason"))
  expect_equal(e3$errors,
               list(list(error = jsonlite::unbox("ERROR"),
                         detail = NULL),
                    list(error = jsonlite::unbox("OTHER"),
                         detail = jsonlite::unbox("reason"))))
  expect_true(v(to_json(e3)))
})
