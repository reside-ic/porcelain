context("validation")


test_that("find schema root", {
  handler <- response_failure # important thing is that it is in the our ns
  expect_equal(schema_root(".", handler), normalizePath("."))
  expect_equal(schema_root(NULL, handler),
               system_file("schema", package = "pkgapi"))
  expect_error(schema_root(tempfile(), handler))
})


test_that("validate successful return", {
  root <- system_file("schema", package = "pkgapi")
  v <- pkgapi_validator("response-success", root)
  expect_true(v(to_json(response_success(NULL))))
  expect_true(v(to_json(response_success(1))))
})


test_that("validate errors", {
  root <- system_file("schema", package = "pkgapi")
  v <- pkgapi_validator("response-failure", root)

  f <- function(x) {
    pkgapi_process_error(pkgapi_error_object(x, 400L))
  }

  e1 <- f(c("ERROR" = "reason"))
  expect_equal(e1$value$errors, list(list(error = jsonlite::unbox("ERROR"),
                                          detail = jsonlite::unbox("reason"))))
  expect_true(v(e1$body))

  e2 <- f(list("ERROR" = NULL))
  expect_equal(e2$value$errors, list(list(error = jsonlite::unbox("ERROR"),
                                          detail = NULL)))
  expect_true(v(e2$body))

  e3 <- f(list("ERROR" = NULL, "OTHER" = "reason"))
  expect_equal(e3$value$errors,
               list(list(error = jsonlite::unbox("ERROR"),
                         detail = NULL),
                    list(error = jsonlite::unbox("OTHER"),
                         detail = jsonlite::unbox("reason"))))
  expect_true(v(e3$body))
})
