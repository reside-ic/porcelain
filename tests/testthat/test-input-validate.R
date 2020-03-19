context("input validation")

test_that("validate mime", {
  expect_error(
    pkgapi_input_validate_mime(NULL, "application/json"),
    "Content-Type was not set (expected 'application/json')",
    fixed = TRUE, class = "pkgapi_error")
  expect_error(
    pkgapi_input_validate_mime("application/octet-stream", "application/json"),
    paste("Expected content type 'application/json'",
          "but was sent 'application/octet-stream'"),
    fixed = TRUE, class = "pkgapi_error")
})
