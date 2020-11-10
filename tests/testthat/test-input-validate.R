context("input validation")

test_that("validate mime", {
  expect_error(
    porcelain_input_validate_mime(NULL, "application/json"),
    "Content-Type was not set (expected 'application/json')",
    fixed = TRUE, class = "porcelain_error")
  expect_error(
    porcelain_input_validate_mime("application/octet-stream",
                                  "application/json"),
    paste("Expected content type 'application/json'",
          "but was sent 'application/octet-stream'"),
    fixed = TRUE, class = "porcelain_error")
})


test_that("validate mime can OR between allowable types", {
  expect_error(
    porcelain_input_validate_mime("a", c("b", "c")),
    "Expected content type 'b'|'c' but was sent 'a'",
    fixed = TRUE, class = "porcelain_error")
})
