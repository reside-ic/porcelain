context("headers")

test_that("headers are set as attributes", {
  headers <- list(
    "Content-Diposition" = "test",
    "Accept-Language" = "ENG"
  )
  out <- pkgapi_add_headers("output", headers)

  expect_equivalent(out, "output")
  expect_equal(attributes(out), headers)
})
