context("headers")

test_that("headers are set as attributes", {
  headers <- list(
    "Content-Diposition" = "test",
    "Accept-Language" = "ENG"
  )
  out <- pkgapi_add_headers("output", headers)

  expect_equivalent(out, "output")
  expect_equal(attributes(out), list("pkgapi_headers" = headers))
})

test_that("headers can get headers from object", {
  headers <- list(
    "Content-Diposition" = "test",
    "Accept-Language" = "ENG"
  )
  out <- pkgapi_add_headers("output", headers)

  expect_equal(get_pkgapi_headers(out), headers)
})

test_that("headers can be removed", {
  headers <- list(
    "Content-Diposition" = "test",
    "Accept-Language" = "ENG"
  )
  out <- pkgapi_add_headers("output", headers)

  expect_equivalent(out, "output")
  expect_equal(attributes(out), list("pkgapi_headers" = headers))

  out <- remove_pkgapi_headers(out)
  expect_equal(out, "output")
})
