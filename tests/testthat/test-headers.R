test_that("headers are set as attributes", {
  headers <- list(
    "Content-Diposition" = "test",
    "Accept-Language" = "ENG"
  )
  out <- porcelain_add_headers("output", headers)

  expect_equal(out, "output", ignore_attr = TRUE)
  expect_equal(attributes(out), list("porcelain_headers" = headers))
})

test_that("headers can get headers from object", {
  headers <- list(
    "Content-Diposition" = "test",
    "Accept-Language" = "ENG"
  )
  out <- porcelain_add_headers("output", headers)

  expect_equal(get_porcelain_headers(out), headers)
})

test_that("headers can be removed", {
  headers <- list(
    "Content-Diposition" = "test",
    "Accept-Language" = "ENG"
  )
  out <- porcelain_add_headers("output", headers)

  expect_equal(out, "output", ignore_attr = TRUE)
  expect_equal(attributes(out), list("porcelain_headers" = headers))

  out <- remove_porcelain_headers(out)
  expect_equal(out, "output")
})
