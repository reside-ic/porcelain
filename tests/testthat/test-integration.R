context("integration")

test_that("Can run add package", {
  skip_on_cran()
  skip_if_not_installed("pkgload")

  add <- system_file("examples/add", package = "porcelain")
  pkg <- pkgload::load_all(add, export_all = FALSE)
  on.exit(pkgload::unload("add"))

  api <- pkg$env$api(TRUE)
  res <- api$request("GET", "/", c(a = 1, b = 2))
  expect_equal(res$headers[["X-Porcelain-Validated"]], "true")
  expect_equal(from_json(res$body)$status, "success")
  expect_equal(from_json(res$body)$data, 3)
})
