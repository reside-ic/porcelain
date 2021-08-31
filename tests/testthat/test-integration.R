test_that("Can run add package", {
  skip_on_cran()
  skip_if_not_installed("pkgload")
  path_pkg <- system_file("examples/add", package = "porcelain")
  pkg <- pkgload::load_all(path_pkg, export_all = FALSE, quiet = TRUE)
  on.exit(pkgload::unload("add"))

  api <- pkg$env$api(TRUE)
  res <- api$request("GET", "/", c(a = 1, b = 2))
  expect_equal(res$headers[["X-Porcelain-Validated"]], "true")
  expect_equal(from_json(res$body)$status, "success")
  expect_equal(from_json(res$body)$data, 3)
})
