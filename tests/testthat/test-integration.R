context("integration")

test_that("Can run add package", {
  skip_on_cran()
  skip_if_not_installed("remotes")
  skip_on_os("windows") # usual issues getting packages installed on CI
  lib <- tempfile()
  dir.create(lib, FALSE, TRUE)
  on.exit(unlink(lib, recursive = TRUE))
  withr::local_libpaths(lib, "prefix")
  add <- system_file("examples/add", package = "porcelain")
  remotes::install_local(add, lib = lib, force = TRUE)
  env <- loadNamespace("add", lib)
  api <- env$api(TRUE)
  res <- api$request("GET", "/", c(a = 1, b = 2))
  expect_equal(res$headers[["X-Porcelain-Validated"]], "true")
  expect_equal(from_json(res$body)$status, "success")
  expect_equal(from_json(res$body)$data, 3)
})
