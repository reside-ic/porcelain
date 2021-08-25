context("integration")

test_that("Can run add package", {
  skip_on_cran()
  lib <- tempfile()
  dir.create(lib, FALSE, TRUE)
  on.exit(unlink(lib, recursive = TRUE))
  withr::local_libpaths(lib, "prefix")
  add <- system_file("examples/add", package = "porcelain")

  system2("R", c("CMD", "INSTALL", paste0("--library=", lib), add),
          stdout = FALSE, stderr = FALSE)
  env <- loadNamespace("add", lib)
  api <- env$api(TRUE)
  res <- api$request("GET", "/", c(a = 1, b = 2))
  expect_equal(res$headers[["X-Porcelain-Validated"]], "true")
  expect_equal(from_json(res$body)$status, "success")
  expect_equal(from_json(res$body)$data, 3)
})
