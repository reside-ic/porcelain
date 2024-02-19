test_that("Can run add package", {
  skip_on_cran()
  skip_if_not_installed("pkgload")
  path_pkg <- system_file("examples/add", package = "porcelain")
  pkg <- pkgload::load_all(path_pkg, export_all = FALSE, quiet = TRUE)
  on.exit(pkgload::unload("add"))

  log_path <- tempfile()
  api <- pkg$env$api(TRUE, log_path)
  res <- api$request("GET", "/", c(a = 1, b = 2))
  expect_equal(res$headers[["X-Porcelain-Validated"]], "true")
  expect_equal(from_json(res$body)$status, "success")
  expect_equal(from_json(res$body)$data, 3)
})

test_that("can get logs from server", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("pkgload")

  path_add <- system_file("examples/add", package = "porcelain")
  pkg <- pkgload::load_all(path_add,
                           export_all = FALSE, attach_testthat = FALSE,
                           warn_conflicts = FALSE, quiet = TRUE)

  msgs <- testthat::capture_messages(
    bg <- porcelain_background$new(pkg$env$api,
                                   # Set log_path to NULL to log to stdout
                                   list(validate = TRUE, log_path = NULL),
                                   verbose = TRUE))

  expect_equal(bg$status(), "stopped")
  msgs <- testthat::capture_messages(bg$start())
  expect_equal(bg$status(), "running")

  r <- bg$request("GET", "/", query = list(a = 1, b = 2),
                  httr::add_headers("x-request-id" = "id123"))

  expect_equal(r$status_code, 200)
  expect_mapequal(
    from_json(httr::content(r, encoding = "UTF-8", as = "text")),
    list(status = "success", errors = NULL, data = 3))

  logs <- readLines(bg$log)
  postroute_log <- jsonlite::fromJSON(logs[[length(logs) - 1]])
  postserialize_log <- jsonlite::fromJSON(logs[[length(logs)]])
  expect_equal(postroute_log$caller, "postroute")
  expect_equal(postroute_log$request_id, "id123")
  expect_equal(postserialize_log$caller, "postserialize")
  expect_equal(postserialize_log$request_id, "id123")

  ## Without a request ID
  r <- bg$request("GET", "/", query = list(a = 1, b = 2))
  logs <- readLines(bg$log)
  postroute_log <- jsonlite::fromJSON(logs[[length(logs) - 1]])
  postserialize_log <- jsonlite::fromJSON(logs[[length(logs)]])
  expect_equal(postroute_log$caller, "postroute")
  expect_match(postroute_log$request_id, uuid_regex)
  expect_equal(postserialize_log$caller, "postserialize")
  expect_equal(postserialize_log$request_id, postroute_log$request_id)
})
