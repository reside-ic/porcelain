test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("lock_bindings", {
  e <- new.env(parent = emptyenv())
  e$a <- 1
  e$b <- 2
  lock_bindings(c("a", "b"), e)
  expect_error(e$a <- 2)
})


test_that("parse_path_parameters", {
  expect_null(parse_path_parameters("/"))
  expect_null(parse_path_parameters("/no/routing/at/all"))
  expect_equal(parse_path_parameters("/my/<id>"),
               cbind(name = "id", type = "string"))
  expect_equal(parse_path_parameters("/my/<dynamic>/path"),
               cbind(name = "dynamic", type = "string"))
  expect_equal(parse_path_parameters("/my/<id:int>"),
               cbind(name = "id", type = "integer"))
  expect_equal(parse_path_parameters("/my/<id:int>/<action>"),
               cbind(name = c("id", "action"),
                     type = c("integer", "string")))
  expect_equal(parse_path_parameters("/my/<id:apple>/<action>"),
               cbind(name = c("id", "action"),
                     type = c("string", "string")))
})


test_that("parse_mime", {
  expect_equal(
    parse_mime("text/plain"),
    list(mime = "text/plain", type = "text", subtype = "plain",
         is_text = TRUE))
  expect_equal(
    parse_mime("application/json"),
    list(mime = "application/json", type = "application", subtype = "json",
         is_text = TRUE))
  expect_equal(
    parse_mime("application/octet-stream"),
    list(mime = "application/octet-stream", type = "application",
         subtype = "octet-stream", is_text = FALSE))

  expect_equal(
    parse_mime("text/plain;charset=UTF-8"),
    parse_mime("text/plain"))
})


test_that("bind_args", {
  f <- function(a, b, c) {
    list(a, b, c)
  }

  g <- bind_args(f, list(a = 1))
  expect_equal(g(2, 3), list(1, 2, 3))

  g <- bind_args(f, list(a = 1, c = 3))
  expect_equal(g(2), list(1, 2, 3))

  g <- bind_args(f, list(c = 3, b = 2))
  expect_equal(g(1), list(1, 2, 3))
})


test_that("detect package root under pkgload", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("mockery")
  skip_on_cran()

  mock_pkgload_loaded <- mockery::mock(FALSE, TRUE, TRUE, TRUE, TRUE)
  mock_is_dev_package <- mockery::mock(FALSE,
                                       TRUE, TRUE,
                                       TRUE, FALSE)

  mockery::stub(package_file_root, "pkgload_loaded",
                mock_pkgload_loaded)
  mockery::stub(package_file_root, "pkgload::is_dev_package",
                mock_is_dev_package)

  package <- "jsonlite"
  root_real <- system.file(package = package, mustWork = TRUE)
  root_pkgload <- file.path(root_real, "inst")

  ## 1. pkgload not loaded so path must be real
  expect_equal(package_file_root(package), root_real)
  mockery::expect_called(mock_pkgload_loaded, 1)
  mockery::expect_called(mock_is_dev_package, 0)

  ## 2. package is not a dev package, so path must be real
  expect_equal(package_file_root(package), root_real)
  mockery::expect_called(mock_pkgload_loaded, 2)
  mockery::expect_called(mock_is_dev_package, 1)

  ## 3. package is a dev package, so is porcelain, so path is real
  expect_equal(package_file_root(package), root_real)
  mockery::expect_called(mock_pkgload_loaded, 3)
  mockery::expect_called(mock_is_dev_package, 3)

  ## 4. package is a dev package, porcelain is not is incorrect
  expect_equal(package_file_root(package), root_pkgload)
  mockery::expect_called(mock_pkgload_loaded, 4)
  mockery::expect_called(mock_is_dev_package, 5)
})


test_that("destructure body", {
  a <- "[1, 2, 3]"
  b <- '{"x": 1, "y": 2}'
  json <- sprintf('{"a": %s, "b": %s}', a, b)

  ## Standardise json spacing:
  std_json <- function(x) {
    cache$v8$eval(sprintf("JSON.stringify(JSON.parse('%s'))", x))
  }

  expect_equal(json_parse_depth1(json),
               list(a = std_json(a),
                    b = std_json(b)))
})
