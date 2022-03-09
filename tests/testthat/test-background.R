test_that("can run background server on pkgload package", {
  skip_on_cran()
  skip_on_os("windows")

  path_add <- system_file("examples/add", package = "porcelain")
  pkg <- pkgload::load_all(path_add,
                           export_all = FALSE, attach_testthat = FALSE,
                           warn_conflicts = FALSE, quiet = TRUE)

  msgs <- testthat::capture_messages(
    bg <- porcelain_background$new(pkg$env$api, list(validate = TRUE),
                                   verbose = TRUE))
  expect_match(
    msgs[[1]],
    "Using development version of 'add' via pkgload")
  expect_match(
    msgs[[2]],
    "Using port [0-9]+")

  expect_equal(bg$status(), "stopped")
  msgs <- testthat::capture_messages(bg$start())
  expect_equal(bg$status(), "running")

  ## Check that startup messages are reasonable:
  expect_equal(msgs[[1]],
               "Waiting for server to become responsive\n")
  expect_match(msgs, "^\\.$", all = FALSE)
  expect_match(msgs[[length(msgs)]], "...OK in .+ s")

  r <- bg$request("GET", "/", query = list(a = 1, b = 2))

  expect_equal(r$status_code, 200)
  expect_mapequal(
    from_json(httr::content(r, encoding = "UTF-8", as = "text")),
    list(status = "success", errors = NULL, data = 3))

  expect_error(
    bg$start(),
    "Server already running")

  bg2 <- porcelain_background$new(pkg$env$api, port = bg$port)
  expect_equal(bg2$status(), "blocked")
  expect_error(
    bg2$start(),
    "Port '[0-9]+' is already in use")

  ## Pop this check here too:
  expect_false(check_port(bg$port))

  expect_message(
    bg$stop(),
    "Stopping server")

  expect_equal(bg$status(), "stopped")
  expect_silent(bg$stop())

  ## Pop this check here too:
  expect_true(check_port(bg$port))
})


test_that("Failure to start returns sensible information", {
  skip_on_cran()

  path_add <- system_file("examples/add", package = "porcelain")
  pkg <- pkgload::load_all(path_add,
                           export_all = FALSE, attach_testthat = FALSE,
                           warn_conflicts = FALSE, quiet = TRUE)

  bg <- porcelain_background$new(pkg$env$api, list(unknown = TRUE))
  err <- expect_error(bg$start())

  expect_s3_class(err, "porcelain_background_error")
  expect_match(err$message, "unused argument")
  expect_match(err$log, "Loading add", all = FALSE)
  expect_match(err$log, "unused argument", all = FALSE)
})


test_that("Timeout error returns sensible information", {
  skip_on_cran()
  skip_on_os("windows")

  path_add <- system_file("examples/add", package = "porcelain")
  pkg <- pkgload::load_all(path_add,
                           export_all = FALSE, attach_testthat = FALSE,
                           warn_conflicts = FALSE, quiet = TRUE)

  bg <- porcelain_background$new(pkg$env$api, timeout = 1e-4)
  err <- expect_error(bg$start())

  expect_s3_class(err, "porcelain_background_error")
  expect_equal(err$message, "Timeout reached")
})


test_that("background status string values are correct", {
  expect_equal(background_status_string(TRUE, TRUE), "running")
  expect_equal(background_status_string(TRUE, FALSE), "starting")
  expect_equal(background_status_string(FALSE, TRUE), "blocked")
  expect_equal(background_status_string(FALSE, FALSE), "stopped")
})


test_that("user hook empty if path_src not given", {
  expect_null(background_user_hook(NULL))
  expect_equal(
    background_user_hook("path"),
    quote(
      pkgload::load_all("path", export_all = FALSE, attach_testthat = FALSE,
                        helpers = FALSE)))
})


test_that("test with non-pkgload version", {
  skip_on_cran()

  create <- function() {
    api <- porcelain::porcelain$new()
    api$handle(
      porcelain::porcelain_endpoint$new(
        "GET", "/", function() jsonlite::unbox("porcelain"),
        returning = porcelain::porcelain_returning_json()))
    api
  }

  bg <- porcelain_background$new(create)
  bg$start()
  r <- bg$request("GET", "/")

  expect_equal(r$status_code, 200)
  expect_mapequal(
    jsonlite::fromJSON(httr::content(r, encoding = "UTF-8", as = "text")),
    list(status = "success", errors = NULL, data = "porcelain"))
})


test_that("Can pass environment variables through", {
  skip_on_cran()

  env <- c("PORCELAIN_TEST_ENV" = "porcelain_test_value")

  create <- function() {
    api <- porcelain::porcelain$new()
    api$handle(
      porcelain::porcelain_endpoint$new(
        "GET", "/", function(name) jsonlite::unbox(Sys.getenv(name)),
        porcelain::porcelain_input_query(name = "string"),
        returning = porcelain::porcelain_returning_json()))
    api
  }

  bg <- porcelain_background$new(create, env = env)
  bg$start()
  r <- bg$request("GET", "/", query = list(name = names(env)))

  expect_equal(r$status_code, 200)
  expect_mapequal(
    jsonlite::fromJSON(httr::content(r, encoding = "UTF-8", as = "text")),
    list(status = "success", errors = NULL, data = "porcelain_test_value"))
})


test_that("pass along environment variables", {
  withr::with_envvar(
    c(PORCELAIN_VALIDATE = "true"),
    expect_equal(background_env(NULL)[["PORCELAIN_VALIDATE"]],
                 "true"))
  withr::with_envvar(
    c(PORCELAIN_VALIDATE = "false"),
    expect_equal(background_env(NULL)[["PORCELAIN_VALIDATE"]],
                 "false"))
  expect_equal(background_env(c("KEY" = "value"))[["KEY"]],
               "value")
})
