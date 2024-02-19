test_that("Can parse basic endpoint", {
  skip_if_no_roxygen()
  expect_mapequal(
    roxy_parse_string("GET / => json", "text", 1),
    list(method = "GET",
         path = "/",
         inputs = NULL,
         returning = list("json")))
})


test_that("Parse return type", {
  skip_if_no_roxygen()
  expect_equal(roxy_parse_returning("json"),
               list("json"))
  expect_equal(roxy_parse_returning("json()"),
               list("json"))
  expect_equal(roxy_parse_returning("json(schema)"),
               list("json", "schema"))
  expect_equal(roxy_parse_returning("json('schema')"),
               list("json", "schema"))
  expect_equal(roxy_parse_returning("json(schema, status_code = 200)"),
               list("json", "schema", status_code = 200))
  expect_error(
    roxy_parse_returning("json(schema", "myfile", 100),
    "Invalid syntax for @porcelain returning argument.*myfile:100")
  ## This would not work in practice but will throw later on during
  ## processing
  expect_equal(roxy_parse_returning("other", "myfile", 100),
               list("other"))
})


test_that("Accept inputs", {
  skip_if_no_roxygen()
  expect_mapequal(
    roxy_parse_string("GET / => json\nquery x :: int", "<text>", 1),
    list(method = "GET",
         path = "/",
         inputs = list(query = list(x = "int")),
         returning = list("json")))
  expect_mapequal(
    roxy_parse_string("GET / => json\nquery x :: int\nquery y :: double",
                      "<text>", 1),
    list(method = "GET",
         path = "/",
         inputs = list(query = list(x = "int", y = "double")),
         returning = list("json")))
  expect_mapequal(
    roxy_parse_string("POST /path => json\nquery x :: int\nbody arg :: json",
                      "<text>", 1L),
    list(method = "POST", path = "/path",
         inputs = list(query = list(x = "int"),
                       body = list(arg = list("json"))),
         returning = list("json")))
})


test_that("Require that all inputs are of known type", {
  skip_if_no_roxygen()
  err <- expect_error(
    roxy_parse_string("GET / => json\nquery x :: int\nQUERY y :: int",
                      "file.R", 10))
  expect_match(err$message, "Invalid input type")
  expect_match(err$message, "error occured at file.R:12")
})


test_that("Ensure that queries have simple types", {
  skip_if_no_roxygen()
  expect_error(
    roxy_parse_string("GET / => json\nquery x :: int(64)", "<text>", 1),
    "Expected simple expression for input 'x'")
})

test_that("Parse from roxygen block", {
  text <- paste(
    "#' @porcelain",
    "#'   GET / =>",
    "#'     json(schema)",
    "f <- function() {}",
    sep = "\n")
  block <- roxygen2::parse_text(text)
  expect_mapequal(
    block[[1]]$tags[[1]]$val,
    list(method = "GET", path = "/", inputs = NULL,
         returning = list("json", "schema")))
})


test_that("Nice error on parse failure", {
  skip_if_no_roxygen()
  text <- paste(
    "#' @porcelain",
    "#'   GET / ->",
    "#'     json(schema)",
    "f <- function() {}",
    sep = "\n")
  err <- expect_error(
    roxygen2::parse_text(text))
  expect_match(
    err$parent$message,
    "Failed to find endpoint description in @porcelain tag.*<text>:1")
})


test_that("Nice error on input parse failure", {
  skip_if_no_roxygen()
  text <- paste(
    "#' @porcelain",
    "#'   GET / =>",
    "#'     json(schema)",
    "#' query x :: int",
    "#' other",
    "f <- function() {}",
    sep = "\n")
  err <- expect_error(
    roxygen2::parse_text(text))
  expect_match(err$parent$message,
               "error occured at <text>:5", fixed = TRUE)
})


test_that("Prevent multiple @porcelain tags for one function", {
  skip_if_no_roxygen()
  text <- paste(
    "#' @porcelain GET / => json",
    "#' @porcelain GET /root => json",
    "f <- function() {}",
    sep = "\n")
  expect_error(
    roxygen_to_env(text),
    "More than one @porcelain block found for single function: <text>:1,2",
    fixed = TRUE)
})


test_that("Prevent compilation with no found tags", {
  skip_if_no_roxygen()
  text <- paste(
    "#' @export",
    "f <- function() {}",
    sep = "\n")
  expect_error(
    roxygen_to_env(text),
    "Package contains no '@porcelain' tags",
    fixed = TRUE)
})


test_that("Create roxygen endpoint with query parameters", {
  skip_if_no_roxygen()
  text <- c("#' @porcelain",
            "#'   GET /sqrt => json",
            "#'   query x :: numeric",
            "f <- function(x) {",
            "  jsonlite::unbox(sqrt(x))",
            "}")
  env <- roxygen_to_env(text)
  endpoint <- porcelain_package_endpoint(env, "GET", "/sqrt")
  expect_equal(endpoint$target(4), jsonlite::unbox(2))

  res <- endpoint$run(4)
  expect_equal(res$data, jsonlite::unbox(2))
  expect_equal(res$status_code, 200)

  api <- porcelain$new()
  api$include_package_endpoints(package = env)
  res_api <- api$request("GET", "/sqrt", query = list(x = 4))
  expect_equal(res_api$status, 200)
  expect_mapequal(from_json(res_api$body),
                  list(status = "success", errors = NULL, data = 2))
})


test_that("Create endpoint that accepts binary input", {
  skip_if_no_roxygen()
  text <- c("#' @porcelain",
            "#'   POST /path => json",
            "#'   body data :: binary",
            "f <- function(data) {",
            "  jsonlite::unbox(length(data))",
            "}")
  env <- roxygen_to_env(text)

  endpoint <- porcelain_package_endpoint(env, "POST", "/path")
  data <- raw(10)
  expect_equal(endpoint$target(data), jsonlite::unbox(10L))

  res <- endpoint$run(data)
  expect_equal(res$data, jsonlite::unbox(10L))
  expect_equal(res$status_code, 200)

  api <- porcelain$new()
  api$include_package_endpoints(package = env)
  res_api <- api$request("POST", "/path", body = data)
  expect_equal(res_api$status, 200)
  expect_mapequal(from_json(res_api$body),
                  list(status = "success", errors = NULL, data = 10))
})


test_that("prevent multiple body inputs", {
  skip_if_no_roxygen()
  text <- c("#' @porcelain",
            "#'   POST /path => json",
            "#'   body data :: json",
            "#'   body other :: json",
            "f <- function(data) {}")
  expect_error(
    roxygen_to_env(text),
    "Currently only a single body parameter supported.*<text>:1")
})


test_that("Create roxygen endpoint with state", {
  skip_if_no_roxygen()
  text <- c("#' @porcelain",
            "#'   GET /count => json",
            "#'   state counter :: counter",
            "f <- function(counter) {",
            "  jsonlite::unbox(counter())",
            "}")

  env <- roxygen_to_env(text)
  state <- list(counter = make_counter())

  endpoint <- porcelain_package_endpoint(env, "GET", "/count", state)
  expect_equal(endpoint$target(), jsonlite::unbox(1L))

  res <- endpoint$run()
  expect_equal(res$data, jsonlite::unbox(2L))
  expect_equal(res$status_code, 200)

  api <- porcelain$new()
  api$include_package_endpoints(state, package = env)
  res_api <- api$request("GET", "/count")
  expect_equal(res_api$status, 200)
  expect_mapequal(from_json(res_api$body),
                  list(status = "success", errors = NULL, data = 3))
})


test_that("process simple package", {
  skip_if_no_roxygen()

  dest <- tempfile()
  on.exit(unlink(dest, recursive = TRUE))
  copy_directory(
    system_file("examples/add2", package = "porcelain"),
    dest)

  ## roxygen uses cat() at some point while writing the namespace, so
  ## we need to capture that to prevent it bubbling out through
  ## testthat
  silently(roxygen2::roxygenise(dest))
  pkg <- load_minimal(dest)

  endpoint <- porcelain_package_endpoint("add", "GET", "/")
  expect_s3_class(endpoint, "porcelain_endpoint")
  expect_equal(endpoint$target(5, 6),
               jsonlite::unbox(11))
  res <- endpoint$run(a = 5, b = 6)
  expect_equal(res$data, jsonlite::unbox(11))

  api <- pkg$env$api()
  res_api <- api$request("GET", "/", query = list(a = 5, b = 6))
  expect_equal(res_api$status, 200)
  expect_equal(res_api$body, res$body)
})


test_that("sensible error if package does not contain roxygen endpoints", {
  skip_if_no_roxygen()
  expect_error(
    package_endpoints(new.env(parent = emptyenv())),
    "No endpoints found: input is not a package name or namespace")
  expect_error(
    package_endpoints("plumber"),
    "No endpoints found in package 'plumber'")
})


test_that("sensible error if endpoint not found in package for testing", {
  skip_if_no_roxygen()
  text <- c("#' @porcelain GET / => json",
            "f <- function() { runif(10) }")
  env <- roxygen_to_env(text)
  expect_error(
    porcelain_package_endpoint(env, "GET", "/endpoint"),
    "Did not find roxygen-based endpoint 'GET /endpoint' in package")
})


test_that("sensible error if returning type is impossible", {
  text <- c("#' @porcelain GET / => other",
            "f <- function() { runif(10) }")
  expect_error(
    roxygen_to_env(text),
    "Did not find returning function 'other'")
})


test_that("sensible error if body type is impossible", {
  skip_if_no_roxygen()
  text <- c("#' @porcelain POST / => json",
            "#'   body data :: other",
            "f <- function(data) { runif(10) }")
  expect_error(
    roxygen_to_env(text),
    "Unknown body type 'other'")
})


test_that("Control validation in endpoints", {
  skip_if_no_roxygen()
  root <- system_file("examples/add/inst/schema", package = "porcelain")
  text <- c("#' @porcelain",
            sprintf("#'   GET /echo => json('numeric', root = '%s')", root),
            "#'   query x :: numeric",
            "f <- function(x) {",
            "  jsonlite::unbox(x)",
            "}")
  env <- roxygen_to_env(text)

  e1 <- porcelain_package_endpoint(env, "GET", "/echo", validate = FALSE)
  e2 <- porcelain_package_endpoint(env, "GET", "/echo", validate = TRUE)
  expect_false(e1$validate)
  expect_true(e2$validate)

  res1 <- e1$run("x")
  expect_equal(res1$status_code, 200)
  expect_false(res1$validated)
  expect_equal(res1$data, jsonlite::unbox("x"))

  res2 <- e2$run("x")
  expect_equal(res2$status_code, 500) # This is a known bug
  expect_false(res2$validated)
  expect_null(res2$data)
  expect_equal(res2$value$errors[[1]]$error,
               jsonlite::unbox("VALIDATION_ERROR"))

  res3 <- e2$run(1)
  expect_equal(res3$status_code, 200)
  expect_true(res3$validated)
  expect_equal(res3$data, jsonlite::unbox(1))
})


test_that("refuse to overwrite code that we did not write", {
  skip_if_no_roxygen()
  tmp <- tempfile()
  on.exit(unlink(tmp))

  msg <- "Not overwriting R/porcelain.R as it was not written by porcelain"

  file.create(tmp)
  expect_error(roxy_output(letters, tmp), msg)
  writeLines("x", tmp)
  expect_error(roxy_output(letters, tmp), msg)
  expect_equal(readLines(tmp), "x")

  header <- "# Generated by porcelain: do not edit by hand"
  writeLines(c(header, "x"), tmp)
  roxy_output(letters, tmp)
  expect_equal(readLines(tmp), c(header, letters))
})


test_that("Identify block if can't find target function", {
  skip_if_no_roxygen()

  dest <- tempfile()
  on.exit(unlink(dest, recursive = TRUE))
  copy_directory(
    system_file("examples/add2", package = "porcelain"),
    dest)

  path_r <- file.path(dest, "R", "api.R")
  code <- readLines(path_r)
  i <- seq_len(grep("^add <- function", code) - 1L)

  writeLines(c(code[i], "NULL", "", code[-i]),
             path_r)
  expect_error(
    silently(roxygen2::roxygenise(dest)),
    "Could not determine endpoint target.*api\\.R:1")
})


test_that("Report errors back helpfully", {
  skip_if_no_roxygen()
  text <- c("#' @porcelain",
            "#'   GET /sqrt => json",
            "#'   query x :: numeric",
            "f <- function(y) {",
            "  jsonlite::unbox(sqrt(y))",
            "}")
  err <- expect_error(roxygen_to_env(text))
  expect_match(err$message, "Created invalid endpoint")
  expect_match(err$message,
               "Argument 'x' (used in query) missing from the target function",
               fixed = TRUE)
})
