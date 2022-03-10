## TODO: required with current plumber?
options(plumber.debug = FALSE)

counter <- R6::R6Class(
  "counter",
  public = list(
    value = 0L,
    increment = function(by = 1L) {
      self$value <- self$value + by
    },
    reset = function() {
      self$value <- 0L
    }
  ))


get_error <- function(expr) {
  tryCatch(expr, error = identity)
}


from_json <- function(x) {
  jsonlite::fromJSON(x, FALSE)
}


validator_response_failure <- jsonvalidate::json_validator(
  system_file("schema/response-failure.schema.json", package = "porcelain"),
  engine = "ajv")
validator_response_success <- jsonvalidate::json_validator(
  system_file("schema/response-success.schema.json", package = "porcelain"),
  engine = "ajv")


same_path <- function(a, b) {
  normalizePath(a, "/", TRUE) == normalizePath(b, "/", TRUE)
}


test_logger <- function(name) {
  testthat::skip_if_not_installed("lgr")
  tmp <- tempfile()
  logger <- porcelain_logger("all", paste0("porcelain/tests/", name), tmp)
  reg.finalizer(logger, function(e) unlink(tmp))
  logger
}


test_logger_read <- function(logger) {
  lapply(readLines(logger$appenders$json$destination), jsonlite::fromJSON,
         simplifyDataFrame = FALSE)
}


copy_directory <- function(src, as) {
  files <- dir(src, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  dir.create(as, FALSE, TRUE)
  ok <- file.copy(files, as, recursive = TRUE)
  if (!all(ok)) {
    stop("Error copying files")
  }
}


load_minimal <- function(path) {
  testthat::skip_if_not_installed("pkgload")
  pkgload::load_all(path,
                    export_all = FALSE, attach_testthat = FALSE,
                    warn_conflicts = FALSE, quiet = TRUE)
}


source_text <- function(code, env) {
  eval(parse(text = code), env)
}


make_counter <- function() {
  e <- environment()
  e$n <- 0L
  function() {
    e$n <- e$n + 1L
    e$n
  }
}


silently <- function(expr) {
  capture.output(suppressMessages(res <- force(expr)))
  res
}


roxygen_to_env <- function(text, quiet = TRUE) {
  env <- new.env()
  blocks <- roxygen2::parse_text(text, env = env)
  roc <- porcelain::porcelain_roclet()
  if (quiet) {
    code <- silently(
      roxygen2::roclet_process(roc, blocks, env, base_path = "."))
  } else {
    code <- roxygen2::roclet_process(roc, blocks, env, base_path = ".")
 }
  source_text(code, env)
  env
}


skip_if_no_roxygen <- function() {
  testthat::skip_if_not_installed("roxygen2")
  testthat::skip_if_not_installed("pkgload")
}
