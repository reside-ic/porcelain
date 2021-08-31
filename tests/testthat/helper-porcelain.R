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
  logger <- lgr::get_logger(paste0("porcelain/tests/", name), reset = TRUE)
  logger$set_propagate(FALSE)
  logger$add_appender(lgr::AppenderJson$new(tmp), name = "json")
  logger$set_threshold("all")
  reg.finalizer(logger, function(e) unlink(tmp))
  logger
}


test_logger_read <- function(logger) {
  lapply(readLines(logger$appenders$json$destination), jsonlite::fromJSON,
         simplifyDataFrame = FALSE)
}
