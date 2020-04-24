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
  system_file("schema/response-failure.schema.json", package = "pkgapi"),
  engine = "ajv")
validator_response_success <- jsonvalidate::json_validator(
  system_file("schema/response-success.schema.json", package = "pkgapi"),
  engine = "ajv")
