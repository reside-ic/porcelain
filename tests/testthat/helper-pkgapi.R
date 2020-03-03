## TODO: required with current plumber?
options(plumber.debug = FALSE)

get_error <- function(expr) {
  tryCatch(expr, error = identity)
}


validator_response_failure <- jsonvalidate::json_validator(
  system_file("schema/response-failure.json", package = "pkgapi"),
  engine = "ajv")
validator_response_success <- jsonvalidate::json_validator(
  system_file("schema/response-success.json", package = "pkgapi"),
  engine = "ajv")
