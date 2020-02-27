## TODO: if we validate errors this needs to change slightly, and
## possibly move this function as a method in the endpoints with
## appropriate defaults
pkgapi_validate <- function(json, validator, validate) {
  if (validate) {
    ## TODO: do something more helpful with an error here; ideally
    ## we'll throw with all the data and then either restart or
    ## trycatch our way out of it.
    rethrow <- function(e) {
      class(e) <- c("pkgapi_validation_error", class(e))
      e$json <- json
      stop(e)
    }
    tryCatch(validator(json, query = "data", error = TRUE),
             validation_error = rethrow)
  }
  invisible(NULL)
}


## TODO: make somewhat conditional on package load - we'll use an
## environment variable to also require it in tests.
pkgapi_validator <- function(schema, root) {
  path_schema <- file.path(root, paste0(schema, ".json"))
  jsonvalidate::json_validator(path_schema, "ajv")
}


schema_root <- function(root, handler) {
  if (is.null(root)) {
    package <- utils::packageName(environment(handler))
    root <- system_file("schema", package = package)
  } else {
    assert_is_directory(root)
    root <- normalizePath(root, mustWork = TRUE)
  }
  root
}
