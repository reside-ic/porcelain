pkgapi_validate <- function(json, validator) {
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
  invisible(NULL)
}


## TODO: make somewhat conditional on package load - we'll use an
## environment variable to also require it in tests.
pkgapi_validator <- function(schema, root) {
  if (is.null(schema)) {
    return(function(...) NULL)
  }
  path_schema <- file.path(root, paste0(schema, ".json"))
  v <- jsonvalidate::json_validator(path_schema, "ajv")
  function(json) {
    pkgapi_validate(json, v)
  }
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
