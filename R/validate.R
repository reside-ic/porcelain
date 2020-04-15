pkgapi_validate <- function(json, validator, query) {
  ## TODO: do something more helpful with an error here; ideally
  ## we'll throw with all the data and then either restart or
  ## trycatch our way out of it.
  rethrow <- function(e) {
    class(e) <- c("pkgapi_validation_error", class(e))
    e$json <- json
    stop(e)
  }
  tryCatch(validator(json, query = query, error = TRUE),
           validation_error = rethrow)
  invisible(NULL)
}


## TODO: make somewhat conditional on package load - we'll use an
## environment variable to also require it in tests.
pkgapi_validator <- function(schema, root, query) {
  if (is.null(schema)) {
    return(function(...) NULL)
  }
  force(query)
  path_schema <- file.path(root, paste0(schema, ".json"))
  v <- jsonvalidate::json_validator(path_schema, "ajv")
  function(json) {
    pkgapi_validate(json, v, query)
    invisible(json)
  }
}


## nolint start
## If we have access to the handler here we could find its schema root
## with something like:
##
##   package <- utils::packageName(environment(handler))
##   root <- system_file("schema", package = package)
##
## but that would need harmonising with any other schema use - and
## that might want to come through the endpoint object or even the
## whole pkgapi object.
## nolint end
schema_root <- function(root) {
  assert_is_directory(root)
  normalizePath(root, mustWork = TRUE)
}


## Ugly name...
pkgapi_validate_default <- function(value) {
  if (is.null(value)) {
    value <- tolower(Sys.getenv("PKGAPI_VALIDATE", "")) == "true"
  }
  value
}
