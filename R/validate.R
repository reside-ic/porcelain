porcelain_validate <- function(json, validator, query) {
  ## TODO: do something more helpful with an error here; ideally
  ## we'll throw with all the data and then either restart or
  ## trycatch our way out of it.
  rethrow <- function(e) {
    class(e) <- c("porcelain_validation_error", class(e))
    e$json <- json
    stop(e)
  }
  tryCatch(validator(json, query = query, error = TRUE),
           validation_error = rethrow)
  invisible(NULL)
}


## TODO: make somewhat conditional on package load - we'll use an
## environment variable to also require it in tests.
porcelain_validator <- function(schema, root, query) {
  if (is.null(schema)) {
    return(function(...) NULL)
  }
  force(query)
  path_schema <- find_schema(schema, root)
  v <- jsonvalidate::json_validator(path_schema, "ajv")
  function(json) {
    porcelain_validate(json, v, query)
    invisible(json)
  }
}


## Given a schema name, we will take 'name', 'name.json' or
## 'name.schema.json' in decreasing order of preference, falling back
## on 'name' if none are found. The fallback behaviour allows inlining
## schemas
find_schema <- function(name, path) {
  filename <- file.path(path, paste0(name, c("", ".json", ".schema.json")))
  exists <- file.exists(filename)
  filename[[if (any(exists)) which(exists)[[1L]] else 1L]]
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
## whole porcelain object.
## nolint end
schema_root <- function(root) {
  if (is.environment(root)) {
    package <- utils::packageName(root)
    path_package <- system.file(package = package, mustWork = TRUE)
    ## TODO: could allow this path to be customised by letting
    ## packages include this in DESCRIPTION as Config/porcelain/schema
    ## perhaps
    root <- file.path(path_package, "schema")
  }
  assert_is_directory(root)
  normalizePath(root, mustWork = TRUE)
}


## Ugly name...
porcelain_validate_default <- function(value) {
  if (is.null(value)) {
    value <- tolower(Sys.getenv("PORCELAIN_VALIDATE", "")) == "true"
  }
  value
}
