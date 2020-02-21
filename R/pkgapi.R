pkgapi <- R6::R6Class(
  "pkgapi",
  inherit = plumber::plumber,

  private = list(
    root = NULL
  ),

  public = list(
    ## initialize = function(...) {
    ##   super$initialize(...)
    ## },
    ## TODO: this ignores the 'preempt' arg - because the underlying
    ## logic of the super method uses missing() it's not
    ## straightforward to wrap.
    ##
    ## TODO: the plumber::PlumberEndpoint class could be used so that
    ## we might get plumber information here using the schema data.
    handle = function(methods, path, handler) {
      stopifnot(inherits(handler, "pkgapi_endpoint"))
      ## endpoint <- plumber::PlumberEndpoint$new(
      ##   methods, path, handler, private$envir,
      ##   serializer = pkgapi_serialize_pass)
      ## super$handle(endpoint = endpoint)
      super$handle(methods, path, handler$wrapped,
                   serializer = pkgapi_serialize_pass)
    }
  ))


pkgapi_endpoint_json <- function(handler, schema, root = NULL) {
  if (is.null(root)) {
    package <- utils::packageName(environment(handler))
    root <- system_file("schema", package = package)
  }
  validator <- pkgapi_validator(schema, root)
  force(handler)
  wrapped <- function(req, res, ..., validate = TRUE) {
    ## TODO: should use tryCatch here to do some automatic failure
    ## handling.
    data <- handler(...)
    value <- response_success(data)
    json <- to_json(value)
    ret <- list(data = data,
                value = value,
                body = to_json(value),
                content_type = "application/json")
    pkgapi_validate(ret, validator, validate)
  }
  ret <- list(handler = handler,
              wrapped = wrapped,
              returns = "json",
              schema = schema)
  class(ret) <- "pkgapi_endpoint"
  ret
}


pkgapi_endpoint_binary <- function(handler) {
  force(handler)
  wrapped <- function(req, res, ..., validate = TRUE) {
    data <- handler(...)
    stopifnot(is.raw(data))
    list(body = data,
         content_type = "application/octet-stream")
  }
  ret <- list(handler = handler,
              wrapped = wrapped,
              returns = "binary")
  class(ret) <- "pkgapi_endpoint"
  ret
}


## Wrap our most common serialise style
pkgapi_serialize_pass <- function(val, req, res, errorHandler) {
  tryCatch({
    res$setHeader("Content-Type", val$content_type)
    if (val$content_type == "application/json") {
      res$body <- as.character(val$body)
    } else {
      res$body <- val$body
    }
    return(res$toResponse())
  }, error = function(e) {
    errorHandler(req, res, e)
  })
}


pkgapi_validate <- function(result, validator, validate) {
  if (result$value$success && validate) {
    ## TODO: do something more helpful with an error here; ideally
    ## we'll throw with all the data and then either restart or
    ## trycatch our way out of it.
    rethrow <- function(e) {
      class(e) <- c("pkgapi_validation_error", class(e))
      e$result <- result
      stop(e)
    }
    tryCatch(validator(result$body, query = "data", error = TRUE),
             validation_error = rethrow)
  }
  result
}


## TODO: make somewhat conditional on package load - we'll use an
## environment variable to also require it in tests.
pkgapi_validator <- function(schema, root) {
  path_schema <- file.path(root, paste0(schema, ".json"))
  jsonvalidate::json_validator(path_schema, "ajv")
}


## Standard response type
response_success <- function(value) {
  list(success = jsonlite::unbox(TRUE), errors = NULL, data = value)
}


## This should probably be tuneable?
to_json <- function(x) {
  jsonlite::toJSON(x, json_verbatim = TRUE, na = "null", null = "null")
}
