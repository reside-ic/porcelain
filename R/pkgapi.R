pkgapi <- R6::R6Class(
  "pkgapi",
  inherit = plumber::plumber,

  public = list(
    initialize = function(...) {
      super$initialize(...)
      self$setErrorHandler(pkgapi_error_handler)
    },

    ## TODO: this ignores the 'preempt' arg - because the underlying
    ## logic of the super method uses missing() it's not
    ## straightforward to wrap.
    ##
    ## TODO: the plumber::PlumberEndpoint class could be used so that
    ## we might get plumber information here using the schema data;
    ## once this is working we'll replace this bit of code
    handle = function(endpoint) {
      stopifnot(inherits(endpoint, "pkgapi_endpoint"))

      ## endpoint <- plumber::PlumberEndpoint$new(
      ##   methods, path, endpoint$plumber, private$envir,
      ##   serializer = pkgapi_serialize_pass)
      ## super$handle(endpoint = endpoint)
      super$handle(endpoint$methods, endpoint$path, endpoint$plumber,
                   serializer = pkgapi_serialize_pass)
    }
  ))


pkgapi_response <- function(status_code, content_type, body, ...) {
  ret <- list(status_code = status_code,
              content_type = content_type,
              body = body,
              ...)
  class(ret) <- "pkgapi_response"
  ret
}


pkgapi_serialize_pass <- function(val, req, res, error_handler) {
  tryCatch(pkgapi_do_serialize_pass(val, res),
           error = function(e) error_handler(req, res, e))
}


pkgapi_do_serialize_pass <- function(val, res) {
  res$setHeader("Content-Type", val$content_type)
  if (val$content_type == "application/json") {
    res$body <- as.character(val$body)
  } else {
    res$body <- val$body
  }
  res$status <- val$status_code %||% 200L
  res$toResponse()
}


pkgapi_error_handler <- function(req, res, e) {
  val <- pkgapi_process_error(e, FALSE)
  pkgapi_serialize_pass(val, req, res, function(...) {})
}


## Standard response types
response_success <- function(value) {
  list(status = jsonlite::unbox("success"), errors = NULL, data = value)
}


response_failure <- function(errors) {
  list(status = jsonlite::unbox("failure"), errors = errors, data = NULL)
}
