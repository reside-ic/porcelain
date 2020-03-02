##' @title A \code{pkgapi} object
##'
##' @description A \code{pkgapi} object.  This extends (via
##'   inheritance) a plumber object, and so only changes to the
##'   plumber API are documented here.
##'
##' @export
pkgapi <- R6::R6Class(
  "pkgapi",
  inherit = plumber::plumber,

  public = list(
    ##' @description Create a pkgapi object
    ##'
    ##' @param ... Parameters passed to \code{\link{plumber}}
    initialize = function(...) {
      ## NOTE: it's not totally clear what the correct environment
      ## here is.
      super$initialize(NULL, pkgapi_filters(), new.env(parent = .GlobalEnv))
      self$setErrorHandler(pkgapi_error_handler)
    },

    ##' @description Handle an endpoint
    ##'
    ##' @param endpoint A \code{\link{pkgapi_endpoint}} object representing
    ##' an endpoint.  Unlike plumber, an R function will \emph{not} work.
    handle = function(endpoint) {
      ## NOTE: this ignores the 'preempt' arg - because the underlying
      ## logic of the super method uses missing() it's not
      ## straightforward to wrap.
      ##
      ## NOTE: This uses private$envir, which is probably not ideal,
      ## but looks fairly uncontroversial and we could have
      ## intercepted it earlier.  It's not totally clear what this
      ## does though...
      ##
      ## NOTE: We could use a different method here rather than
      ## overloading handle, as to add plain plumber endpoints.
      assert_is(endpoint, "pkgapi_endpoint")
      endpoint <- plumber::PlumberEndpoint$new(
        endpoint$method, endpoint$path, endpoint$plumber, private$envir,
        serializer = pkgapi_serialize_pass)
      super$handle(endpoint = endpoint)
      invisible(self)
    },

    ##' @description Send a request to plumber for debugging
    ##'
    ##' Sends a request to plumber so that the API can be easily
    ##' tested without running the whole API. The interface here will
    ##' probably change, and may end up using the interface of \code{httr}.
    ##'
    ##' @param method Name of HTTP method to use (e.g., \code{GET})
    ##'
    ##' @param path Path to send the request to
    ##'
    ##' @param query Optional query parameters as a named list or
    ##' character vector.
    request = function(method, path, query = NULL) {
      plumber_request(self, method, path, query)
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
  val <- pkgapi_process_error(e)
  pkgapi_serialize_pass(val, req, res, function(...) NULL)
}


## Standard response types
response_success <- function(data) {
  list(status = jsonlite::unbox("success"), errors = NULL, data = data)
}


response_failure <- function(errors) {
  list(status = jsonlite::unbox("failure"), errors = errors, data = NULL)
}
