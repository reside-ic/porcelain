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

  private = list(
    validate = NULL
  ),

  ##' @description Create a pkgapi object
  public = list(
    ##'
    ##' @param ... Parameters passed to \code{\link{plumber}}
    ##'
    ##' @param validate Logical, indicating if any validation
    ##'   (implemented by the \code{validate_response} argument) should
    ##'   be enabled.  This should be set to \code{FALSE} in production
    ##'   environments.  By default (if \code{validate} is \code{NULL}),
    ##'   we look at the value of the environment \code{PKGAPI_VALIDATE}
    ##'   - if \code{true} (case insensitive) then we will validate.
    ##'   This is intended to support easy use of validation on
    ##'   continuous integration systems.
    initialize = function(..., validate = FALSE) {
      ## NOTE: it's not totally clear what the correct environment
      ## here is.
      super$initialize(NULL, pkgapi_filters(), new.env(parent = .GlobalEnv))
      private$validate <- validate
      self$setErrorHandler(pkgapi_error_handler)
      self$set404Handler(pkgapi_404_handler)
    },

    ##' @description Handle an endpoint
    ##'
    ##' @param ... Either a single argument, being a
    ##'   \code{\link{pkgapi_endpoint}} object representing an endpoint, or
    ##'  arguments to pass through to \code{plumber}.
    handle = function(...) {
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
      if (inherits(..1, "pkgapi_endpoint")) {
        if (...length() > 1L) {
          stop("If first argument is a 'pkgapi_endpoint' no others allowed")
        }
        super$handle(endpoint = ..1$create(private$envir, private$validate))
      } else {
        super$handle(...)
      }
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
    ##'
    ##' @param body Optional body (only valid with \code{PUT}, \code{POST},
    ##' etc).
    ##'
    ##' @param content_type Optional content type (mime) which can be
    ##' provided alongside \code{body}.  If not provided it is set to
    ##' \code{application/octet-stream} if \code{body} is raw, or
    ##' \code{application/json} otherwise.
    request = function(method, path, query = NULL, body = NULL,
                       content_type = NULL) {
      plumber_request(self, method, path, query, body = body,
                      content_type = content_type)
    }
  ))


pkgapi_response <- function(status_code, content_type, body, headers, ...) {
  ret <- list(status_code = status_code,
              content_type = content_type,
              body = body,
              headers = headers,
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
  if (!is.null(val$headers)) {
    for (header in names(val$headers)) {
      ## TODO: What do we want to do if a header already exists?
      ## Looks like adding another one like Content-Type here ends up with
      ## 2 Content-Type headers
      res$setHeader(header, val$headers[[header]])
    }
  }
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


## This causes a proper fight with plumber as it bypasses all our
## serialisers and error handlers in hard to deal with ways.
pkgapi_404_handler <- function(req, res) {
  e <- pkgapi_error_object(c("NOT_FOUND" = "Resource not found"), 404L)
  val <- pkgapi_process_error(e)
  res$status <- 404
  val$value$data <- jsonlite::unbox(NA)
  val$value
}


## Standard response types
response_success <- function(data) {
  list(status = jsonlite::unbox("success"), errors = NULL, data = data)
}


response_failure <- function(errors) {
  list(status = jsonlite::unbox("failure"), errors = errors, data = NULL)
}
