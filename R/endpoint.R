##' @title Basic endpoint object
##'
##' @description Create a \code{pkgapi_endpoint} object that collects
##'   together an HTTP method (e.g., \code{GET}), a path (e.g.,
##'   \code{/path}) and a target R function.  Unlike plumber
##'   endpoints, pkgapi endpoints are meant to be used in testing.
##'   See \code{\link{pkgapi_endpoint_json}} and
##'   \code{\link{pkgapi_endpoint_binary}} for examples of use.
##'
##' @export
pkgapi_endpoint <- R6::R6Class(
  "pkgapi_endpoint",
  cloneable = FALSE,

  private = list(
    process = NULL,
    validate_response = NULL
  ),

  public = list(
    ##' @field method HTTP method
    method = NULL,
    ##' @field path HTTP path
    path = NULL,
    ##' @field target R function used for the endpoint
    target = NULL,
    ##' @field validate Logical, indicating if response validation is used
    validate = NULL,
    ##' @field content_type the HTTP content type for a successful response
    content_type = NULL,

    ##' @description Create an endpoint
    ##'
    ##' @param method The HTTP method to support
    ##'
    ##' @param path The server path for the endpoint
    ##'
    ##' @param target An R function to run as the endpoint
    ##'
    ##' @param content_type A string describing the content type
    ##'
    ##' @param validate Logical, indicating if any validation
    ##' (implemented by the \code{validate_response} argument) should be
    ##' enabled.  This should be set to \code{FALSE} in production
    ##' environments.
    ##'
    ##' @param process Optional processing function that will serialise
    ##' the data produced by \code{target} into an appropriate body to
    ##' be returned by plumber.
    ##'
    ##' @param validate_response Optional function that throws an error
    ##' of the processed body is "invalid".
    initialize = function(method, path, target, content_type,
                          validate = FALSE, process = NULL,
                          validate_response = NULL) {
      self$method <- method
      self$path <- path
      self$target <- target
      self$content_type <- content_type
      self$validate <- validate
      private$process <- process %||% identity
      private$validate_response <- validate_response %||% identity
      lock_bindings(c("method", "path", "target", "content_type"), self)
    },

    ##' @description Run the endpoint.  This will produce a
    ##' standardised response pbject that contains \code{status_code},
    ##' \code{content_type}, \code{body} (the serialised output as tun
    ##' through the \code{process} method and returned by plumber) and
    ##' \code{data} (the result of running the target function)
    ##'
    ##' @param ... Arguments passed through to the \code{target} function
    run = function(...) {
      tryCatch({
        data <- self$target(...)
        body <- private$process(data)
        if (self$validate) {
          private$validate_response(body)
        }
        pkgapi_response(200, self$content_type, body, data = data)
      }, error = pkgapi_process_error)
    },

    ##' @description Helper method for use with plumber - not designed
    ##' for end-user use.
    ##'
    ##' @param req,res Conventional plumber request/response objects
    ##' @param ... Additional arguments passed through to \code{run}
    plumber = function(req, res, ...) {
      self$run(...)
    }
  ))


##' Create a JSON endpoint
##'
##' Note: We look for schema files in \code{<root>/<schema>.json}.
##'
##' @title Create JSON endpoint
##'
##' @param method The HTTP method to support
##'
##' @param path The server path for the endpoint
##'
##' @param target An R function to run as the endpoint
##'
##' @param validate Logical, indicating if any validation (implemented
##'   by the \code{validate_response} argument) should be enabled.
##'   This should be set to \code{FALSE} in production environments.
##'
##' @param schema The name of the json schema to use
##'
##' @param root The root of the schema directory.  If not provided,
##'   and if \code{target} is in a package, then we'll look in that
##'   package's installed \code{schema} directory.
##'
##' @export
pkgapi_endpoint_json <- function(method, path, target, validate = FALSE,
                                 schema = NULL, root = NULL) {
  pkgapi_endpoint$new(
    method, path, target, "application/json",
    process = pkgapi_endpoint_json_process,
    validate_response = pkgapi_validator(schema, schema_root(root, target)),
    validate = validate)
}


##' Create a binary endpoint
##'
##' @inheritParams pkgapi_endpoint_json
##' @export
pkgapi_endpoint_binary <- function(method, path, target, validate = FALSE) {
  pkgapi_endpoint$new(
    method, path, target, "application/octet-stream",
    validate_response = pkgapi_endpoint_binary_validate_response,
    validate = validate)
}


pkgapi_endpoint_json_process <- function(data) {
  to_json_string(response_success(data))
}


pkgapi_endpoint_binary_validate_response <- function(body) {
  assert_is(body, "raw")
}
