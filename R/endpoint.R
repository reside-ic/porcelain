##' @title Basic endpoint objects
##'
##' @description Create a \code{pkgapi_endpoint} object that collects
##'   together an HTTP method (e.g., \code{GET}), a path (e.g.,
##'   \code{/path}) and a target R function.  Unlike plumber
##'   endpoints, pkgapi endpoints are meant to be used in testing.
##'
##' @export
pkgapi_endpoint <- R6::R6Class(
  "pkgapi_endpoint",
  cloneable = FALSE,

  public = list(
    method = NULL,
    path = NULL,
    target = NULL,
    validate = NULL,
    content_type = NULL,

    ##' @description Create an endpoint
    ##'
    ##' @param method The HTTP method to support
    ##'
    ##' @param path The server path for the endpoint
    ##'
    ##' @param target An R function to run as the endpoint
    ##'
    ##' @param validate Logical, indicating if any validation
    ##' (implemented by a subclass) should be enabled.  This should be
    ##' set to \code{FALSE} in production environments.
    initialize = function(method, path, target, validate = FALSE) {
      self$method <- method
      self$path <- path
      self$target <- target
      self$validate <- validate
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
        body <- self$process(data)
        if (self$validate) {
          self$validate_response(body)
        }
        pkgapi_response(200, self$content_type, body, data = data)
      }, error = pkgapi_process_error)
    },

    ##' @description Process the data (result of \code{target}),
    ##' including all serialisation.
    ##'
    ##' @param data Data to process (the result of \code{target})
    process = function(data) {
      data
    },

    ##' @description Validate that the body conforms to the schema
    ##'
    ##' @param body The serialised body as returned by \code{process}
    validate_response = function(body) {
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


pkgapi_endpoint_json <- R6::R6Class(
  "pkgapi_endpoint_json",
  inherit = pkgapi_endpoint,

  public = list(
    content_type = "application/json",
    schema = NULL,
    validator = NULL,

    initialize = function(method, path, target, schema = NULL, root = NULL,
                          validate = FALSE) {
      super$initialize(method, path, target, validate)
      self$schema <- schema
      self$validator <- pkgapi_validator(schema, schema_root(root, target))
    },

    process = function(data) {
      to_json_string(response_success(data))
    },

    validate_response = function(body) {
      pkgapi_validate(body, self$validator)
    }
  ))


pkgapi_endpoint_binary <- R6::R6Class(
  "pkgapi_endpoint_binary",
  inherit = pkgapi_endpoint,

  public = list(
    content_type = "application/octet-stream",

    validate_response = function(body) {
      assert_is(body, "raw")
    }
  ))
