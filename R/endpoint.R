##' @title Basic endpoint object
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
    ##' @field inputs Input control
    inputs = NULL,
    ##' @field returning An \code{\link{pkgapi_returning}} object
    ##' controlling the return type (content type, status code,
    ##' serialisation and validation information).
    returning = NULL,

    ##' @description Create an endpoint
    ##'
    ##' @param method The HTTP method to support
    ##'
    ##' @param path The server path for the endpoint
    ##'
    ##' @param target An R function to run as the endpoint
    ##'
    ##' @param returning Information about what the endpoint returns,
    ##    as created by \code{\link{pkgapi_returning}}
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
    initialize = function(method, path, target, returning,
                          input_query = NULL, input_body = NULL,
                          validate = FALSE) {
      self$method <- method
      self$path <- path
      self$target <- target
      assert_is(returning, "pkgapi_returning")
      self$returning <- returning

      ## TODO: Assert HTTP/REST compliance on presence of body

      ## This is only part of the problem here: we need the metadata
      ## stored somewhere too; things like "required" don't make it
      ## out of here but should and will require a little tweaking.
      self$inputs <- pkgapi_inputs_init(path, input_query, input_body,
                                        formals(target))

      self$validate <- validate
      lock_bindings(c("method", "path", "target", "returning"), self)
    },

    ##' @description Run the endpoint.  This will produce a
    ##' standardised response object that contains \code{status_code},
    ##' \code{content_type}, \code{body} (the serialised output as run
    ##' through the \code{process} method and returned by plumber) and
    ##' \code{data} (the result of running the target function)
    ##'
    ##' @param ... Arguments passed through to the \code{target} function
    run = function(...) {
      tryCatch({
        data <- self$target(...)
        body <- self$returning$process(data)
        if (self$validate) {
          self$returning$validate(body)
        }
        pkgapi_response(self$returning$status_code,
                        self$returning$content_type, body, data = data)
      }, error = pkgapi_process_error)
    },

    ##' @description Helper method for use with plumber - not designed
    ##' for end-user use.
    ##'
    ##' @param req,res Conventional plumber request/response objects
    ##' @param ... Additional arguments passed through to \code{run}
    plumber = function(req, res, ...) {
      ## TODO: It's not abundantly clear here what we do to get the
      ## args out, but this works at least:
      pkgapi_path <- req$args[seq_len(length(req$args) - 2L)]
      tryCatch({
        args <- self$inputs(pkgapi_path, req$pkgapi_query, req$pkgapi_body)
        do.call(self$run, args)
      }, error = pkgapi_process_error)
    }
  ))
