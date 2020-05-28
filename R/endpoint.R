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
    ##' @field state Possibly mutable state
    state = NULL,
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
    ##'    as created by \code{\link{pkgapi_returning}}
    ##'
    ##' @param validate Logical, indicating if any validation
    ##'   (implemented by the \code{validate_response} argument) should
    ##'   be enabled.  This should be set to \code{FALSE} in production
    ##'   environments.  By default (if \code{validate} is \code{NULL}),
    ##'   we look at the value of the environment \code{PKGAPI_VALIDATE}
    ##'   - if \code{true} (case insensitive) then we will validate.
    ##'   This is intended to support easy use of validation on
    ##'   continuous integration systems.
    ##'
    ##' @param ... Additional parameters, currently representing
    ##' \emph{inputs}.  You can use the functions
    ##' \code{\link{pkgapi_input_query}},
    ##' \code{\link{pkgapi_input_body_binary}} and
    ##' \code{\link{pkgapi_input_body_json}} to define inputs and pass
    ##' them into this method.  The names used must match those in
    ##' \code{target}.
    ##'
    ##' @param validate_response Optional function that throws an error
    ##' of the processed body is "invalid".
    initialize = function(method, path, target, ..., returning,
                          validate = NULL) {
      self$method <- method
      self$path <- path
      self$target <- target
      assert_is(returning, "pkgapi_returning")
      self$returning <- returning

      other <- list(...)
      done <- logical(length(other))

      ## There are a number of ways of achieving this, but by far the
      ## simplest is to create a closure that binds the appropriate
      ## arguments:
      is_state <- vlapply(other, inherits, "pkgapi_state")
      if (any(is_state)) {
        ## It's possible that this can be relaxed (to allow multiple
        ## state arguments to be added) but that feels pretty weird so
        ## just error.
        if (sum(is_state) != 1L) {
          stop("Only one 'pkgapi_state' can be passed to an endpoint")
        }
        ## This alters the target function by binding state into it.
        ## One downside of this is that if an input tries to work with
        ## an argument that is already bound to state then the error
        ## is just "this is not an argument" rather than "this
        ## argument is already bound as state" which is not ideal.
        self$target <- other[[which(is_state)]]$bind(self$target)
        done[is_state] <- TRUE
      }

      input_classes <- c("pkgapi_input", "pkgapi_input_collection")
      is_input <- vlapply(other, inherits, input_classes)
      self$inputs <- pkgapi_inputs$new(
        c(pkgapi_input_path(path), other[is_input]))$bind(self$target)
      done[is_input] <- TRUE

      if (any(!done)) {
        ## NOTE: this is really hard to get a great error message out
        ## of, as these arguments could be anywhere.  httr does not do
        ## a much better job (httr::GET("https://example.com", 1)) but
        ## it would be nice to be able to guide the user here.
        err <- other[!done]
        nms <- names(err) %||% rep("", length(err))
        nms[!nzchar(nms)] <- "unnamed argument"
        cl <- vcapply(err, function(x) paste(class(x), collapse = "/"))
        stop("Unconsumed dot arguments: ",
             paste(sprintf("%s (%s)", cl, nms), collapse = ", "),
             call. = FALSE)
      }

      self$validate <- pkgapi_validate_default(validate)
      lock_bindings(
        c("method", "path", "target", "inputs", "state", "returning"),
        self)
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
        ## Remove headers attribute as we don't want to serialize this in
        ## the response
        attr(body, "headers") <- NULL
        if (self$validate) {
          self$returning$validate(body)
        }
        pkgapi_response(self$returning$status_code,
                        self$returning$content_type, body, data = data,
                        headers = get_pkgapi_headers(data))
      }, error = pkgapi_process_error)
    },

    ##' @description Test the endpoint.  This creates a full plumber
    ##' object and serves one request to the endpoint.  Argument are as
    ##' passed through to \code{\link{pkgapi}}'s \code{$request()}
    ##' method, except that \code{method} and \code{path} are
    ##' automatically taken from the endpoint itself.
    ##'
    ##' @param ... Arguments passed through to the \code{request} method
    ##'   (\code{query}, \code{body} and \code{content_type}).
    request = function(...) {
      pkgapi$new()$handle(self)$request(self$method, self$path, ...)
    },

    ##' @description Helper method for use with plumber - not designed
    ##' for end-user use.  This is what gets called by plumber when the
    ##' endpoint recieves a request.
    ##'
    ##' @param req,res Conventional plumber request/response objects
    ##' @param ... Additional arguments passed through to \code{run}
    plumber = function(req, res, ...) {
      ## It's not abundantly clear here what we do to get the path
      ## args, and they cannot be retrieved from the filters it seems.
      tryCatch({
        given <- list(
          path = req$args[seq_len(length(req$args) - 2L)],
          query = req$pkgapi_query,
          body = req$pkgapi_body)
        args <- self$inputs$validate(given)
        do.call(self$run, args)
      }, error = pkgapi_process_error)
    },

    ##' @description Create a plumber endpoint
    ##'
    ##' @param envir Environment as used by plumber (currently unclear)
    ##'
    ##' @param validate Logical, allowing override of validation at the api
    ##'   level.  This takes precedence over the value set when creating the
    ##'   endpoint.
    create = function(envir, validate) {
      if (!identical(validate, self$validate)) {
        self <- self$clone()
        self$validate <- validate
      }
      plumber::PlumberEndpoint$new(
        self$method, self$path, self$plumber, envir,
        serializer = pkgapi_serialize_pass)
    }
  ))
