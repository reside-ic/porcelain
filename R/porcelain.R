##' @title A \code{porcelain} object
##'
##' @description A \code{porcelain} object.  This extends (via
##'   inheritance) a plumber object, and so only changes to the
##'   plumber API are documented here.
##'
##' @export
porcelain <- R6::R6Class(
  "porcelain",
  inherit = plumber_base_class(),

  private = list(
    validate = NULL
  ),

  public = list(
    ##' @description Create a porcelain object
    ##'
    ##' @param ... Parameters passed to \code{\link{plumber}}
    ##'
    ##' @param validate Logical, indicating if any validation
    ##'   (implemented by the \code{validate_response} argument) should
    ##'   be enabled.  This should be set to \code{FALSE} in production
    ##'   environments.  By default (if \code{validate} is \code{NULL}),
    ##'   we look at the value of the environment \code{PORCELAIN_VALIDATE} -
    ##'   if \code{true} (case insensitive) then we will validate.
    ##'   This is intended to support easy use of validation on
    ##'   continuous integration systems.
    ##'
    ##' @param logger Optional logger, from the `lgr` package, perhaps
    ##'   created with [porcelain::porcelain_logger].  If given, then we
    ##'   will log at the beginning and end of the request.
    initialize = function(..., validate = FALSE, logger = NULL) {
      ## NOTE: it's not totally clear what the correct environment
      ## here is.
      super$initialize(NULL, porcelain_filters(), new.env(parent = .GlobalEnv))
      private$validate <- porcelain_validate_default(validate)
      self$setErrorHandler(porcelain_error_handler)
      self$set404Handler(porcelain_404_handler)

      if (!is.null(logger)) {
        assert_is(logger, "Logger")
        self$registerHook("postroute", porcelain_log_postroute(logger))
        self$registerHook("postserialize", porcelain_log_postserialize(logger))
      }
    },

    ##' @description Include package endpoints
    ##'
    ##' @param state A named list of state, if your package requires
    ##'   any state-binding endpoints. Typically these will be mutable
    ##'   state (database connections, job queues, or similar).  You must
    ##'   provide all states as required by the combination of all
    ##'   endpoints.
    ##'
    ##' @param package Either a package name or environment (optional,
    ##'   usually we'll find the right thing)
    include_package_endpoints = function(state = NULL, package = NULL) {
      env <- parent.frame()
      calls <- sys.calls()
      if (is.null(package)) {
        env <- parent.frame()
        package <- package_name(env)
      }
      endpoints <- package_endpoints(package)

      if (!is.null(state)) {
        assert_named(state, unique = TRUE)
      }

      for (e in endpoints) {
        self$handle(e(state, private$validate))
      }
      invisible(self)
    },

    ##' @description Handle an endpoint
    ##'
    ##' @param ... Either a single argument, being a
    ##'   \code{\link{porcelain_endpoint}} object representing an endpoint, or
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
      if (inherits(..1, "porcelain_endpoint")) {
        if (...length() > 1L) {
          stop("If first argument is a 'porcelain_endpoint' no others allowed")
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


porcelain_response <- function(status_code, content_type, body, headers,
                               validated, ...) {
  ret <- list(status_code = status_code,
              content_type = content_type,
              body = body,
              headers = headers,
              validated = validated,
              ...)
  class(ret) <- "porcelain_response"
  ret
}


porcelain_serialize_pass <- function(val, req, res, error_handler) {
  tryCatch(porcelain_do_serialize_pass(val, res),
           error = function(e) error_handler(req, res, e))
}


porcelain_do_serialize_pass <- function(val, res) {
  res$setHeader("Content-Type", val$content_type)
  if (!is.null(val$headers)) {
    for (header in names(val$headers)) {
      if (header %in% names(res$headers)) {
        stop(sprintf(paste0("Can't add header '%s' with value '%s'. ",
                            "Header already exists with value '%s'."),
                     header, val$headers[[header]], res$headers[[header]]))
      } else {
        res$setHeader(header, val$headers[[header]])
      }
    }
  }
  res$setHeader("X-Porcelain-Validated",
                tolower(as.character(val$validated %||% FALSE)))
  if (val$content_type == "application/json") {
    res$body <- as.character(val$body)
  } else {
    res$body <- val$body
  }
  res$status <- val$status_code %||% 200L
  res$toResponse()
}


porcelain_error_handler <- function(req, res, e) {
  val <- porcelain_process_error(e)
  porcelain_serialize_pass(val, req, res, function(...) NULL)
}


## This causes a proper fight with plumber as it bypasses all our
## serialisers and error handlers in hard to deal with ways.
porcelain_404_handler <- function(req, res) {
  e <- porcelain_error_object(
    list("NOT_FOUND" = list(detail = "Resource not found")), 404L)
  val <- porcelain_process_error(e)
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
